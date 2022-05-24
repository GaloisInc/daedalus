{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Talos.Strategy.MemoSearch
  ( memoSearch
  , randRestartPolicy
  , randDFSPolicy
  , BacktrackPolicy(..)
  ) where

import           Control.Lens
import           Control.Monad.Reader           (runReaderT)
import           Control.Monad.State
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Writer.CPS
import           Data.Foldable                  (foldlM, toList)
import           Data.Functor                   (($>))
import           Data.Generics.Product          (field)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Map.Merge.Lazy            as Map
import           Data.Maybe                     (maybeToList)
import           Data.Monoid                    (First (First), getFirst)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set

import           GHC.Generics                   (Generic)
import           SimpleSMT                      (SExpr)
import qualified SimpleSMT                      as SMT

import           Daedalus.Core                  (Name, Typed (..), caseVar)
import           Daedalus.Core.Free             (freeVars)
import           Daedalus.PP                    (showPP)
import           Daedalus.Panic                 (panic)

import           Talos.Analysis.Exported        (ExpSlice)
import           Talos.Analysis.Slice           (Slice' (..))
import           Talos.Strategy.Monad           (LiftStrategyM, StrategyM,
                                                 isRecVar, randR)
import           Talos.Strategy.SearchTree      (Location)
import qualified Talos.Strategy.SearchTree      as ST
import           Talos.Strategy.SymbolicM       hiding (inSolver)
import           Talos.SymExec.SemiExpr         (SemiSExpr)
import           Talos.SymExec.SemiValue        (SemiValue (..))
import           Talos.SymExec.SolverT          (SMTVar, SolverContext,
                                                 SolverFrame, SolverT,
                                                 collapseContext, extendContext,
                                                 freshContext, freshSymbol,
                                                 getContext,
                                                 instantiateSolverFrame,
                                                 restoreContext, substSExpr)
import           Talos.SymExec.Type             (symExecTy)


--------------------------------------------------------------------------------
-- Memoization
--
-- The idea here is to share solutions across the search space.
-- Consider code like
--
--   x = P a
--   y = block
--     let w = G
--     Guard (p w)
--     ^ w
--   z = Q b
--   Guard (g x z)
--
-- If the guard g x z fails, then we may have to backtrack to x, and
-- then proceed forward again.  Without memoisation, this would
-- require the solver to re-discover solutions for y s.t. p y, which
-- may be expensive.
-- 
-- We associate solutions with the variables which binds them, so when
-- we have 'x = R a b c' we consult the memo table for x.  This is
-- more fine-grained than memoing at the function level, although that
-- may be simpler.
--
-- To memoize, we note that the solution for a grammar 'R a b c' is
-- dependent on the values of a, b, and c, along with any context
-- constraining these variables.  We want to share the solution to 'R
-- a b c', so the first thing we do is find solutions in the empty
-- context: this over-approximates the set of solutions, but allows us
-- to avoid over-constraining the solution through outside assertions
-- (which may come through other parts of the grammar via transitive
-- SMT constraints).
--
-- We wish to take advantage of information we have about the free
-- variables, at least the information we have in the value
-- environment (i.e., not the solver environment).  In particular,
-- knowing which sum type variant a variable is makes e.g. case much
-- simpler.  Thus, we don't want to abstract over the variables
-- entirely.  Conversely, having the SMT variables (in the value
-- environment) be fresh means when we are figuring out whether we can
-- re-use a solution we don't need to worry about overlap of variables
-- between the orignal environment (the one used to find the solution)
-- and the new environment.  Overlap may happen, for example, if we
-- are re-using a solution where only some (or no) deps have changed.
--
-- Fresh variables also means we don't need to unify SExprs.
--
-- One wrinkle is that we cannot memo recursive functions as the
-- recursive call would occur in the solution for itself.

-- Notes:
-- 
--  - Proving outside the context means we have to do a lot of
--    pushing/popping, which may be expensive (and negate the gain of
--    memoing).
--
--  - Proving outside the context means we also have to check the
--    solution after we find it, as instantiating it may further
--    constrain bound variables which leads to unsat constraints.
--
--  - In a related note, abstracting the structure of the SMT portion
--    of the environment may also lead to unsat constraints.

-- Ideas:
--   - We could ignore 'unimportant' variables, although slicing
--     should do most of that already(?)

--------------------------------------------------------------------------------
-- Unification of SemiSExprs
--
-- The idea here is that solutions are shared between different
--- environments, where the environments are unifiable at the SMTVar
-- level.  This is more general than using the index of the solution
-- for each free variable, as multiple solutions may have the same
-- value (but, e.g., different constraints).  By using unification we
-- can share solutions between solutions of free variables which have
-- the same shape.  Note that this means we also don't need a solution
-- ID for each free variable, which may not exist in e.g. recursive
-- calls where we unfold terms rather than memoize.
--
-- In addition, if we have sequential uses of a grammar like
-- 
--   ys = Many { let y = P x; Q y }
--
-- different iterations can share the same instance for y, although we
-- will need to make sure that any (new) SMT variables are renamed.

-- FIXME: we should name every computation, even terminal ones
-- (e.g. P -> x = P; ^ x).

type Unifier = Map SMTVar SExpr

emptyUnifier :: Unifier
emptyUnifier = Map.empty

-- Makes sure that we don't bind the same variable twice.  We only
-- need to worry about this if we repeat variables in the environment,
-- which currently we don't do (we replace every 'VOther' by a fresh
-- variable) --- probably we can just the Monoid inst. for maps, but
-- this is clearer.
mergeUnifiers :: Unifier -> Unifier -> Maybe Unifier
mergeUnifiers =
  Map.mergeA Map.preserveMissing Map.preserveMissing
             (Map.zipWithAMatched (\_k x y -> guard (x == y) $> x))

mergeUnifierss :: [Unifier] -> Maybe Unifier
mergeUnifierss = foldlM mergeUnifiers emptyUnifier

-- This is allowed to be incomplete, e.g. for maps.  We can also assume type safety.
unifySemiSExprs :: SemiValue SMTVar -> SemiSExpr -> Maybe Unifier
unifySemiSExprs orig new =
  case (orig, new) of
    (VValue v1, VValue v2) -> guard (v1 == v2) $> emptyUnifier
    (VOther v,  VOther s2) -> Just (Map.singleton v (typedThing s2))
    (VUnionElem l1 se1, VUnionElem l2 se2)
      | l1 == l2  -> unifySemiSExprs se1 se2
      | otherwise -> Nothing
    -- FIXME: we assume labels are in the same order      
    (VStruct fs1, VStruct fs2)             -> unifys (map snd fs1) (map snd fs2)
    (VSequence _b1 vs1, VSequence _b2 vs2) -> unifys vs1 vs2
    (VMaybe m_v1, VMaybe m_v2) -> join (unifySemiSExprs <$> m_v1 <*> m_v2)
    -- We require the keys to be in the same order, which is not
    -- strictly necessary.
    (VMap kvs1, VMap kvs2) -> unifys2 kvs1 kvs2
    (VIterator kvs1, VIterator kvs2) -> unifys2 kvs1 kvs2
    _ -> Nothing
  where
    unifys xs ys = do
      guard (length xs == length ys)
      mergeUnifierss =<< zipWithM unifySemiSExprs xs ys
    unifys2 kvs1 kvs2 =
      let (ks1, vs1) = unzip kvs1
          (ks2, vs2) = unzip kvs2
      in join (mergeUnifiers <$> unifys ks1 ks2 <*> unifys vs1 vs2)

unifyEnvs :: SolutionEnv -> SymbolicEnv -> Maybe Unifier
unifyEnvs s1 s2 = doMerge s1 s2 >>= mergeUnifierss . Map.elems
  where
    doMerge = Map.mergeA panicOnMissing Map.dropMissing
                (Map.zipWithAMatched (const unifySemiSExprs))
    panicOnMissing = Map.mapMissing (\k _ -> panic "Missing key" [showPP k])

--------------------------------------------------------------------------------
-- Instance management

-- SMT Context management
-- ======================
-- 
-- The SMT API in SolverT has the
-- freshContext/getContext/restoreContext ops.  These are used during
-- regular backtracking to decouple the solver state from where we are
-- in the grammar.  With the addition of memoisation, we need to be
-- able to generate new solutions in the solution's context, and then
-- continue in the requestors context, plus extras from the solution.
--
-- When we generate a solution, we also export the SMT context that we
-- have when the solution is found (i.e., all the bits that we need).
-- As we start with the empty context for memo'd variables, this
-- context contains only the bits from the corresponding grammar, and
-- needs to be appended to the context of the caller, and checked for
-- satisfiability (as we may make inconsistent assumptions in the
-- solution).
--
-- We do all context management here so it isn't spread through the
-- code (and so we could inspect it instead of it being implicit in
-- the monadic code).  Note that the first time we enter a memo node
-- the code _will_ set up the context.

data Solution = Solution
  { sValue    :: SemiSExpr
  , sPath     :: PathBuilder
  , sCtxFrame :: SolverFrame
  }

type SolutionEnv = Map Name (SemiValue SMTVar)

-- Proposition: Given a solution S for monad M under env E, we can get
-- a solution S' for M under E' where S' = subst t S and E and E' are
-- unified by t, where t contains SMT variables.
data MemoInstance = MemoInstance
  { miVarShape   :: SolutionEnv
  -- We could also leave the solutions (and failures) in the search
  -- tree and store a parallel search tree in the MemoTag (instead of
  -- the nseen Int).  That way we avoid just using the found solutions
  -- (although finding existing solutions might be tricky, hence this
  -- approach).
  , miSolutions  :: [Solution]
  , miUnexplored :: Maybe Location'
  }

-- This will allocate the symbols, but not add them to the solver context.
nameSExprs :: SemiSExpr ->
              WriterT (Map SMTVar (Typed SExpr)) MemoM (SemiValue (Typed SMTVar))
nameSExprs = traverse nameOne
  where
    nameOne se = do
      sym <- lift . inSolver $ freshSymbol "memo"
      tell (Map.singleton sym se)
      pure (se {typedThing = sym})

addMemoInstance :: Name -> SymbolicEnv -> SymbolicM Result ->
                   MemoM (MemoIdx, Unifier)
addMemoInstance n e m = do
  fvs   <- gets ((Map.! n) . frees)
  -- Replace all SExprs in e with fresh (SMT) variables
  (sole, vmap) <- runWriterT (traverse nameSExprs (Map.restrictKeys e fvs))
  sc <- inSolver $ freshContext (Map.toList (symExecTy . typedType <$> vmap))
  let -- turn a SolutionEnv into a SymbolicEnv (i.e., turn vars into sexprs)
      e'   = fmap (fmap (fmap SMT.const)) sole
      -- We reset the context and declare the new variables
      m'   = runReaderT (getSymbolicM m) e'
      mi   = MemoInstance { miVarShape  = fmap (fmap typedThing) sole
                          , miSolutions = []
                          , miUnexplored = Just (ST.empty (sc, m'))
                          }
  m_old <- field @"memos" %%= Map.insertLookupWithKey (\_ -> flip (<>)) n [mi]
  -- We return the index of the new MemoInstance, which is at the end
  -- of any existing instances.
  pure (maybe 0 length m_old, typedThing <$> vmap)

findMemoInstance :: Name -> SymbolicEnv -> MemoM (Maybe (MemoIdx, Unifier))
findMemoInstance n e = uses (field @"memos" . at n) (go =<<)
  where
    go = getFirst . ifoldMap (\i m -> First $ (,) i <$> unifyEnvs (miVarShape m) e)

memoIdx :: Name -> SymbolicEnv -> SymbolicM Result ->
           MemoM (MemoIdx, Unifier)
memoIdx n e m = do
  m_res <- findMemoInstance n e
  case m_res of
    Just r  -> pure r
    Nothing -> addMemoInstance n e m

-- This is the main worker: we want to generate another solution.
memoGen :: MemoInstance -> MemoM (Maybe Solution, MemoInstance)
memoGen mi
  | Just loc <- miUnexplored mi = do
      (m_r, m_loc) <- memoLocation loc
      let mk_soln (r, p) = do
            fr <- inSolver (collapseContext =<< getContext)
            pure Solution
              { sValue    = r
              , sPath     = p
              , sCtxFrame = fr
              }
      m_soln <- traverse mk_soln m_r
      let mi' = mi { miUnexplored = m_loc
                   , miSolutions  = miSolutions mi <> maybeToList m_soln
                   }
      pure (m_soln, mi')

  | otherwise = pure (Nothing, mi)

-- Gets another solution, may do a whole bunch of work to find it.
-- FIXME: parameterise by no. attempts?
-- FIXME: who is responsible for doing the check?
nextSolution :: SearchTag ->
                MemoM (SearchTag, Maybe (SolverContext, SearchT' Result))
nextSolution tag =
  case tag of
    FixedTag -> pure (tag, Nothing)
    MemoTag n idx nseen u sc m -> do
      m_mi <- preuse (field @"memos" . ix n . ix idx)
      let mi = case m_mi of
            Nothing -> panic "Missing MemoInstance" []
            Just r  -> r
      m_soln <- case miSolutions mi ^? ix nseen of
        Just soln -> pure (Just soln)
        Nothing   -> do
          (r, mi') <- memoGen mi
          field @"memos" . ix n . ix idx .= mi'
          pure r

      case m_soln of
        Nothing -> pure (tag, Nothing)
        Just soln -> do
          let tag' = MemoTag n idx (nseen + 1) u sc m
          -- Make all vars in the context fresh so we don't clash
          -- with other instances.  We also instantiate the unifier
          -- we got from matching the memoinst. env.
          (fr', u') <- inSolver $ instantiateSolverFrame u (sCtxFrame soln)
          -- We need to instantiate all the vars in the path as well.
          -- fmaps because: 
          --   PathBuilderF (SemiValue (Typed <here>))
          let path' = fmap (fmap (fmap (substSExpr u'))) <$> sPath soln
              sc'   = extendContext sc fr'
          pure (tag', Just (sc', m (sValue soln, path')))

    NestTag _n Nothing _m -> pure (tag, Nothing)
    NestTag n (Just loc) m -> do
      sc <- inSolver getContext
      (m_r, m_loc') <- memoLocation loc
      let tag' = NestTag n m_loc' m
          mk r = (sc, m r)          
      pure (tag', mk <$> m_r)

--------------------------------------------------------------------------------
-- Backtracking policy

data BacktrackPolicy = BacktrackPolicy
  { btsChoose :: Location' -> MemoM (Maybe Location')
  -- ^ Called When we see a choose node
  , btsReenter :: Location' -> MemoM (Maybe Location')
  -- ^ Called when we need to get more solutions from a memo'd var, in
  -- the parent of the last successful node.
  , btsBacktrack :: Location' -> MemoM (Maybe Location')
  -- ^ Called when we need another solutino, in the parent of the last
  -- successful node.
  }

memoChoose :: Location' -> MemoM (Maybe Location')
memoChoose loc = do
  bts <- gets policy
  btsChoose bts loc

memoBacktrack :: Location' -> MemoM (Maybe Location')
memoBacktrack loc = do
  bts <- gets policy
  btsBacktrack bts loc

memoReenter :: Location' -> MemoM (Maybe Location')
memoReenter loc = do
  bts <- gets policy
  btsReenter bts loc
  
--------------------------------------------------------------------------------
-- Monad and State

-- This is the state that is shared across the entire search space
-- (i.e., it isn't local to a path).

type MemoIdx = Int

data SearchTag =
  FixedTag
  -- ^ The only choices are the ones we have seen
  | MemoTag Name MemoIdx Int Unifier SolverContext (Result -> SearchT' Result)
  -- ^ The bound var, no. solutions we have seen, and the rest of the comp.
  | NestTag Name (Maybe Location') (Result -> SearchT' Result)
  -- ^ An inlined bind node, where the name is used for backtracking.
  -- Doing it this way means we can't backtrack outside of the nested
  -- node when exploring it (or at least, not easily).  The
  -- SolverContext is the one that is inside the Location'

-- type SearchTree' = SearchTree (SolverContext, SearchT' Result) SearchTag
type Location'   = Location   (SolverContext, SearchT' Result) SearchTag

data MemoState = MemoState
  { -- We could associate the SymbolidM here with the name, but we
    -- always have it when we need to extend memos (i.e., when we see
    -- a Bind).
    memos  :: Map Name [MemoInstance]
  , frees  :: Map Name (Set Name)
  , policy :: BacktrackPolicy
  } deriving (Generic)

newtype MemoM a = MemoM { _getMemoM :: StateT MemoState (SolverT StrategyM) a }
  deriving (Functor, Applicative, Monad, MonadState MemoState, LiftStrategyM, MonadIO)

inSolver :: SolverT StrategyM a -> MemoM a
inSolver = MemoM . lift

runMemoM :: Map Name (Set Name) -> BacktrackPolicy -> MemoM a -> SolverT StrategyM a
runMemoM fs bts (MemoM m) = evalStateT m st0
  where
    st0 = MemoState
      { memos = mempty
      , frees = fs
      , policy = bts
      }

--------------------------------------------------------------------------------
-- Top-level entry point for the memo search strat.

memoSearch :: BacktrackPolicy -> SearchStrat
memoSearch bts = SearchStrat $ \sls m -> do
  let fs = buildFreeMap sls
  sc <- getContext
  fst <$> runMemoM fs bts (memoLocation (ST.empty (sc, m)))

buildFreeMap :: [ExpSlice] -> Map Name (Set Name)
buildFreeMap = execWriter . mapM_ go
  where
    go sl = do
      let dflt = pure (freeVars sl)
      case sl of
        SHole    -> dflt
        SPure {} -> dflt
        SDo x lsl rsl -> do
          lfs <- go lsl
          rfs <- go rsl
          let fs = lfs `Set.union` Set.delete x rfs
          tell (Map.singleton x fs)
          pure fs
        SMatch {}   -> dflt
        SChoice sls -> mconcat <$> mapM go sls
        SCall {}    -> dflt
        SCase _ c   -> Set.insert (caseVar c) . mconcat <$> traverse go (toList c)

        -- We don't care about the inverse function when dealing with
        -- slice deps. (it should only be used when turning a val into
        -- bytes later on).
        SInverse n _ifn p -> pure (Set.delete n (freeVars p))
    
-- FIXME: we don't really care that we have a Location', we could
-- generalise and let the policy figure it out.
memoLocation :: Location' ->
                MemoM (Maybe Result, Maybe Location')
memoLocation loc = m_go =<< memoReenter loc
  where
    m_go = maybe (pure (Nothing, Nothing)) go

    -- Just check that we arrived at an unexplored loc.
    go loc' | ST.Unexplored r <- ST.locTree loc' = next loc' r
            | otherwise = panic "Expecting to be at an unexplored node" []

    next :: Location' -> (SolverContext, SearchT' Result) ->
            MemoM (Maybe Result, Maybe Location')
    next loc' (sc, m) = do
      r <- MemoM . lift $ do
        restoreContext sc
        runFreeT (getSearchT m)
      case r of
        Free (Choose ms) -> do
          sc' <- inSolver getContext
          let ms' = map ((,) sc' . SearchT) ms
          m_go =<< memoChoose (ST.replaceUnexplored FixedTag ms' loc')
  
        Free Backtrack {} ->
          m_go =<< maybe (pure Nothing) memoBacktrack (ST.forgetGoUp loc')

        Free (Bind n e lhs rhs) -> do
          -- We can't memo nodes with recursive calls as we would need
          -- search strats to be reentrant.
          isRec <- isRecVar n
          let rhs' = SearchT <$> rhs
          tag <- mkTag n e isRec lhs rhs' =<< inSolver getContext
          m_go =<< memoChoose (ST.replaceUnexplored tag [] loc')
        Pure r' -> pure (Just r', ST.forgetGoUp loc')

    mkTag n e isRec lhs rhs' sc'
      | isRec = do
          let m = runReaderT (getSymbolicM lhs) e
          pure $ NestTag n (Just $ ST.empty (sc', m)) rhs'
      | otherwise = do
          (i, u) <- memoIdx n e lhs
          pure (MemoTag n i 0 u sc' rhs')


--------------------------------------------------------------------------------
-- Policies

-- FIXME: move

btFindUnexplored :: BacktrackPolicy ->
                    Location' -> MemoM (Maybe Location')
btFindUnexplored pol = go
  where
    go loc | ST.Unexplored {} <- ST.locTree loc = pure (Just loc)
    go loc = do
      m_loc' <- btsChoose pol loc
      maybe (pure Nothing) go m_loc'

-- Random restart
randRestartPolicy :: BacktrackPolicy
randRestartPolicy = BacktrackPolicy
  { btsChoose     = btChooseRand randRestartPolicy
  , btsReenter    = btReenterRestart randRestartPolicy
  , btsBacktrack  = btReenterRestart randRestartPolicy
  }

btReenterRestart :: BacktrackPolicy ->
                    Location' -> MemoM (Maybe Location')
btReenterRestart pol = go . moveToRoot
  where
    -- Go to the top of the tree after forgetting the current node.
    moveToRoot = ST.maximally ST.upward

    -- Find something to do, starting at the root.
    go :: Location' -> MemoM (Maybe Location')
    go = btFindUnexplored pol

-- | 1-step choice, backtracking if the current node is empty.
btChooseRand :: BacktrackPolicy ->
                Location' -> MemoM (Maybe Location')
btChooseRand pol loc = case ST.locTree loc of
  ST.Unexplored {} -> pure (Just loc)
  ST.Node tag [] -> do
    (tag', m_r) <- nextSolution tag
    case m_r of
      Just r -> do
        -- FIXME: this is gross
        let loc' = loc { ST.locTree = ST.Node tag' [ST.Unexplored r] }
        pure (ST.downward 0 loc')

      -- No more solutions, we need to backtrack.
      Nothing -> maybe (pure Nothing) (btsBacktrack pol) (ST.forgetGoUp loc)
            
  -- Prefer existing solutions.
  ST.Node _ sts -> do
    i <- randR (0, length sts - 1)
    pure (ST.downward i loc)

-- Random DFS
randDFSPolicy :: BacktrackPolicy
randDFSPolicy = BacktrackPolicy
  { btsChoose     = btChooseRand randDFSPolicy
  , btsReenter    = btLocalBacktrack randDFSPolicy
  , btsBacktrack  = btLocalBacktrack randDFSPolicy
  }

btLocalBacktrack :: BacktrackPolicy ->
                    Location' -> MemoM (Maybe Location')
btLocalBacktrack = btFindUnexplored 

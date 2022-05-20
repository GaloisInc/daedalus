{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Talos.Strategy.MemoSearch where

import Data.Maybe (maybe)
import           Control.Monad.State
import           Control.Monad.Trans.Free
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import qualified Data.Map.Merge.Lazy       as Map

import qualified Data.Set                     as Set
import Data.Set (Set)

import           Daedalus.Core             (Name, Typed(..))

import           Talos.Strategy.Monad      (StrategyM, LiftStrategyM, isRecVar)
import           Talos.Strategy.SearchTree (Location, SearchTree)
import qualified Talos.Strategy.SearchTree as ST
import           Talos.Strategy.SymbolicM
import           Talos.SymExec.Path        (SelectedPath)
import           Talos.SymExec.SemiExpr    (SemiSExpr)
import           Talos.SymExec.SolverT     (SolverContext, SolverT, SMTVar, freshSymbol)
import Daedalus.Panic (panic)
import Control.Monad.Reader (runReaderT)
import Data.Functor (($>))
import Control.Lens
import GHC.Generics (Generic)
import Data.Generics.Product (field)
import Talos.SymExec.SemiValue (SemiValue(..))
import SimpleSMT (SExpr)
import qualified SimpleSMT as SMT

import Data.Function (on)
import Data.Foldable (foldlM)
import Daedalus.Core (Typed)
import Control.Monad.Trans.Writer.CPS
import Data.Monoid (First(First), getFirst)
import Daedalus.PP (showPP)

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
unifySemiSExprs orig new = do
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
    doMerge = Map.mergeA panicOnMissing Map.dropMissing (Map.zipWithAMatched (\_k -> unifySemiSExprs))
    panicOnMissing = Map.mapMissing (\k _ -> panic "Missing key" [showPP k])
    
--------------------------------------------------------------------------------
-- Instance management

type SolutionEnv = Map Name (SemiValue SMTVar)

-- Proposition: Given a solution S for monad M under env E, we can get
-- a solution S' for M under E' where S' = subst t S and E and E' are
-- unified by t, where t contains SMT variables.
data MemoInstance = MemoInstance
  { miVarShape   :: SolutionEnv
  , miVars       :: [Typed SMTVar]
  , miSolutions  :: [Solution]
  , miUnexplored :: Maybe Location'
  }

-- This will allocate the symbols, but not add them to the solver context.
nameSExprs :: SemiSExpr -> WriterT (Map SMTVar (Typed SExpr)) MemoM (SemiValue (Typed SMTVar))
nameSExprs = traverse nameOne
  where
    nameOne se = do
      sym <- lift . MemoM . lift $ freshSymbol "memo"
      tell (Map.singleton sym se)
      pure (se {typedThing = sym})
      
addMemoInstance :: Name -> SymbolicEnv -> SymbolicM Solution ->
                   MemoM (MemoIdx, Unifier)
addMemoInstance n e m = do
  fvs   <- gets ((Map.! n) . frees)
  (sole, vmap) <- runWriterT (traverse nameSExprs (Map.restrictKeys e fvs))
  let vars = [ se { typedThing = n' } | (n', se) <- Map.toList vmap ]
      -- turn a SolutionEnv into a SymbolicEnv
      e'   = fmap (fmap (fmap SMT.const)) sole
      m'   = runReaderT (getSymbolicM m) e'
      mi   = MemoInstance { miVarShape  = fmap (fmap typedThing) sole
                          , miVars      = vars
                          , miSolutions = []
                          , miUnexplored = Just (ST.empty m')
                          }
  m_old <- field @"memos" %%= Map.insertLookupWithKey (\_ -> flip (<>)) n [mi]
  pure (maybe 0 length m_old, typedThing <$> vmap)

findMemoInstance :: Name -> SymbolicEnv -> MemoM (Maybe (MemoIdx, Unifier))
findMemoInstance n e = uses (field @"memos" . at n) (go =<<)
  where
    go = getFirst . ifoldMap (\i m -> First $ (,) i <$> unifyEnvs (miVarShape m) e) 
  
memoIdx :: Name -> SymbolicEnv -> SymbolicM Solution ->
           MemoM (MemoIdx, Unifier)
memoIdx n e m = do
  m_res <- findMemoInstance n e
  case m_res of
    Just r  -> pure r
    Nothing -> addMemoInstance n e m

--------------------------------------------------------------------------------
-- Backtracking policy

data BacktrackStrat = BacktrackStrat
  { btsChoose :: Location' -> MemoM Location'
  -- ^ Called When we see a choose node
  , btsBacktrack :: Bool -> Location' -> MemoM (Maybe Location')
  -- ^ Called when we need another solution, the first argument is
  -- whether the current location succeeded last time around.
  }

memoChoose :: Location' -> MemoM Location'
memoChoose loc = do
  bts <- gets policy
  btsChoose bts loc
  
memoBacktrack :: Bool -> Location' -> MemoM (Maybe Location')
memoBacktrack wasFail loc = do
  bts <- gets policy
  btsBacktrack bts wasFail loc
      
--------------------------------------------------------------------------------
-- Monad and State

-- This is the state that is shared across the entire search space
-- (i.e., it isn't local to a path).

type MemoIdx = Int

data SearchTag =
  FixedTag
  -- ^ The only choices are the ones we have seen
  | MemoTag Name MemoIdx Unifier Int (Solution -> SearchT' Solution)
  -- ^ The bound var, no. solutions we have seen, and the rest of the comp.
  | NestTag Name Location' (Solution -> SearchT' Solution)
  -- ^ An inlined bind node, where the name is used for backtracking.

type SearchTree' = SearchTree (SearchT' Solution) SearchTag
type Location'   = Location   (SearchT' Solution) SearchTag

data MemoState = MemoState
  { -- We could associate the SymbolidM here with the name, but we
    -- always have it when we need to extend memos (i.e., when we see
    -- a Bind).
    memos :: Map Name [MemoInstance]
  , frees  :: Map Name (Set Name)
  , policy :: BacktrackStrat
  } deriving (Generic)

newtype MemoM a = MemoM { getMemoM :: StateT MemoState (SolverT StrategyM) a }
  deriving (Functor, Applicative, Monad, MonadState MemoState, LiftStrategyM)

-- Top-level entry point for the memo search strat.
memoSearch :: SearchStrat
memoSearch = SearchStrat $ go . ST.empty
  where
    go = undefined

memoSearchTree :: Location' ->
                  MemoM (Maybe Solution, Maybe Location')
memoSearchTree loc = m_go =<< memoBacktrack True loc
  where
    m_go = maybe (pure (Nothing, Nothing)) go

    -- Just check that we arrived at an unexplored loc.
    go loc' | ST.Unexplored m <- ST.locTree loc' = next loc' m
            | otherwise = panic "Expecting to be at an unexplored node" []

    next :: Location' -> SearchT' Solution -> MemoM (Maybe Solution, Maybe Location')
    next loc' m = do
      r <- MemoM . lift $ runFreeT (getSearchT m)
      case r of
        Free (Choose xs') ->
          go =<< memoChoose (ST.replaceUnexplored FixedTag (map SearchT xs') loc')
        Free (Backtrack {}) -> m_go =<< memoBacktrack False loc'
        Free (Bind n e lhs rhs) -> do
          -- We can't memo nodes with recursive calls as we would need
          -- search strats to be reentrant.
          isRec <- isRecVar n
          let rhs' = SearchT <$> rhs
          tag <-
            if isRec
            then pure $ NestTag n (ST.empty (runReaderT (getSymbolicM lhs) e)) rhs'
            else do
              (i, u) <- memoIdx n e lhs
              pure (MemoTag n i u 0 rhs')
          
          go =<< memoChoose (ST.replaceUnexplored tag [] loc')
        Pure s -> pure (Just s, Just loc')

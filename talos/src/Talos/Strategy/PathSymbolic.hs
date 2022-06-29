{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language OverloadedStrings #-}

-- Symbolic but the only non-symbolic path choices are those in
-- recursive functions (i.e., we only unroll loops).

-- FIXME: factor out commonalities with Symbolic.hs
module Talos.Strategy.PathSymbolic (pathSymbolicStrats) where

import           Control.Lens                 (_2, at, over, (.=))
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer         (pass)
import           Data.Bifunctor               (second)
import qualified Data.ByteString              as BS
import           Data.Generics.Product        (field)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Vector                  as Vector
import           Data.Word                    (Word8)
import           GHC.Generics                 (Generic)
import qualified SimpleSMT                    as S

import           Daedalus.Core                hiding (streamOffset)
import           Daedalus.Core.Free           (freeVars)
import qualified Daedalus.Core.Semantics.Env  as I
import qualified Daedalus.Core.Semantics.Expr as I
import           Daedalus.Core.Type
import           Daedalus.Panic
import           Daedalus.Rec                 (topoOrder)
import qualified Daedalus.Value               as I

import           Talos.Analysis.Exported      (ExpCallNode (..), ExpSlice,
                                               SliceId, sliceToCallees)
import           Talos.Analysis.Slice
import           Talos.Strategy.Monad
import           Talos.Strategy.MuxValue      (GuardedSemiSExpr,
                                               GuardedSemiSExprs, MuxValue (..),
                                               PathVar, ValueGuard (vgChoices),
                                               ValueGuardCaseInfo (..), vUnit,
                                               vgCases)
import qualified Talos.Strategy.MuxValue      as MV
import           Talos.Strategy.PathSymbolicM
import qualified Talos.SymExec.Expr           as SE
import           Talos.SymExec.Funs           (defineSliceFunDefs,
                                               defineSlicePolyFuns)
import           Talos.SymExec.ModelParser    (evalModelP, pByte, pNumber,
                                               pValue)
import           Talos.SymExec.Path
import           Talos.SymExec.SolverT        (SMTVar, SolverT, declareName,
                                               declareSymbol, liftSolver, reset,
                                               scoped)
import qualified Talos.SymExec.SolverT        as Solv
import           Talos.SymExec.StdLib
import           Talos.SymExec.Type           (defineSliceTypeDefs, symExecTy)

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

pathSymbolicStrats :: [Strategy]
pathSymbolicStrats = strats
  where
    strats = [ Strategy { stratName  = "pathsymb"
                        , stratDescr = "Symbolic execution including non-recursive paths"
                        , stratFun   = SolverStrat symbolicFun
                        }
             ]

-- ----------------------------------------------------------------------------------------
-- Main functions

-- FIXME: define all types etc. eagerly
symbolicFun :: ProvenanceTag ->
               ExpSlice ->
               SolverT StrategyM (Maybe SelectedPath)
symbolicFun ptag sl = do
  -- defined referenced types/functions
  reset -- FIXME

  md <- getModule
  deps <- sliceToDeps sl
  let slAndDeps = map snd deps ++ [sl]

  forM_ slAndDeps $ \sl' -> do
    defineSliceTypeDefs md sl'
    defineSlicePolyFuns sl'
    defineSliceFunDefs md sl'

  -- FIXME: this should be calculated once, along with how it is used
  -- by e.g. memoSearch
  scoped $ do
    let topoDeps = topoOrder (second sliceToCallees) deps
    (m_res, assns) <- runSymbolicM (sl, topoDeps) (stratSlice ptag sl)
    let go (_, pb) = do
          mapM_ Solv.assert assns
          r <- Solv.check
          case r of
            S.Unsat   -> pure Nothing
            S.Unknown -> pure Nothing
            S.Sat -> Just <$> buildPath pb
    join <$> traverse go m_res
    
-- We need to get types etc for called slices (including root slice)
sliceToDeps :: (Monad m, LiftStrategyM m) => ExpSlice -> m [(SliceId, ExpSlice)]
sliceToDeps sl = go Set.empty (sliceToCallees sl) []
  where
    go seen new acc
      | Just (n, rest) <- Set.minView new = do
          sl' <- getSlice n
          let new' = sliceToCallees sl' `Set.difference` seen
          go (Set.insert n seen) (new' `Set.union` rest) ((n, sl') : acc)

    go _ _ acc = pure acc

-- We return the symbolic value (which may contain bound variables, so
-- those have to be all at the top level) and a computation for
-- constructing the path --- we do it in this way to avoid doing a lot
-- of work that gets thrown away on backtracking, so this gets run
-- once in the final (satisfying) state.  Note that the path
-- construction code shouldn't backtrack, so it could be in (SolverT IO)

stratSlice :: ProvenanceTag -> ExpSlice -> SymbolicM Result
stratSlice ptag = go
  where
    go sl =  do
      -- liftIO (putStrLn "Slice" >> print (pp sl))
      case sl of
        SHole -> pure (vUnit, SelectedHole)

        SPure e -> do
          let mk v = (v, SelectedHole)
          mk <$> synthesiseExpr e

        SDo x lsl rsl -> do
          bindNameIn x (go lsl)
            (\lpath -> over _2 (SelectedDo lpath) <$> go rsl)

        -- FIXME: we could pick concrete values here if we are willing
        -- to branch and have a concrete bset.
        SMatch bset -> do
          sym <- liftSolver (declareSymbol "b" tByte)
          let bse = S.const sym
              bv  = MV.vOther MV.trueValueGuard (Typed TByte sym)
              
          bassn <- synthesiseByteSet bset bse
          -- This just constrains the byte, we expect it to be satisfiable
          -- (byte sets are typically not empty)
          assertSExpr bassn
          -- liftSolver check -- required?

          pure (bv, SelectedBytes ptag (ByteResult sym))

        SChoice sls -> do
          pv <- freshPathVar (length sls)
          (vs, paths) <- unzip . concat <$> zipWithM (guardedChoice pv) [0..] (map go sls)
          v <- hoistMaybe (MV.unionGuardedSemiExprs' vs)
          pure (v, SelectedChoice (SymbolicPath pv paths))

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> stratCase ptag c

        SInverse n ifn p -> do
          n' <- MV.vSExpr MV.trueValueGuard (typeOf n) =<< liftSolver (declareName n (symExecTy (typeOf n)))
          primBindName n n' $ do
            pe <- synthesiseExpr p
            ienv <- getIEnv
            assertSExpr (MV.guardedSemiSExprsToSExpr (I.tEnv ienv) TBool pe)

            -- Once we have a model, we need to convert all the free
            -- variables in the inverse function expression into values,
            -- and then call the inverse function in the _interpreter_.
            -- Note that n is free in ifn.
            -- 
            -- We could also have the solver execute the function for
            -- us.
            --
            -- We need to resolve any locally bound variables before we
            -- return the path constructor.

            let fvM = Map.fromSet id $ freeVars ifn
            -- Resolve all Names (to semisexprs and solver names)
            venv <- traverse getName fvM
            pure (n', SelectedBytes ptag (InverseResult venv ifn))
    
-- This function runs the given monadic action under the current pathc
-- extended with the give path condition.  In practice, it will run
-- the action, collect any assertions, predicate them by the path
-- condition, and also update the value.  If the computation fails,
-- then the negated path condition will be asserted and 
guardedChoice :: PathVar -> Int -> SymbolicM Result ->
                 SymbolicM [(GuardedSemiSExprs, (Int, PathBuilder))]
guardedChoice pv i m = pass $ do
  m_r <- getMaybe m
  case m_r of
    -- Refining should always succeed.
    Just (v, p)
      | Just v' <- MV.refineGuardedSemiExprs g v -> pure ([(v', (i, p))], addImpl)
    _ -> pure ([], const [S.not pathGuard])
  where
    g = MV.valueGuardExtendChoice pv i MV.trueValueGuard
    pathGuard = MV.valueGuardToSExpr g
    addImpl [] = []
    addImpl sexprs = [S.implies pathGuard (S.andMany sexprs)]
    
-- FIXME: maybe name e?
stratCase :: ProvenanceTag -> Case ExpSlice -> SymbolicM Result
stratCase ptag cs = do
  (alts, pred) <- liftSemiSolverM (MV.semiExecCase cs)
  undefined

  
  -- case m_alt of
  --   DidMatch i sl -> over _3 (SelectedCase) <$> enterPathNode (stratSlice ptag sl)
  --   NoMatch  -> do
  --     -- x <- fmap (text . flip S.ppSExpr "" . typedThing) <$> getName (caseVar cs)
  --     -- liftIO $ print ("No match for " <> pp (caseVar cs) <> " = " <> pp x $$ pp cs)
      
  --     backtrack (ConcreteFailure cids) -- just backtrack, no cases matched
      
  --   TooSymbolic -> do
  --     ps <- liftSemiSolverM (symExecToSemiExec (symExecCaseAlts cs))
  --     (cid, (i, (p, sl))) <- choose (enumerate ps)
  --     enterPathNode (Set.singleton cid) $ do
  --       assert (vSExpr TBool p)
  --       check
  --       over _3 (SelectedCase i) <$> stratSlice ptag sl

-- FIXME: this is copied from BTRand
stratCallNode :: ProvenanceTag -> ExpCallNode ->
                 SymbolicM Result
stratCallNode ptag cn = do
  -- liftIO $ print ("Entering: " <> pp cn)
  sl <- getSlice (ecnSliceId cn)
  over _2 (SelectedCall (ecnIdx cn)) <$> enterFunction (ecnParamMap cn) (stratSlice ptag sl)


-- -----------------------------------------------------------------------------
-- Merging values


-- The idea here is that we have e.g. a choose where we have a number
-- of results, depending on which path is chosen.  Where possible we
-- would like to keep values in Haskell (i.e. avoid turning everything
-- into an sexpr) so we can simplify e.g. field access.  In particular
-- we would like to avoid having to use recursive functions in the
-- solver, so we try to keep e.g. arrays in Haskell too.

-- Consider
--
-- def P = block
--   a = First
--      lA = @${'a']
--      bA = @$['A']
--   b = First
--      lB = @${'b']
--      bB = @$['B']
--   { a is lA; b is lB } | {a is bA; b is bB }
--
-- i.e., we are not allows bA, lB or lA, bB.  We could represent a as
--
--  [ (lA, ca = 0), (bA, ca = 1) ]
--
-- where ca is the choice variable for a, and similarly for b.  Now,
-- when we get to the LHS of the predicate (call the choice variable
-- cP) we will have a 'case a of lA -> ()' for the first 'is', which
-- generates the path condition
--
-- cP = 0 --> (ca = 0 /\ ca \neq 1)
--
-- where the 'ca \neq 1' is because the case is partial (any
-- constructor without a corresponding alternative is
-- negated). Likewise, we get a similar constraint for 'b is lB'.

-- Consider
--
-- def P = block
--   x = Choose
--     x1 = ^ 1
--     x2 = ^ 2
--   n = ^ case x of
--     x1 v -> v
--     x2 v -> v
--   ...
--
-- the interesting question here being what should n look like?  An
-- obvious answer is
--
--  [ (1, cx = 0), (2, cx = 1) ]
--
-- and so for 'n + n' we would have
--
-- [ (2, cx = 0 /\ cx = 0)
-- , (3, cx = 0 /\ cx = 1)
-- , (3, cx = 1 /\ cx = 0)
-- , (4, cx = 1 /\ cx = 1)
-- ]
--
-- where, if no other processing is donw, we may get large numbers of
-- infeasible values (as for 'n + n = 3' in this example).  For choice
-- variables this should be easy, but for cases over constants 'case x
-- of 1,2,3 -> ...' we can have equality between expressions and
-- literals.


-- def M_P n b i =
--   if i < n then
--     block
--       let v = P
--       let b' = push v b
--       M_P n b' (i + 1)
--   else pure b

-- def C_M_P =
--   n = UInt8
--   rs = M_P n emptyBuilder 0
--   s = Q
--   map (r in rs) (R r s)

-- def M = block
--   x = P
--   ys = Many n (Q x)
  



 --  { r = [0, 1, 2] }

-- M_P n emptyBuilder 0 { [0, 1, 2] }
-- (unfold M_P)
--
-- if i < n then
--   block
--     let v = UInt8
--     M_P n (push v b) (i + 1)
-- else pure b
-- { [0, 1, 2] | bs = [] }

-- i < n /\
--     let v1 = UInt8 /\
--       WP (if i + 1 < n then
--         block
--         let v2 = UInt8
--         M_P n (push v2 (push v1 b)) ((i + 1) + 1)
--        else pure (push v1 b), { [0, 1, 2] | bs = [] })

-- n <= i /\ b = [0, 1, 2] /\ bs = []

-- { [0, 1, 2] | bs = [] }


-- { b = [0, 1, 2] /\ i = n /\ bs = []
-- \/ (b = [1, 2] /\ i + 1 = n /\ bs = [0] )
-- \/ (b = [2] /\ i + 2 = n /\ bs = [0, 1] )
-- \/ (b = [] /\ i + 3 = n /\ bs = [0, 1, 2]) }

--   (0 + 3 = n /\ bs = [0, 1, 2])

--  ==> n = 3 /\ bs = [0, 1, 2]

-- ================================================================================
  
-- if i < n then
--   block
--     let v = UInt8
--     M_P { n = n, b = push v b', i = i' + 1 }
-- else (i >= n /\ b = [0, 1, 2]) pure b 

-- (i >= n /\ b = [0, 1, 2])

-- {  }
-- if i < n then
--   block
--     let v' = UInt8
--     { i' + 1 >= n /\ push v' b' = [0, 1, 2] } === { i' + 1 >= n /\ v' = 0 /\ b' = [1, 2] }

-- if i < n then
--   block
--     { i' + 1 >= n /\ push v' b' = [0, 1, 2] }

-- ================================================================================

-- if i < n then
--   block
--     let v = UInt8
--     M_P n (push v b) (i + 1)
-- else pure b
-- { RESULT = vs }
--
-- {i >= n /\ b = vs}
--
-- (vs = [] /\ i = 0) -- initial call was the one we wanted
--
-- { b = push v' b', i = i' + 1) (b = vs /\ (vs \neq [] \/ i \neq 0))
--
-- push v' b' = vs /\ (vs \neq [] \/ i + 1 \neq 0)
-- {\exists v'. v' = UInt8 /\ push v' b' = vs /\ i' < n } M_P n b' i'

-- case vs of
--   []      -> emit [] >> pure 0
--   v : vs' -> emit v >> n' = G vs' (i - 1) >> return (n' + 1)
--

-- def M_P n =
--  if n = 0 then pure []
--  else { let v = P; vs = M_P (n - 1); ^ v : vs }

-- { RESULT = vs }

-- def S = { $['(']; $$ = S; $[')']; } <| $[ !['(',')] ]
-- 
-- { RESULT = 'foo' }

-- ----------------------------------------------------------------------------------------
-- Solver helpers

synthesiseExpr :: Expr -> SymbolicM GuardedSemiSExprs
synthesiseExpr e = do
  liftSemiSolverM . MV.semiExecExpr $ e

-- Not a GuardedSemiSExprs here as there is no real need
synthesiseByteSet :: ByteSet -> S.SExpr -> SymbolicM S.SExpr
synthesiseByteSet bs b = liftSymExecM $ SE.symExecByteSet b bs

-- ----------------------------------------------------------------------------------------
-- Utils

enumerate :: Traversable t => t a -> t (Int, a)
enumerate t = evalState (traverse go t) 0
  where
    go a = state (\i -> ((i, a), i + 1))

-- This code uses the Monad instance for Maybe pretty heavily, sorry :/

data ModelParserState = ModelParserState
  { mpsPathVars :: Map PathVar Int
  , mpsBytes    :: Map SMTVar Word8
  , mpsOthers   :: Map SMTVar I.Value
  } deriving Generic

-- FIXME: clag
getByteVar :: SMTVar -> ModelParserM Word8
getByteVar symB = do
  m_i <- gets (Map.lookup symB . mpsBytes)
  case m_i of
    Just i -> pure i
    Nothing -> do
      sexp <- lift (Solv.getValue (S.const symB))
      n <- case evalModelP pByte sexp of
             []    -> panic "No parse" []
             b : _ -> pure (fromIntegral b)
      field @"mpsBytes" . at symB .= Just n
      pure n

getPathVar :: PathVar -> ModelParserM Int
getPathVar pv = do
  m_i <- gets (Map.lookup pv . mpsPathVars)
  case m_i of
    Just i -> pure i
    Nothing -> do
      sexp <- lift (Solv.getValue (MV.pathVarToSExpr pv))
      n <- case evalModelP pNumber sexp of
             []    -> panic "No parse" []
             b : _ -> pure (fromIntegral b)
      field @"mpsPathVars" . at pv .= Just n
      pure n

getValueVar :: Typed SMTVar -> ModelParserM I.Value
getValueVar (Typed ty x) = do
  m_v <- gets (Map.lookup x . mpsOthers)
  case m_v of
    Just v -> pure v
    Nothing -> do
      sexp <- lift (Solv.getValue (S.const x))
      v <- case evalModelP (pValue ty) sexp of
             []     -> panic "No parse" []
             v' : _ -> pure v'
      field @"mpsOthers" . at x .= Just v
      pure v

type ModelParserM a = StateT ModelParserState (SolverT StrategyM) a

runModelParserM :: ModelParserM a -> SolverT StrategyM a
runModelParserM = flip evalStateT (ModelParserState mempty mempty mempty)

-- FIXME: we could get all the sexps from the solver in a single query.
buildPath :: PathBuilder -> SolverT StrategyM SelectedPath
buildPath = runModelParserM . go
  where
    go :: PathBuilder -> ModelParserM SelectedPath
    go SelectedHole = pure SelectedHole
    go (SelectedBytes ptag r) = SelectedBytes ptag <$> resolveResult r
    go (SelectedDo l r) = SelectedDo <$> go l <*> go r
    go (SelectedChoice pib) = SelectedChoice <$> buildPathIndex pib
    go (SelectedCall i p) = SelectedCall i <$> go p
    go (SelectedCase pib) = SelectedCase <$> buildPathIndex pib

    buildPathIndex :: PathIndexBuilder PathBuilder ->
                      ModelParserM (PathIndex SelectedPath)
    buildPathIndex (ConcretePath i p) = PathIndex i <$> go p
    buildPathIndex (SymbolicPath pv ps) = do
      i <- getPathVar pv
      let p = case lookup i ps of
                Nothing  -> panic "Missing choice" []
                Just p'  -> p'
      PathIndex i <$> go p
    
    resolveResult :: SolverResult -> ModelParserM BS.ByteString
    resolveResult (ByteResult b) = BS.singleton <$> getByteVar b
    resolveResult (InverseResult env ifn) = do
      -- FIXME: we should maybe make this lazy
      m_venv <- sequence <$> traverse gsesModel env
      case m_venv of
        Nothing -> panic "Couldn't construct environment" []
        Just venv -> do
          ienv <- liftStrategy getIEnv -- for fun defns
          let ienv' = ienv { I.vEnv = venv }
          pure (I.valueToByteString (I.eval ifn ienv'))

-- short-circuiting
andM :: Monad m => [m Bool] -> m Bool
andM [] = pure True
andM (m : ms) = do
  b <- m
  if b then andM ms else pure False

-- short-circuiting
firstM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _f [] = pure Nothing
firstM f (x : xs) = do
  b <- f x
  if b then pure (Just x) else firstM f xs

vgciSatisfied :: ValueGuardCaseInfo -> I.Value -> Bool
vgciSatisfied ValueGuardCaseInfo { vgciConstraint = Left p } v =
  I.matches p v
vgciSatisfied ValueGuardCaseInfo { vgciConstraint = Right ps } v =
  not (any (flip I.matches v) (Set.toList ps))

valueGuardModel :: ValueGuard -> ModelParserM Bool
valueGuardModel vg = do
  -- We try choices first as they are more likely to fail (?)  
  choiceb <- andM [ (==) i <$> getPathVar pv
                  | (pv, i) <- Map.toList (vgChoices vg) ]
  if choiceb
    then andM [ vgciModel x vgci
              | (x, vgci) <- Map.toList (vgCases vg) ]
    else pure False
  where
    vgciModel :: SMTVar -> ValueGuardCaseInfo -> ModelParserM Bool
    vgciModel x vgci =
      vgciSatisfied vgci <$> getValueVar (Typed (vgciType vgci) x)

gseModel :: GuardedSemiSExpr -> ModelParserM (Maybe I.Value)
gseModel gse =
  case gse of
    VValue v -> pure (Just v)
    VOther x -> Just <$> getValueVar x
    VUnionElem l gses -> fmap (I.VUnionElem l) <$> gsesModel gses
    VStruct flds -> do
      let (ls, gsess) = unzip flds
      m_gsess <- sequence <$> mapM gsesModel gsess
      pure (I.VStruct . zip ls <$> m_gsess)
      
    VSequence True gsess -> 
      fmap (I.VBuilder . reverse) . sequence <$> mapM gsesModel gsess
    VSequence False gsess -> 
      fmap (I.VArray . Vector.fromList) . sequence <$> mapM gsesModel gsess
      
    VJust gses -> fmap (I.VMaybe . Just) <$> gsesModel gses
    VMap els -> do
      let (ks, vs) = unzip els
      m_kvs <- sequence <$> mapM gsesModel ks
      m_vvs <- sequence <$> mapM gsesModel ks
      pure (I.VMap . Map.fromList <$> (zip <$> m_kvs <*> m_vvs))
  
  -- We support symbolic keys, so we can't use Map here
    VIterator els -> do
      let (ks, vs) = unzip els
      m_kvs <- sequence <$> mapM gsesModel ks
      m_vvs <- sequence <$> mapM gsesModel ks
      pure (I.VIterator <$> (zip <$> m_kvs <*> m_vvs))

gsesModel :: GuardedSemiSExprs -> ModelParserM (Maybe I.Value)
gsesModel gses = join <$> (traverse (gseModel . snd) =<< firstM go els)
  where
    go = valueGuardModel . fst
    els = MV.guardedValues gses

-- valueModel :: GuardedSemiSExprs -> ModelParserM I.Value
-- valueModel :: GuardedSemiSExprs -> ModelParserM I.Value

-- valueModel (VValue v) = pure v
-- valueModel sv =
--   SE.toValue <$> traverse go sv
--   where
--     go tse = do
--       sexp <- Solv.getValue (typedThing tse)
--       case evalModelP (pValue (typedType tse)) sexp of
--         [] -> panic "No parse" []
--         v : _ -> pure v













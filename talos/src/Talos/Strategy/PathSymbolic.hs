{-# LANGUAGE RankNTypes #-}
{-# Language OverloadedStrings #-}

-- Symbolic but the only non-symbolic path choices are those in
-- recursive functions (i.e., we only unroll loops).

-- FIXME: factor out commonalities with Symbolic.hs
module Talos.Strategy.PathSymbolic (pathSymbolicStrats) where

import           Control.Lens                 (_2, _3, over, view)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor               (second)
import qualified Data.ByteString              as BS
import           Data.Foldable                (fold)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Word                    (Word8)
import qualified SimpleSMT                    as S

import           Daedalus.Core                hiding (streamOffset)
import           Daedalus.Core.Free           (FreeVars, freeVars)
import qualified Daedalus.Core.Semantics.Env  as I
import qualified Daedalus.Core.Semantics.Expr as I
import           Daedalus.Core.Type
import           Daedalus.PP                  hiding (empty)
import           Daedalus.Panic
import           Daedalus.Rec                 (forgetRecs, topoOrder)
import qualified Daedalus.Value               as I

import           Talos.Analysis.Exported      (ExpCallNode (..), ExpSlice,
                                               SliceId, sliceToCallees)
import           Talos.Analysis.Slice
import           Talos.Strategy.Monad
import           Talos.Strategy.PathSymbolicM
import           Talos.SymExec.Expr           (symExecCaseAlts)
import           Talos.SymExec.Funs           (defineSliceFunDefs,
                                               defineSlicePolyFuns)
import           Talos.SymExec.ModelParser    (evalModelP, pByte, pValue)
import           Talos.SymExec.Path
import           Talos.SymExec.SemiExpr
import           Talos.SymExec.SemiValue      as SE
import           Talos.SymExec.SolverT        (SolverT, declareName,
                                               declareSymbol, reset, scoped)
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

  -- FIXME: this should be calculated once, along with how it is used by e.g. memoSearch
  scoped $ do
    let topoDeps = topoOrder (second sliceToCallees) deps
    (_, pbuilder) <- runSymbolicM (sl, topoDeps) (stratSlice ptag sl <* inSolver Solv.check)
    buildPath pbuilder

-- FIXME: we could get all the sexps from the solver in a single query.
buildPath :: PathBuilder -> SolverT StrategyM (Maybe SelectedPath)
buildPath = undefined -- traverse resolveResult
  -- where
  --   resolveResult :: SolverResult -> SolverT StrategyM BS.ByteString
  --   resolveResult (ByteResult b) = BS.singleton <$> byteModel b
  --   resolveResult (InverseResult env ifn) = do
  --     venv <- traverse valueModel env
  --     ienv <- liftStrategy getIEnv -- for fun defns
  --     let ienv' = ienv { I.vEnv = venv }
  --     pure (I.valueToByteString (I.eval ifn ienv'))

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
          bname <- vSExpr (TUInt (TSize 8)) <$> inSolver (declareSymbol "b" tByte)
          bassn <- synthesiseByteSet bset bname
          -- This just constrains the byte, we expect it to be satisfiable
          -- (byte sets are typically not empty)
          assert bassn
          -- inSolver check -- required?

          -- FIXME: Probably not needed?  Only if we case over a byte ...
          pure (bname, SelectedBytes ptag (ByteResult bname))

        -- SAssertion e -> do
        --   se <- synthesiseExpr e
        --   assert se
        --   check
        --   pure (uncPath vUnit)

        SChoice sls -> do
          c <- freshPathVar (length sls)
          let mk i sl' = extendPath (SPEChoose c i) (go sl')
          (vs, paths) <- unzip <$> zipWithM mk [0..] sls
          pure (undefined  , SelectedChoice c paths)

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> stratCase ptag c

        SInverse n ifn p -> do
          n' <- vSExpr (typeOf n) <$> inSolver (declareName n (symExecTy (typeOf n)))
          primBindName n n' $ do
            pe <- synthesiseExpr p
            assert pe

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

-- FIXME: maybe name e?
stratCase :: ProvenanceTag -> Case ExpSlice -> SymbolicM Result
stratCase ptag cs = do
  m_alt <- liftSemiSolverM (semiExecCase cs)
  case m_alt of
    DidMatch i sl -> over _3 (SelectedCase) <$> enterPathNode (stratSlice ptag sl)
    NoMatch  -> do
      -- x <- fmap (text . flip S.ppSExpr "" . typedThing) <$> getName (caseVar cs)
      -- liftIO $ print ("No match for " <> pp (caseVar cs) <> " = " <> pp x $$ pp cs)
      
      backtrack (ConcreteFailure cids) -- just backtrack, no cases matched
      
    TooSymbolic -> do
      ps <- liftSemiSolverM (symExecToSemiExec (symExecCaseAlts cs))
      (cid, (i, (p, sl))) <- choose (enumerate ps)
      enterPathNode (Set.singleton cid) $ do
        assert (vSExpr TBool p)
        check
        over _3 (SelectedCase i) <$> stratSlice ptag sl

-- FIXME: this is copied from BTRand
stratCallNode :: ProvenanceTag -> ExpCallNode ->
                 SymbolicM Result
stratCallNode ptag cn = do
  -- liftIO $ print ("Entering: " <> pp cn)
  sl <- getSlice (ecnSliceId cn)
  over _3 (SelectedCall (ecnIdx cn)) <$> enterFunction (ecnParamMap cn) (stratSlice ptag sl)


-- -----------------------------------------------------------------------------
-- Merging values


-- The idea here is that we have e.g. a choose where we have a number
-- of results, depending on which path is chosen.  Where possible we
-- would like to keep values in Haskell (i.e. avoid turning everything
-- into an sexpr) so we can simplify e.g. field access.  In particular
-- we would like to avoid having to use recursive functions in the
-- solver, so we try to keep e.g. arrays in Haskell too.
--
-- For sum types, this gets tricky, so those are just converted to
-- sexprs (for now).  


mergeValues :: [(SymPathElement, SemiSExpr)] -> SemiSExpr
mergeValues [] = panic "Empty values" [] -- assert false?
mergeValues vs = undefined

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

synthesiseExpr :: Expr -> SymbolicM SemiSExpr
synthesiseExpr e = do
  liftSemiSolverM . semiExecExpr $ e

synthesiseByteSet :: ByteSet -> SemiSExpr -> SymbolicM SemiSExpr
synthesiseByteSet bs = liftSemiSolverM . semiExecByteSet bs

-- ----------------------------------------------------------------------------------------
-- Utils

enumerate :: Traversable t => t a -> t (Int, a)
enumerate t = evalState (traverse go t) 0
  where
    go a = state (\i -> ((i, a), i + 1))

-- THis all happens after we have finished, so we need to be a bit
-- careful about what is in scope (only thins in the current solver
-- frame, i.e., not do-bound variables.)

byteModel :: SemiSExpr -> SolverT StrategyM Word8
byteModel (VOther symB) = do
  sexp <- Solv.getValue (typedThing symB)
  case evalModelP pByte sexp of
    [] -> panic "No parse" []
    b : _ -> pure b
-- probably shouldn't happen    
byteModel sv = panic "Unimplemented" [show sv]

valueModel :: SemiSExpr -> SolverT StrategyM I.Value
valueModel (VValue v)    = pure v
valueModel sv =
  SE.toValue <$> traverse go sv
  where
    go tse = do
      sexp <- Solv.getValue (typedThing tse)
      case evalModelP (pValue (typedType tse)) sexp of
        [] -> panic "No parse" []
        v : _ -> pure v













{-# LANGUAGE RankNTypes #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.Symbolic (symbolicStrats) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor               (second)
import qualified Data.ByteString              as BS
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Data.Word                    (Word8)
import qualified SimpleSMT                    as S

import           Daedalus.Core                hiding (streamOffset)
import           Daedalus.Core.Free           (freeVars)
import qualified Daedalus.Core.Semantics.Env  as I
import qualified Daedalus.Core.Semantics.Expr as I
import           Daedalus.Core.Type
import           Daedalus.PP                  hiding (empty)
import           Daedalus.Panic
import qualified Daedalus.Value               as I

import           Talos.Analysis.Exported      (ExpCallNode (..), ExpSlice,
                                               sliceToCallees)
import           Talos.Analysis.Slice
import           Talos.Strategy.Monad
import           Talos.Strategy.SymbolicM
import           Talos.SymExec.Expr           (symExecCaseAlts)
import           Talos.SymExec.Funs           (defineSliceFunDefs,
                                               defineSlicePolyFuns)
import           Talos.SymExec.ModelParser    (evalModelP, pByte, pValue)
import           Talos.SymExec.Path
import           Talos.SymExec.SemiExpr
import           Talos.SymExec.SemiValue      as SE
import           Talos.SymExec.SolverT        (SolverT, declareName,
                                               declareSymbol, reset, scoped)
import qualified Talos.SymExec.SolverT        as Solv hiding (getName)
import           Talos.SymExec.StdLib
import           Talos.SymExec.Type           (defineSliceTypeDefs, symExecTy)

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

symbolicStrats :: [Strategy]
symbolicStrats = map mkOne strats
  where
    strats = [("dfs", dfs)]--, ("bfs", bfs), ("rand-dfs", randDFS), ("rand-restart", randRestart) ]
    mkOne :: (Doc, SearchStrat) -> Strategy
    mkOne (name, sstrat) = 
      Strategy { stratName  = "symbolic-" ++ show name
               , stratDescr = "Symbolic execution (" <> name <> ")"
               , stratFun   = SolverStrat (symbolicFun sstrat)
           }

-- ----------------------------------------------------------------------------------------
-- Main functions

-- FIXME: define all types etc. eagerly
symbolicFun :: SearchStrat ->
               ProvenanceTag ->
               ExpSlice ->
               SolverT StrategyM (Maybe SelectedPath)
symbolicFun sstrat ptag sl = do
  -- defined referenced types/functions
  reset -- FIXME

  md <- getModule
  slAndDeps <- sliceToDeps sl
  forM_ slAndDeps $ \sl' -> do
    defineSliceTypeDefs md sl'    
    defineSlicePolyFuns sl'    
    defineSliceFunDefs md sl'
    
  scoped $ runSymbolicM sstrat $ do
    (_, path) <- stratSlice ptag sl
    check -- FIXME: only required if there was no recent 'check' command
    inSolver (buildPath path)

buildPath :: PathBuilder -> SolverT StrategyM SelectedPath
buildPath = traverse resolveResult
  where
    resolveResult (ByteResult b) = BS.singleton <$> byteModel b
    resolveResult (InverseResult env ifn) = do
      venv <- traverse valueModel env
      ienv <- liftStrategy getIEnv -- for fun defns
      let ienv' = ienv { I.vEnv = venv }
      pure (I.valueToByteString (I.eval ifn ienv'))

-- We need to get types etc for called slices (including root slice)
sliceToDeps :: (Monad m, LiftStrategyM m) => ExpSlice -> m [ExpSlice]
sliceToDeps sl = go Set.empty (sliceToCallees sl) [sl]
  where
    go seen new acc
      | Just (n, rest) <- Set.minView new = do
          sl' <- getSlice n
          let new' = sliceToCallees sl' `Set.difference` seen
          go (Set.insert n seen) (new' `Set.union` rest) (sl' : acc)

    go _ _ acc = pure acc

-- We return the symbolic value (which may contain bound variables, so
-- those have to be all at the top level) and a computation for
-- constructing the path --- we do it in this way to avoid doing a lot
-- of work that gets thrown away on backtracking, so this gets run
-- once in the final (satisfying) state.  Note that the path
-- construction code shouldn't backtrack, so it could be in (SolverT IO)

stratSlice :: ProvenanceTag -> ExpSlice -> SymbolicM (SemiSExpr, PathBuilder)
stratSlice ptag = go
  where
    go sl =  do
      -- liftIO (putStrLn "Slice" >> print (pp sl))
      case sl of
        SHole -> pure (uncPath vUnit)
        
        SPure e -> uncPath <$> synthesiseExpr e
          
        SDo x lsl rsl -> do
          bindNameIn x (go lsl)
            (\lpath -> second (SelectedDo lpath) <$> go rsl)

        SMatch bset -> do
          bname <- vSExpr (TUInt (TSize 8)) <$> inSolver (declareSymbol "b" tByte)
          bassn <- synthesiseByteSet bset bname
          -- This just constrains the byte, we expect it to be satisfiable
          -- (byte sets are typically not empty)
          assert bassn
          -- inSolver check -- required?          
          pure (bname, SelectedBytes ptag (ByteResult bname))

        -- SMatch (MatchBytes _e) -> unimplemented sl -- should probably not happen?
        -- SMatch {} -> unimplemented sl

        SAssertion e -> do
          se <- synthesiseExpr e
          assert se
          check
          pure (uncPath vUnit)

        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          -- liftIO (putStrLn ("Chose " ++ show i ++ " " ++ showPP sl'))
          -- e <- ask
          -- liftIO (print [ (pp n, v) | (n, v) <- Map.toList e] )          
          second (SelectedChoice i) <$> go sl'

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> stratCase ptag c

        SInverse n ifn p -> do
          n' <- vSExpr (typeOf n) <$> inSolver (declareName n (symExecTy (typeOf n)))
          pe <- synthesiseExpr p
          assert pe
          check -- FIXME: necessary?

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

    uncPath v = (v, SelectedHole)
    
    -- unimplemented sl = panic "Unimplemented" [showPP sl]

-- FIXME: maybe name e?
stratCase :: ProvenanceTag -> Case ExpSlice -> SymbolicM (SemiSExpr, PathBuilder)
stratCase ptag cs = do
  m_alt <- liftSemiSolverM (semiExecCase cs)
  case m_alt of
    DidMatch i sl -> second (SelectedCase i) <$> stratSlice ptag sl
    NoMatch  -> backtrack Set.empty -- just backtrack, no cases matched
    TooSymbolic -> do
      ps <- liftSemiSolverM (symExecToSemiExec (symExecCaseAlts cs))
      (i, (p, sl)) <- choose (enumerate ps)
      assert (vSExpr TBool p)
      check
      second (SelectedCase i) <$> stratSlice ptag sl

-- -- Synthesise for each call
-- stratCallNode :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> CallNode FInstId -> 
--                  ReaderT I.Env m (I.Value, SelectedPath)
-- stratCallNode ptag cn = do
--   env <- ask
--   (sls, argNameMaps) <- unzip <$> callNodeToSlices cn
--   let argNameMap = mconcat argNameMaps
--       env' = env { I.vEnv = Map.compose (I.vEnv env) argNameMap }
--       (results0, asserts0) = partition fst sls
--       results = map snd results0
--       asserts = map snd asserts0
--   (_rvs, nonRes) <- unzip <$> mapM (doOne env') asserts  
--   (v, res)    <- case results of
--     [] -> pure (I.vUnit, SelectedHole)
--     _  -> doOne env' (foldl1' merge results)

--   pure (v, SelectedCall (callClass cn) (foldl' merge res nonRes))
--   where
--     doOne env' = local (const env') . stratSlice ptag

-- FIXME: this is copied from BTRand
stratCallNode :: ProvenanceTag -> ExpCallNode ->
                 SymbolicM (SemiSExpr, PathBuilder)
stratCallNode ptag cn = do
  env <- ask
  let env' = Map.compose env (ecnParamMap cn)
  sl <- getSlice (ecnSliceId cn)
  second (SelectedCall (ecnIdx cn)) <$> local (const env') (stratSlice ptag sl)
  
  -- -- define all arguments.  We need to evaluate the args in the
  -- -- current env, as in recursive calls we will have shadowing (in the SolverT env.)
  -- allUsedArgsV <- traverse synthesiseExpr allUsedArgs
  -- let binds = Map.toList allUsedArgsV
  -- flip (foldr (uncurry bindNameIn)) binds $ do
  --   (_, nonRes) <- unzip <$> mapM doOne asserts
  --   (v, res)    <- case results of
  --     [] -> pure (vUnit, pure Unconstrained)
  --     _  -> do sl <- foldl1' merge <$> mapM (getParamSlice fn cl) results -- merge slices
  --              stratSlice ptag sl

  --   pure (v, SelectedCall cl <$> (foldl' merge <$> res <*> sequence nonRes))
  -- where
  --   -- This works because 'ResultVar' is < than all over basevars
  --   (results, asserts) = partition hasResultVar (Set.toList paths)
  --   doOne ev = do
  --     sl <- getParamSlice fn cl ev
  --     stratSlice ptag sl

  --   -- FIXME: do this as a post-processing step after analysis, once and for all?
  --   allUsedParams = foldMap programVars paths
  --   allUsedArgs   = Map.restrictKeys allArgs allUsedParams

-- ----------------------------------------------------------------------------------------
-- Solver helpers

liftSemiSolverM :: SemiSolverM StrategyM a -> SymbolicM a
liftSemiSolverM m = do
  funs <- getFunDefs
  lenv <- ask
  env  <- getIEnv  
  inSolver (runSemiSolverM funs lenv env m)  

synthesiseExpr :: Expr -> SymbolicM SemiSExpr
synthesiseExpr = liftSemiSolverM . semiExecExpr

synthesiseByteSet :: ByteSet -> SemiSExpr -> SymbolicM SemiSExpr
synthesiseByteSet bs = liftSemiSolverM . semiExecByteSet bs

check :: SymbolicM ()
check = do
  r <- inSolver Solv.check
  case r of
    S.Sat     -> pure ()
    S.Unsat   -> backtrack Set.empty
    S.Unknown -> backtrack Set.empty

assert :: SemiSExpr -> SymbolicM ()
assert sv =
  case sv of
    VOther p -> inSolver (Solv.assert (typedThing p))
    VValue (I.VBool True) -> pure ()
    VValue (I.VBool False) -> backtrack Set.empty
    _ -> panic "Malformed boolean" [show sv]


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













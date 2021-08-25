{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.BackwardSymbolic (backwardSymbolicStrat) where

import Control.Monad.Reader

import Control.Monad.State
import qualified Data.ByteString as BS
import Data.List (foldl', partition, foldl1')
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Word (Word8)

import Daedalus.Panic
import Daedalus.PP hiding (empty)

import Daedalus.Core hiding (streamOffset)
import Daedalus.Core.Type

import SimpleSMT (SExpr)
import qualified SimpleSMT as S

import Talos.Analysis.EntangledVars
import Talos.Analysis.Slice
import Talos.SymExec.Path
import Talos.Strategy.Monad
import Talos.Strategy.SymbolicM

import Talos.SymExec.SolverT (SolverT, reset, scoped, defineName, declareSymbol, assert, declareName)
import qualified Talos.SymExec.SolverT as Solv
import Talos.SymExec.StdLib
import Talos.SymExec.Core
import Talos.SymExec.ModelParser (evalModelP, pByte)

import Talos.Analysis.Projection (projectE, typeToInhabitant)
import Daedalus.Rec (forgetRecs)

-- FIXME: copied from Symbolic, maybe try to merge?

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

backwardSymbolicStrat :: Strategy
backwardSymbolicStrat =
  Strategy { stratName  = "symbolic.backward"
           , stratDescr = "Symbolic execution, from the leaves inward"
           , stratFun   = SolverStrat symbolicFun
           }

-- ----------------------------------------------------------------------------------------
-- Main functions

-- FIXME: define all types etc. eagerly
symbolicFun :: ProvenanceTag -> Slice -> SolverT StrategyM (Maybe SelectedPath)
symbolicFun ptag sl = do
  -- defined referenced types/functions
  reset -- FIXME

  md <- getModule
  slAndDeps <- sliceToDeps sl
  forM_ slAndDeps $ \sl' -> do
    defineSlicePolyFuns sl'    
    defineSliceTypeDefs md sl'
    defineSliceFunDefs md sl'
    
  scoped $ runSymbolicM $ do
    pathM <- stratSlice ptag (\_ -> pure ()) sl
    check -- FIXME: only required if there was no recent 'check' command
    pathM

-- We need to get types etc for called slices (including root slice)
sliceToDeps :: (Monad m, LiftStrategyM m) => Slice -> m [Slice]
sliceToDeps sl = (:) sl <$> go Set.empty (sliceToCallees sl)
  where
    go seen new
      | Just (n, rest) <- Set.minView new = do
          sl' <- getOne n
          let new' = sliceToCallees sl' `Set.difference` seen
          go (Set.insert n seen) (new' `Set.union` rest)

    go seen _ = mapM getOne (Set.toList seen)

    getOne (f,cl,ev) = getParamSlice f cl ev

-- We return the symbolic value (which may contain bound variables, so
-- those have to be all at the top level) and a computation for
-- constructing the path --- we do it in this way to avoid doing a lot
-- of work that gets thrown away on backtracking, so this gets run
-- once in the final (satisfying) state.  Note that the path
-- construction code shouldn't backtrack, so it could be in (SolverT IO)

stratSlice :: ProvenanceTag -> (SExpr -> SymbolicM ()) -> Slice -> SymbolicM (SymbolicM SelectedPath)
stratSlice ptag = go
  where
    go :: (SExpr -> SymbolicM ()) -> Slice -> SymbolicM (SymbolicM SelectedPath)
    go def sl =  do
      -- liftIO (putStrLn "Slice" >> print (pp sl))
      case sl of
        SDontCare n sl' -> fmap (dontCare n) <$> go def sl'
        SDo m_x lsl rsl -> do
          -- name return value of the lhs
          def' <- case m_x of
            Nothing -> pure $ \_ -> pure ()
            Just x  -> do
              x' <- solverOp (declareName x (symExecTy (typeOf x)))
              pure $ \v -> solverOp (assert (S.eq x' v)) >> check
          
          rpath  <- go def  rsl
          lpath  <- go def' lsl

          pure ((pathNode . SelectedDo <$> lpath) <*> rpath)

        SUnconstrained  -> pure (pure Unconstrained)
        SLeaf sl'       -> fmap (flip pathNode Unconstrained) <$> goLeaf def sl'

    goLeaf :: (SExpr -> SymbolicM ()) -> SliceLeaf -> SymbolicM (SymbolicM SelectedNode)
    goLeaf def sl = do
      -- liftIO (putStr "Leaf: " >> print (pp sl))
      case sl of
        SPure fset e -> do
          md <- getModule
          let tyMap = Map.fromList [ (tName td, td) | td <- forgetRecs (mTypes md) ]
          v <- synthesiseExpr (projectE (Just . typeToInhabitant tyMap) fset e)
          def v
          pure uncPath

        SMatch bset -> do
          bname <- solverOp (declareSymbol "b" tByte)
          bassn <- solverOp (symExecByteSet bset bname)
          solverOp (assert bassn)
          -- solverOp check -- required?
          def bname
          pure (SelectedMatch ptag . BS.singleton <$> byteModel bname)

        -- SMatch (MatchBytes _e) -> unimplemented sl -- should probably not happen?
        -- SMatch {} -> unimplemented sl

        SAssertion (GuardAssertion e) -> do
          se <- synthesiseExpr e
          solverOp (assert se)
          check
          pure uncPath

        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          -- liftIO (putStrLn ("Chose " ++ show i)) 
          fmap (SelectedChoice i) <$> go def sl'

        SCall cn -> stratCallNode ptag def cn

        SCase _ c -> stratCase ptag def c

    uncPath = pure (SelectedDo Unconstrained)

    -- unimplemented sl = panic "Unimplemented" [showPP sl]

-- FIXME: maybe name e?
stratCase :: ProvenanceTag -> (SExpr -> SymbolicM ()) -> Case Slice -> SymbolicM (SymbolicM SelectedNode)
stratCase ptag def (Case e ps) = do
  se <- solverOp (symExec e)
  (i, (p, sl)) <- choose (enumerate ps)
  -- liftIO (putStr "Chose " >> print (pp p))

  let assn = case p of
        PBool b  -> if b then se else S.not se
        PNothing -> S.fun "is-Nothing" [se]
        PJust    -> S.fun "is-Just" [se]
        PNum n   -> S.eq se (mkLit n)
        PCon l   -> S.fun ("is-" ++ labelToField tyName l) [se]
        PAny     -> S.bool True

  solverOp (assert assn)
  check
  fmap (SelectedCase i) <$> stratSlice ptag def sl
  where
    ty = typeOf e
    mkLit n = -- FIXME: cut from Core.hs
      symExecOp0 (IntL n ty)

    tyName = case ty of
      TUser ut -> utName ut
      _        -> panic "Not a user type" [showPP ty]

-- Synthesise for each call 
stratCallNode :: ProvenanceTag -> (SExpr -> SymbolicM ()) -> CallNode -> SymbolicM (SymbolicM SelectedNode)
stratCallNode ptag def CallNode { callName = fn, callClass = cl, callAllArgs = allArgs, callPaths = paths } = do
  -- define all arguments.  We need to evaluate the args in the
  -- current env, as in recursive calls we will have shadowing (in the SolverT env.)
  allUsedArgsV <- traverse (solverOp . symExec) allUsedArgs
  Map.foldMapWithKey defineName' allUsedArgsV

  nonRes <- mapM doOne asserts
  res    <- case results of
    [] -> pure (pure Unconstrained)
    _  -> do sl <- foldl1' merge <$> mapM (getParamSlice fn cl) results -- merge slices
             stratSlice ptag def sl

  pure (SelectedCall cl <$> (foldl' merge <$> res <*> sequence nonRes))
  where
    -- This works because 'ResultVar' is < than all over basevars
    (results, asserts) = partition hasResultVar (Set.toList paths)

    defineName' x e = void $ solverOp (defineName x (symExecTy (typeOf x)) e)

    doOne ev = do
      sl <- getParamSlice fn cl ev
      stratSlice ptag (\_ -> pure ()) sl

    allUsedParams = foldMap programVars paths
    allUsedArgs   = Map.restrictKeys allArgs allUsedParams

-- ----------------------------------------------------------------------------------------
-- Strategy helpers

-- Backtracking choice + random permutation
choose :: (MonadPlus m, LiftStrategyM m) => [a] -> m a
choose bs = do
  bs' <- randPermute bs
  foldr (mplus . pure) doFail bs'
  where
    doFail = do
      -- liftStrategy (liftIO (putStrLn "No more choices"))
      mzero

-- ----------------------------------------------------------------------------------------
-- Solver helpers

synthesiseExpr :: SymExec a => a -> SymbolicM SExpr
synthesiseExpr e = solverOp (symExec e)

check :: SymbolicM ()
check = do
  r <- solverOp Solv.check
  case r of
    S.Sat     -> pure ()
    S.Unsat   -> mzero
    S.Unknown -> mzero


-- ----------------------------------------------------------------------------------------
-- Utils

enumerate :: Traversable t => t a -> t (Int, a)
enumerate t = evalState (traverse go t) 0
  where
    go a = state (\i -> ((i, a), i + 1))

solverOp :: SolverT StrategyM a -> SymbolicM a
solverOp = SymbolicM . lift

getValue :: SExpr -> SymbolicM SExpr
getValue v = solverOp (Solv.getValue v)

byteModel :: SExpr -> SymbolicM Word8
byteModel symB = do
  sexp <- getValue symB
  case evalModelP pByte sexp of
    [] -> panic "No parse" []
    b : _ -> pure b













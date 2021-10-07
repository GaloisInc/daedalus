{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.Symbolic (symbolicStrat) where

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

import Talos.SymExec.SolverT (SolverT, reset, scoped, declareSymbol, assert, declareName, getName)
import qualified Talos.SymExec.SolverT as Solv
import Talos.SymExec.StdLib
import Talos.SymExec.Core
import Talos.SymExec.ModelParser (evalModelP, pByte, pValue)

import Talos.Analysis.Projection (projectE, typeToInhabitant)
import Daedalus.Rec (forgetRecs)
import qualified Daedalus.Value as I
import Daedalus.Core.Free (freeVars)
import qualified Daedalus.Core.Semantics.Env as I
import qualified Daedalus.Core.Semantics.Expr as I

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

symbolicStrat :: Strategy
symbolicStrat =
  Strategy { stratName  = "symbolic"
           , stratDescr = "Symbolic execution"
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
    (_, pathM) <- stratSlice ptag sl
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

stratSlice :: ProvenanceTag -> Slice -> SymbolicM (SemiSExpr, SymbolicM SelectedPath)
stratSlice ptag = go
  where
    go sl =  do
      -- liftIO (putStrLn "Slice" >> print (pp sl))
      case sl of
        SDontCare n sl' -> onSlice (fmap (dontCare n)) <$> go sl'
        SDo m_x lsl rsl -> do
          (v, lpath)  <- go lsl
          -- bind name
          onSlice (\rhs -> pathNode <$> (SelectedDo <$> lpath) <*> rhs) <*>
            case m_x of
              Nothing -> go rsl
              Just x  -> bindNameIn x v (go rsl)
              
        SUnconstrained  -> pure (sUnit, pure Unconstrained)
        SLeaf sl'       -> onSlice (fmap (flip pathNode Unconstrained)) <$> goLeaf sl'

    goLeaf sl = do
      -- liftIO (putStr "Leaf: " >> print (pp sl))
      case sl of
        SPure fset e -> do
          md <- getModule
          let tyMap = Map.fromList [ (tName td, td) | td <- forgetRecs (mTypes md) ]
          
          uncPath <$> synthesiseExpr (projectE (Just . typeToInhabitant tyMap) fset e)

        SMatch bset -> do
          bname <- solverOp (declareSymbol "b" tByte)
          bassn <- solverOp (symExecByteSet bset bname)
          solverOp (assert bassn)
          -- solverOp check -- required?          
          pure (bname, SelectedBytes ptag . BS.singleton <$> byteModel bname)

        -- SMatch (MatchBytes _e) -> unimplemented sl -- should probably not happen?
        -- SMatch {} -> unimplemented sl

        SAssertion (GuardAssertion e) -> do
          se <- synthesiseExpr e
          solverOp (assert se)
          check
          pure (uncPath sUnit)

        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          -- liftIO (putStrLn ("Chose " ++ show i)) 
          onSlice (fmap (SelectedChoice i)) <$> go sl'

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> stratCase ptag c

        SInverse n ifn p -> do
          n' <- solverOp (declareName n (symExecTy (typeOf n)))
          pe <- synthesiseExpr p
          solverOp (assert pe)
          check -- FIXME: necessary?

          -- Once we have a model, we need to convert all the free
          -- variables in the inverse function expression into values,
          -- and then call the inverse function in the _interpreter_.
          -- Note that n is free in ifn.
          -- 
          -- We could also have the solver execute the function for
          -- us.
          
          pure (n', SelectedBytes ptag <$> applyInverse ifn)

    uncPath v = (v, pure (SelectedDo Unconstrained))
    
    applyInverse ifn = do
      let fvM = Map.fromSet id $ freeVars ifn
          getOne n =  solverOp (getName n) >>= valueModel (typeOf n)
      env <- traverse getOne fvM
      ienv <- liftStrategy getIEnv -- for fun defns
      let ienv' = ienv { I.vEnv = env }
      pure (I.valueToByteString (I.eval ifn ienv'))

    -- unimplemented sl = panic "Unimplemented" [showPP sl]

onSlice :: (SymbolicM a -> SymbolicM b)
        -> (c, SymbolicM a) -> (c, SymbolicM b)
onSlice f = \(a, sl') -> (a, f sl')

-- FIXME: maybe name e?
stratCase :: ProvenanceTag -> Case Slice -> SymbolicM (SExpr, SymbolicM SelectedNode)
stratCase ptag (Case e ps) = do
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
  onSlice (fmap (SelectedCase i)) <$> stratSlice ptag sl
  where
    ty = typeOf e
    mkLit n = -- FIXME: cut from Core.hs
      symExecOp0 (IntL n ty)

    tyName = case ty of
      TUser ut -> utName ut
      _        -> panic "Not a user type" [showPP ty]

-- Synthesise for each call 
stratCallNode :: ProvenanceTag -> CallNode -> SymbolicM (SExpr, SymbolicM SelectedNode)
stratCallNode ptag CallNode { callName = fn, callClass = cl, callAllArgs = allArgs, callPaths = paths } = do
  -- define all arguments.  We need to evaluate the args in the
  -- current env, as in recursive calls we will have shadowing (in the SolverT env.)
  allUsedArgsV <- traverse (solverOp . symExec) allUsedArgs
  Map.foldMapWithKey defineName' allUsedArgsV

  (_, nonRes) <- unzip <$> mapM doOne asserts
  (v, res)    <- case results of
    [] -> pure (sUnit, pure Unconstrained)
    _  -> do sl <- foldl1' merge <$> mapM (getParamSlice fn cl) results -- merge slices
             stratSlice ptag sl

  pure (v, SelectedCall cl <$> (foldl' merge <$> res <*> sequence nonRes))
  where
    -- This works because 'ResultVar' is < than all over basevars
    (results, asserts) = partition hasResultVar (Set.toList paths)

    defineName' x e = void $ solverOp (defineName x (symExecTy (typeOf x)) e)

    doOne ev = do
      sl <- getParamSlice fn cl ev
      stratSlice ptag sl

    -- FIXME: do this as a post-processing step after analysis, once and for all?
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

valueModel :: Type -> SExpr -> SymbolicM I.Value
valueModel ty symV = do
  sexp <- getValue symV
  case evalModelP (pValue ty) sexp of
    [] -> panic "No parse" []
    v : _ -> pure v













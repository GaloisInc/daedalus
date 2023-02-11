{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Talos.Strategy.What4.Solver (
    Solver(..)
  , solverAdapter
  , withOnlineSolver
  , SolverSym(..)
  , SolverM(..)
  , withAssumption
  , checkSat
  , execGroundFn
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Bits ( (.|.) )
import qualified Data.Text as T
import qualified What4.Config as WC
import qualified What4.Expr as WE
import qualified What4.Interface as WI
import qualified What4.Protocol.Online as WPO
import qualified What4.ProblemFeatures as WP
import qualified What4.ProgramLoc as WI
import qualified What4.Solver as WS
import qualified What4.SatResult as W4R
import qualified What4.Expr.GroundEval as W4G
import qualified What4.Protocol.SMTWriter as W4

import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import Control.Monad.Error
import Control.Exception (AsyncException)

data Solver = CVC4
            | Yices
            | Z3
            deriving (Eq, Ord, Show, Read)

withOnlineSolver
  :: (MonadIO m, MonadMask m, sym ~ WE.ExprBuilder scope st fs, CB.IsSymInterface sym)
  => Solver
  -> Maybe FilePath
  -> WE.ExprBuilder scope st fs
  -> (forall solver. WPO.OnlineSolver solver =>
       CBO.OnlineBackend solver scope st fs -> m a)
  -> m a
withOnlineSolver solver mif sym k = do
  sym' <- liftIO $ WE.exprBuilderSplitConfig sym
  case solver of
    CVC4 -> CBO.withCVC4OnlineBackend sym' CBO.NoUnsatFeatures probFeatures
                  (\bak -> installSolverInteraction mif >> k bak)
    Yices -> CBO.withYicesOnlineBackend sym' CBO.NoUnsatFeatures probFeatures
                  (\bak -> installSolverInteraction mif >> k bak)
    Z3 -> CBO.withZ3OnlineBackend sym' CBO.NoUnsatFeatures probFeatures
                  (\bak -> installSolverInteraction mif >> k bak)

  where
    probFeatures = WP.useSymbolicArrays .|. WP.useStructs .|. WP.useBitvectors

    installSolverInteraction Nothing = return ()
    installSolverInteraction (Just interactionFile) = do
      let conf = WI.getConfiguration sym
      setSIF <- liftIO $ WC.getOptionSetting CBO.solverInteractionFile conf
      _diags <- liftIO $ WC.setOpt setSIF (T.pack interactionFile)
      return ()

solverAdapter :: (WI.IsExprBuilder sym) => sym -> Solver -> IO (WS.SolverAdapter st)
solverAdapter _sym s =
  case s of
    CVC4 -> return WS.cvc4Adapter
    Yices -> return WS.yicesAdapter
    Z3 -> return WS.z3Adapter

data SolverSym sym where
  SolverSym :: ( sym ~ WE.ExprBuilder scope st fs
              , WPO.OnlineSolver solver
              , CB.IsSymInterface sym
              )
            => sym
            -> CBO.OnlineBackend solver scope st fs
            -> SolverSym sym

class (MonadIO m, MonadCatch m, MonadMask m) => SolverM sym m | m -> sym where
  getSolverSym :: m (SolverSym sym)

withOnlineBackend ::
  SolverM sym m =>
  (forall scope st fs solver. 
    (sym ~ WE.ExprBuilder scope st fs, WPO.OnlineSolver solver, CB.IsSymInterface sym) =>
     CBO.OnlineBackend solver scope st fs -> m a) ->
  m a
withOnlineBackend f = do
  SolverSym _ bak <- getSolverSym
  f bak

withSolverProcess ::
  SolverM sym m =>
  (forall scope st fs solver.
     (sym ~ WE.ExprBuilder scope st fs) => WPO.OnlineSolver solver =>
     WPO.SolverProcess scope solver -> IO a) ->
  m a
withSolverProcess f = withOnlineBackend $ \bak -> do
  let doPanic = fail "Online solving not enabled"
  liftIO $ CBO.withSolverProcess bak doPanic $ \sp -> f sp

safePop ::
  SolverM sym m =>
  CB.FrameIdentifier ->
  CB.AssumptionState sym ->
  m ()
safePop frame st = withOnlineBackend $ \bak -> liftIO $
  catchJust filterAsync (void $ CB.popAssumptionFrame bak frame) (\_ -> CBO.restoreSolverState bak st)

withAssumption ::
  SolverM sym m =>
  WI.Pred sym ->
  m a ->
  m a
withAssumption p f = do
  bracket
    (withOnlineBackend $ \bak -> do
      st <- liftIO $ CB.saveAssumptionState bak
      frame <- liftIO $ CB.pushAssumptionFrame bak
      return (frame, st))
    (\(frame,st) -> safePop frame st) $ \_ -> 
    withOnlineBackend $ \bak -> do
      liftIO $ CB.addAssumption bak (CB.GenericAssumption WI.initializationLoc "withAssumption" p)
      f

data SymGroundEvalFn sym where
  SymGroundEvalFn :: W4G.GroundEvalFn scope -> SymGroundEvalFn (WE.ExprBuilder scope solver fs)

execGroundFn ::
  SolverM sym m =>
  SymGroundEvalFn sym  ->
  WI.SymExpr sym tp ->
  m (W4G.GroundValue tp)
execGroundFn (SymGroundEvalFn fn) e = liftIO $ W4G.groundEval fn e

filterAsync :: SomeException -> Maybe SomeException
filterAsync e
  | Just (_ :: AsyncException) <- fromException e = Nothing
  | otherwise = Just e

checkSat ::
  SolverM sym m =>
  WI.Pred sym ->
  (W4R.SatResult (SymGroundEvalFn sym) () -> m a) ->
  m a  
checkSat p f = checkSatisfiableWithModel p f >>= \case
  Left err -> throwM err
  Right a -> return a

checkSatisfiableWithModel ::
  SolverM sym m =>
  WI.Pred sym ->
  (W4R.SatResult (SymGroundEvalFn sym) () -> m a) ->
  m (Either SomeException a)
checkSatisfiableWithModel p k = withOnlineBackend $ \bak -> do
  st <-  liftIO $ CB.saveAssumptionState bak
  mres <- withSolverProcess $ \sp -> do
    WPO.push sp
    W4.assume (WPO.solverConn sp) p
    tryJust filterAsync $ do
      res <- WPO.checkAndGetModel sp "checkSatisfiableWithModel"
      W4R.traverseSatResult (\r' -> pure $ SymGroundEvalFn r') pure res
  case mres of
    Left err -> do
      _ <- liftIO $ tryJust filterAsync (CBO.restoreSolverState bak st)
      withSolverProcess $ \_ -> do
        liftIO $ CBO.restoreSolverState bak st
        return $ Left err
    Right res -> do
      fmap Right $ k res `finally`
        (catchJust filterAsync (withSolverProcess $ \sp -> WPO.pop sp) (\_ -> withSolverProcess $ \_ -> CBO.restoreSolverState bak st))
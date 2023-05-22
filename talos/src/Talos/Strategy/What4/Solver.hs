{-|
 
 Provides an abstract interface for interacting with an "Online" What4 solver process.
 It uses 'Lang.Crucible.Backend' to manage assumption frames, in particular it provides
 safe wrappers around assumption pushing/popping to ensure that throwing exceptions does
 not invalidate the assumption state of the solver.

-}

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
  , withSym
  , withOnlineSolver
  , SolverSym(..)
  , SolverM(..)
  , withAssumption
  , checkSat
  , SymGroundEvalFn
  , execGroundFn
  ) where

import           Control.Monad.Catch
import           Data.Bits ( (.|.) )
import qualified Data.Text as T
import qualified What4.Config as WC
import qualified What4.Expr as WE
import qualified What4.Interface as WI
import qualified What4.Protocol.Online as WPO
import qualified What4.ProblemFeatures as WP
import qualified What4.ProgramLoc as WI
import qualified What4.SatResult as W4R
import qualified What4.Expr.GroundEval as W4G
import qualified What4.Protocol.SMTWriter as W4

import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import Control.Monad.Except

import Control.Exception (AsyncException)

-- | Enum for selecting a solver backend
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

-- | Wraps a reference to an online solver backend for 'sym.
data SolverSym sym where
  SolverSym :: ( sym ~ WE.ExprBuilder scope st fs
              , WPO.OnlineSolver solver
              , CB.IsSymInterface sym
              )
            => sym
            -> CBO.OnlineBackend solver scope st fs
            -> SolverSym sym

-- | Defines a monad as being solver-aware. The implementation simply requires
--   that the monad be able to produce a 'SolverSym', which contains a reference
--   to the live solver connection.
--   The implicit assumption is that the same solver backend is returned
--   every time this function is run.
class (MonadIO m, MonadCatch m, MonadMask m) => SolverM sym m | m -> sym where
  getSolverSym :: m (SolverSym sym)

-- | Produces a reference to the underlying expression builder 'sym', and establishes
--   the type constraint that it is an expression builder in the given
--   continuation.
withSym :: SolverM sym m => (WI.IsSymExprBuilder sym => sym ->  m a) -> m a
withSym f = do
  SolverSym sym _ <- getSolverSym
  f sym

-- | Produces a reference on the online solver backend associated with 'sym',
--   and establishes the type constraints needed to associate them.
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

-- | Attempt to pop an assumption frame normally, but this fails
--   (i.e. due to a mismatched assumption stack), then restart
--   the solver and restore the entire state.
--   This is an attempt to repair any mismatched frames
--   that may have occurred as a result of some inner exception.
safePop ::
  SolverM sym m =>
  CB.FrameIdentifier ->
  CB.AssumptionState sym ->
  m ()
safePop frame st = withOnlineBackend $ \bak -> liftIO $
  catchJust filterAsync 
    (void $ CB.popAssumptionFrame bak frame) 
    (\_ -> CBO.restoreSolverState bak st)

-- | Push the given predicate 'p' onto the assumption stack (in a new frame)
--   before executing the given function 'f'.
--   The assumption is popped after 'f' is finished. If an exception was
--   raised during execution, it will be caught here and the assumption state
--   will be restored, and then the exception will be re-raised.
--
--   Note: If 'p' is unsatisfiable in the current assumption context, there are
--   two possibilities for the result of this function:
--
--   1) If it can be statically determined to be unsat (i.e. based just on
--   What4 simplification rules), then an 'CB.AbortExecReason' exception will be thrown.
--   2) Otherwise, the inner function will simply be executed in an assumption context
--   which assumes false.
--
--   TODO: Write a safe variant of this wrapper that always checks that the given
--   assumption is satisfiable in the current context before executing the given function.
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
  MonadIO m =>
  SymGroundEvalFn sym  ->
  WI.SymExpr sym tp ->
  m (W4G.GroundValue tp)
execGroundFn (SymGroundEvalFn fn) e = liftIO $ W4G.groundEval fn e

filterAsync :: SomeException -> Maybe SomeException
filterAsync e
  | Just (_ :: AsyncException) <- fromException e = Nothing
  | otherwise = Just e

-- | Use the online solver to check if 'p' is satisfiable in the current
--   assumption context, and pass the result to 'f'.
--   In the case of a 'W4R.Sat' result, the model that is returned
--   (i.e. as a 'SymGroundEvalFn') can be used to ground What4 expressions
--   within the 'f' continuation.
--
--   TODO: Within the continuation we should disallow 'withAssumption'
--   or 'checkSat' calls if a 'W4R.Sat' result is returned, since they
--   will invalidate the returned 'SymGroundEvalFn'.
--   One idea would be to create another type class that describes
--   an alternative solver context for grounding terms but not
--   pushing or popping assumptions.
checkSat ::
  SolverM sym m =>
  WI.Pred sym ->
  (W4R.SatResult (SymGroundEvalFn sym) () -> m a) ->
  m a  
checkSat p f = checkSatisfiableWithModel p f >>= \case
  Left err -> throwM err
  Right a -> return a

-- | Brackets the given computation with the appropriate setup and
-- teardown of the solver state in order to check the satisfiability
-- of the given predicate. Within the given continuation, the model is
-- valid and can be queried.
--
-- NOTE: Rather than simple catch/throw, this uses the 'generalBracket'
-- function from 'MonadMask' in order to handle more complicated
-- control flow (i.e. backtracking monads).
-- In particular, this expects that the implementation of 'generalBracket'
-- will itself handle the case where the inner computation aborts.
checkSatisfiableWithModel ::
  SolverM sym m =>
  WI.Pred sym ->
  (W4R.SatResult (SymGroundEvalFn sym) () -> m a) ->
  m (Either SomeException a)
checkSatisfiableWithModel p k = withOnlineBackend $ \bak -> do
  (_, r) <- generalBracket
    (do
      st <- liftIO $ CB.saveAssumptionState bak
      frame <- liftIO $ CB.pushAssumptionFrame bak
      withSolverProcess $ \sp -> do
        W4.assume (WPO.solverConn sp) p
        return (frame, st))
    (\(frame, st) exitCase -> case exitCase of
        ExitCaseSuccess (Right res) -> do
          safePop frame st
          return $ ExitCaseSuccess res
        _ -> do
          -- this is a slightly more aggressive variant of 'safePop' that
          -- attempts to handle the case where the solver process has 
          -- unexpectedly terminated
          catchJust filterAsync 
            (void $ liftIO $ CB.popAssumptionFrame bak frame) $ \_ -> do
              -- This is a small hack to work around an issue with
              -- the 'CBO.restoreSolverState' function when the solver process
              -- has crashed. We need to attempt to start it twice to properly
              -- restore the solver state.
              _ <- liftIO $ tryJust filterAsync (CBO.restoreSolverState bak st)
              withSolverProcess $ \_ -> do
                liftIO $ CBO.restoreSolverState bak st
          case exitCase of
            -- solver execution threw an error, contination was not run
            ExitCaseSuccess (Left e) -> return $ ExitCaseException e
            -- continuation was attempted, but threw an error
            ExitCaseException e -> return $ ExitCaseException e
            -- monad-specific abort condition
            ExitCaseAbort -> return ExitCaseAbort)
    (\(_frame, _st) -> do
      -- actually attempt to retrieve the model from the solver after
      -- pushing the given predicate into a fresh assumption frame
      mres <- tryJust filterAsync $ withSolverProcess $ \sp -> do
        res <- WPO.checkAndGetModel sp "checkSatisfiableWithModel"
        W4R.traverseSatResult (\r' -> pure $ SymGroundEvalFn r') pure res
      case mres of
        -- an error was thrown and caught when checking for a model, 
        -- so this is an 'ExitCaseSuccess (Left e)' result
        Left e -> return $ Left e
        -- this is the normal execution path, where the given 'k' is run
        -- in the context where 'p' is assumed
        -- if an error is thrown here, it is an 'ExitCaseException' result,
        -- otherwise it is an 'ExitCaseSuccess (Right a)' result
        Right a -> Right <$> k a)
  case r of
    ExitCaseSuccess a -> return $ Right a
    ExitCaseException e -> return $ Left e
    -- Propagate the abort via 'fail', which happens *after* the 
    -- assumption state has been restored.
    ExitCaseAbort -> liftIO $ fail "ExitCaseAbort"
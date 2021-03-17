{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- API for strategies, which say how to produce a path from a slice.

module Talos.Strategy.Monad ( Strategy(..)
                            , StratFun
                            , StrategyM, StrategyMState, emptyStrategyMState
                            , runStrategyM -- just type, not ctors
                            , LiftStrategyM (..)
                            , summaries, getGFun, withSolver
                            , rand, randR, randL, randPermute
                            , timeStrategy
                            ) where

import Control.Exception (evaluate)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import System.Random
import Data.Foldable (find)

import Control.DeepSeq (force)
import System.Clock (Clock(MonotonicRaw), getTime, diffTimeSpec, toNanoSecs)

import SimpleSMT (Solver)

import Daedalus.Core
import Daedalus.GUID
import Daedalus.Panic
import Daedalus.PP

import Talos.SymExec.Path
import Talos.Analysis.Slice
import Talos.Analysis.Monad (Summaries)

-- ----------------------------------------------------------------------------------------
-- Core datatypes

-- FIXME: add: config (e.g. depth/max backtracks/etc.)
type StratFun = ProvenanceTag -> Slice -> StrategyM (Maybe SelectedPath)

data Strategy =
  Strategy { stratName  :: String
           , stratDescr :: Doc
           , stratFun   :: StratFun
           }

-- -----------------------------------------------------------------------------
-- Monad

data StrategyMState =
  StrategyMState { stsStdGen    :: StdGen
                   -- Read only
                 , stsSolver :: Solver
                 , stsSummaries :: Summaries
                 , stsModule    :: Module
                 , stsNextGUID  :: GUID
                 }

emptyStrategyMState :: StdGen -> Solver -> Summaries -> Module -> GUID -> StrategyMState
emptyStrategyMState = StrategyMState

newtype StrategyM a =
  StrategyM { getStrategyM :: StateT StrategyMState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runStrategyM :: StrategyM a -> StrategyMState -> IO (a, StrategyMState)
runStrategyM m st = runStateT (getStrategyM m) st

-- -----------------------------------------------------------------------------
-- State access

summaries :: LiftStrategyM m => m Summaries
summaries = liftStrategy (StrategyM (gets stsSummaries))

getGFun :: LiftStrategyM m => FName -> m (Fun Grammar)
getGFun f = getFun <$> liftStrategy (StrategyM (gets stsModule))
  where
    getFun md = case find ((==) f . fName) (mGFuns md) of -- FIXME: us a map or something
      Nothing -> panic "Missing function" [showPP f]
      Just v  -> v

-- We could maybe start the solver if needed.
withSolver :: LiftStrategyM m => (Solver -> m a) -> m a
withSolver f = liftStrategy (StrategyM $ gets stsSolver) >>= f

-- -----------------------------------------------------------------------------
-- Random values

rand :: (LiftStrategyM m, Random a) => m a
rand = liftStrategy (StrategyM $ state go)
  where
    go s = let (b, g') = random (stsStdGen s) in (b, s { stsStdGen = g' })

randR :: (LiftStrategyM m, Random a) => (a, a) -> m a
randR r = liftStrategy (StrategyM $ state go)
  where
    go s = let (b, g') = randomR r (stsStdGen s) in (b, s { stsStdGen = g' })

randL :: LiftStrategyM m => [a] -> m a
randL [] = panic "randL: empty list" []
randL vs = (!!) vs <$> randR (0, length vs - 1)

randPermute :: LiftStrategyM m => [a] -> m [a]
randPermute = go
  where
    go [] = pure []
    go xs = do idx <- randR (0, length xs - 1)
               let (pfx, x : sfx) = splitAt idx xs
               (:) x <$> go (pfx ++ sfx)

-- -----------------------------------------------------------------------------
-- Timing

-- Returns the result and wall-clock time (in ns)
timeStrategy :: Strategy -> ProvenanceTag -> Slice -> StrategyM (Maybe SelectedPath, Integer)
timeStrategy f ptag sl = StrategyM $ do
  st <- get
  (res, st') <- liftIO $ do
    start     <- getTime MonotonicRaw
    (rv, st') <- runStrategyM (stratFun f ptag sl) st
    rv' <- evaluate $ force rv
    end       <- getTime MonotonicRaw
    pure ((rv', toNanoSecs (diffTimeSpec end start)), st')
  put st'
  pure res

-- -----------------------------------------------------------------------------
-- Class

class Monad m => LiftStrategyM m where
  liftStrategy :: StrategyM a -> m a

instance LiftStrategyM StrategyM where
  liftStrategy = id

instance LiftStrategyM m => LiftStrategyM (StateT s m) where
  liftStrategy m = lift (liftStrategy m)

instance LiftStrategyM m => LiftStrategyM (ReaderT s m) where
  liftStrategy m = lift (liftStrategy m)

instance LiftStrategyM m => LiftStrategyM (MaybeT m) where
  liftStrategy m = lift (liftStrategy m)

-- -----------------------------------------------------------------------------
-- Instances

instance HasGUID StrategyM where
  guidState f = StrategyM (state go)
    where
      go s = let (r, guid') = f (stsNextGUID s)
             in (r, s { stsNextGUID = guid' })


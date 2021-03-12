{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- API for strategies, which say how to produce a path from a slice.

module Talos.Strategy.Monad ( Strategy(..)
                            , StratFun
                            , StrategyM
                            , LiftStrategyM (..)
                            , rand, randR, randL, randPermute
                            ) where

import Control.Monad.State
import Control.Monad.Reader
import System.Random

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
type StratFun = ProvenanceTag -> Slice -> StrategyM SelectedPath

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
                 , stsNextGUID     :: GUID
                 }

newtype StrategyM a =
  StrategyM { getStrategyM :: StateT StrategyMState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

class Monad m => LiftStrategyM m where
  liftStrategy :: StrategyM a -> m a

instance LiftStrategyM StrategyM where
  liftStrategy = id

instance LiftStrategyM m => LiftStrategyM (StateT s m) where
  liftStrategy m = lift (liftStrategy m)

instance LiftStrategyM m => LiftStrategyM (ReaderT s m) where
  liftStrategy m = lift (liftStrategy m)

-- -- -----------------------------------------------------------------------------
-- -- Random values

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

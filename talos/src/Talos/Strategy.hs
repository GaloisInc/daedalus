{-# Language OverloadedStrings #-}

module Talos.Strategy (strategies, runStrategies) where

import Control.Monad.IO.Class

import Talos.Analysis.Slice
import Talos.SymExec.Path
import Talos.Strategy.Monad (Strategy(..), LiftStrategyM(..))
-- strategies
import Talos.Strategy.BTRand

strategies :: [Strategy]
strategies = [ randRestart, randMaybeT, randDFS ]

runStrategies :: LiftStrategyM m => [Strategy] -> ProvenanceTag -> Slice -> m (Maybe SelectedPath)
runStrategies strats0 ptag sl = liftStrategy $ go strats0
  where
    -- FIXME: There is probably a nicer way of doing this
    go [] = pure Nothing
    go (strat : strats) = do
      liftStrategy (liftIO (putStrLn $ "Trying strategy " ++ stratName strat))
      m_r <- stratFun strat ptag sl
      case m_r of
        Just {} -> pure m_r
        Nothing -> go strats
  
  
  

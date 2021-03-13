{-# Language OverloadedStrings #-}

module Talos.Strategy (strategies, runStrategies) where

import Talos.Analysis.Slice
import Talos.SymExec.Path
import Talos.Strategy.Monad (Strategy(..), LiftStrategyM(..))
-- strategies
import Talos.Strategy.BTRand (randDFS)

strategies :: [Strategy]
strategies = [ randDFS ]

runStrategies :: LiftStrategyM m => [Strategy] -> ProvenanceTag -> Slice -> m (Maybe SelectedPath)
runStrategies strats0 ptag sl = liftStrategy $ go strats0
  where
    -- FIXME: There is probably a nicer way of doing this
    go [] = pure Nothing
    go (strat : strats) = do
      m_r <- stratFun strat ptag sl
      case m_r of
        Just {} -> pure m_r
        Nothing -> go strats
  
  
  

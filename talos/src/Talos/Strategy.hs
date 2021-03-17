{-# Language OverloadedStrings #-}

module Talos.Strategy (allStrategies, runStrategies) where

import Control.Monad.IO.Class
import Text.Printf

import Talos.Analysis.Slice
import Talos.SymExec.Path
import Talos.Strategy.Monad (Strategy(..), LiftStrategyM(..), timeStrategy)
-- strategies
import Talos.Strategy.BTRand



allStrategies :: [Strategy]
allStrategies = [ randRestart, randMaybeT, randDFS ]

runStrategies :: LiftStrategyM m => [Strategy] -> ProvenanceTag -> Slice -> m (Maybe SelectedPath)
runStrategies strats0 ptag sl = liftStrategy $ go strats0
  where
    -- FIXME: There is probably a nicer way of doing this
    go [] = pure Nothing
    go (strat : strats) = do
      liftStrategy (liftIO (putStr $ "Trying strategy " ++ stratName strat))
      (m_r, ns) <- timeStrategy strat ptag sl
      let dns = (fromIntegral ns :: Double)
      liftStrategy (liftIO (printf " (%.3fms)\n" (dns  / 1000000)))
      case m_r of
        Just {} -> pure m_r
        Nothing -> go strats
  
  
  

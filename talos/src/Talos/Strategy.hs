{-# Language OverloadedStrings #-}

module Talos.Strategy (allStrategies, runStrategies) where

import Control.Monad.IO.Class
import Text.Printf

import Talos.Analysis.Slice
import Talos.SymExec.SolverT (SolverState, runSolverT)
import Talos.SymExec.Path
import Talos.Strategy.Monad (Strategy(..), StratFun(..), LiftStrategyM(..), StrategyM {-, timeStrategy -})
-- strategies
import Talos.Strategy.BTRand
import Talos.Strategy.Symbolic (symbolicStrat)

allStrategies :: [Strategy]
allStrategies = [ randRestart, randMaybeT, randDFS, symbolicStrat ]

runStrategy :: SolverState -> Strategy -> ProvenanceTag -> Slice ->
               StrategyM (Maybe SelectedPath, SolverState)
runStrategy solvSt strat ptag sl =
  case stratFun strat of
    SimpleStrat f -> flip (,) solvSt <$> f ptag sl
    SolverStrat f -> runSolverT (f ptag sl) solvSt

runStrategies :: LiftStrategyM m => SolverState -> [Strategy] -> ProvenanceTag -> Slice ->
                 m (Maybe SelectedPath, SolverState)
runStrategies solvSt strats0 ptag sl = liftStrategy $ go solvSt strats0
  where
    -- FIXME: There is probably a nicer way of doing this
    -- go (strat : strats) = do
    --   liftStrategy (liftIO (putStr $ "Trying strategy " ++ stratName strat))
    --   (m_r, ns) <- timeStrategy strat ptag sl
    --   let dns = (fromIntegral ns :: Double)
    --   liftStrategy (liftIO (printf " (%.3fms)\n" (dns  / 1000000)))
    go s [] = pure (Nothing, s)
    go s (strat : strats) = do
      (m_r, s') <- runStrategy s strat ptag sl
      case m_r of
        Just {} -> pure (m_r, s')
        Nothing -> go s' strats
  
  
  

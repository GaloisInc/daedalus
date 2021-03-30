{-# Language OverloadedStrings #-}

module Talos.Strategy (strategies, runStrategies) where

import Talos.Analysis.Slice
import Talos.SymExec.SolverT (SolverState, runSolverT)
import Talos.SymExec.Path
import Talos.Strategy.Monad (Strategy(..), StratFun(..), LiftStrategyM(..), StrategyM)
-- strategies
import Talos.Strategy.BTRand (randDFS)
import Talos.Strategy.Symbolic (symbolicStrat)

strategies :: [Strategy]
strategies = [ symbolicStrat ]

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
    go s [] = pure (Nothing, s)
    go s (strat : strats) = do
      (m_r, s') <- runStrategy s strat ptag sl
      case m_r of
        Just {} -> pure (m_r, s')
        Nothing -> go s' strats
  
  
  

-- {-# Language OverloadedStrings #-}

module Talos.Strategy (allStrategies, runStrategies, runStrategy) where

import System.IO (hFlush, stdout)
import Control.Exception (evaluate)
import Control.Monad.IO.Class
import Data.Maybe (isNothing)
import Text.Printf

import Control.DeepSeq (force)
import System.Clock (Clock(MonotonicRaw), getTime, diffTimeSpec, toNanoSecs)

import Daedalus.PP

import Talos.Analysis.Slice
import Talos.SymExec.SolverT (SolverState, runSolverT)
import Talos.SymExec.Path
import Talos.Strategy.Monad (Strategy(..), StratFun(..), LiftStrategyM(..), StrategyM {-, timeStrategy -})
-- strategies
import Talos.Strategy.BTRand
import Talos.Strategy.Symbolic (symbolicStrat)
-- import Talos.Strategy.BackwardSymbolic (backwardSymbolicStrat)

import Daedalus.Core

allStrategies :: [Strategy]
allStrategies = [ randRestart, randMaybeT, randDFS, symbolicStrat {- , backwardSymbolicStrat -} ]

runStrategy :: SolverState -> Strategy -> ProvenanceTag -> Slice ->
               StrategyM (Maybe SelectedPath, SolverState)
runStrategy solvSt strat ptag sl =
  case stratFun strat of
    SimpleStrat f -> flip (,) solvSt <$> f ptag sl
    SolverStrat f -> runSolverT (f ptag sl) solvSt

-- Returns the result and wall-clock time (in ns)
timeStrategy :: SolverState -> Strategy -> ProvenanceTag -> Slice -> StrategyM ((Maybe SelectedPath, Integer), SolverState)
timeStrategy solvSt strat ptag sl = do
  start         <- liftIO $ getTime MonotonicRaw
  (rv, solvSt') <- runStrategy solvSt strat ptag sl
  rv'           <- liftIO $ evaluate $ force rv
  end           <- liftIO $ getTime MonotonicRaw
  pure ((rv', toNanoSecs (diffTimeSpec end start)), solvSt')

runStrategies :: LiftStrategyM m => SolverState -> [Strategy] -> ProvenanceTag -> FName -> Name -> Slice ->
                 m (Maybe SelectedPath, SolverState)
runStrategies solvSt strats0 ptag fn x sl = liftStrategy $ go solvSt strats0
  where
    -- FIXME: There is probably a nicer way of doing this
    go s [] = pure (Nothing, s)
    go s (strat : strats) = do
      liftStrategy (liftIO (do { putStr $ "Trying strategy " ++ stratName strat ++ " at " ++ showPP fn ++ "." ++ showPP x ++ " ... "; hFlush stdout }))
      ((m_r, ns), s') <- timeStrategy s strat ptag sl
      let dns = (fromIntegral ns :: Double)
      let resReport = if isNothing m_r then "failed" else "succeeded"
      liftStrategy (liftIO (printf "%s (%.3fms)\n" resReport (dns  / 1000000)))
      case m_r of
        Just {} -> pure (m_r, s')
        Nothing -> go s' strats
      
    -- go s [] = pure (Nothing, s)
    -- go s (strat : strats) = do
    --   (m_r, s') <- runStrategy s strat ptag sl
    --   case m_r of
    --     Just {} -> pure (m_r, s')
    --     Nothing -> go s' strats
  
  
  

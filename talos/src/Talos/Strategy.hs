-- {-# Language OverloadedStrings #-}

module Talos.Strategy (allStrategies, parseStrategies, runStrategies, runStrategy) where

import           Control.DeepSeq             (force)
import           Control.Exception           (evaluate)
import           Control.Monad.IO.Class
import           Data.Maybe                  (isNothing)
import           System.IO                   (hFlush, stdout)
import           Text.Printf

import           Daedalus.Core
import           Daedalus.PP
import           Daedalus.Time (timeIt)

import           Talos.Analysis.Exported     (ExpSlice)
import           Talos.Strategy.BTRand
import           Talos.Strategy.Monad        (LiftStrategyM (..), StratFun (..),
                                              Strategy (..), StrategyM, StrategyInstance (siFun), siName)
import           Talos.Strategy.PathSymbolic (pathSymbolicStrat)
import           Talos.Strategy.Symbolic     (symbolicStrat)
import           Talos.SymExec.Path
import           Talos.SymExec.SolverT       (SolverState, runSolverT)
import qualified Talos.Strategy.Monad as M

allStrategies :: [Strategy]
allStrategies = [ randRestart, randMaybeT, randDFS, pathSymbolicStrat, symbolicStrat] {- , backwardSymbolicStrat -}

parseStrategies :: [String] -> Either String [StrategyInstance]
parseStrategies ss = M.parseStrategies ss allStrategies
  
runStrategy :: SolverState -> StrategyInstance -> ProvenanceTag -> ExpSlice ->
               StrategyM (Maybe SelectedPath, SolverState)
runStrategy solvSt strat ptag sl =
  case siFun strat of
    SimpleStrat f -> flip (,) solvSt <$> f ptag sl
    SolverStrat f -> runSolverT (f ptag sl) solvSt

-- Returns the result and wall-clock time (in ns)
timeStrategy :: SolverState -> StrategyInstance -> ProvenanceTag -> ExpSlice -> StrategyM ((Maybe SelectedPath, SolverState), Integer)
timeStrategy solvSt strat ptag sl = timeIt $ do
  (rv, solvSt') <- runStrategy solvSt strat ptag sl
  rv'           <- liftIO $ evaluate $ force rv
  pure (rv', solvSt')

runStrategies :: LiftStrategyM m => SolverState -> [StrategyInstance] -> ProvenanceTag -> FName -> Name -> ExpSlice ->
                 m (Maybe SelectedPath, SolverState)
runStrategies solvSt strats0 ptag fn x sl = liftStrategy $ go solvSt strats0
  where
    -- FIXME: There is probably a nicer way of doing this
    go s [] = pure (Nothing, s)
    go s (strat : strats) = do
      liftStrategy (liftIO (do { putStr $ "Trying strategy " ++ siName strat ++ " at " ++ showPP fn ++ "." ++ showPP x ++ " ... "; hFlush stdout }))
      ((m_r, s'), ns) <- timeStrategy s strat ptag sl
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
  
  
  

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# Language OverloadedStrings #-}

module Talos.Strategy (allStrategies, parseStrategies, ModelCache, newModelCache, findModel) where

import           Control.DeepSeq        (force)
import           Control.Exception      (evaluate)
import           Control.Lens           (at, (&), (?~))
import           Control.Monad.IO.Class (liftIO)
import           Data.Generics.Product  (field)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Vector            as V
import           GHC.Generics           (Generic)
import           System.IO              (hFlush, stdout)
import           Text.Printf

import           Daedalus.Core
import           Daedalus.PP
import           Daedalus.Time (timeIt)

import           Talos.Analysis.Exported     (ExpSlice, SliceId)
import           Talos.Strategy.BTRand
import           Talos.Strategy.Monad        (LiftStrategyM (..), StratGen(..),
                                              Strategy (..), StrategyM, StrategyInstance (siFun), siName, getSlice)
import           Talos.Strategy.PathSymbolic (pathSymbolicStrat)
-- import           Talos.Strategy.Symbolic     (symbolicStrat)
import           Talos.Path
import           Talos.Solver.SolverT       (SolverState, runSolverT)
import qualified Talos.Strategy.Monad as M


allStrategies :: [Strategy]
allStrategies = [ randRestart, randMaybeT, randDFS, pathSymbolicStrat] {- , backwardSymbolicStrat -}

parseStrategies :: [String] -> Either String [StrategyInstance]
parseStrategies ss = M.parseStrategies ss allStrategies
  
runStrategy :: SolverState -> StrategyInstance -> ProvenanceTag -> ExpSlice ->
               StrategyM (([SelectedPath], Maybe StratGen), SolverState)
runStrategy solvSt strat ptag sl = runSolverT (getStratGen (siFun strat ptag sl)) solvSt

-- Returns the result and wall-clock time (in ns)
timeStrategy :: SolverState -> StrategyInstance -> ProvenanceTag -> ExpSlice ->
                StrategyM (([SelectedPath], Maybe StratGen, SolverState), Integer)
timeStrategy solvSt strat ptag sl = timeIt $ do
  ((rv, gen), solvSt') <- runStrategy solvSt strat ptag sl
  rv'           <- liftIO $ evaluate $ force rv
  pure (rv', gen, solvSt')

runStrategies :: LiftStrategyM m => SolverState -> [StrategyInstance] -> ProvenanceTag -> FName -> ExpSlice ->
                 m ([SelectedPath], Maybe StratGen, SolverState)
runStrategies solvSt strats0 ptag fn sl = liftStrategy $ go solvSt strats0
  where
    -- FIXME: There is probably a nicer way of doing this
    go s [] = pure ([], Nothing, s)
    go s (strat : strats) = do
      -- logMessage 1 $ "Trying strategy " ++ siName strat ++ " at " ++ showPP fn ++ "." ++ showPP x ++ " ... "
      ((r, m_gen, s'), ns) <- timeStrategy s strat ptag sl
      let dns = (fromIntegral ns :: Double)
      let resReport = if null r then "failed" else "succeeded"
      -- logMessage 1 $ printf "%s (%.3fms)\n" resReport (dns  / 1000000)
      if null r
        then go s' strats
        else pure (r, m_gen, s')
      
--     -- go s [] = pure (Nothing, s)
--     -- go s (strat : strats) = do
--     --   (m_r, s') <- runStrategy s strat ptag sl
--     --   case m_r of
--     --     Just {} -> pure (m_r, s')
--     --     Nothing -> go s' strats
  
  
-- --------------------------------------------------------------------------------
-- Model cache

data ModelCacheEntry = ModelCacheEntry
  { mceModels     :: V.Vector SelectedPath
  , mceNextModel  :: Int
  , mceGenerators :: [StratGen]
  } deriving Generic

data ModelCache = ModelCache
  { mcCache :: Map SliceId ModelCacheEntry
  , mcStratInstances :: [StrategyInstance] -- ^ Read only
  , mcSolverState :: SolverState
  } deriving Generic

newModelCache :: [StrategyInstance] -> SolverState -> ModelCache
newModelCache = ModelCache mempty

findModel :: LiftStrategyM m => ModelCache -> ProvenanceTag -> FName -> SliceId -> 
             m (Maybe SelectedPath, ModelCache)
findModel mc ptag fn sid
  -- For the moment we never re-generate models
  | Just mce <- Map.lookup sid (mcCache mc) =
      if V.null (mceModels mce)
      then pure (Nothing, mc)
      else pure (nextModel mc mce)
      
    -- We haven't seen this slice before, so solve
  | otherwise = do
      sl <- getSlice sid
      (r, _m_gen, solvSt') <- runStrategies (mcSolverState mc) (mcStratInstances mc) ptag fn sl
      pure $ if null r
             then (Nothing, mc)
             else nextModel (mc { mcSolverState = solvSt' })
                            (ModelCacheEntry { mceModels    = V.fromList r
                                             , mceNextModel = 0
                                             , mceGenerators = [] -- FIXME
                                             })
  where
    nextModel :: ModelCache -> ModelCacheEntry -> (Maybe SelectedPath, ModelCache)
    nextModel mc' mce = ( Just (mceModels mce V.! mceNextModel mce)
                        , mc' & field @"mcCache" . at sid ?~ mce { mceNextModel = nextId mce}
                        )
    nextId mce = (mceNextModel mce + 1) `mod` V.length (mceModels mce)
  


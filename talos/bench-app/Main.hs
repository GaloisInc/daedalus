{-# Language OverloadedStrings #-}

module Main where

import           Control.Monad           (replicateM, void)
import           Data.Foldable           (find, forM_)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (isJust, mapMaybe)
import           System.Random           (mkStdGen, randomIO)


import           Daedalus.Core           (FName, Module (mTypes), TDecl (tName))
import           Daedalus.GUID           (GUID)
import           Daedalus.PP             (showPP)
import           Daedalus.Rec            (forgetRecs)

import           Talos                   (runDaedalus)
import qualified Talos.Analysis          as A
import           Talos.Analysis.AbsEnv   (AbsEnvTy (AbsEnvTy))
import           Talos.Analysis.Exported (ExpSummary, esRootSlices,
                                          exportSummaries)
import           Talos.Analysis.Monad    (Summaries)
import           Talos.Analysis.Slice    (FInstId)
import           Talos.Strategy          (allStrategies, runStrategy)
import           Talos.Strategy.Monad    (Strategy, emptyStrategyMState,
                                          runStrategyM, stratName)
import           Talos.SymExec.SolverT   (emptySolverState)
import qualified Talos.SymExec.SolverT   as SMT


import           Criterion
import           Criterion.Main


-- This will load a ddl file, and benchmark the given strategies on the slices.
-- FIXME: maybe generate the whole doc as well?

data Bench = Bench
  { bname      :: String
  , ddlFile    :: FilePath
  , invFile    :: Maybe FilePath
  , entry      :: String
  , strategies :: [String]
  , bseeds      :: Either Int [Int] -- Either pick N seeds or use the
                   -- given ones.
  }

-- Ugly, paths are relative to the daedalus root

benches :: [Bench]
benches = [ Bench { bname = "NITF Header"
                  , ddlFile = "formats/nitf/nitf-simple/nitf_header.ddl"
                  , invFile = Nothing -- Just "nitf/nitf-simple/nitf_inverses.ddl"
                  , entry   = "Header"
                  , strategies = ["symbolic-rand-dfs", "symbolic-bfs"]
                  , bseeds = Left 1
                  }
          ]

z3Opts  = [ ("auto-config", "false")
          , ("smt.phase_selection", "5")
            -- see :smt.arith.random_initial_value also and seed options
          ]

-- FIXME
backend = "z3"
z3Args = ["-in", "-smt2"]

-- We create a new benchmark for each slice in the program.  We need
-- to analyze the program for each inv file (and no inv file).

mkBenchmarks :: SMT.Solver -> [Int] -> Summaries ae -> Module -> GUID -> [Strategy] ->
                FName -> Map FInstId ExpSummary -> Benchmark
mkBenchmarks solv seeds summaries md nguid strats fn clM =
  bgroup (showPP fn) [ bgroup (showPP cl)
                       (concat [ goSl n i sl
                               | (n, sls) <- Map.toList slM
                               , (i, sl) <- zip [(0::Int)..] sls
                               ])
                     | (cl, slM) <- Map.toList clM
                     ]
  where
    goSl n i sl =
      [ bench (showPP n ++ "/" ++ showPP i ++ "/" ++ stratName strat {- ++ "/" ++ show seed -})
              (benchSl seed strat sl)
      | seed <- seeds, strat <- strats]

    benchSl seed strat sl =
      let st0     = emptyStrategyMState (mkStdGen seed) summaries md nguid
      in whnfAppIO (benchIt strat sl st0) solv

    benchIt strat sl st0 = \solv ->
      let solvSt0 = emptySolverState solv
      in  fst <$> runStrategyM (runStrategy solvSt0 strat 0 sl) st0


benchToBenchmark :: Bench -> IO Benchmark
benchToBenchmark b = do
  seeds <- case bseeds b of
    Left n   -> replicateM n randomIO
    Right ss -> pure ss

  bgroup (bname b) <$> sequence (goInv seeds Nothing
                                 : [goInv seeds m_inv | isJust m_inv])
  where
    m_inv = invFile b
    invN m_inv' = case m_inv' of
      Nothing -> "no inverse"
      Just f  -> "inverse (" ++ f ++ ")"

    -- FIXME: claggy 
    strats = mapMaybe (\n -> find (\s -> (stratName s) == n) allStrategies) (strategies b)

    absEnv = "vars"

    goInv seeds m_inv' = bgroup (invN m_inv') <$> do
      (_mainRule, md, nguid) <- runDaedalus (ddlFile b) m_inv' (Just $ entry b)
      AbsEnvTy p <- case lookup absEnv A.absEnvTys of
        Just x -> pure x
        _      -> error ("Unknown abstract env " ++ absEnv)

      let sm@(summaries, _) = A.summarise p md nguid
          tyDefs  = Map.fromList [ (tName td, td) | td <- forgetRecs (mTypes md) ]
          (expSummaries, nguid') = exportSummaries tyDefs sm
          
      -- This would be better done before each benchmark, but we dont have NFData for Solver
      solv <- startSolver
      pure (map (uncurry (mkBenchmarks solv seeds summaries md nguid' strats))
             (Map.toList (esRootSlices expSummaries)))

    startSolver = do
      solver <- SMT.newSolver backend z3Args Nothing
      -- Check version: z3 before 4.8.10 (or .9) seems to have an issue
      -- with match.  
      -- z3VersionCheck solver
      -- Set options
      forM_ z3Opts $ \(opt, val) -> 
        void $ SMT.setOptionMaybe solver (":" <> opt) val

      pure solver

main = defaultMain =<< mapM benchToBenchmark benches






module Main where

import           System.Random (randomIO, mkStdGen)

import Criterion
import Control.Monad (replicateM, void)
import Data.Maybe (mapMaybe, isJust)
import Talos (runDaedalus)
import qualified Talos.Analysis as A
import Talos.Strategy (runStrategy, allStrategies)
import qualified Data.Map as Map
import Daedalus.PP (showPP)
import Talos.Strategy.Monad (stratName, Strategy, emptyStrategyMState, runStrategyM)
import Data.Foldable (find, forM_)
import Talos.Analysis.Monad (Summary(pathRootMap), Summaries)
import qualified SimpleSMT as SMT
import Daedalus.GUID (GUID)
import Daedalus.Core (Module, FName)
import Talos.Analysis.Slice (SummaryClass)
import Data.Map (Map)
import Talos.SymExec.SolverT (emptySolverState)
import Criterion.Main

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
                  , ddlFile = "nitf/nitf-simple/nitf_header.ddl"
                  , invFile = Just "nitf/nitf-simple/nitf_inverses.ddl"
                  , entry   = "Header"
                  , strategies = ["symbolic-dfs", "symbolic-bfs"]
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

mkBenchmarks :: SMT.Solver -> [Int] -> Summaries -> Module -> GUID -> [Strategy] ->
                FName -> Map SummaryClass Summary -> Benchmark
mkBenchmarks solv seeds summaries md nguid strats fn clM =
  bgroup (showPP fn) [ bgroup (showPP cl)
                       (concat [ goSl n fset sl
                               | (n, sls) <- Map.toList (pathRootMap summary)
                               , (fset, sl) <- sls
                               ])
                     | (cl, summary) <- Map.toList clM
                     ]
  where
    goSl n fset sl =
      [ bench (showPP n ++ "/" ++ showPP fset ++ "/" ++ stratName strat {- ++ "/" ++ show seed -})
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

    goInv seeds m_inv' = bgroup (invN m_inv') <$> do
      (_mainRule, md, nguid) <- runDaedalus (ddlFile b) m_inv' (Just $ entry b)
      let (summaries, nguid') = A.summarise md nguid
      -- This would be better done before each benchmark, but we dont have NFData for Solver
      solv <- startSolver
      pure (map (uncurry (mkBenchmarks solv seeds summaries md nguid' strats))
             (Map.toList summaries))

    startSolver = do
      solver <- SMT.newSolver backend z3Args Nothing
      -- Check version: z3 before 4.8.10 (or .9) seems to have an issue
      -- with match.  
      -- z3VersionCheck solver
      -- Set options
      forM_ z3Opts $ \(opt, val) -> 
        void $ SMT.setOptionMaybe solver (':' : opt) val

      pure solver

main = defaultMain =<< mapM benchToBenchmark benches





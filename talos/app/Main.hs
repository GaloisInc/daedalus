
{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}

module Main where

import           Control.Monad         (when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import           Hexdump
import qualified Data.Set              as Set
import qualified Streaming.Prelude     as S
import           System.Console.ANSI
import           System.IO             (IOMode (..), hFlush, openFile, stdout)

import           Daedalus.Core.CFG (cfg, cfgFunToDot, cfgFuns, cfgModuleToCallGraph, callGraphToDot)
import           Daedalus.PP
import           Talos

import           CommandLine
import Daedalus.GUID (runFresh)
import Data.Foldable (traverse_)

-- debugging
-- import qualified SimpleSMT as S
-- import Talos.Analysis.Monad (Summary(..))
-- import Talos.Analysis (summarise)
-- import Analysis

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  opts <- getOptions

  case optMode opts of
    SynthesisMode     -> doSynthesis opts
    SummaryMode       -> doSummary opts
    DumpCoreMode      -> doDumpCore opts
    CFGDotMode        -> doCFGDot opts
    CallGraphDotMode  -> doCallGraphDot opts
    CavityFinderMode  -> doFindCavities opts

doDumpCore :: Options -> IO ()
doDumpCore opts = do
  (_mainRule, md, _nguid) <- runDaedalus (optDDLInput opts) (optInvFile opts) (optDDLEntry opts) (optNoLoops opts)
  print (pp md)

doCFGDot :: Options -> IO ()
doCFGDot opts = do
  (_mainRule, md, nguid) <- runDaedalus (optDDLInput opts) (optInvFile opts) (optDDLEntry opts) (optNoLoops opts)
  let (cfgm, _nguid') = runFresh (cfg md) nguid
  traverse_ (print . pp . cfgFunToDot) (cfgFuns cfgm)

doCallGraphDot :: Options -> IO ()
doCallGraphDot opts = do
  (_mainRule, md, nguid) <- runDaedalus (optDDLInput opts) (optInvFile opts) (optDDLEntry opts) (optNoLoops opts)
  let (cfgm, _nguid') = runFresh (cfg md) nguid
  print $ pp $ callGraphToDot $ cfgModuleToCallGraph cfgm

doFindCavities :: Options -> IO ()
doFindCavities opts = do
  (_mainRule, md, nguid) <- runDaedalus (optDDLInput opts) (optInvFile opts) (optDDLEntry opts) (optNoLoops opts)
  let (cfgm, _nguid') = runFresh (cfg md) nguid
  traverse_ (print . pp . cfgFunToDot) (cfgFuns cfgm)
  let cavityLocs = findCavities md cfgm
  if null cavityLocs then
    print $ text "No cavities."
  else
    print $ text "Cavity locations:" $$ (vcat $ map pp $ Set.toList $ findCavities md cfgm)

doSummary :: Options -> IO ()
doSummary opts = do
  putStrLn "Summarising ..."
  let absEnv = fromMaybe "fields" (optAnalysisKind opts)
  summaryDoc <- summarise (optDDLInput opts) (optInvFile opts) (optDDLEntry opts) (optVerbosity opts) (optNoLoops opts) absEnv
  print summaryDoc

doSynthesis :: Options -> IO ()
doSynthesis opts = do
  strm <- synthesise sopts

  -- model output
  let indent = unlines . map ((++) "  ") . lines
      prettyBytes _n _v bs provmap = do
        putStrLn "Synthesised input: "
        putStr (indent (prettyHexWithProv provmap bs))
        -- putStrLn "Semantic value: "
        -- print ("  " <> pp v)
        putStrLn ""
        -- putStrLn "Provenance map: "
        -- print provmap
        -- putStrLn ""
      writeStdOut _n _v bs _provmap = BS.hPutStrLn stdout bs >> hFlush stdout

  writeModel <-
    case optOutfile opts of
      Nothing              -> pure writeStdOut -- (\_ bs -> BS.hPutStrLn stdout bs >> hFlush stdout)
      Just (AllOutput "-") -> pure writeStdOut -- (\_ bs -> BS.hPutStrLn stdout bs >> hFlush stdout)
      Just (AllOutput fp) -> do
        hdl <- openFile fp WriteMode
        pure (\_ _ bs _ -> BS.hPutStrLn hdl bs >> hFlush hdl)
      Just (PatOutput pat) -> do
        let mk = case break (== '%') pat of
                   (lhs, '%' : rhs) -> \n -> lhs ++ show n ++ rhs
                   _                -> \n -> pat ++ "." ++ show n
        pure (\n _ bs _ -> BS.writeFile (mk n) bs)

  let doWriteModel ((v, bs, provmap), n) = do
        writeModel n v bs provmap
        when (optPrettyModel opts) $ prettyBytes n v bs provmap
        case optProvFile opts of
          Nothing -> pure ()
          Just f  -> writeFile f (show provmap)

  let strm' = S.take (optNModels opts) $ S.zip strm (S.each [(0 :: Int)..])
  S.mapM_ doWriteModel strm'
  where
    sopts = SynthesisOptions
      { inputFile       = optDDLInput opts
      , inverseFile     = optInvFile opts
      , entry           = optDDLEntry opts
      , solverPath      = optSolver opts
      , solverArgs      = ["-smt2", "-in"]
      , solverOpts      = bOpts
      , solverInit      = pure ()
      , synthesisStrats = optStrategy opts 
      , smtLogFile      = optSMTOutput opts
      , seed            = optSeed opts
      , analysisEnv     = absEnv
      , verbosity       = optVerbosity opts
      , eraseLoops      = optNoLoops opts
      , logFile         = optLogOutput opts
      , debugKeys       = optDebugKeys opts
      , statsFile       = optStatsOutput opts
      , problemFile     = optProblemFilePfx opts
      , statsKeys       = optStatsKeys opts
      }
      
    bOpts = [ ("auto-config", "false")
            , ("smt.phase_selection", "5")
            -- see :smt.arith.random_initial_value also and seed options
            ]
            ++ [("model-validate", "true") | optValidateModel opts]
    absEnv = fromMaybe "fl" (optAnalysisKind opts) -- FIXME: don't hardcode analysis


prettyHexWithProv :: ProvenanceMap -> BS.ByteString -> String
prettyHexWithProv provmap bs =
  prettyHexCfg (Cfg 0 mkColor) bs
    ++ (setSGRCode [])
  where
    mkColor off s =
      let (i, col) = case Map.lookup off provmap of
                         Just p -> colors !! (p `mod` (length colors))
                         Nothing -> (Vivid, White)
      in
        (setSGRCode [SetColor Foreground i col]) ++ s

    colors = [(Vivid, Red),
              (Vivid, Green),
              (Vivid, Yellow),
              (Vivid, Blue),
              (Vivid, Magenta),
              (Vivid, Cyan),
              (Dull, Red),
              (Dull, Green),
              (Dull, Yellow),
              (Dull, Blue),
              (Dull, Magenta),
              (Dull, Cyan)
              ]

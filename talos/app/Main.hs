{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language ViewPatterns, FlexibleContexts #-}

module Main where

import System.Exit(exitFailure)
import System.IO (hFlush, hPutStrLn, stdout, stderr
                 , openFile, IOMode(..))
import Control.Monad (replicateM, zipWithM_, when)

import System.Console.ANSI

import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams

import Hexdump

import Daedalus.PP

import CommandLine
import Talos
import Data.Maybe (fromMaybe)

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
    SynthesisMode -> doSynthesis opts
    SummaryMode   -> doSummary opts
    DumpCoreMode  -> doDumpCore opts


doDumpCore :: Options -> IO ()
doDumpCore opts = do
  (_mainRule, md, _nguid) <- runDaedalus (optDDLInput opts) (optInvFile opts) (optDDLEntry opts)
  print (pp md)

doSummary :: Options -> IO ()
doSummary opts = do
  putStrLn "Summarising ..."
  let absEnv = fromMaybe "fields" (optAnalysisKind opts)
  summaryDoc <- summarise (optDDLInput opts) (optInvFile opts) (optDDLEntry opts) absEnv
  print summaryDoc
    
doSynthesis :: Options -> IO ()
doSynthesis opts = do
  let bOpts = [ ("auto-config", "false")
              , ("smt.phase_selection", "5")
              -- see :smt.arith.random_initial_value also and seed options
              ]
              ++ if optValidateModel opts
                 then [("model-validate", "true")]
                  else []

  let logOpt = (\x -> (x, optLogOutput opts)) <$> optLogLevel opts
      absEnv = fromMaybe "fields" (optAnalysisKind opts) -- FIXME: don't hardcode analysis
  strm <- synthesise (optDDLInput opts) (optInvFile opts) (optDDLEntry opts) (optSolver opts) 
            ["-smt2", "-in"] bOpts (pure ()) (optStrategy opts)
            logOpt (optSeed opts) absEnv

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

  let doWriteModel n m_bs =
        case m_bs of
          Nothing      -> hPutStrLn stderr "Not enough models" >> exitFailure
          Just (v, bs, provmap) -> 
            do writeModel n v bs provmap
               when (optPrettyModel opts) $ prettyBytes n v bs provmap
               case optProvFile opts of
                 Nothing -> pure ()
                 Just f  -> writeFile f (show provmap)
                   
  bss <- replicateM (optNModels opts) (Streams.read strm)
  zipWithM_ doWriteModel [(0 :: Int)..] bss

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

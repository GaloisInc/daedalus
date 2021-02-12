{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language ViewPatterns, FlexibleContexts #-}

module Main where

import System.Exit(exitFailure)
import System.IO (hFlush, hPutStrLn, stdout, stderr
                 , openFile, IOMode(..))
import Control.Monad (replicateM, zipWithM_, when)

import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as Streams

import Hexdump

import Daedalus.PP

import CommandLine
import Talos
import Talos.SymExec (futurePathSetConfig, futurePathSetRel)

-- debugging
import qualified SimpleSMT as S
import Talos.Analysis.Monad (Summary(..))
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

doSummary :: Options -> IO ()
doSummary opts = do
  putStrLn "Summarising ..."
  summaries <- summarise (optDDLInput opts) (optDDLEntry opts)
  mapM_ (print . doOne) (Map.toList summaries)
  where
    doOne (nm, summary) = pp nm <+> " :: " <+> bullets (map doOneS (Map.toList summary))
    doOneS (cl, s) = pp cl <> ": "  <> pp s
    --                    $+$ bullets (map ppCfg (Map.elems (pathRootMap s)))
    -- ppCfg c = (ppSExpr (futurePathSetConfig c)) $+$
    --           (ppSExpr (futurePathSetRel (S.const "$cfg") c))

    -- ppSExpr = vcat . map text . lines . flip S.ppSExpr ""
    
doSynthesis :: Options -> IO ()
doSynthesis opts = do
  let bOpts = [ ("auto-config", "false")
              , ("smt.phase_selection", "5") ]
              ++ if optValidateModel opts
                 then [("model-validate", "true")]
                 else []

  let logOpt = (\x -> (x, optLogOutput opts)) <$> optLogLevel opts

  strm <- synthesise (optDDLInput opts) (optDDLEntry opts) (optSolver opts)
            ["-smt2", "-in"] bOpts (pure ())
            logOpt (optSeed opts)

  -- model output
  let indent = unlines . map ((++) "  ") . lines
      prettyBytes _n v bs = do
        putStrLn "Synthesised input: "
        putStr (indent (prettyHex bs))
        putStrLn "Semantic value: "
        print ("  " <> pp v)
        putStrLn ""
      writeStdOut _n _v bs = BS.hPutStrLn stdout bs >> hFlush stdout


  writeModel <-
    case optOutfile opts of
      Nothing              -> pure writeStdOut -- (\_ bs -> BS.hPutStrLn stdout bs >> hFlush stdout)
      Just (AllOutput "-") -> pure writeStdOut -- (\_ bs -> BS.hPutStrLn stdout bs >> hFlush stdout)
      Just (AllOutput fp) -> do
        hdl <- openFile fp WriteMode
        pure (\_ _ bs -> BS.hPutStrLn hdl bs >> hFlush hdl)
      Just (PatOutput pat) -> do
        let mk = case break (== '%') pat of
                   (lhs, '%' : rhs) -> \n -> lhs ++ show n ++ rhs
                   _                -> \n -> pat ++ "." ++ show n
        pure (\n _ -> BS.writeFile (mk n))

  let doWriteModel n m_bs =
        case m_bs of
          Nothing      -> hPutStrLn stderr "Not enough models" >> exitFailure
          Just (v, bs) -> do writeModel n v bs
                             when (optPrettyModel opts) $ prettyBytes n v bs

  bss <- replicateM (optNModels opts) (Streams.read strm)
  zipWithM_ doWriteModel [(0 :: Int)..] bss



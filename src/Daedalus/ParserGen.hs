module Daedalus.ParserGen
  ( buildMapAut
  , buildArrayAut
  , runnerBias
  , runnerLL
  , extractValues
  , extractParseError
  , autToGraphviz
  , createDFA
  , statsDFA
  )
where

import qualified Data.ByteString as BS

import System.Console.ANSI
import Hexdump

import RTS.Input(Input(..))

import Daedalus.ParserGen.Action (showCallStack)
import Daedalus.ParserGen.Compile (buildMapAut, buildArrayAut)
import Daedalus.ParserGen.Cfg as PGenCfg
import Daedalus.ParserGen.RunnerBias (runnerBias, runnerLL, Result(..), extractValues)
import Daedalus.ParserGen.Utils (autToGraphviz)
import Daedalus.ParserGen.Det (createDFA, statsDFA)

extractParseError :: BS.ByteString -> Result -> String
extractParseError orig res =
  case parseError res of
    Nothing -> error "Weird: parse error without backtracking"
    Just (_n, PGenCfg.Cfg inp ctrl _out _q) -> showCallStack ctrl ++ "\ninput: " ++ dumpErrorInput orig inp

dumpErrorInput :: BS.ByteString -> Input -> String
dumpErrorInput orig inp =
  let ctxtAmt = 32
      errLoc  = (inputOffset inp)
      start = max 0 (errLoc - ctxtAmt)
      end   = errLoc + 10
      len   = end - start
      ctx = BS.take len (BS.drop start orig)
      startErr =
        setSGRCode [ SetConsoleIntensity
                     BoldIntensity
                   , SetColor Foreground Vivid Red ]
      endErr = setSGRCode [ Reset ]
      cfg = defaultCfg { startByte = start
                       , transformByte =
                         wrapRange startErr endErr
                         errLoc errLoc }
  in  "File context:\n" ++ prettyHexCfg cfg ctx

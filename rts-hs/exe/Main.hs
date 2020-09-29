module Main where

import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Console.ANSI
import Hexdump
import Text.Show.Pretty(pPrint)
import qualified Data.Text as Text
import Data.Text.Encoding(encodeUtf8)

import Midi(pMain)
import RTS.Parser(runParser)
import RTS.Input
import RTS.ParserAPI


main :: IO ()
main =
  do args <- getArgs
     case args of
       [f] -> do bs <- BS.readFile f
                 let inp = newInput (encodeUtf8 (Text.pack f)) bs
                 case runParser pMain inp of
                   r@(NoResults err) -> do pPrint err
                                           humanError bs err
                   Results as ->
                     do putStrLn $ "--- Found " ++
                                          show (length as) ++ " results:"
                        pPrint as

       _   -> do putStrLn "Need a file to parse."
                 exitFailure


humanError :: ByteString -> ParseError -> IO ()
humanError bs err =
  do putStrLn "--- Parse error: "
     print (ppParseError err)
     let ctxtAmt = 32
         errLoc  = peOffset err
         start = max 0 (errLoc - ctxtAmt)
         end   = errLoc + 10
         len   = end - start
         ctx = BS.take len (BS.drop start bs)
         startErr =
            setSGRCode [ SetConsoleIntensity
                         BoldIntensity
                       , SetColor Foreground Vivid Red ]
         endErr = setSGRCode [ Reset ]
         cfg = defaultCfg { startByte = start
                          , transformByte =
                             wrapRange startErr endErr
                             errLoc errLoc }
     putStrLn "File context:"
     putStrLn $ prettyHexCfg cfg ctx




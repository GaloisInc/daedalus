module Main where

import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Console.ANSI
import Hexdump
import Text.Show.Pretty(pPrint)

import Midi(pMain)
import RTS.Parser(runParser)
import RTS.ParserAPI(Input(..), Result(..), ParseError(..))


main :: IO ()
main =
  do args <- getArgs
     case args of
       [f] -> do bs <- BS.readFile f
                 let inp = Input { inputBytes  = bs
                                 , inputOffset = 0
                                 }
                 case runParser pMain inp of
                   r@(NoResults err) -> do pPrint err
                                           ppErr bs err
                   Results as ->
                     do putStrLn $ "--- Found " ++
                                          show (length as) ++ " results:"
                        pPrint as

       _   -> do putStrLn "Need a file to parse."
                 exitFailure


ppErr :: ByteString -> ParseError -> IO ()
ppErr bs e =
  do let ctxtAmt = 32
         errLoc  = peOffset e
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
     exitFailure



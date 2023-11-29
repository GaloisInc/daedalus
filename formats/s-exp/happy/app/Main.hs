module Main where

import Data.Word
import qualified Data.ByteString.Lazy as LBS
import Lexer
import Grammar

main :: IO ()
main =
  do bytes <- LBS.readFile "../data.dat"
     case sexp (alexScanTokens bytes) of
       Nothing -> print "parse error"
       Just s  -> print (count s)


count :: SExp -> Word64
count s =
  case s of
    Symbol _ -> 1
    Node xs  -> sum (map count xs)

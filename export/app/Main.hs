module Main where

import Lexer

main :: IO ()
main =
  testFromFile "export/tests/simple.export"

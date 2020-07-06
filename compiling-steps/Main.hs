module Main where

import Text.PrettyPrint(Doc)
import Data.List.NonEmpty(NonEmpty)

import AST
import qualified Reference
import PP
import Compile
import Run
import Value(I(..))

main :: IO ()
main = doExample ex0 "CAAA"


doExample :: ([Fun], P) -> String -> IO ()
doExample e bs =
  do outline "Compiled"   $ print $ compile e
     let i = I { loc = 0, bytes = bs }
     outline "Running"    $ print $ parse e i
     outline "Reference"  $ print $ Reference.parse e i

outline :: String -> IO a -> IO a
outline x m =
  do putStrLn ("=== " ++ x ++ " ===")
     a <- m
     putStrLn "-------------\n"
     pure a

compile :: ([Fun],P) -> Doc
compile = pp . compileFuns

parse :: ([Fun],P) -> I -> Either Int (NonEmpty (V,I))
parse fs = runProgram (compileFuns fs)


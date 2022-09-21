module Main(main) where

import System.Directory(setCurrentDirectory)
import System.Process(callProcess)

main :: IO ()
main =
  do setCurrentDirectory "test-with-make"
     callProcess "./run-test" []

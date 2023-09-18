module Main where

import Control.Monad   (when)
import System.Exit     (exitWith, exitFailure)
import Test.HUnit

import qualified Talos.Polyglot.Tests

main :: IO ()
main = do
    count <- runTestTT tests
    when ((errors count) /= 0 || (failures count) /= 0) exitFailure

tests :: Test
tests = TestList 
  [ "Talos.Polyglot" ~: Talos.Polyglot.Tests.tests ]

module Main where

import Control.Monad (when)
import System.Exit (exitWith, exitFailure)
import Test.HUnit

import qualified Talos.Dataflow.Tests
import qualified Talos.StatefulProcessor.Tests

main :: IO ()
main = do
    count <- runTestTT tests
    when ((errors count) /= 0 || (failures count) /= 0) exitFailure

tests :: Test
tests = TestList 
    [ "Talos.Dataflow.Tests" ~: Talos.Dataflow.Tests.tests
    , "Talos.StatefulProcessor.Tests" ~: Talos.StatefulProcessor.Tests.tests
    ]
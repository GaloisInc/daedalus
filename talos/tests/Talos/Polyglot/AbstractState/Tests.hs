module Talos.Polyglot.AbstractState.Tests where

import Test.HUnit

import Talos.Polyglot.AbstractState.ReadFrontier.Tests
import Talos.Polyglot.AbstractState.ThreadSet.Tests

tests :: Test
tests = TestList
  [ "ThreadSet" ~: Talos.Polyglot.AbstractState.ThreadSet.Tests.tests
  , "ReadFrontier" ~: Talos.Polyglot.AbstractState.ReadFrontier.Tests.tests
  ]

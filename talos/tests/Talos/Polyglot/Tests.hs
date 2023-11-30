module Talos.Polyglot.Tests where

import Test.HUnit
import Talos.Polyglot.AbstractState.Tests

tests :: Test
tests = TestList
  [ "AbstractState" ~: Talos.Polyglot.AbstractState.Tests.tests ]

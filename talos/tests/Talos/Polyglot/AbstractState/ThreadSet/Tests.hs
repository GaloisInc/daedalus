module Talos.Polyglot.AbstractState.ThreadSet.Tests where

import           Prelude hiding (fromList, join, joins)

import Talos.Polyglot.AbstractState.ThreadSet

import qualified Data.Set as Set
import           Test.HUnit

emptyInt :: ThreadSet Int
emptyInt = empty

-- TODO(cns): Property-based tests.

tests :: Test
tests = TestList
  [ "eq-empty"            ~: True ~=? emptyInt == emptyInt
  , "eq-singleton"        ~: True ~=? singleton 1 == singleton 1
  , "neq-empty-singleton" ~: True ~=? emptyInt /= singleton 1
  , "from-list-single"    ~: singleton 1 ~=? fromList [[1]]
  , "from-list-absorb"    ~: fromList [[1, 2]] ~=? fromList [[1, 2], [1, 2]]
  , "from-list-comm"      ~: fromList [[2, 3], [1, 2]] ~=? fromList [[1, 2], [2, 3]]
  , "join"                ~: fromList [[1], [2]] ~=? (join (singleton 1) (singleton 2))
  , "join-absorb"         ~: fromList [[1]] ~=? (join (singleton 1) (singleton 1))
  , "sequence-one-empty"  ~: singleton 1 ~=? sequenceOne 1 empty
  , "sequence-one-absorb" ~: singleton 1 ~=? sequenceOne 1 (singleton 1)
  , "sequence-one-insert" ~: fromList [[1, 3], [3]] ~=? sequenceOne 3 (fromList [[1], [3]])
  , "flatten"             ~: Set.fromList [1, 2, 3] ~=? flatten (fromList [[1], [2, 3]])
  , "remove"              ~: fromList [[3]] ~=? removeAllIntersecting (Set.singleton 1) (fromList [[1], [1, 2], [3]])
  , "remove-all"          ~: ThreadSet (Set.empty) ~=? removeAllIntersecting (Set.singleton 1) (fromList [[1], [1, 2], [1, 3]])
  , "contains-true"       ~: True ~=? contains 1 (fromList [[2], [3, 1]])
  , "contains-false"      ~: False ~=? contains 4 (fromList [[2], [3, 1]])
  ]

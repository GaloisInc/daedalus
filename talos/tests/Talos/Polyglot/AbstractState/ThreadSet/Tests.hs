module Talos.Polyglot.AbstractState.ThreadSet.Tests where

import           Prelude hiding (fromList, join, joins, sequence)

import Talos.Polyglot.AbstractState.ThreadSet

import qualified Data.Set as Set
import           Test.HUnit

-- TODO(cns): Property-based tests.

tests :: Test
tests = TestList
  [ "from-list-single"    ~: singleton 1 ~=? fromList [[1]]
  , "from-list-absorb"    ~: fromList [[1, 2]] ~=? fromList [[1, 2], [1, 2]]
  , "from-list-comm"      ~: fromList [[2, 3], [1, 2]] ~=? fromList [[1, 2], [2, 3]]
  , "join"                ~: fromList [[1], [2]] ~=? (join (singleton 1) (singleton 2))
  , "join-absorb"         ~: fromList [[1]] ~=? (join (singleton 1) (singleton 1))
  , "sequence-one-empty"  ~: empty ~=? sequenceOne 1 empty
  , "sequence-one-emptyt" ~: singleton 1 ~=? sequenceOne 1 emptyThread
  , "sequence-one-absorb" ~: singleton 1 ~=? sequenceOne 1 (singleton 1)
  , "sequence-one-insert" ~: fromList [[1, 3], [3]] ~=? sequenceOne 3 (fromList [[1], [3]])
  , "sequence-many"       ~: fromList [[1, 2, 3], [2, 3]] ~=? sequenceMany (Set.fromList [2, 3]) (fromList [[1], [3]])
  , "sequence"            ~: fromList [[1, 2, 3], [2, 3]] ~=? sequence (fromList [[1], [3]]) (fromList [[2, 3]])
  , "flatten"             ~: Set.fromList [1, 2, 3] ~=? flatten (fromList [[1], [2, 3]])
  , "remove"              ~: fromList [[3]] ~=? removeAllIntersecting (Set.singleton 1) (fromList [[1], [1, 2], [3]])
  , "remove-all"          ~: empty ~=? removeAllIntersecting (Set.singleton 1) (fromList [[1], [1, 2], [1, 3]])
  , "contains-true"       ~: True ~=? contains 1 (fromList [[2], [3, 1]])
  , "contains-false"      ~: False ~=? contains 4 (fromList [[2], [3, 1]])

  , "exists-disjoint-true"  ~: True  ~=? existsDisjoint (fromList [[1, 2], [3]]) (fromList [[4, 5], [5]])
  , "exists-disjoint-false" ~: False ~=? existsDisjoint (fromList [[1, 2], [3]]) (fromList [[2, 3], [1, 3]])
  ]

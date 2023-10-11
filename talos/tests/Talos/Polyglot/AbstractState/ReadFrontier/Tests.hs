module Talos.Polyglot.AbstractState.ReadFrontier.Tests where

import           Daedalus.GUID (firstValidGUID, succGUID)
import           Talos.Polyglot.AbstractState.ReadFrontier
import qualified Talos.Polyglot.AbstractState.ThreadSet as TS

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Test.HUnit

id1 = firstValidGUID
id2 = succGUID id1
id3 = succGUID id2

-- TODO(cns): Property-based testing.

tests :: Test
tests = TestList
  [ "contains-empty" ~: False ~=? contains empty id1 id1
  , "contains-false" ~: False ~=? contains (singleton id1 $ TS.singleton id2) id1 id1
  , "contains-true" ~: True ~=? contains (singleton id1 $ TS.singleton id1) id1 id1

  , "join-empty" ~:
      singleton id1 (TS.singleton id2) ~=?
        join empty (singleton id1 $ TS.singleton id2)
  , "join-idemp" ~:
      singleton id1 (TS.singleton id2) ~=?
        join (singleton id1 $ TS.singleton id2) (singleton id1 $ TS.singleton id2)
  , "join" ~:
      Map.fromList [(id1, TS.fromList [[id1], [id2]])] ~=?
        join (singleton id1 $ TS.singleton id2) (singleton id1 $ TS.singleton id1)

  , "sequence-empty" ~: singleton id1 TS.empty ~=? sequenceOne empty id1
  , "sequence-absent" ~:
      Map.fromList [(id1, TS.empty), (id2, TS.singleton id1)] ~=?
        sequenceOne (singleton id2 TS.empty) id1
  , "sequence-present" ~:
      Map.fromList [(id1, TS.singleton id1), (id2, TS.singleton id1)] ~=?
        sequenceOne (Map.fromList [(id1, TS.empty), (id2, TS.empty)]) id1
  ]

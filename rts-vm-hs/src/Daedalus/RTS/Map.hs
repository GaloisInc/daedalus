module Daedalus.RTS.Map
  ( Map
  , Map.empty
  , Map.lookup
  , Map.member
  , Map.insert
  , Map.toList
  , insertMaybe
  ) where

import Data.Map(Map)
import qualified Data.Map as Map

insertMaybe :: Ord k => k -> v -> Map k v -> Maybe (Map k v)
insertMaybe k v mp =
  case Map.insertLookupWithKey pick k v mp of
    (Nothing, mp1) -> Just mp1
    _              -> Nothing
  where
  pick _ x _ = x    -- doesn't matter




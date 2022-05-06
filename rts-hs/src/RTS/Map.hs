{-# Language TypeFamilies, MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
module RTS.Map
  ( Map
  , Map.empty
  , Map.lookup
  , Map.insert
  , Map.toList
  , insertMaybe
  ) where

import Data.Map(Map)
import qualified Data.Map as Map

import RTS.Base()

insertMaybe :: Ord k => k -> v -> Map k v -> Maybe (Map k v)
insertMaybe k v mp =
  case Map.insertLookupWithKey pick k v mp of
    (Nothing, mp1) -> Just mp1
    _              -> Nothing
  where
  pick _ x _ = x    -- doesn't matter




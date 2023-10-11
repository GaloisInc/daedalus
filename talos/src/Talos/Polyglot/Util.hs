{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.Util where

import           Data.Map              (Map)
import           Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Debug.Trace           (trace)

import Daedalus.PP

mapUnion :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
mapUnion f = merge preserveMissing preserveMissing (zipWithMatched (\_ -> f))

flattenSets :: Ord a => Set (Set a) -> Set a
flattenSets = Set.foldl Set.union Set.empty

ppSets :: PP a => Set (Set a) -> Doc
ppSets sets = hsep $ map doSet (Set.toList sets)
  where
    doSet set = braces . hsep $ map pp (Set.toList set)

-- For debugging.  Not for production use.
debug :: c -> String -> c
-- debug = flip trace
debug c _ = c
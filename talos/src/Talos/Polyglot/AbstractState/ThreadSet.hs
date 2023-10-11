{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.AbstractState.ThreadSet where

import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Daedalus.PP

newtype Ord a => ThreadSet a =
  ThreadSet (Set (Set a))
  deriving (Eq, Ord, Show)

-- | Thread set containing a single empty thread.
empty :: Ord a => ThreadSet a
empty = ThreadSet (Set.singleton Set.empty)

-- | Thread set containing a single thread with elt.
singleton :: Ord a => a -> ThreadSet a
singleton elt = ThreadSet $ Set.singleton $ Set.singleton elt

-- | Build a thread set from a list of lists.  Each sublist becomes
-- a thread.
fromList :: Ord a => [[a]] -> ThreadSet a
fromList lists = ThreadSet sets
  where
    sets = foldl (\acc l -> Set.insert (Set.fromList l) acc) Set.empty lists

-- | Union two thread sets.
join :: Ord a => ThreadSet a -> ThreadSet a -> ThreadSet a
join (ThreadSet left) (ThreadSet right) = ThreadSet $ Set.union left right

-- | Add `elt` to all threads.
sequenceOne :: Ord a => a -> ThreadSet a -> ThreadSet a
sequenceOne elt (ThreadSet left) = ThreadSet $ Set.map (Set.insert elt) left

-- | Return a set of all node IDs in the ThreadSet.
flatten :: Ord a => ThreadSet a -> Set a
flatten (ThreadSet threads) = Set.foldl Set.union Set.empty threads

-- | Remove all threads instersecting `elts`.
removeAllIntersecting :: Ord a => Set a -> ThreadSet a -> ThreadSet a
removeAllIntersecting elts (ThreadSet threads) = ThreadSet $ Set.filter (Set.disjoint elts) threads

-- | True if any set contains elt.
contains :: Ord a => a -> ThreadSet a -> Bool
contains elt (ThreadSet threads) = Set.foldl (\acc thread -> acc || Set.member elt thread) False threads

instance (Ord a, PP a) => PP (ThreadSet a) where
  pp (ThreadSet threads)= braces . hsep $ map doThread (Set.toList threads)
    where
      doThread t = braces . hsep $ map pp (Set.toList t)
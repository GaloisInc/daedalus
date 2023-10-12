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

-- | Thread set containing no threads.  This is the zero element; it is the
-- additive (join) identity and multiplicative (sequence) annihilator.
empty :: Ord a => ThreadSet a
empty = ThreadSet Set.empty

-- | Thread set containing a single empty thread.  This is the one element;
-- it is the multiplicative (sequence) identity.
emptyThread :: Ord a => ThreadSet a
emptyThread = ThreadSet (Set.singleton Set.empty)

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

-- | Add `elts` to all threads.
sequenceMany :: Ord a => Set a -> ThreadSet a -> ThreadSet a
sequenceMany elts left = Set.foldl (\acc elt -> sequenceOne elt acc) left elts

-- | Adds each thread in right to all threads in left.
sequence :: Ord a => ThreadSet a -> ThreadSet a -> ThreadSet a
sequence left (ThreadSet right) = Set.foldl (\acc rt -> sequenceMany rt acc) left right

-- | Return a set of all node IDs in the ThreadSet.
flatten :: Ord a => ThreadSet a -> Set a
flatten (ThreadSet threads) = Set.foldl Set.union Set.empty threads

-- | Remove all threads instersecting `elts`.
removeAllIntersecting :: Ord a => Set a -> ThreadSet a -> ThreadSet a
removeAllIntersecting elts (ThreadSet threads) = ThreadSet $ Set.filter (Set.disjoint elts) threads

-- | True if any set contains elt.
contains :: Ord a => a -> ThreadSet a -> Bool
contains elt (ThreadSet threads) = Set.foldl (\acc thread -> acc || Set.member elt thread) False threads

-- | Test if the predicate holds on at least one thread in the set.
existsThread :: Ord a => (Set a -> Bool) -> ThreadSet a -> Bool
existsThread p (ThreadSet threads) = Set.foldl (\acc set -> acc || p set) False threads

-- | Remove threads that do not match the predicate.
filter :: Ord a => (Set a -> Bool) -> ThreadSet a -> ThreadSet a
filter p (ThreadSet threads) = ThreadSet $ Set.filter p threads

-- | True if there exists a thread lt in left and rt in right such that t1 and
-- t2 are disjoins.
existsDisjoint :: Ord a => ThreadSet a -> ThreadSet a -> Bool
existsDisjoint (ThreadSet left) (ThreadSet right) = 
  Set.foldl (\lacc lt -> 
    lacc || (Set.foldl (\racc rt -> 
      Set.disjoint lt rt || racc 
    ) False right)
  ) False left

instance (Ord a, PP a) => PP (ThreadSet a) where
  pp (ThreadSet threads)= braces . hsep $ map doThread (Set.toList threads)
    where
      doThread t = braces . hsep $ map pp (Set.toList t)
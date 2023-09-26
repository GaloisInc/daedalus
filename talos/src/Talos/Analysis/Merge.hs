
module Talos.Analysis.Merge where

import           Data.Functor.Identity (Identity (Identity), runIdentity)
import           Data.Map              (Map)
import qualified Data.Map              as Map

import           Daedalus.Core         (Case (..), LoopClass' (..),
                                        LoopCollection' (..),
                                        LoopMorphism' (..))
import           Daedalus.Panic        (panic)
import Data.List (partition)
import Data.Foldable (foldl', toList)

-- Merging
--
-- Similar to a semigroup, but with more restrictions about how it can
-- be used (i.e., the merged objects come from the same program)

class HasEmpty e where
  empty :: e

class Merge a where
  merge :: a -> a -> a

-- | Convenience type for taking advantage of functions which use `Monoid` instances
newtype MergeAsMonoid m = MergeAsMonoid { getMergeAsMonoid :: m }

instance Merge m => Semigroup (MergeAsMonoid m) where
  MergeAsMonoid m1 <> MergeAsMonoid m2 = MergeAsMonoid (m1 `merge` m2)

instance (HasEmpty m, Merge m) => Monoid (MergeAsMonoid m) where
  mempty = MergeAsMonoid empty

instance (Ord k, Merge v) => Merge (Map k v) where
  merge = Map.unionWith merge

instance (Merge a, Merge b) => Merge (a, b) where
  merge (a, b) (a', b') = (merge a a', merge b b')

instance Merge a => Merge (Case a) where
  merge (Case e alts1) (Case _e alts2) = Case e (zipWith goAlt alts1 alts2)
    where
      goAlt (p, a1) (_p, a2) = (p, merge a1 a2)

instance Merge a => Merge (Identity a) where
  merge v1 v2 = Identity (merge (runIdentity v1) (runIdentity v2))

instance Merge a => Merge (Maybe a) where
  -- Is this too permissive?  We may require same ctor?
  merge (Just a) (Just b) = Just (merge a b)
  merge a@(Just {}) _ = a
  merge _ b@(Just {}) = b
  merge Nothing Nothing = Nothing

-- This assumes the list lengths are the same.
instance Merge a => Merge [a] where
  merge = zipWith merge
  
instance (Merge e, Merge b) => Merge (LoopClass' e b) where
  merge l r =
    case (l, r) of
      (ManyLoop bt n e m_e b, ManyLoop _bt _n e' m_e' b') ->
        ManyLoop bt n (merge e e') (merge m_e m_e') (merge b b')
      (RepeatLoop bt n e b, RepeatLoop _bt _n e' b') ->
        RepeatLoop bt n (merge e e') (merge b b')
      (MorphismLoop lm, MorphismLoop lm') -> MorphismLoop (merge lm lm')
      _ -> panic "Mismatched loop classes" []

instance (Merge e, Merge b) => Merge (LoopMorphism' e b) where
  merge l r =
    case (l, r) of
      (FoldMorphism n e lc b, FoldMorphism _n e' lc' b') ->
        FoldMorphism n (merge e e') (merge lc lc') (merge b b')
      (MapMorphism lc b, MapMorphism lc' b') ->
        MapMorphism (merge lc lc') (merge b b')
      _ -> panic "Mismatched loop morphisms" []

instance Merge e => Merge (LoopCollection' e) where
  merge l r = l { lcCol = merge (lcCol l) (lcCol r) }


-- -----------------------------------------------------------------------------
-- Helper functions

-- | Merge two lists of non-overlapping elements
mergeOverlapping :: Merge p => (p -> p -> Bool) -> [p] -> [p] -> [p]
mergeOverlapping ovlf = go
  where
    go [] d2 = d2
    go (d : d1) d2 = go d1 (newds : indep)
      where
        newds = foldl merge d dep
        (dep, indep) = partition (ovlf d) d2

mergeMaybe :: (Merge p, Foldable t) => t p -> Maybe p
mergeMaybe ps = foldl' merge Nothing (Just <$> toList ps)

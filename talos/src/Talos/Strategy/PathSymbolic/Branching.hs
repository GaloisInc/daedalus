{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- A branching thing

module Talos.Strategy.PathSymbolic.Branching
  ( Branching(..)
  -- * Constructors
  , singleton  
  , branching
  
  -- * Operations
  , disjointUnion
  , fold
  , fold1  
  , foldM
  , fold1M
  , unzip
  , unzip3
  , catMaybes
  , mapVariants
  , partitionEithers

  , empty
  , null
  , select
  , resolve
  , muxMaps
  , toSExpr
  , reducePS
  -- * Debugging
  , invariant
  ) where

import           Control.Monad                       (ap)
import qualified Data.Either                         as Either
import           Data.Foldable                       (foldl', foldlM)
import qualified Data.List                           as List
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (mapMaybe)
import           GHC.Generics                        (Generic)
import           Prelude                             hiding (null, unzip,
                                                      unzip3)
import qualified Prelude
import qualified SimpleSMT                           as S

import           Daedalus.Panic                      (panic)
import           Daedalus.PP                         (PP, pp, vcat)

import           Talos.Lib                           (andMany, findM)
import qualified Talos.Strategy.PathSymbolic.PathSet as PS
import           Talos.Strategy.PathSymbolic.PathSet (PathSet)

-- FIXME: maybe rep. as a tree
-- data Branching a = Leaf (PathSet, a) | Conj PathSet [Branching a]

-- | A 'Branching' represents a branching value, where exactly one
-- path is viable (perhaps assuming some restricting context).
-- Because the branches are disjoint and total, it is possible to
-- remove one when converting to an if-then-else SMT expression, as
-- the negation of the remaining branches should entail the elided
-- branch.
newtype Branching a = Branching
  { variants :: [ (PathSet, a) ]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Applicative Branching where
  pure v = Branching { variants = [(PS.true, v)] }
  (<*>) = ap

instance MonadFail Branching where
  fail _ = Branching []

-- Essentially 'join', we could use this structure to optimise
-- e.g. query size.
instance Monad Branching where
  b >>= f =
    branching [ (PS.conj ps1 ps2, v)
              | (ps1, w) <- variants b
              , (ps2, v) <- variants (f w)
              ]
    
singleton :: a -> Branching a
singleton = pure

-- | Smart constructor, prunes false pathsets
branching :: [(PathSet, a)] -> Branching a
branching = Branching . filter (not . PS.null . fst) 

empty :: Branching a
empty = Branching []
  
-- | An empty branching is denoted as False
null :: Branching a -> Bool
null = Prelude.null . variants

-- For debugging/checking.  Note the empty branching does not satisfy
-- this.
invariant :: Branching a -> S.SExpr
invariant b = andMany atMostOneTrue
  where
    -- FIXME: name these.
    ps = map (PS.toSExpr . fst) (variants b)
    -- atLeastOneTrue = orMany ps
    atMostOneTrue =
      [ ps' `S.implies` S.not (andMany pss)
      | (ps', pss) <- holes [] [] ps ] 

    -- Ignores order
    holes _lhs acc [] = acc
    holes lhs acc (x : xs) = holes (x : lhs) ((x, lhs ++ xs) : acc) xs

-- -- FIXME: maybe discard the largest path
-- split :: Branching a -> Maybe (a, [(ps, a)])
-- split = undefined

-- Standard operations

disjointUnion :: Branching a -> Branching a -> Branching a
disjointUnion b1 b2 = branching (variants b1 <> variants b2)

fold :: (PathSet -> a -> b -> b) -> b -> Branching a -> b
fold f i b = foldl' (\a' (ps, a) -> f ps a a') i (variants b)

fold1 :: (PathSet -> a -> a -> a) -> Branching a -> a
fold1 _f Branching { variants = [] } = panic "Empty branching in fold1" []
fold1 f Branching { variants = (_, v) : vs } =
  foldl' (\a' (ps, a) -> f ps a a') v vs

foldM :: Monad m => (PathSet -> a -> b -> m b) -> b -> Branching a -> m b
foldM f i b = foldlM (\a' (ps, a) -> f ps a a') i (variants b)

fold1M :: Monad m => (PathSet -> a -> a -> m a) -> Branching a -> m a
fold1M _f Branching { variants = [] } = panic "Empty branching in fold1M" []
fold1M f Branching { variants = (_, v) : vs } =
  foldlM (\a' (ps, a) -> f ps a a') v vs

catMaybes :: Branching (Maybe a) -> Branching a
catMaybes b = branching $ mapMaybe sequence (variants b) -- sequence :: (a, Maybe b) -> Maybe (a, b)

-- FIXME: duplicates the pathsets
unzip :: Branching (a, b) -> (Branching a, Branching b)
unzip b = ( branching (zip pss vs1)
          , branching (zip pss vs2)
          )
  where
    (pss, vs)  = Prelude.unzip (variants b)
    (vs1, vs2) = Prelude.unzip vs

-- FIXME: duplicates pathsets
unzip3 :: Branching (a, b, c) -> (Branching a, Branching b, Branching c)
unzip3 b = ( branching (zip pss vs1)
           , branching (zip pss vs2)
           , branching (zip pss vs3)
           )
  where
    (pss, vs)  = Prelude.unzip (variants b)
    (vs1, vs2, vs3) = Prelude.unzip3 vs

-- Does not duplicate
partitionEithers :: Branching (Either a b) -> (Branching a, Branching b)
partitionEithers b = (branching ls, branching rs)
  where
    (ls, rs) = Either.partitionEithers [ either (Left . (,) p) (Right . (,) p) v
                                       | (p, v) <- variants b ]

mapVariants :: (PathSet -> a -> Maybe (PathSet, a)) -> Branching a -> Branching a
mapVariants f bvs = branching new
  where
    new = mapMaybe (uncurry f) (variants bvs)

muxMaps :: Ord k => Branching (Map k v) -> Map k (Branching v)
muxMaps bmv = branching <$> ms'
  where
  ms' = Map.unionsWith (<>) [ List.singleton . (,) ps <$> m' | (ps, m') <- variants bmv ]

-- | Find the reachable branching value in a model
resolve :: PS.PathSetModelMonad m => Branching a -> m (Maybe a)
resolve bvs =
  fmap snd <$> findM (PS.fromModel . fst) (variants bvs)

reducePS :: Branching PathSet -> PathSet
reducePS b = PS.disjMany (map (uncurry PS.conj) (variants b))

-- | Return some element, if it exists.
select :: Branching a -> Maybe a
select b | (_, x) : _ <- variants b = Just x
         | otherwise = Nothing

toSExpr :: Branching S.SExpr -> S.SExpr
toSExpr = fold (S.ite . PS.toSExpr) (S.bool False)

-- -----------------------------------------------------------------------------
-- Instances

instance PP a => PP (Branching a) where
  pp b = vcat [ pp ps <> " ==> " <> pp v  | (ps, v) <- variants b ]

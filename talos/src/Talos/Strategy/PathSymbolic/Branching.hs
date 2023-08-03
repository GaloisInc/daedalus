{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- A branching thing

module Talos.Strategy.PathSymbolic.Branching
  ( Branching(..)
  , branching
  , branchingMaybe
  , fold
  , foldM
  , mapVariants
  
  , resolve
  , muxMaps
  
  ) where

import           GHC.Generics                              (Generic)

import Talos.Strategy.PathSymbolic.PathSet ( PathSet )
import qualified Talos.Strategy.PathSymbolic.PathSet as PS
import Control.Monad (ap)
import Data.Foldable (foldl', foldlM)
import           Talos.Lib                           (findM)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Merge.Strict               as Map
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import Daedalus.Panic (panic)

data Branching a = Branching
  { variants :: [ (PathSet, a) ]
  , base     :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Applicative Branching where
  pure v = Branching { variants = [], base = v }
  (<*>) = ap

instance Monad Branching where
  b >>= f = Branching { variants = vs , base = base'}
    where
      bs = f <$> b
      vsOne (ps1, b') = (ps1, base b') :
                        [ (ps', v)
                        | (ps2, v) <- variants b'
                        , Just ps' <- [ PS.conjPathSet ps1 ps2 ]
                        ]
      vs = variants (base bs) ++ concatMap vsOne (variants bs)
      base' = base (base bs)

branching :: [(PathSet, a)] -> a -> Branching a
branching = Branching

branchingMaybe :: [(PathSet, a)] -> Maybe a -> Maybe (Branching a)
branchingMaybe vs m_b
  | (_, v) : vs' <- vs, Nothing <- m_b = Just (Branching vs' v)
  | otherwise = Branching vs <$> m_b

branchingMaybe' :: [(PathSet, a)] -> Maybe a -> Branching a
branchingMaybe' vs m_b = fromMaybe err (branchingMaybe vs m_b)
  where
    err = panic "Expecting non-empty branching" []

fold :: (PathSet -> a -> a -> a) -> Branching a -> a
fold f b = foldl' (\a' (ps, a) -> f ps a a') (base b) (variants b)

foldM :: Monad m => (PathSet -> a -> a -> m a) -> Branching a -> m a
foldM f b = foldlM (\a' (ps, a) -> f ps a a') (base b) (variants b)

mapVariants :: (PathSet -> a -> Maybe (PathSet, a)) -> Branching a -> Branching a
mapVariants f bvs = bvs { variants = new }
  where
    new = mapMaybe (uncurry f) (variants bvs)

muxMaps :: Ord k => Branching (Map k v) -> Map k (Branching v)
muxMaps bmv = do
  ms' <- Map.unionsWith (<>) [ Map.mapMaybeWithKey (f' ps) m'
                             | (ps, m') <- variants bmv ]
  let lhs = Map.mapMissing (const (`branchingMaybe'` Nothing))
      rhs = Map.mapMissing (const (Branching []))
  Map.merge lhs rhs (Map.zipWithMatched (const Branching)) ms' (base bmv)
  where
    f' ps _k el = fmap (: []) <$> f ps el
    g' _k xs y = g xs (Just y)

-- | Find the reachable branching value in a model
resolve :: PS.PathSetModelMonad m => Branching a -> m a
resolve bvs =
  maybe (base bvs) snd <$> findM (PS.fromModel . fst) (variants bvs)


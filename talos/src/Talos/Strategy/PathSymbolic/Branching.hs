{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- A branching thing

module Talos.Strategy.PathSymbolic.Branching
  ( Branching(..)
  -- * Constructors
  , singleton  
  , branching
  , branchingMaybe
  , branchingNE
  
  -- * Operations
  , fold
  , foldM
  , unzip
  , unzip3
  , catMaybes
  , mapVariants
  
  , resolve
  , muxMaps
  , toSExpr
  ) where

import Prelude hiding (unzip, unzip3)
import qualified Prelude

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
import Data.List.NonEmpty (NonEmpty(..))

import qualified SimpleSMT as S
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

singleton :: a -> Branching a
singleton = pure

branching :: [(PathSet, a)] -> a -> Branching a
branching = Branching


branchingNE :: NonEmpty (PathSet, a) -> Branching a
branchingNE ((_, a) :| rest) = branching rest a

branchingMaybe :: [(PathSet, a)] -> Maybe a -> Maybe (Branching a)
branchingMaybe vs m_b
  | (_, v) : vs' <- vs, Nothing <- m_b = Just (Branching vs' v)
  | otherwise = Branching vs <$> m_b

branchingMaybe' :: [(PathSet, a)] -> Maybe a -> Branching a
branchingMaybe' vs m_b = fromMaybe err (branchingMaybe vs m_b)
  where
    err = panic "Expecting non-empty branching" []

-- Standard operations

fold :: (PathSet -> a -> a -> a) -> Branching a -> a
fold f b = foldl' (\a' (ps, a) -> f ps a a') (base b) (variants b)

foldM :: Monad m => (PathSet -> a -> a -> m a) -> Branching a -> m a
foldM f b = foldlM (\a' (ps, a) -> f ps a a') (base b) (variants b)

catMaybes :: Branching (Maybe a) -> Maybe (Branching a)
catMaybes b = -- sequence :: (a, Maybe b) -> Maybe (a, b)
  branchingMaybe (mapMaybe sequence (variants b)) (base b)

-- FIXME: duplicates the pathsets
unzip :: Branching (a, b) -> (Branching a, Branching b)
unzip b = ( Branching { variants = zip pss vs1, base = fst (base b) }
          , Branching { variants = zip pss vs2, base = snd (base b) }
          )
  where
    (pss, vs)  = Prelude.unzip (variants b)
    (vs1, vs2) = Prelude.unzip vs

-- FIXME: duplicates pathsets
unzip3 :: Branching (a, b, c) -> (Branching a, Branching b, Branching c)
unzip3 b = ( Branching { variants = zip pss vs1, base = b1 }
           , Branching { variants = zip pss vs2, base = b2 }
           , Branching { variants = zip pss vs3, base = b3 }
           )
  where
    (pss, vs)  = Prelude.unzip (variants b)
    (vs1, vs2, vs3) = Prelude.unzip3 vs
    (b1, b2, b3) = base b

mapVariants :: (PathSet -> a -> Maybe (PathSet, a)) -> Branching a -> Branching a
mapVariants f bvs = bvs { variants = new }
  where
    new = mapMaybe (uncurry f) (variants bvs)

muxMaps :: Ord k => Branching (Map k v) -> Map k (Branching v)
muxMaps bmv = Map.merge lhs rhs (Map.zipWithMatched (const Branching)) ms' (base bmv)
  where
  ms' = Map.unionsWith (<>) [ (: []) . (,) ps <$> m' | (ps, m') <- variants bmv ]
  lhs = Map.mapMissing (const (`branchingMaybe'` Nothing))
  rhs = Map.mapMissing (const (Branching []))

-- | Find the reachable branching value in a model
resolve :: PS.PathSetModelMonad m => Branching a -> m a
resolve bvs =
  maybe (base bvs) snd <$> findM (PS.fromModel . fst) (variants bvs)

toSExpr :: Branching S.SExpr -> S.SExpr
toSExpr = fold (S.ite . PS.toSExpr)

{-# LANGUAGE ViewPatterns #-}

module Talos.Strategy.PathCache ( PathCache, emptyPathCache, lookupPathCache, addPathCache
                                , PathCachePolicy(..)
                                ) where

import qualified Data.Vector as V -- FIXME: use mvector

import Talos.SymExec.Path
import Data.Map (Map)
import Talos.Analysis.Monad (SliceName)
import qualified Data.Map as Map

import Talos.Strategy.Monad

type PathCacheNode = V.Vector SelectedPath


data PathCachePolicy =
  CacheNever | CacheBounded Int Int

data PathCache =
  PathCache { pcEntries :: Map SliceName PathCacheNode
            , pcPolicy  :: PathCachePolicy
            }

emptyPathCache :: PathCachePolicy -> PathCache
emptyPathCache = PathCache mempty 

addPathCache :: SliceName -> SelectedPath -> PathCache -> PathCache
addPathCache _sn _sp (pc@PathCache { pcPolicy = CacheNever }) = pc
addPathCache sn sp (pc@PathCache { pcPolicy = CacheBounded l h}) =
  case Map.lookup sn (pcEntries pc) of
    Nothing  -> pc { pcEntries = Map.insert sn (V.singleton sp) (pcEntries pc) }
    Just pcn | V.length pcn >= h -> pc
    Just pcn -> pc { pcEntries = Map.insert sn (V.snoc pcn sp) (pcEntries pc) }

-- | This will return an entry if we have enough and the policy tells us to
lookupPathCache :: LiftStrategyM m => SliceName -> PathCache -> m (Maybe SelectedPath)
lookupPathCache sn pc =
  case Map.lookup sn (pcEntries pc) of
    Just pcn
      | CacheBounded l h <- pcPolicy pc, V.length pcn >= l -> do
          -- Randomly pick an element in the (0, max) range, and return it if it exists
          ix <- randR (0, h)
          pure $ pcn V.!? ix
    _ -> pure Nothing

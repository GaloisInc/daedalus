{-# Language OverloadedStrings, ViewPatterns #-}
module Daedalus.SourceRange
  ( SourcePos(..)
  , prettySourcePos
  , prettySourcePosLong
  , beforeStartPos

  , SourceRange(..)
  , prettySourceRange
  , prettySourceRangeLong

  , (<->)
  , sourceRangeContainedIn

  , HasRange(..)

  , HasSourcePaths(..)

  , synthetic
  , syntheticPos
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

import AlexTools hiding ((<->))

synthetic :: SourceRange
synthetic = SourceRange syntheticPos syntheticPos

syntheticPos :: SourcePos
syntheticPos = SourcePos 0 0 0 "synthetic"

-- | Union of ranges
(<->) :: (HasRange a, HasRange b) => a -> b -> SourceRange
x <-> y = SourceRange { sourceFrom = from, sourceTo = to }
  where
  xRange   = range x
  yRange   = range y
  (from,_) = sortSourcePos (sourceFrom xRange) (sourceFrom yRange)
  (_,to)   = sortSourcePos (sourceTo xRange)   (sourceTo yRange)

sortSourcePos :: SourcePos -> SourcePos -> (SourcePos,SourcePos)
sortSourcePos x y
  | sourceFile x == sourceFile y && sourceIndex x > sourceIndex y = (y,x)
  | otherwise = (x,y)

-- | Is one of the ranges contained in the other?
-- If so, returned it (the smaller one).
sourceRangeContainedIn ::
  (HasRange a, HasRange b) => a -> b -> Maybe SourceRange
x `sourceRangeContainedIn` y
  | fromX <= fromY = if toX >= toY then Just yr else Nothing
  | otherwise      = if toY >= toX then Just xr else Nothing
  where
  xr@SourceRange { sourceFrom = sourceIndex -> fromX
                 , sourceTo   = sourceIndex -> toX
                 } = range x
  yr@SourceRange { sourceFrom = sourceIndex -> fromY
                 , sourceTo   = sourceIndex -> toY
                 } = range y

class HasSourcePaths a where
  getSourcePaths :: a -> Set FilePath
  mapSourcePaths :: (FilePath -> FilePath) -> a -> a

instance (HasSourcePaths a, HasSourcePaths b) => HasSourcePaths (a,b) where
  getSourcePaths (a,b) = Set.union (getSourcePaths a) (getSourcePaths b)
  mapSourcePaths f (a,b) = (mapSourcePaths f a, mapSourcePaths f b)

instance HasSourcePaths a => HasSourcePaths [a] where
  getSourcePaths   = Set.unions . map getSourcePaths
  mapSourcePaths f = fmap (mapSourcePaths f)

instance HasSourcePaths a => HasSourcePaths (Maybe a) where
  getSourcePaths   = maybe Set.empty getSourcePaths
  mapSourcePaths f = fmap (mapSourcePaths f)

instance HasSourcePaths SourceRange where
  getSourcePaths x = getSourcePaths (sourceFrom x, sourceTo x)
  mapSourcePaths f x =
    SourceRange { sourceFrom = mapSourcePaths f (sourceFrom x)
                    , sourceTo   = mapSourcePaths f (sourceTo x)
                    }

instance HasSourcePaths SourcePos where
  getSourcePaths x = Set.singleton (Text.unpack (sourceFile x))
  mapSourcePaths f x =
    x { sourceFile = Text.pack (f (Text.unpack (sourceFile x))) }

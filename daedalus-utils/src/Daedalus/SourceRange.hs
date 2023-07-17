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

  , synthetic
  , syntheticPos
  ) where

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







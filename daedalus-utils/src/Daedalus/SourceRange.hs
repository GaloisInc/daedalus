{-# Language OverloadedStrings #-}
module Daedalus.SourceRange
  ( SourcePos(..)
  , prettySourcePos
  , prettySourcePosLong
  , beforeStartPos

  , SourceRange(..)
  , prettySourceRange
  , prettySourceRangeLong

  , HasRange(..)
  , (<->)

  , synthetic
  , syntheticPos
  ) where

import AlexTools

synthetic :: SourceRange
synthetic = SourceRange syntheticPos syntheticPos

syntheticPos :: SourcePos
syntheticPos = SourcePos 0 0 0 "synthetic"

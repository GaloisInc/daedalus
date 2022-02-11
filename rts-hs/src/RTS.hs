{-# Language DataKinds, KindSignatures, FunctionalDependencies #-}
{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# Language MonoLocalBinds #-}
module RTS
  ( Numeric(..), Arith(..), UInt, SInt, SizeType, uint8
  , Bitdata(..)
  , fromUInt, fromSInt
  , shiftl, shiftr, lcat, cat
  , Literal
  , HasStruct, HasUnion
  , module RTS.ParserAPI
  , DDL
  , Convert(..)
  , IsLoop(..)
  , IsMapLoop(..)
  , ColElType
  , ColKeyType
  , wordToFloat
  , wordToDouble
  )
  where

import Data.Map(Map)
import GHC.Float

import RTS.Base
import RTS.Numeric
import RTS.Vector
import RTS.Input
import RTS.ParserAPI

class Convert a b where
  convert :: a -> b
  convertMaybe :: a -> Maybe b

instance {-# OVERLAPPING #-} Convert a a where
  convert = id
  convertMaybe = Just
  {-# INLINE convert #-}
  {-# INLINE convertMaybe #-}

instance {-# OVERLAPPING #-} Convert Float Double where
  convert = float2Double
  convertMaybe = Just . float2Double

instance {-# OVERLAPPING #-} Convert Double Float where
  convert = double2Float
  convertMaybe x
    | isNaN x || float2Double y == x = Just y
    | otherwise                      = Nothing
    where y = double2Float x


instance {-# OVERLAPPING #-} Convert Integer Float where
  convert      = cvtHsNum
  convertMaybe = cvtNumToFloatingMaybe

instance {-# OVERLAPPING #-} SizeType n => Convert (UInt n) Float where
  convert      = cvtHsNum
  convertMaybe = cvtNumToFloatingMaybe

instance {-# OVERLAPPING #-} SizeType n => Convert (SInt n) Float where
  convert      = cvtHsNum
  convertMaybe = cvtNumToFloatingMaybe

instance {-# OVERLAPPING #-} Convert Integer Double where
  convert      = cvtHsNum
  convertMaybe = cvtNumToFloatingMaybe

instance {-# OVERLAPPING #-} SizeType n => Convert (UInt n) Double where
  convert      = cvtHsNum
  convertMaybe = cvtNumToFloatingMaybe

instance {-# OVERLAPPING #-} SizeType n => Convert (SInt n) Double where
  convert      = cvtHsNum
  convertMaybe = cvtNumToFloatingMaybe


instance {-# OVERLAPPING #-} Convert Double Integer where
  convert = cvtFloatingToNum
  convertMaybe = cvtFloatingToNumMaybe

instance {-# OVERLAPPING #-} SizeType n => Convert Double (UInt n) where
  convert = cvtFloatingToNum
  convertMaybe = cvtFloatingToNumMaybe

instance {-# OVERLAPPING #-} SizeType n => Convert Double (SInt n) where
  convert = cvtFloatingToNum
  convertMaybe = cvtFloatingToNumMaybe

instance {-# OVERLAPPING #-} Convert Float Integer where
  convert = cvtFloatingToNum
  convertMaybe = cvtFloatingToNumMaybe

instance {-# OVERLAPPING #-} SizeType n => Convert Float (UInt n) where
  convert = cvtFloatingToNum
  convertMaybe = cvtFloatingToNumMaybe

instance {-# OVERLAPPING #-} SizeType n => Convert Float (SInt n) where
  convert = cvtFloatingToNum
  convertMaybe = cvtFloatingToNumMaybe







instance (Numeric a, Numeric b) => Convert a b where
  convert = cvtNum
  convertMaybe = cvtNumMaybe
  {-# INLINE convert #-}
  {-# INLINE convertMaybe #-}


-- | Operations available on any DDL type
-- The Ord superclass is for Maps
class (Show a, Ord a, VecElem a) => DDL a

instance DDL Integer
instance DDL Bool
instance DDL ()
instance (VecElem (UInt n), SizeType n) => DDL (UInt n)
instance (VecElem (SInt n), SizeType n) => DDL (SInt n)
instance DDL a => DDL (Maybe a)
instance (DDL k, DDL v) => DDL (Map k v)
instance DDL a => DDL (Vector a)
instance DDL Input
instance DDL Float
instance DDL Double



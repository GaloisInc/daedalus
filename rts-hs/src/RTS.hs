{-# Language DataKinds, KindSignatures, FunctionalDependencies #-}
{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# Language MonoLocalBinds #-}
module RTS
  ( Numeric(..), UInt, SInt, SizeType, uint8
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
  )
  where

import Data.Map(Map)

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



{-# Language ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}
module Daedalus.Value.Extract
  ( Value(..)
  , FromValue(..)
  ) where

import qualified Data.Text as Text
import GHC.OverloadedLabels(IsLabel(..))
import GHC.TypeLits(symbolVal,KnownSymbol)
import Data.Proxy(Proxy(..))
import Data.Vector(Vector)
import Data.Map(Map,(!))
import qualified Data.Map as Map
import Data.ByteString(ByteString)
import Data.Word(Word8)

import Daedalus.Value

class FromValue a where
  fromValue :: Value -> a

instance FromValue Value where
  fromValue = id

instance FromValue Bool where
  fromValue = valueToBool

instance FromValue Integer where
  fromValue = valueToIntegral

instance FromValue Word8 where
  fromValue = valueToByte

instance FromValue Char where
  fromValue = toEnum . fromEnum . valueToByte

instance FromValue ByteString where
  fromValue = valueToByteString

instance FromValue a => FromValue (Vector a) where
  fromValue = fmap fromValue . valueToVector

instance FromValue a => FromValue [a] where
  fromValue = fmap fromValue . valueToList

instance FromValue a => FromValue (Maybe a) where
  fromValue = fmap fromValue . valueToMaybe

instance (Ord k, FromValue k, FromValue v) => FromValue (Map k v) where
  fromValue val =
    Map.fromList [ (fromValue k, fromValue v)
                 | (k,v) <- Map.toList (valueToMap val)
                 ]

instance (KnownSymbol l, FromValue a) => IsLabel l (Value -> a) where
  fromLabel = let lab = Text.pack (symbolVal (Proxy :: Proxy l))
              in \v -> fromValue (valueToStructMap v ! lab)



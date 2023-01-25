module RTS.HasInputs where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString(ByteString)
import Data.ByteString.Short(ShortByteString)


class HasInputs a where
  getInputs :: a -> Map ShortByteString ByteString

instance HasInputs () where
  getInputs _ = Map.empty

instance HasInputs a => HasInputs [a] where
  getInputs = Map.unions . map getInputs

instance HasInputs a => HasInputs (Maybe a) where
  getInputs = maybe Map.empty getInputs




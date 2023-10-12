module Daedalus.RTS.HasInputs where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString(ByteString)
import Data.ByteString.Short(ShortByteString)


class HasInputs a where
  getInputs :: a -> Map ShortByteString ByteString

instance HasInputs () where
  getInputs = const Map.empty
  {-# INLINE getInputs #-}

instance HasInputs a => HasInputs [a] where
  getInputs = Map.unions . map getInputs
  {-# INLINE getInputs #-}

instance HasInputs a => HasInputs (Maybe a) where
  getInputs = maybe Map.empty getInputs
  {-# INLINE getInputs #-}




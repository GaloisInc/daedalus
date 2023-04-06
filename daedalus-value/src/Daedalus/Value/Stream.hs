{-# Language BlockArguments #-}
module Daedalus.Value.Stream where

import Daedalus.RTS.Input
import Daedalus.RTS.Numeric
import Daedalus.Value.Type
import Daedalus.Value.Utils

vStreamFromArray :: Value -> Value -> Value
vStreamFromArray a b = VStream (newInput nm bs)
  where nm = valueToByteString a
        bs = valueToByteString b

vBytesOfStream :: Value -> Value
vBytesOfStream = tracedFun (vByteString . inputBytes . valueToStream)

vStreamOffset :: Value -> Value
vStreamOffset = tracedFun (vSize . toInteger . inputOffset . valueToStream)

vStreamLength :: Value -> Value
vStreamLength = tracedFun (vSize . toInteger . inputLength . valueToStream)

vStreamIsEmpty :: Value -> Value
vStreamIsEmpty = tracedFun (VBool . inputEmpty . valueToStream)

vStreamHead :: Value -> Partial Value
vStreamHead = tracedFun \v ->
  case inputByte (valueToStream v) of
    Just (w,_) -> pure (vByte w)
    Nothing    -> vErr "Head of empty list"

vStreamTake :: Value -> Value -> Value
vStreamTake = tracedFun \a b ->
  let sz = toUInt (fromInteger (valueToSize a))
  in VStream (inputTake sz (valueToStream b))

vStreamDrop :: Value -> Value -> Partial Value
vStreamDrop = tracedFun \a b ->
  case vStreamDropMaybe' a b of
    Just v  -> pure v
    Nothing -> vErr "Not enough bytes in `Drop`"

vStreamDropMaybe' :: Value -> Value -> Maybe Value
vStreamDropMaybe' a b =
  do x <- valueToIntSize a
     VStream <$> advanceBy (UInt (fromIntegral x)) (valueToStream b)

vStreamDropMaybe :: Value -> Value -> Value
vStreamDropMaybe = tracedFun \a b -> VMaybe (vStreamDropMaybe' a b)




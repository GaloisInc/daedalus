module Daedalus.Value.Stream where

import RTS.Input
import RTS.Numeric
import Daedalus.Value.Type

vStreamFromArray :: Value -> Value -> Value
vStreamFromArray a b = VStream (newInput nm bs)
  where nm = valueToByteString a
        bs = valueToByteString b

vStreamOffset :: Value -> Value
vStreamOffset = vSize . toInteger . inputOffset . valueToStream

vStreamLength :: Value -> Value
vStreamLength = vSize . toInteger . inputLength . valueToStream

vStreamIsEmpty :: Value -> Value
vStreamIsEmpty = VBool . inputEmpty . valueToStream

vStreamHead :: Value -> Partial Value
vStreamHead v =
  case inputByte (valueToStream v) of
    Just (w,_) -> pure (vByte w)
    Nothing    -> vErr "Head of empty list"

vStreamTake :: Value -> Value -> Partial Value
vStreamTake a b =
  case valueToIntSize a of
    Nothing -> pure b
    Just x  ->
      case limitLen (UInt (fromIntegral x)) (valueToStream b) of
        Nothing -> vErr "Not enough bytes in `Take`"
        Just i  -> pure (VStream i)

vStreamDrop :: Value -> Value -> Partial Value
vStreamDrop a b =
  case valueToIntSize a of
    Nothing -> notEnough
    Just x  ->
      case advanceBy (UInt (fromIntegral x)) (valueToStream b) of
        Nothing -> notEnough
        Just i  -> pure (VStream i)

  where
  notEnough = vErr "Not enough bytes in `Drop`"


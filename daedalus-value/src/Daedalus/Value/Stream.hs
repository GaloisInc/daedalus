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

vStreamTake :: Value -> Value -> Partial Value
vStreamTake = tracedFun \a b ->
  case valueToIntSize a of
    Nothing -> pure b
    Just x  ->
      case limitLen (UInt (fromIntegral x)) (valueToStream b) of
        Nothing -> vErr "Not enough bytes in `Take`"
        Just i  -> pure (VStream i)

vStreamDrop :: Value -> Value -> Partial Value
vStreamDrop = tracedFun \a b ->
  let notEnough = vErr "Not enough bytes in `Drop`"
  in
  case valueToIntSize a of
    Nothing -> notEnough
    Just x  ->
      case advanceBy (UInt (fromIntegral x)) (valueToStream b) of
        Nothing -> notEnough
        Just i  -> pure (VStream i)



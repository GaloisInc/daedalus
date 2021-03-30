module Daedalus.Value.Stream where

import RTS.Input
import RTS.Numeric
import Daedalus.Value.Type

vArrayStream :: Value -> Value -> Value
vArrayStream a b = VStream (newInput nm bs)
  where nm = valueToByteString a
        bs = valueToByteString b

vOffset :: Value -> Value
vOffset = vSize . toInteger . inputOffset . valueToStream

vTake :: Value -> Value -> Partial Value
vTake a b =
  case valueToIntSize a of
    Nothing -> pure b
    Just x  ->
      case limitLen (UInt (fromIntegral x)) (valueToStream b) of
        Nothing -> vErr "Not enough bytes in `Take`"
        Just i  -> pure (VStream i)

vDrop :: Value -> Value -> Partial Value
vDrop a b =
  case valueToIntSize a of
    Nothing -> notEnough
    Just x  ->
      case advanceBy (UInt (fromIntegral x)) (valueToStream b) of
        Nothing -> notEnough
        Just i  -> pure (VStream i)

  where
  notEnough = vErr "Not enough bytes in `Drop`"


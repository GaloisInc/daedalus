module Daedalus.Value.Float where

import GHC.Float

import Daedalus.Panic
import Daedalus.Value.Type


vWordToFloat :: Value -> Value
vWordToFloat v =
  case v of
    VUInt 32 b -> vFloat (castWord32ToFloat (fromInteger b))
    _          -> panic "vWordToFloat" ["Invalid argument", show v]

vFloatPi :: Value
vFloatPi = vFloat pi

vDoublePi :: Value
vDoublePi = vDouble pi

vWordToDouble :: Value -> Value
vWordToDouble v =
  case v of
    VUInt 64 b -> vDouble (castWord64ToDouble (fromInteger b))
    _ -> panic "vWordToDouble" ["Invalid argument", show v]

vIsNaN :: Value -> Value
vIsNaN v =
  case v of
    VFloat f  -> VBool (isNaN f)
    VDouble d -> VBool (isNaN d)
    _ -> panic "vIsNaN" ["Invalid argument",show v]

vIsInfinite :: Value -> Value
vIsInfinite v =
  case v of
    VFloat f  -> VBool (isInfinite f)
    VDouble d -> VBool (isInfinite d)
    _ -> panic "vIsInfinite" ["Invalid argument",show v]

vIsDenormalized :: Value -> Value
vIsDenormalized v =
  case v of
    VFloat f  -> VBool (isDenormalized f)
    VDouble d -> VBool (isDenormalized d)
    _ -> panic "vIsDenormalized" ["Invalid argument",show v]

vIsNegativeZero :: Value -> Value
vIsNegativeZero v =
  case v of
    VFloat f  -> VBool (isNegativeZero f)
    VDouble d -> VBool (isNegativeZero d)
    _ -> panic "vIsNegativeZero" ["Invalid argument",show v]





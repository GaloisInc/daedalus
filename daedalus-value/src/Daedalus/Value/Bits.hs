{-# Language BlockArguments #-}
module Daedalus.Value.Bits where

import Data.Bits

import Daedalus.Panic(panic)
import Daedalus.Value.Utils
import Daedalus.Value.Type

vCat :: Value -> Value -> Value
vCat =
  tracedFun \a b ->
  case (a,b) of
    (VUInt m x, VUInt n y) -> vUInt (m + n) ((x `shiftL` fromIntegral n) .|. y)
    _ -> panic "vCat" [ "Invalid bit concatenation"
                      , "Operand 1: " ++ show a
                      , "Operand 2: " ++ show b
                      ]


-- x <# y  ==   x * 2^n + y,   where y : uint n
vLCat :: Value -> Value -> Partial Value
vLCat =
  tracedFun \a b ->
  let bad = panic "vLCat" [ "Invalid lcat"
                          , "Operand 1: " ++ show a
                          , "Operand 2: " ++ show b
                          ]
  in
  case b of
    VUInt w y ->
      let mk f i = f ((i `shiftL` w) .|. y)
      in
      case a of
        VInteger x -> mk (pure . VInteger)   x
        VUInt n x  -> mk (pure . vUInt n) x
        VSInt n x  -> mk (vSInt n) x
        _          -> bad

    _ -> bad

vBitAnd :: Value -> Value -> Value
vBitAnd = bitwise2 ".&." (.&.)

vBitOr :: Value -> Value -> Value
vBitOr = bitwise2 ".|." (.|.)

vBitXor :: Value -> Value -> Value
vBitXor = bitwise2 "xor" xor

vComplement :: Value -> Value
vComplement = bitwise1 "~" complement

vShiftL :: Value -> Value -> Partial Value
vShiftL = shiftOp "<<" shiftL

vShiftR :: Value -> Value -> Partial Value
vShiftR = shiftOp ">>" shiftR


module Daedalus.Value
  ( -- * Value types
    Value(..)
  , Label
  , TValue(..)
  , Partial

    -- * Numbers
  , vUInt
  , vSInt
  , vSInt'
  , vSize
  , vByte
  , vFloat
  , vDouble
  , vCoerceTo

    -- * Structs
  , vUnit
  , vStruct
  , vStructLookup

    -- * Bitdata
  , BDStruct(..)
  , BDUnion(..)
  , vFromBits
  , vToBits

    -- * Deconstructing values
  , valueToIntegral
  , valueToBool
  , valueToMaybe
  , valueToVector
  , valueToList
  , valueToStruct
  , valueToStructMap
  , valueToUnion
  , valueToMap
  , valueToStream
  , valueToSize
  , valueToIntSize

  , valueToByte
  , valueToByteString
  , valueToString

    -- * Comparisons
  , vEq
  , vNeq
  , vLt
  , vGt
  , vLeq
  , vGeq

    -- * Booleans
  , vNot

    -- * Floating Point
  , vWordToFloat
  , vWordToDouble
  , vFloatPi
  , vDoublePi
  , vIsNaN
  , vIsInfinite
  , vIsDenormalized
  , vIsNegativeZero

  -- * Arithmetic
  , vAdd
  , vSub
  , vNeg
  , vMul
  , vDiv
  , vMod

  -- * Bit operations
  , vCat
  , vLCat
  , vBitAnd
  , vBitOr
  , vBitXor
  , vComplement
  , vShiftL
  , vShiftR

  -- * Arrays
  , vArray
  , vArrayLength
  , vArrayIndex
  , vArrayConcat
  , vRangeUp
  , vRangeDown
  , vByteString
  , vBuilder
  , vEmit
  , vEmitArray
  , vEmitBuilder
  , vFinishBuilder

  -- * Maps
  , vMapEmpty
  , vMapInsert
  , vMapLookup
  , vMapMember

  -- * Streams
  , vStreamFromArray
  , vStreamIsEmpty
  , vStreamHead
  , vStreamOffset
  , vStreamLength
  , vStreamTake
  , vStreamDrop
  , vBytesOfStream

  -- * Iterators
  , vIteratorFromArray
  , vIteratorFromMap
  , vIteratorDone
  , vIteratorKey
  , vIteratorValue
  , vIteratorNext

    -- * Export
  , valueToDoc
  , valueToJS
  ) where

import Daedalus.Panic
import Daedalus.PP

import Daedalus.Value.Type
import Daedalus.Value.Bool
import Daedalus.Value.Float
import Daedalus.Value.Arith
import Daedalus.Value.Bits
import Daedalus.Value.Compare
import Daedalus.Value.Array
import Daedalus.Value.Map
import Daedalus.Value.Stream
import Daedalus.Value.Iterator
import Daedalus.Value.Coerce

-- | Pretty print a value
valueToDoc :: Value -> Doc
valueToDoc = pp

vStruct :: [(Label,Value)] -> Value
vStruct = VStruct

vStructLookup :: Value -> Label -> Value
vStructLookup v l =
  case v of
    VStruct fs ->
      case lookup l fs of
        Just i  -> i
        Nothing -> bad
    VBDStruct t i -> bdGetField t l i
    _ -> bad

  where
  bad = panic "vStructLookup" [ "Invalid struct lookup"
                              , "Label: " ++ show l
                              , "Struct " ++ show v
                              ]



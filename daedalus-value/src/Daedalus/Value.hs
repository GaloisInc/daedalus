module Daedalus.Value
  ( -- * Value types
    Value(..)
  , Label
  , TValue(..)
  , Partial

    -- * Numbers
  , vUInt
  , vSInt
  , vSize
  , vByte
  , vCoerceTo

    -- * Structs
  , vUnit
  , vStruct
  , vStructLookup

    -- * Deconstructing values
  , valueToIntegral
  , valueToBool
  , valueToMaybe
  , valueToVector
  , valueToStruct
  , valueToUnion
  , valueToMap
  , valueToStream
  , valueToSize
  , valueToIntSize

  , valueToByte
  , valueToByteString

    -- * Comparisons
  , vEq
  , vNeq
  , vLt
  , vGt
  , vLeq
  , vGeq

    -- * Booleans
  , vNot

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
  , vConsBuilder
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
  case lookup l (valueToStruct v) of
    Just i -> i
    Nothing -> panic "vStructLookup" [ "Invalid struct lookup"
                                     , "Label: " ++ show l
                                     , "Struct " ++ show v
                                     ]


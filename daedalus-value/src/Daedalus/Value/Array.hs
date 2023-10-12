{-# Language BlockArguments #-}
module Daedalus.Value.Array where

import qualified Data.Vector as Vector

import Daedalus.Value.Type
import Daedalus.Value.Utils

vArray :: [Value] -> Value
vArray = VArray . Vector.fromList

vArrayLengthInt :: Value -> Int
vArrayLengthInt = Vector.length . valueToVector

vArrayLength :: Value -> Value
vArrayLength = vSize . toInteger . vArrayLengthInt

vArrayIndex :: Value -> Value -> Partial Value
vArrayIndex =
  tracedFun \a b ->
  case valueToIntSize b of
    Nothing -> outOfBounds
    Just i  -> case valueToVector a Vector.!? i of
                 Just v  -> pure v
                 Nothing -> outOfBounds
  where
  outOfBounds =  vErr "Array index out of bounds"

vArrayConcat :: Value -> Value
vArrayConcat =
  tracedFun \a ->
    VArray $ Vector.concat
           $ Vector.toList
           $ Vector.map valueToVector
           $ valueToVector a


vRangeUp :: Value -> Value -> Value -> Partial Value
vRangeUp = rangeOp "rangeUp" (+) (>)

vRangeDown :: Value -> Value -> Value -> Partial Value
vRangeDown = rangeOp "rangeDown" subtract (<)

vBuilder :: Value
vBuilder = VBuilder []

-- | Builder, Array
vEmit :: Value -> Value -> Value
vEmit = tracedFun \b v -> VBuilder (v : valueToBuilder b)

-- | Builder, Array
vEmitArray :: Value -> Value -> Value
vEmitArray = tracedFun \b arr ->
  VBuilder (reverse (Vector.toList (valueToVector arr)) ++ valueToBuilder b)

-- | Builder, Array
vEmitBuilder :: Value -> Value -> Value
vEmitBuilder = tracedFun \b bv ->
  VBuilder (valueToBuilder bv ++ valueToBuilder b)

vFinishBuilder :: Value -> Value
vFinishBuilder = tracedFun (vArray . reverse . valueToBuilder)




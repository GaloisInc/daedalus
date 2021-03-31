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
vArrayIndex a b =
  case valueToIntSize b of
    Nothing -> outOfBounds
    Just i  -> case valueToVector a Vector.!? i of
                 Just v  -> pure v
                 Nothing -> outOfBounds
  where
  outOfBounds =  vErr "Array index out of bounds"

vConcat :: Value -> Value
vConcat = VArray . Vector.concat
                 . Vector.toList
                 . Vector.map valueToVector
                 . valueToVector


vRangeUp :: Value -> Value -> Value -> Partial Value
vRangeUp = rangeOp "rangeUp" (+) (>)

vRangeDown :: Value -> Value -> Value -> Partial Value
vRangeDown = rangeOp "rangeDown" subtract (<)










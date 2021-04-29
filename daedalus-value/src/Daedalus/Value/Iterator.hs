module Daedalus.Value.Iterator where

import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Daedalus.Value.Type

vIteratorFromArray :: Value -> Value
vIteratorFromArray = VIterator . zip keys . Vector.toList . valueToVector
  where keys = map vSize [ 0 .. ]

vIteratorFromMap :: Value -> Value
vIteratorFromMap = VIterator . Map.toList . valueToMap

vIteratorDone :: Value -> Value
vIteratorDone = VBool . null . valueToIterator

vIteratorKey :: Value -> Partial Value
vIteratorKey v =
  case valueToIterator v of
    (k,_) : _ -> pure k
    []        -> vErr "Iterator has no key"

vIteratorValue :: Value -> Partial Value
vIteratorValue val =
  case valueToIterator val of
    (_,v) : _ -> pure v
    []        -> vErr "Iterator has no value"

vIteratorNext :: Value -> Partial Value
vIteratorNext v =
  case valueToIterator v of
    _ : xs -> pure (VIterator xs)
    _      -> vErr "Iterator has no successor"



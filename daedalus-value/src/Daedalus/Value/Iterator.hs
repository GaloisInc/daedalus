{-# Language BlockArguments #-}
module Daedalus.Value.Iterator where

import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Daedalus.Value.Type
import Daedalus.Value.Utils

vIteratorFromArray :: Value -> Value
vIteratorFromArray =
  tracedFun (VIterator . zip keys . Vector.toList . valueToVector)
  where keys = map vSize [ 0 .. ]

vIteratorFromMap :: Value -> Value
vIteratorFromMap = tracedFun (VIterator . Map.toList . valueToMap)

vIteratorDone :: Value -> Value
vIteratorDone = tracedFun (VBool . null . valueToIterator)

vIteratorKey :: Value -> Partial Value
vIteratorKey = tracedFun \v ->
  case valueToIterator v of
    (k,_) : _ -> pure k
    []        -> vErr "Iterator has no key"

vIteratorValue :: Value -> Partial Value
vIteratorValue = tracedFun \val ->
  case valueToIterator val of
    (_,v) : _ -> pure v
    []        -> vErr "Iterator has no value"

vIteratorNext :: Value -> Partial Value
vIteratorNext = tracedFun \v ->
  case valueToIterator v of
    _ : xs -> pure (VIterator xs)
    _      -> vErr "Iterator has no successor"



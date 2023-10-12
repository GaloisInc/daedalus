{-# Language BlockArguments #-}
module Daedalus.Value.Map where

import qualified Data.Map as Map

import Daedalus.Value.Type
import Daedalus.Value.Utils

vMapEmpty :: Value
vMapEmpty = VMap Map.empty

vMapInsert ::
  Value {- ^ key -} -> Value {- ^ value -} -> Value {- ^ map -} -> Value
vMapInsert k v = addTraced k
               . addTraced v
               . tracedFun (VMap . Map.insert k v . valueToMap)

vMapLookup :: Value {- ^ key -} -> Value {- ^ map -} -> Value
vMapLookup = tracedFun \k m -> VMaybe (Map.lookup k (valueToMap m))

vMapMember :: Value {-^ key -} -> Value {- map -} -> Value
vMapMember = tracedFun \k m -> VBool (Map.member k (valueToMap m))


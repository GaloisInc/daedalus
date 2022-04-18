module Daedalus.Value.Map where

import qualified Data.Map as Map

import Daedalus.Value.Type

vMapEmpty :: Value
vMapEmpty = VMap Map.empty

vMapInsert ::
  Value {- ^ key -} -> Value {- ^ value -} -> Value {- ^ map -} -> Value
vMapInsert k v m = VMap (Map.insert k v mp)
  where
  mp = valueToMap m

vMapLookup :: Value {- ^ key -} -> Value {- ^ map -} -> Value
vMapLookup k m = VMaybe (Map.lookup k (valueToMap m))

vMapMember :: Value {-^ key -} -> Value {- map -} -> Value
vMapMember k m = VBool (Map.member k (valueToMap m))


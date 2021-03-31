module Daedalus.Value.Map where

import qualified Data.Map as Map

import Daedalus.Value.Type

vMapEmpty :: Value
vMapEmpty = VMap Map.empty

vMapInsert ::
  Value {- ^ key -} -> Value {- ^ value -} -> Value {- ^ map -} -> Partial Value
vMapInsert k v m
  | k `Map.member` mp = vErr "Key already exists in value"
  | otherwise         = pure (VMap (Map.insert k v mp))
  where
  mp = valueToMap m

vMapLookup :: Value {- ^ key -} -> Value {- ^ map -} -> Partial Value
vMapLookup k m =
  case Map.lookup k (valueToMap m) of
    Just v  -> pure v
    Nothing -> vErr "Key not in map"

vMapMember :: Value {-^ key -} -> Value {- map -} -> Value
vMapMember k m = VBool (Map.member k (valueToMap m))


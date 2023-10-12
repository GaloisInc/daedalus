{-# Language BlockArguments #-}
module Daedalus.Value.Compare (vEq, vNeq, vLt, vGt, vLeq, vGeq) where

import Daedalus.Value.Type
import Daedalus.Value.Utils

vEq :: Value -> Value -> Value
vEq = tracedFun \x y -> VBool (vCompare x y == EQ)

vNeq :: Value -> Value -> Value
vNeq = tracedFun \x y -> VBool (vCompare x y /= EQ)

vLt :: Value -> Value -> Value
vLt = tracedFun \x y -> VBool (vCompare x y == LT)

vGt :: Value -> Value -> Value
vGt = tracedFun \x y -> VBool (vCompare x y == GT)

vLeq :: Value -> Value -> Value
vLeq = tracedFun \x y -> VBool (vCompare x y /= GT)

vGeq :: Value -> Value -> Value
vGeq = tracedFun \x y -> VBool (vCompare x y /= LT)

module Daedalus.Value.Compare (vEq, vNeq, vLt, vGt, vLeq, vGeq) where

import Daedalus.Value.Type

vEq :: Value -> Value -> Value
vEq x y = VBool (vCompare x y == EQ)

vNeq :: Value -> Value -> Value
vNeq x y = VBool (vCompare x y /= EQ)

vLt :: Value -> Value -> Value
vLt x y = VBool (vCompare x y == LT)

vGt :: Value -> Value -> Value
vGt x y = VBool (vCompare x y == GT)

vLeq :: Value -> Value -> Value
vLeq x y = VBool (vCompare x y /= GT)

vGeq :: Value -> Value -> Value
vGeq x y = VBool (vCompare x y /= LT)

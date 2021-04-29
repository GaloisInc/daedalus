module Daedalus.Value.Arith (vAdd, vSub, vNeg, vMul, vDiv, vMod) where

import Daedalus.Value.Utils
import Daedalus.Value.Type

vAdd :: Value -> Value -> Partial Value
vAdd = numeric2 "+" (+)

vSub :: Value -> Value -> Partial Value
vSub = numeric2 "-" (-)

vNeg :: Value -> Partial Value
vNeg = numeric1 "-" negate

vMul :: Value -> Value -> Partial Value
vMul = numeric2 "*" (*)


-- XXX: div/mod or quot/rem?

vDiv :: Value -> Value -> Partial Value
vDiv = numeric2' "/" div'
  where div' x y = if y == 0 then vErr "Division by 0" else pure (div x y)

vMod :: Value -> Value -> Partial Value
vMod = numeric2' "mod" mod'
  where mod' x y = if y == 0 then vErr "Modulus by 0" else pure (mod x y)




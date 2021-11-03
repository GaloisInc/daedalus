{-# Language RankNTypes #-}
module Daedalus.Value.Arith (vAdd, vSub, vNeg, vMul, vDiv, vMod) where


import Daedalus.Panic(panic)
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
vDiv a b =
  case (a,b) of
    (VInteger x, VInteger y)           -> VInteger <$> f x y
    (VUInt n x,  VUInt n' y) | n == n' -> vUInt n <$> f x y
    (VSInt n x,  VSInt n' y) | n == n' -> vSInt n =<< f x y
    (VFloat x, VFloat y)               -> pure (VFloat (x/y))
    (VDouble x, VDouble y)             -> pure (VDouble (x/y))
    _ -> panic "numeric2" [ "Invalid binary numeric operation"
                          , "Operation: /"
                          , "Operand 1: " ++ show a
                          , "Operand 2: " ++ show b
                          ]
  where
  f x y = if y == 0 then vErr "Division by 0" else pure (div x y)

vMod :: Value -> Value -> Partial Value
vMod a b =
  case (a,b) of
    (VInteger x, VInteger y)           -> VInteger <$> f x y
    (VUInt n x,  VUInt n' y) | n == n' -> vUInt n <$> f x y
    (VSInt n x,  VSInt n' y) | n == n' -> vSInt n =<< f x y
    _ -> panic "numeric2" [ "Invalid binary numeric operation"
                          , "Operation: mod"
                          , "Operand 1: " ++ show a
                          , "Operand 2: " ++ show b
                          ]
  where f x y = if y == 0 then vErr "Modulus by 0" else pure (mod x y)




numeric1 :: String -> (forall a. Num a => a -> a) -> Value -> Partial Value
numeric1 name f a =
  case a of
    VInteger x -> pure (VInteger (f x))
    VUInt n x  -> pure (vUInt n (f x))
    VSInt n x  -> vSInt n (f x)
    VFloat x   -> pure (VFloat (f x))
    VDouble x  -> pure (VDouble (f x))
    _ -> panic "numeric1" [ "Invalid unary numeric operation"
                          , "Operation: " ++ name
                          , "Operand: "   ++ show a
                          ]
numeric2 ::
  String -> (forall a. Num a => a -> a -> a) -> Value -> Value -> Partial Value
numeric2 name f a b =
  case (a,b) of
    (VInteger x, VInteger y)           -> pure (VInteger (f x y))
    (VUInt n x,  VUInt n' y) | n == n' -> pure (vUInt n (f x y))
    (VSInt n x,  VSInt n' y) | n == n' -> vSInt n (f x y)
    (VFloat x, VFloat y)               -> pure (VFloat (f x y))
    (VDouble x, VDouble y)             -> pure (VDouble (f x y))
    _ -> panic "numeric2" [ "Invalid binary numeric operation"
                          , "Operation: " ++ name
                          , "Operand 1: " ++ show a
                          , "Operand 2: " ++ show b
                          ]




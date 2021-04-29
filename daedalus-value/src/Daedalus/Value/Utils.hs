module Daedalus.Value.Utils where

import qualified Data.Vector as Vector

import Daedalus.PP(pp)
import Daedalus.Panic(panic)
import Daedalus.Value.Type


numeric1 ::
  String -> (Integer -> Integer) -> Value -> Partial Value
numeric1 name f a =
  case a of
    VInteger x -> pure (VInteger (f x))
    VUInt n x  -> pure (vUInt n (f x))
    VSInt n x  -> vSInt n (f x)
    _ -> panic "numeric1" [ "Invalid unary numeric operation"
                          , "Operation: " ++ name
                          , "Operand: "   ++ show a
                          ]


numeric2 ::
  String -> (Integer -> Integer -> Integer) -> Value -> Value -> Partial Value
numeric2 name f a b =
  case (a,b) of
    (VInteger x, VInteger y)           -> pure (VInteger (f x y))
    (VUInt n x,  VUInt n' y) | n == n' -> pure (vUInt n (f x y))
    (VSInt n x,  VSInt n' y) | n == n' -> vSInt n (f x y)
    _ -> panic "numeric2" [ "Invalid binary numeric operation"
                          , "Operation: " ++ name
                          , "Operand 1: " ++ show a
                          , "Operand 2: " ++ show b
                          ]

numeric2' ::
  String -> (Integer -> Integer -> Partial Integer) ->
  Value -> Value -> Partial Value
numeric2' name f a b =
  case (a,b) of
    (VInteger x, VInteger y)           -> VInteger <$> f x y
    (VUInt n x,  VUInt n' y) | n == n' -> vUInt n <$> f x y
    (VSInt n x,  VSInt n' y) | n == n' -> vSInt n =<< f x y
    _ -> panic "numeric2" [ "Invalid binary numeric operation"
                          , "Operation: " ++ name
                          , "Operand 1: " ++ show a
                          , "Operand 2: " ++ show b
                          ]


-- complement
bitwise1 ::
  String -> (Integer -> Integer) -> Value -> Value
bitwise1 name f a =
  case a of
    VUInt n x -> vUInt n (f x)
    _ -> panic "bitwise1" [ "Invalid unary bitwise operation"
                          , "Operation: " ++ name
                          , "Operand: " ++ show a
                          ]
-- and or xor
bitwise2 ::
  String -> (Integer -> Integer -> Integer) -> Value -> Value -> Value
bitwise2 name f a b =
  case (a,b) of
    (VUInt n x, VUInt n' y) | n == n' -> vUInt n (f x y)
    _ -> panic "bitwise2" [ "Invalid binary bitwise operation"
                          , "Operation: " ++ name
                          , "Operand 1: " ++ show a
                          , "Operand 2: " ++ show b
                          ]


shiftOp ::
  String -> (Integer -> Int -> Integer) -> Value -> Value -> Partial Value
shiftOp name f a b =
  case valueToIntSize b of
    Nothing -> toobig
    Just y  ->
      case a of
        VInteger x                    -> pure (VInteger (f x y))
        VUInt n x | 0 <= n && y < n   -> pure (vUInt n (f x y))
                  | otherwise -> toobig
        VSInt n x | 0 <= n && y < n   -> vSInt n (f x y)
                  | otherwise -> toobig
        _ -> panic "shiftOp" [ "Invalid shift operation"
                             , "Operator: " ++ name
                             , "Operand 1: " ++ show a
                             , "Operand 2: " ++ show b
                             ]
  where
  toobig = vErr ("Shift amount is too big: " ++ show (pp b))



rangeOp ::
  String -> (Integer -> Integer -> Integer) -> (Integer -> Integer -> Bool) ->
  Value -> Value -> Value -> Partial Value
rangeOp name next notDone a b c
  | step <= 0 = vErr (name ++ ": invalid range step")
  | otherwise = pure
              $ VArray
              $ Vector.fromList
              $ map tag
              $ takeWhile (notDone end)
              $ iterate (next step) start
  where
  start = valueToIntegral a
  end   = valueToIntegral b
  step  = valueToIntegral c

  -- we don't need the bounds check here because we know that we are between
  -- then end points, and we've already checked that those are in bounds.
  tag   = case a of
            VUInt n _   -> VUInt n
            VSInt n _   -> VSInt n
            VInteger _  -> VInteger
            _           -> panic "rangeOp" [ "Invalid range operand"
                                           , "Operator:  " ++ name
                                           , "Operand 1: " ++ show a
                                           , "Operand 2: " ++ show b
                                           , "Operand 3: " ++ show c
                                           ]






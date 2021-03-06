{-# Language BlockArguments #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language GADTs #-}
module Daedalus.Core.Semantics.Value where

import Data.Map(Map)
import Data.Vector(Vector)
import qualified Data.Vector as Vector
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.Maybe(fromMaybe)
import Data.Word(Word8)

import Data.BitVector.Sized(BV)
import qualified Data.BitVector.Sized as BV
import Data.Parameterized.NatRepr
import Data.Parameterized.Some

import RTS.Input(Input(..))
import RTS.Numeric(UInt(..))

import Daedalus.Panic(panic)
import Daedalus.Core(Label, Type, UserType)


data Value =
    VUnit
  | VStruct  SType     !(Map Label Value)
  | VUnion   UserType  !Label !Value

  | VInt               !Integer
  | forall w.
    VUInt !(NatRepr w) !(BV w)
  | forall w. (1 <= w) =>
    VSInt !(NatRepr w) !(BV w)

  | VBool              !Bool
  | VBuilder Type      ![Value]
  | VArray   Type      !(Vector Value)
  | VMap     Type Type !(Map Value Value)
  | VInput             !Input

  | VNothing Type
  | VJust              !Value

  | VIterator Type     ![(Value,Value)]


data SType = SType UserType [Label]


-- | Assumes types match
instance Eq Value where
  v1 == v2 = compare v1 v2 == EQ

-- | Assumes types match
instance Ord Value where
  compare v1 v2 =
    case (v1, v2) of
      (VUnit, VUnit)                        -> EQ
      (VStruct _ xs, VStruct _ ys)          -> compare xs ys
      (VUnion _ l1 x, VUnion _ l2 y)        -> compare (l1,x) (l2,y)
      (VInt x, VInt y)                      -> compare x y
      (VUInt w1 x, VUInt w2 y)
        | Just Refl <- testEquality w1 w2   -> compare x y
      (VSInt w1 x, VSInt w2 y)
        | Just Refl <- testEquality w1 w2   -> compare x y
      (VBool x, VBool y)                    -> compare x y
      (VBuilder _ xs, VBuilder _ ys)        -> compare xs ys
      (VArray _ xs, VArray _ ys)            -> compare xs ys
      (VMap _ _ xs, VMap _ _ ys)            -> compare xs ys
      (VInput x, VInput y)                  -> compare x y

      (VNothing _, VNothing _)              -> EQ
      (VNothing _, VJust _)                 -> LT
      (VJust x, VJust y)                    -> compare x y
      (VJust _, VNothing _)                 -> GT

      (VIterator _ xs, VIterator _ ys)      -> compare xs ys

      _ -> panic "compare @Value" [ "Values have different shapes." ]






--------------------------------------------------------------------------------


vByte :: Word8 -> Value
vByte w = VUInt knownNat (BV.word8 w)

vUInt :: Integer -> Integer -> Value
vUInt wi i =
  case someNat wi of
    Just (Some w) -> VUInt w (BV.mkBV w i)
    _ -> panic "vUInt" ["Invalid width: " ++ show wi ]

vSInt :: Integer -> Integer -> Value
vSInt wi i =
  fromMaybe (panic "vSInt" [ "Invalid width: " ++ show wi ])
  do Some w   <- someNat wi
     LeqProof <- isPosNat w
     pure (VSInt w (BV.mkBV w i))



--------------------------------------------------------------------------------
fromVInput :: Value -> Input
fromVInput v =
  case v of
    VInput i -> i
    _        -> typeError "Input" v

fromVSize :: Value -> UInt 64
fromVSize v =
  case v of
    VUInt _ i -> UInt (fromInteger (BV.asUnsigned i))
    _ -> typeError "Int" v



fromVInt :: Value -> Integer
fromVInt v =
  case v of
    VInt i -> i
    _ -> typeError "Int" v

fromVByte :: Value -> Word8
fromVByte v =
  case v of
    VUInt _ i -> fromInteger (BV.asUnsigned i)
    _         -> typeError "Byte" v

fromVBool :: Value -> Bool
fromVBool v =
  case v of
    VBool b -> b
    _       -> typeError "Bool" v

fromVArray :: Value -> Vector Value
fromVArray v =
  case v of
    VArray _ a -> a
    _          -> typeError "Array" v

fromVMap :: Value -> Map Value Value
fromVMap v =
  case v of
    VMap _ _ m -> m
    _          -> typeError "Map" v

fromVByteArray :: Value -> ByteString
fromVByteArray v = BS.pack [ fromVByte b | b <- Vector.toList (fromVArray v) ]

typeError :: String -> Value -> a
typeError x _v = panic "typeError" [ "Expected " ++ x ]

typeErrorT :: String -> Type -> a
typeErrorT x _t = panic "typeError" [ "Expected " ++ x ]


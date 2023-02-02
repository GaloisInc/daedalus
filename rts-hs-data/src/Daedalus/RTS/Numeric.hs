module Daedalus.RTS.Numeric
  ( UInt(..), UIntRep
  , SInt(..), SIntRep
  , Numeric(..)
  , Arith(..)
  , Literal
  , SizeType


    -- * Conversions
  , fromUInt, uint8, toUInt
  , fromSInt, sint8, toSInt
  , cvtNum

    -- ** Checked
  , cvtNumToFloatingMaybe
  , cvtFloatingToNumMaybe
  , cvtNumMaybe

    -- ** Floating
  , wordToFloat, floatToWord
  , wordToDouble, doubleToWord

    -- ** Sizes
  , intToSize
  , sizeToInt
  , toInt

    -- * Bit operations
  , shiftl, shiftr, cat, lcat
  ) where

import GHC.TypeNats
import GHC.Float
import Data.Kind(Constraint)
import Data.Word
import Data.Int
import Data.Bits

import Daedalus.RTS.JSON


data {-kind-} Size = S8 | S16 | S32 | S64 | SBig

type family SizeOf (n :: Nat) :: Size where
  SizeOf n = SizeOf8 n (n <=? 8)

type family SizeOf8 (n :: Nat) (b :: Bool) where
  SizeOf8 n 'True  = 'S8
  SizeOf8 n 'False = SizeOf16 n (n <=? 16)

type family SizeOf16 (n :: Nat) (b :: Bool) where
  SizeOf16 n 'True  = 'S16
  SizeOf16 n 'False = SizeOf32 n (n <=? 32)

type family SizeOf32 (n :: Nat) (b :: Bool) where
  SizeOf32 n 'True  = 'S32
  SizeOf32 n 'False = SizeOf64 (n <=? 64)

type family SizeOf64 (b :: Bool) where
  SizeOf64 'True  = 'S64
  SizeOf64 'False = 'SBig

type family UIntRepOf (n :: Size) where
  UIntRepOf 'S8   = Word8
  UIntRepOf 'S16  = Word16
  UIntRepOf 'S32  = Word32
  UIntRepOf 'S64  = Word64
  UIntRepOf 'SBig = Integer

type family SIntRepOf (n :: Size) where
  SIntRepOf 'S8   = Int8
  SIntRepOf 'S16  = Int16
  SIntRepOf 'S32  = Int32
  SIntRepOf 'S64  = Int64
  SIntRepOf 'SBig = Integer

type family UIntRep (n :: Nat) where
  UIntRep n = UIntRepOf (SizeOf n)

type family SIntRep (n :: Nat) where
  SIntRep n = SIntRepOf (SizeOf n)

newtype UInt (n :: Nat) = UInt (UIntRep n)
newtype SInt (n :: Nat) = SInt (SIntRep n)

uint8 :: Word8 -> UInt 8
uint8 = UInt
{-# INLINE uint8 #-}

sint8 :: Int8 -> SInt 8
sint8 = SInt
{-# INLINE sint8 #-}

fromUInt :: UInt n -> UIntRep n
fromUInt (UInt x) = x
{-# INLINE fromUInt #-}

toUInt :: SizeType n => UIntRep n -> UInt n
toUInt x = normU (UInt x)
{-# INLINE toUInt #-}

fromSInt :: SInt n -> SIntRep n
fromSInt (SInt x) = x
{-# INLINE fromSInt #-}

toSInt :: SizeType n => SIntRep n -> SInt n
toSInt x = normS (SInt x)
{-# INLINE toSInt #-}


--------------------------------------------------------------------------------

type Supports c n  = (c (UIntRep n) :: Constraint, c (SIntRep n))

-- Constraints used for normalizing values
type NormCtrs n    = (KnownNat n, Supports Bits n, Supports Integral n)

type SizeTypeDef n = ( NormCtrs n
                     , NormU n (SizeOf n)
                     , NormS n (SizeOf n)
                     , Supports Show n
                     )

class SizeTypeDef n => SizeType n

-- The 0 instance is here to ensure that GHC does
-- not "simplify" `SizeType` into a whole lot of other constraints,
-- which would result in equivalent but much messier types.
instance {-# OVERLAPPING #-} SizeType 0
instance SizeTypeDef n => SizeType n


class (SizeOf w ~ s) => NormU (w :: Nat) (s :: Size) where
  normU :: UInt w -> UInt w

instance {-# OVERLAPPING #-} NormU 8  'S8  where normU = id
instance {-# OVERLAPPING #-} NormU 16 'S16 where normU = id
instance {-# OVERLAPPING #-} NormU 32 'S32 where normU = id
instance {-# OVERLAPPING #-} NormU 64 'S64 where normU = id

instance (NormCtrs w, SizeOf w ~ s) => NormU w s where
  normU n@(UInt w) = UInt (mask n w)

mask :: (Bits a, Num a, KnownNat n) => f n -> a -> a
mask num a = a .&. ((1 `shiftL` thisWidth num) - 1)

-- XXX: may overflow
thisWidth :: KnownNat n => num n -> Int
thisWidth x = fromIntegral (natVal x)


normS' :: NormCtrs n => Int -> SInt n -> SInt n
normS' repW n@(SInt i) = SInt ((mask n i `shiftL` amt) `shiftR` amt)
  where amt = repW - thisWidth n


class (SizeOf n ~ sz) => NormS n sz where
  normS :: SInt n -> SInt n


instance (SizeOf w ~ 'SBig, NormCtrs w) => NormS w 'SBig where
  normS n@(SInt i) = SInt (((i + half) `Prelude.mod` whole) - half)
    -- XXX: this could do a lot of allocation. Maybe using `if` is better?
    where w     = thisWidth n
          whole = 1 `shiftL` w
          half  = whole `shiftR` 1

instance {-# OVERLAPPING #-}              NormS 8  'S8  where normS = id
instance {-# OVERLAPPING #-}              NormS 16 'S16 where normS = id
instance {-# OVERLAPPING #-}              NormS 32 'S32 where normS = id
instance {-# OVERLAPPING #-}              NormS 64 'S64 where normS = id

instance (SizeOf w ~ 'S8 , NormCtrs w) => NormS w  'S8  where normS = normS' 8
instance (SizeOf w ~ 'S16, NormCtrs w) => NormS w  'S16 where normS = normS' 16
instance (SizeOf w ~ 'S32, NormCtrs w) => NormS w  'S32 where normS = normS' 32
instance (SizeOf w ~ 'S64, NormCtrs w) => NormS w  'S64 where normS = normS' 64


type Literal (x :: Nat) t = Arith t

class Arith t where
  lit :: Integer -> t
  add :: t -> t -> t
  sub :: t -> t -> t
  mul :: t -> t -> t
  div :: t -> t -> t
  neg :: t -> t

instance Arith Float where
  lit = fromInteger
  add = (+)
  sub = (-)
  mul = (*)
  div = (/)
  neg = negate

instance Arith Double where
  lit = fromInteger
  add = (+)
  sub = (-)
  mul = (*)
  div = (/)
  neg = negate

class Arith t => Numeric t where
  mod :: t -> t -> t
  asInt :: t -> Integer

  shiftl' :: t -> Int -> t
  shiftr' :: t -> Int -> t
  bitOr :: t -> t -> t
  bitAnd :: t -> t -> t
  bitXor :: t -> t -> t
  bitCompl :: t -> t

instance Arith Integer where
  lit = id
  add = (+)
  sub = (-)
  mul = (*)
  div = Prelude.div
  neg = negate

instance Numeric Integer where
  mod = Prelude.mod

  asInt = id

  shiftl' = shiftL
  shiftr' = shiftR
  bitOr = (.|.)
  bitAnd = (.&.)
  bitXor = xor
  bitCompl = complement

binU :: SizeType n => (UIntRep n -> UIntRep n -> UIntRep n) ->
                      UInt n -> UInt n -> UInt n
binU f = \(UInt x) (UInt y) -> UInt (f x y)
{-# INLINE binU #-}

normBinU :: SizeType n => (UIntRep n -> UIntRep n -> UIntRep n) ->
                          UInt n -> UInt n -> UInt n
normBinU f = \x y -> normU (binU f x y)
{-# INLINE normBinU #-}

binS :: SizeType n => (SIntRep n -> SIntRep n -> SIntRep n) ->
                      SInt n -> SInt n -> SInt n
binS f = \(SInt x) (SInt y) -> SInt (f x y)
{-# INLINE binS #-}

normBinS :: SizeType n => (SIntRep n -> SIntRep n -> SIntRep n) ->
                          SInt n -> SInt n -> SInt n
normBinS f = \x y -> normS (binS f x y)
{-# INLINE normBinS #-}


unU :: SizeType n => (UIntRep n -> UIntRep n) -> UInt n -> UInt n
unU f (UInt x) = UInt (f x)
{-# INLINE unU #-}

normUnU :: SizeType n => (UIntRep n -> UIntRep n) -> UInt n -> UInt n
normUnU f x = normU (unU f x)
{-# INLINE normUnU #-}


unS :: SizeType n => (SIntRep n -> SIntRep n) -> SInt n -> SInt n
unS f (SInt x) = SInt (f x)
{-# INLINE unS #-}

normUnS :: SizeType n => (SIntRep n -> SIntRep n) -> SInt n -> SInt n
normUnS f x = normS (unS f x)
{-# INLINE normUnS #-}



instance SizeType n => Arith (UInt n) where
  lit x           = normU (UInt (fromInteger x))
  add             = normBinU (+)
  sub             = normBinU (-)
  mul             = normBinU (*)
  div             = normBinU Prelude.div
  neg             = normUnU negate

instance SizeType n => Numeric (UInt n) where
  mod             = normBinU Prelude.mod

  bitOr           = binU (.|.)
  bitAnd          = binU (.&.)
  bitXor          = binU xor
  bitCompl        = unU complement
  shiftl' x i     = normUnU (`shiftL` i) x
  shiftr' x i     = normUnU (`shiftR` i) x

  asInt (UInt x)  = toInteger x


instance SizeType n => Arith (SInt n) where
  lit x           = normS (SInt (fromInteger x))
  add             = normBinS (+)
  sub             = normBinS (-)
  mul             = normBinS (*)
  div             = normBinS Prelude.div
  neg             = normUnS negate

instance SizeType n => Numeric (SInt n) where
  mod             = normBinS Prelude.mod

  bitOr           = binS (.|.)
  bitAnd          = binS (.&.)
  bitXor          = binS xor
  bitCompl        = unS complement
  shiftl' x i     = normUnS (`shiftL` i) x
  shiftr' x i     = normUnS (`shiftR` i) x

  asInt (SInt x)  = toInteger x

deriving instance SizeType n => Show (UInt n)
deriving instance SizeType n => Eq   (UInt n)
deriving instance SizeType n => Ord  (UInt n)

deriving instance SizeType n => Show (SInt n)
deriving instance SizeType n => Eq   (SInt n)
deriving instance SizeType n => Ord  (SInt n)

--------------------------------------------------------------------------------


cvtNumMaybe :: (Numeric a, Numeric b) => a -> Maybe b
cvtNumMaybe a = if asInt b == ia then Just b else Nothing
  where
  ia = asInt a
  b  = lit ia

-- Number to Floating Maybe.
-- XXX: these can be more effecient
cvtNumToFloatingMaybe :: (Numeric a, Fractional b, Real b) => a -> Maybe b
cvtNumToFloatingMaybe x
  | toRational y == r = Just y
  | otherwise         = Nothing
  where
  r = toRational (asInt x)
  y = fromRational r

cvtFloatingToNumMaybe :: (RealFloat a, Numeric b) => a -> Maybe b
cvtFloatingToNumMaybe x
  | isNaN x      = Nothing
  | isInfinite x = Nothing
  | otherwise    = if fromInteger (asInt y) == x then Just y else Nothing
    where y = lit (truncate x)



cvtNum :: (Numeric a, Arith b) => a -> b
cvtNum = lit . asInt


lcat :: (Numeric a, SizeType n) => a -> UInt n -> a
lcat x y = shiftl' x (thisWidth y) `bitOr` cvtNum y

cat :: (SizeType m, SizeType n, SizeType (m+n)) =>
  UInt m -> UInt n -> UInt (m+n)
cat x y = cvtNum x `lcat` y

shiftl :: Numeric t => t -> UInt 64 -> t
shiftl t x = shiftl' t (sizeToInt x)

shiftr :: Numeric t => t -> UInt 64 -> t
shiftr t x = shiftr' t (sizeToInt x)

intToSize :: Int -> UInt 64
intToSize n = UInt (toEnum n)

-- Hopefully it fits
sizeToInt :: UInt 64 -> Int
sizeToInt (UInt n) = fromEnum n


toInt :: Integer -> Maybe Int
toInt n
  | lower <= n && n <= upper = Just (fromIntegral n)
  | otherwise                = Nothing
  where
  lower = toInteger (minBound :: Int)
  upper = toInteger (maxBound :: Int)


--------------------------------------------------------------------------------

wordToFloat :: UInt 32 -> Float
wordToFloat x = castWord32ToFloat (fromUInt x)
{-# INLINE wordToFloat #-}

floatToWord :: Float -> UInt 32
floatToWord = UInt . castFloatToWord32
{-# INLINE floatToWord #-}

wordToDouble :: UInt 64 -> Double
wordToDouble x = castWord64ToDouble (fromUInt x)
{-# INLINE wordToDouble #-}

doubleToWord :: Double -> UInt 64
doubleToWord = UInt . castDoubleToWord64
{-# INLINE doubleToWord #-}


--------------------------------------------------------------------------------


-------------------------------------------------------------------------------

instance SizeType n => ToJSON (UInt n) where
  toJSON = toJSON . asInt

instance SizeType n => ToJSON (SInt n) where
  toJSON = toJSON . asInt



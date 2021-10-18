{-# Language TypeFamilies, DataKinds, TypeOperators #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleContexts, FlexibleInstances #-}
{-# Language ConstraintKinds #-}
{-# Language StandaloneDeriving #-}
{-# Language MultiParamTypeClasses #-}
module RTS.Numeric
  ( UInt(..)
  , SInt(..)
  , Numeric(..)
  , Arith(..)
  , Literal
  , SizeType
  , uint8
  , sint8
  , cvtNum
  , cvtNumMaybe

  , shiftl, shiftr, cat, lcat

  , Size(..)
  , SizeOf

  , UIntRep, fromUInt
  , SIntRep, fromSInt

  , intToSize
  , sizeToInt
  , toInt

  , wordToFloat
  , wordToDouble
  ) where

import GHC.TypeNats
import GHC.Float
import Control.Monad(foldM)
import Data.Kind(Constraint)
import Data.Word
import Data.Int
import Data.Bits
import Data.List(foldl',unfoldr)

import RTS.Base


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

fromSInt :: SInt n -> SIntRep n
fromSInt (SInt x) = x
{-# INLINE fromSInt #-}


--------------------------------------------------------------------------------

type Supports c n  = (c (UIntRep n) :: Constraint, c (SIntRep n))

type NormCtrs n    = (KnownNat n, Supports Bits n, Supports Integral n)
type SizeTypeDef n = ( NormCtrs n
                     , NormS (SizeOf n)
                     , Supports Show n
                     )

class SizeTypeDef n => SizeType n

-- The 0 instance is here to ensure that GHC does
-- not "simplify" `SizeType` into a whole lot of other constraints,
-- which would reult in equivalent but much messier types.
instance {-# OVERLAPPING #-} SizeType 0
instance SizeTypeDef n => SizeType n

normU :: NormCtrs n => UInt n -> UInt n
normU n@(UInt w) = UInt (mask n w)

mask :: (Bits a, Num a, KnownNat n) => f n -> a -> a
mask num a = a .&. complement (1 `shiftL` thisWidth num)

-- XXX: may overflow
thisWidth :: KnownNat n => num n -> Int
thisWidth x = fromIntegral (natVal x)

normS' :: NormCtrs n => Int -> SInt n -> SInt n
normS' repW n@(SInt i) = SInt ((mask n i `shiftL` amt) `shiftR` amt)
  where amt = repW - thisWidth n


class NormS sz where
  normS :: (KnownNat n, SizeOf n ~ sz) => SInt n -> SInt n

instance NormS 'S8   where normS = normS' 8
instance NormS 'S16  where normS = normS' 16
instance NormS 'S32  where normS = normS' 32
instance NormS 'S64  where normS = normS' 64
instance NormS 'SBig where
  normS n@(SInt i) = SInt (((i + half) `Prelude.mod` whole) - half)
    -- XXX: this could do a lot of allocation. Maybe using `if` is better?
    where w = thisWidth n
          whole = 1 `shiftL` w
          half  = whole `shiftR` 1


type Literal (x :: Nat) t = Numeric t

class Arith t where
  add :: t -> t -> t
  sub :: t -> t -> t
  mul :: t -> t -> t
  div :: t -> t -> t
  neg :: t -> t

class Arith t => Numeric t where
  mod :: t -> t -> t
  lit :: Integer -> t
  asInt :: t -> Integer

  shiftl' :: t -> Int -> t
  shiftr' :: t -> Int -> t
  bitOr :: t -> t -> t
  bitAnd :: t -> t -> t
  bitXor :: t -> t -> t
  bitCompl :: t -> t

instance Arith Integer where
  add = (+)
  sub = (-)
  mul = (*)
  div = Prelude.div
  neg = negate

instance Numeric Integer where
  mod = Prelude.mod

  lit = id
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
  lit x           = normU (UInt (fromInteger x))


instance SizeType n => Arith (SInt n) where
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
  lit x           = normS (SInt (fromInteger x))

deriving instance SizeType n => Show (UInt n)
deriving instance SizeType n => Eq   (UInt n)
deriving instance SizeType n => Ord  (UInt n)

deriving instance SizeType n => Show (SInt n)
deriving instance SizeType n => Eq   (SInt n)
deriving instance SizeType n => Ord  (SInt n)

--------------------------------------------------------------------------------

cvtNum :: (Numeric a, Numeric b) => a -> b
cvtNum = lit . asInt

cvtNumMaybe :: (Numeric a, Numeric b) => a -> Maybe b
cvtNumMaybe a = if asInt b == ia then Just b else Nothing
  where
  ia = asInt a
  b  = lit ia


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

type instance ElType  (UInt n) = UInt n
type instance KeyType (UInt n) = UInt n

instance SizeType n => IsLoop (UInt n) where

  loopFold  f s lim = foldl' f s (rngU lim)
  loopFoldM f s lim = foldM f s (rngU lim)
  loopIFold f s lim = foldl' f' s (rngU lim)
     where f' s' i = f s' i i
  loopIFoldM f s lim = foldM f' s (rngU lim)
     where f' s' i = f s' i i

unsafeIncU :: SizeType n => UInt n -> UInt n
unsafeIncU (UInt x) = UInt (x + 1)
{-# INLINE unsafeIncU #-}

rngU :: SizeType n => UInt n -> [UInt n]
rngU lim = unfoldr step (lit 0)
  where step n = if n < lim then Just (n, unsafeIncU n) else Nothing
{-# INLINE rngU #-}


--------------------------------------------------------------------------------

wordToFloat :: UInt 32 -> Float
wordToFloat x = castWord32ToFloat (fromUInt x)
{-# INLINE wordToFloat #-}

wordToDouble :: UInt 64 -> Double
wordToDouble x = castWord64ToDouble (fromUInt x)
{-# INLINE wordToDouble #-}



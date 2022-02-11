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
  , cvtHsNum
  , cvtNumToFloatingMaybe
  , cvtFloatingToNum
  , cvtFloatingToNumMaybe
  , cvtU
  , cvtNumMaybe

  , Bitdata(..)

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
import RTS.JSON


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

type CvtU a b = (SizeType a, SizeType b, CvtU' a b (a <=? b))

class (extending ~ (a <=? b)) => CvtU' a b (extending :: Bool) where
  cvtU' :: UInt a -> UInt b

instance ((a <=? b) ~ 'True, SizeType a, SizeType b) => CvtU' a b 'True where
  cvtU' (UInt x) = UInt (fromIntegral x)

instance ((a <=? b) ~ 'False, SizeType a, SizeType b) => CvtU' a b 'False where
  cvtU' (UInt x) = normU (UInt (fromIntegral x))

cvtU :: CvtU x y => UInt x -> UInt y
cvtU = cvtU'

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

cvtNum :: (Numeric a, Arith b) => a -> b
cvtNum = lit . asInt

cvtNumMaybe :: (Numeric a, Numeric b) => a -> Maybe b
cvtNumMaybe a = if asInt b == ia then Just b else Nothing
  where
  ia = asInt a
  b  = lit ia

-- Number to Floating
-- XXX: these can be more effecient
cvtHsNum :: (Numeric a, Num b) => a -> b
cvtHsNum = fromInteger . asInt

-- Number to Floating Maybe.
-- XXX: these can be more effecient
cvtNumToFloatingMaybe :: (Numeric a, Fractional b, Real b) => a -> Maybe b
cvtNumToFloatingMaybe x
  | toRational y == r = Just y
  | otherwise         = Nothing
  where
  r = toRational (asInt x)
  y = fromRational r

cvtFloatingToNum :: (RealFloat a, Numeric b) => a -> b
cvtFloatingToNum x
  | isNaN x      = error "Cannot cast NaN"
  | isInfinite x = error "Cannot cast Inf"
  | otherwise    = lit (truncate x)

cvtFloatingToNumMaybe :: (RealFloat a, Numeric b) => a -> Maybe b
cvtFloatingToNumMaybe x
  | isNaN x      = Nothing
  | isInfinite x = Nothing
  | otherwise    = if cvtHsNum y == x then Just y else Nothing
    where y = lit (truncate x)





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


--------------------------------------------------------------------------------

class Bitdata t where
  type family BDWidth t :: Nat
  bdToRep   :: SizeType (BDWidth t) => t -> UInt (BDWidth t)
  bdFromRep :: SizeType (BDWidth t) => UInt (BDWidth t) -> t

instance Bitdata () where
  type instance BDWidth () = 0
  bdToRep _   = UInt 0
  bdFromRep _ = ()
  {-# INLINE bdToRep #-}
  {-# INLINE bdFromRep #-}

instance Bitdata (UInt n) where
  type instance BDWidth (UInt n) = n
  bdToRep   = id
  bdFromRep = id
  {-# INLINE bdToRep #-}
  {-# INLINE bdFromRep #-}

instance Bitdata (SInt n) where
  type instance BDWidth (SInt n) = n
  bdToRep = cvtNum
  bdFromRep = cvtNum
  {-# INLINE bdToRep #-}
  {-# INLINE bdFromRep #-}

instance Bitdata Float where
  type instance BDWidth Float = 32
  bdToRep x  = UInt (castFloatToWord32 x)
  bdFromRep  = wordToFloat
  {-# INLINE bdToRep #-}
  {-# INLINE bdFromRep #-}

instance Bitdata Double where
  type instance BDWidth Double = 64
  bdToRep x  = UInt (castDoubleToWord64 x)
  bdFromRep  = wordToDouble
  {-# INLINE bdToRep #-}
  {-# INLINE bdFromRep #-}

--------------------------------------------------------------------------------

instance SizeType n => ToJSON (UInt n) where
  toJSON = toJSON . asInt

instance SizeType n => ToJSON (SInt n) where
  toJSON = toJSON . asInt



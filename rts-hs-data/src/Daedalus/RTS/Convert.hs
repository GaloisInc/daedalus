module Daedalus.RTS.Convert where

import GHC.Float(float2Double,double2Float)
import GHC.TypeLits(type Nat, type (<=?))
import Daedalus.RTS.Numeric
import Daedalus.RTS.Vector(Vector)
import Daedalus.RTS.Map(Map)

-- | Semantic conversions between types
class Convert a b where
  convert :: a -> b

-- | Bit-level representations of types
class Bitdata t where
  type family BDWidth t :: Nat
  toBits    :: SizeType (BDWidth t) => t -> UInt (BDWidth t)
  fromBits  :: SizeType (BDWidth t) => UInt (BDWidth t) -> t

-- Unit ------------------------------------------------------------------------
instance Bitdata () where
  type instance BDWidth () = 0
  toBits _    = UInt 0
  fromBits  _ = ()
  {-# INLINE toBits #-}
  {-# INLINE fromBits #-}

instance Convert () () where
  convert = id
  {-# INLINE convert #-}

-- Bool ------------------------------------------------------------------------
instance Bitdata Bool where
  type instance BDWidth Bool = 1
  toBits b          = if b then UInt 1 else UInt 0
  fromBits (UInt x) = x /= 0
  {-# INLINE toBits #-}
  {-# INLINE fromBits #-}

instance Convert Bool Bool where
  convert = id
  {-# INLINE convert #-}

-- Maybe -----------------------------------------------------------------------
instance (a ~ b) => Convert (Maybe a) (Maybe b) where
  convert = id
  {-# INLINE convert #-}

-- Vector ----------------------------------------------------------------------
instance (a ~ b) => Convert (Vector a) (Vector b) where
  convert = id
  {-# INLINE convert #-}

-- Map -------------------------------------------------------------------------
instance (a ~ b, x ~ y) => Convert (Map a x) (Map b y) where
  convert = id
  {-# INLINE convert #-}


-- Float -----------------------------------------------------------------------
instance Bitdata Float where
  type instance BDWidth Float = 32
  toBits    = floatToWord
  fromBits  = wordToFloat
  {-# INLINE toBits #-}
  {-# INLINE fromBits #-}

instance Convert Float Float        where convert = id; {-# INLINE convert #-}
instance Convert Float Double       where convert = float2Double; {-# INLINE convert #-}
instance Convert Float Integer      where convert = truncate; {-# INLINE convert #-}
instance SizeType n =>
         Convert Float (UInt n)     where convert = lit . convert; {-# INLINE convert #-}
instance SizeType n =>
         Convert Float (SInt n)     where convert = lit . convert; {-# INLINE convert #-}

-- Double ----------------------------------------------------------------------
instance Bitdata Double where
  type instance BDWidth Double = 64
  toBits    = doubleToWord
  fromBits  = wordToDouble
  {-# INLINE toBits #-}
  {-# INLINE fromBits #-}

instance Convert Double Float       where convert = double2Float; {-# INLINE convert #-}
instance Convert Double Double      where convert = id; {-# INLINE convert #-}
instance Convert Double Integer     where convert = truncate; {-# INLINE convert #-}
instance SizeType n =>
         Convert Double (UInt n)    where convert = lit . convert; {-# INLINE convert #-}
instance SizeType n =>
         Convert Double (SInt n)    where convert = lit . convert; {-# INLINE convert #-}

-- Integer ---------------------------------------------------------------------
instance Convert Integer Float      where convert = fromInteger; {-# INLINE convert #-}
instance Convert Integer Double     where convert = fromInteger; {-# INLINE convert #-}
instance Convert Integer Integer    where convert = id; {-# INLINE convert #-}
instance SizeType n =>
         Convert Integer (UInt n)   where convert = lit; {-# INLINE convert #-}
instance SizeType n =>
         Convert Integer (SInt n)   where convert = lit; {-# INLINE convert #-}


-- UInt ------------------------------------------------------------------------
instance Bitdata (UInt n) where
  type instance BDWidth (UInt n) = n
  toBits   = id
  fromBits = id
  {-# INLINE toBits #-}
  {-# INLINE fromBits #-}

instance SizeType n =>
         Convert (UInt n) Float     where convert = fromInteger . asInt; {-# INLINE convert #-}

instance SizeType n =>
         Convert (UInt n) Double    where convert = fromInteger . asInt; {-# INLINE convert #-}

instance SizeType n =>
         Convert (UInt n) Integer   where convert = asInt; {-# INLINE convert #-}

instance ConvertU m n (m <=? n) =>
         Convert (UInt m) (UInt n)  where convert = cvtU; {-# INLINE convert #-}

instance (SizeType m, SizeType n) =>
         Convert (UInt m) (SInt n)  where convert = lit . asInt; {-# INLINE convert #-}


class (extending ~ (m <=? n)) => ConvertU m n (extending :: Bool) where
  cvtU :: UInt m -> UInt n

instance ((m <=? n) ~ 'True, SizeType m, SizeType n) =>
  ConvertU m n 'True where
  cvtU (UInt x) = UInt (fromIntegral x)
  {-# INLINE cvtU #-}

instance ((m <=? n) ~ 'False, SizeType m, SizeType n) =>
  ConvertU m n 'False where
  cvtU (UInt x) = toUInt (fromIntegral x)
  {-# INLINE cvtU #-}



-- SInt ------------------------------------------------------------------------

instance Bitdata (SInt n) where
  type instance BDWidth (SInt n) = n
  toBits (SInt n)   = toUInt (fromIntegral n)
  fromBits (UInt n) = toSInt (fromIntegral n)
  {-# INLINE toBits #-}
  {-# INLINE fromBits #-}

instance SizeType n =>
         Convert (SInt n) Float     where convert = fromInteger . asInt; {-# INLINE convert #-}

instance SizeType n =>
         Convert (SInt n) Double    where convert = fromInteger . asInt; {-# INLINE convert #-}

instance SizeType n =>
         Convert (SInt n) Integer   where convert = asInt; {-# INLINE convert #-}

instance (SizeType m, SizeType n) =>
         Convert (SInt m) (UInt n)  where convert = lit . asInt; {-# INLINE convert #-}

instance (SizeType m, SizeType n) =>
         Convert (SInt m) (SInt n)  where convert = lit . asInt; {-# INLINE convert #-}






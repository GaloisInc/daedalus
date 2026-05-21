{-# Language BlockArguments #-}
module Daedalus.Value.Coerce where

import GHC.Float
import Daedalus.Panic(panic)
import Daedalus.PP(showPP)
import Daedalus.Range(uintRange,sintRange,inRange)
import Daedalus.Value.Type


-- | The second value in the result indicates if the coercion is exact
vCoerceTo :: TValue -> Value -> (Value,Bool)
vCoerceTo tgt v =
  case v of

    VTraced v1 i ->
      case vCoerceTo tgt v1 of
        (v1',ex) -> (vTraced v1' i, ex)

    VUInt _ n ->
      case tgt of
        TVInteger     -> ((VInteger  n),    exact)
        TVUInt st     -> ((vUIntWrapping  st n),    inRange (uintRange st) n)
        TVSInt st     -> ((vSInt' st n),    inRange (sintRange st) n)
        TVBDStruct bd -> ((VBDStruct bd n), bdValid bd n)
        TVBDUnion bd  -> ((VBDUnion bd n),  bduValid bd n)

        TVFloat   -> ((VFloat x), b)
           where (x,b) = numToFloating n

        TVDouble  -> ((VDouble x), b)
           where (x,b) = numToFloating n

        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VSInt _ n ->
      case tgt of
        TVInteger -> ((VInteger  n), exact)
        TVUInt st -> ((vUIntWrapping  st n), inRange (uintRange st) n)
        TVSInt st -> ((vSInt' st n), inRange (sintRange st) n)

        TVFloat   -> ((VFloat x), b)
           where (x,b) = numToFloating n

        TVDouble  -> ((VDouble x), b)
           where (x,b) = numToFloating n

        TVBDStruct {} -> bug
        TVBDUnion {}  -> bug
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVOther       -> bug

    VInteger n ->
      case tgt of
        TVInteger -> (v,             exact)
        TVUInt st -> ((vUIntWrapping  st n), inRange (uintRange st) n)
        TVSInt st -> ((vSInt' st n), inRange (sintRange st) n)

        TVFloat   -> ((VFloat x), b)
           where (x,b) = numToFloating n

        TVDouble  -> ((VDouble x), b)
           where (x,b) = numToFloating n

        TVBDStruct {} -> bug
        TVBDUnion {}  -> bug
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVOther       -> bug

    VFloat n ->
      case tgt of
        TVInteger     -> (VInteger i, fromInteger i == n)
          where i = floatingToInt n

        TVUInt w      -> (r, let VUInt _ j = r in fromInteger j == n)
          where r = floatingToUInt w n

        TVSInt w      -> (r, let VSInt _ j = r in fromInteger j == n)
          where r = floatingToSInt w n

        TVFloat       -> (v, exact)
        TVDouble      -> (VDouble (float2Double n), True)
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> bug
        TVBDUnion {}  -> bug
        TVOther       -> bug

    VDouble n ->
      case tgt of
        TVInteger     -> (VInteger i, fromInteger i == n)
          where i = floatingToInt n

        TVUInt w      -> (r, let VUInt _ j = r in fromInteger j == n)
          where r = floatingToUInt w n

        TVSInt w      -> (r, let VSInt _ j = r in fromInteger j == n)
          where r = floatingToSInt w n

        TVFloat       -> ((VFloat x), b)
          where x = double2Float n
                b = isNaN n || float2Double x == n

        TVDouble      -> (v, exact)
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> bug
        TVBDUnion {}  -> bug
        TVOther       -> bug

    VBDStruct _ n ->
      case tgt of
        TVUInt w      -> ((VUInt w n), exact)
        -- assumes that the widths match
        TVInteger     -> bug
        TVSInt {}     -> bug
        TVFloat       -> bug
        TVDouble      -> bug
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> (v, exact)
        TVBDUnion {}  -> bug
        TVOther       -> bug

    VBDUnion _ n ->
      case tgt of
        TVUInt w      -> ((VUInt w n), exact)
        -- assumes that the widths match
        TVInteger     -> bug
        TVSInt {}     -> bug
        TVFloat       -> bug
        TVDouble      -> bug
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> bug
        TVBDUnion {}  -> (v,exact)
        TVOther       -> bug

    VBool {}      -> (v, exact)
    VUnionElem {} -> (v, exact)
    VStruct {}    -> (v, exact)
    VMap {}       -> (v, exact)
    VStream {}    -> (v, exact)
    VArray {}     -> (v, exact)
    VMaybe {}     -> (v, exact)
    VBuilder {}   -> (v, exact)
    VIterator {}  -> (v, exact)



  where
  exact = True

  bug   = panic "vCoerceTo" [ "Invalid coercion"
                            , "Target: " ++ showPP tgt
                            , "Operand: " ++ showPP v
                            ]

numToFloating :: (Real a, Real b, Fractional b) => a -> (b,Bool)
numToFloating a = (y, toRational y == x)
  where
  x = toRational a
  y = fromRational x

-- | Total conversion from float to Integer: NaN/Inf -> 0, otherwise truncate.
floatingToInt :: RealFloat a => a -> Integer
floatingToInt a
  | isNaN a      = 0
  | isInfinite a = 0
  | otherwise    = truncate a

-- | Saturating conversion from float to UInt: NaN -> 0, clamp to [0, max].
floatingToUInt :: RealFloat a => Int -> a -> Value
floatingToUInt w a
  | isNaN a || a <= 0    = VUInt w 0
  | isInfinite a         = VUInt w hi
  | i >= hi              = VUInt w hi
  | otherwise            = VUInt w i
  where
    i  = truncate a
    hi = snd (uintRange w)

-- | Saturating conversion from float to SInt: NaN → 0, clamp to [min, max].
floatingToSInt :: RealFloat a => Int -> a -> Value
floatingToSInt w a
  | isNaN a              = VSInt w 0
  | isInfinite a && a > 0 = VSInt w hi
  | isInfinite a         = VSInt w lo
  | i < lo               = VSInt w lo
  | i > hi               = VSInt w hi
  | otherwise            = VSInt w i
  where
    i       = truncate a
    (lo,hi) = sintRange w




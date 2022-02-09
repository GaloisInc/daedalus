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

    VUInt _ n ->
      case tgt of
        TVInteger -> (VInteger  n, exact)
        TVUInt st -> (vUInt  st n, inRange (uintRange st) n)
        TVSInt st -> (vSInt' st n, inRange (sintRange st) n)
        TVBDStruct bd -> (VBDStruct bd n, bdValid bd n)
        TVBDUnion bd  -> (VBDUnion bd n, bduValid bd n)

        TVFloat   -> (VFloat x, b)
           where (x,b) = numToFloating n

        TVDouble  -> (VDouble x, b)
           where (x,b) = numToFloating n

        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VSInt _ n ->
      case tgt of
        TVInteger -> (VInteger  n, exact)
        TVUInt st -> (vUInt  st n, inRange (uintRange st) n)
        TVSInt st -> (vSInt' st n, inRange (sintRange st) n)

        TVFloat   -> (VFloat x, b)
           where (x,b) = numToFloating n

        TVDouble  -> (VDouble x, b)
           where (x,b) = numToFloating n

        TVBDStruct {} -> bug
        TVBDUnion {} -> bug
        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VInteger n ->
      case tgt of
        TVInteger -> (v, exact)
        TVUInt st -> (vUInt  st n, inRange (uintRange st) n)
        TVSInt st -> (vSInt' st n, inRange (sintRange st) n)

        TVFloat   -> (VFloat x, b)
           where (x,b) = numToFloating n

        TVDouble  -> (VDouble x, b)
           where (x,b) = numToFloating n

        TVBDStruct {} -> bug
        TVBDUnion {} -> bug
        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VFloat n ->
      case tgt of
        TVInteger     -> bug -- XXX
        TVUInt {}     -> bug -- XXX
        TVSInt {}     -> bug -- XXX
        TVFloat       -> (v, exact)
        TVDouble      -> (VDouble (float2Double n), True)
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> bug
        TVBDUnion {} -> bug
        TVOther       -> bug

    VDouble n ->
      case tgt of
        TVInteger     -> bug -- XXX
        TVUInt {}     -> bug -- XXX
        TVSInt {}     -> bug -- XXX
        TVFloat       -> (VFloat x, b)
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
        TVUInt w      -> (VUInt w n, exact)
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
        TVUInt w      -> (VUInt w n, exact)
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




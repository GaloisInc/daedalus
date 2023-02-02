{-# Language BlockArguments #-}
module Daedalus.Value.Coerce where

import GHC.Float
import Daedalus.Panic(panic)
import Daedalus.PP(showPP)
import Daedalus.Range(uintRange,sintRange,inRange)
import Daedalus.Value.Type


-- | The second value in the result indicates if the coercion is exact
vCoerceTo :: TValue -> Value -> (Partial Value,Bool)
vCoerceTo tgt v =
  case v of

    VTraced v1 i ->
      case vCoerceTo tgt v1 of
        (pv,ex) -> (fmap (`vTraced` i) pv, ex)

    VUInt _ n ->
      case tgt of
        TVInteger     -> (Right (VInteger  n),    exact)
        TVUInt st     -> (Right (vUInt  st n),    inRange (uintRange st) n)
        TVSInt st     -> (Right (vSInt' st n),    inRange (sintRange st) n)
        TVBDStruct bd -> (Right (VBDStruct bd n), bdValid bd n)
        TVBDUnion bd  -> (Right (VBDUnion bd n),  bduValid bd n)

        TVFloat   -> (Right (VFloat x), b)
           where (x,b) = numToFloating n

        TVDouble  -> (Right (VDouble x), b)
           where (x,b) = numToFloating n

        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VSInt _ n ->
      case tgt of
        TVInteger -> (Right (VInteger  n), exact)
        TVUInt st -> (Right (vUInt  st n), inRange (uintRange st) n)
        TVSInt st -> (Right (vSInt' st n), inRange (sintRange st) n)

        TVFloat   -> (Right (VFloat x), b)
           where (x,b) = numToFloating n

        TVDouble  -> (Right (VDouble x), b)
           where (x,b) = numToFloating n

        TVBDStruct {} -> bug
        TVBDUnion {}  -> bug
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVOther       -> bug

    VInteger n ->
      case tgt of
        TVInteger -> (Right v,             exact)
        TVUInt st -> (Right (vUInt  st n), inRange (uintRange st) n)
        TVSInt st -> (Right (vSInt' st n), inRange (sintRange st) n)

        TVFloat   -> (Right (VFloat x), b)
           where (x,b) = numToFloating n

        TVDouble  -> (Right (VDouble x), b)
           where (x,b) = numToFloating n

        TVBDStruct {} -> bug
        TVBDUnion {}  -> bug
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVOther       -> bug

    VFloat n ->
      case tgt of
        TVInteger     -> (r, ifDef r \ ~(VInteger j) -> fromInteger j == n)
          where r = VInteger <$> floatingToInt n

        TVUInt w      -> (r, ifDef r \ ~(VUInt _ j) -> fromInteger j == n)
          where
          r = vUInt w <$> floatingToInt n

        TVSInt w      -> (r, ifDef r \ ~(VSInt _ j) -> fromInteger j == n)
          where
          r = vSInt' w <$> floatingToInt n

        TVFloat       -> (Right v, exact)
        TVDouble      -> (Right (VDouble (float2Double n)), True)
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> bug
        TVBDUnion {}  -> bug
        TVOther       -> bug

    VDouble n ->
      case tgt of
        TVInteger     -> (r, ifDef r \ ~(VInteger j) -> fromInteger j == n)
          where r = VInteger <$> floatingToInt n

        TVUInt w      -> (r, ifDef r \ ~(VUInt _ j) -> fromInteger j == n)
          where
          r = vUInt w <$> floatingToInt n

        TVSInt w      -> (r, ifDef r \ ~(VSInt _ j) -> fromInteger j == n)
          where
          r = vSInt' w <$> floatingToInt n

        TVFloat       -> (Right (VFloat x), b)
          where x = double2Float n
                b = isNaN n || float2Double x == n

        TVDouble      -> (Right v, exact)
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> bug
        TVBDUnion {}  -> bug
        TVOther       -> bug

    VBDStruct _ n ->
      case tgt of
        TVUInt w      -> (Right (VUInt w n), exact)
        -- assumes that the widths match
        TVInteger     -> bug
        TVSInt {}     -> bug
        TVFloat       -> bug
        TVDouble      -> bug
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> (Right v, exact)
        TVBDUnion {}  -> bug
        TVOther       -> bug

    VBDUnion _ n ->
      case tgt of
        TVUInt w      -> (Right (VUInt w n), exact)
        -- assumes that the widths match
        TVInteger     -> bug
        TVSInt {}     -> bug
        TVFloat       -> bug
        TVDouble      -> bug
        TVNum {}      -> bug
        TVArray       -> bug
        TVMap         -> bug
        TVBDStruct {} -> bug
        TVBDUnion {}  -> (Right v,exact)
        TVOther       -> bug

    VBool {}      -> (Right v, exact)
    VUnionElem {} -> (Right v, exact)
    VStruct {}    -> (Right v, exact)
    VMap {}       -> (Right v, exact)
    VStream {}    -> (Right v, exact)
    VArray {}     -> (Right v, exact)
    VMaybe {}     -> (Right v, exact)
    VBuilder {}   -> (Right v, exact)
    VIterator {}  -> (Right v, exact)



  where
  exact = True
  ifDef x p = case x of
                Left {} -> False
                Right a -> p a

  bug   = panic "vCoerceTo" [ "Invalid coercion"
                            , "Target: " ++ showPP tgt
                            , "Operand: " ++ showPP v
                            ]

numToFloating :: (Real a, Real b, Fractional b) => a -> (b,Bool)
numToFloating a = (y, toRational y == x)
  where
  x = toRational a
  y = fromRational x

floatingToInt :: RealFloat a => a -> Partial Integer
floatingToInt a
  | isNaN a      = vErr "Cannot cast NaN"
  | isInfinite a = vErr "Cannon cast Inf"
  | otherwise    = Right (truncate a)




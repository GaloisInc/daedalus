module Daedalus.Value.Coerce where

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
        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VSInt _ n ->
      case tgt of
        TVInteger -> (VInteger  n, exact)
        TVUInt st -> (vUInt  st n, inRange (uintRange st) n)
        TVSInt st -> (vSInt' st n, inRange (sintRange st) n)
        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

    VInteger n ->
      case tgt of
        TVInteger -> (v, exact)
        TVUInt st -> (vUInt  st n, inRange (uintRange st) n)
        TVSInt st -> (vSInt' st n, inRange (sintRange st) n)
        TVNum {}  -> bug
        TVArray   -> bug
        TVMap     -> bug
        TVOther   -> bug

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



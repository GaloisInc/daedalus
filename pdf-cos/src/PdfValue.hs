{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language ViewPatterns #-}
module PdfValue where
 
import qualified PdfMonad as D
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Input as RTS
import qualified RTS.Map as Map
import qualified RTS.Vector as Vector
 
 
data Number
  = Number HS.Integer HS.Integer
  
 
deriving instance HS.Eq Number
 
deriving instance HS.Ord Number
 
deriving instance HS.Show Number
 
instance RTS.DDL Number where
 
instance HS.HasField "num" Number HS.Integer where
  getField (Number x _) = x
 
instance HS.HasField "exp" Number HS.Integer where
  getField (Number _ x) = x
 
data Ref
  = Ref HS.Integer HS.Integer
  
 
deriving instance HS.Eq Ref
 
deriving instance HS.Ord Ref
 
deriving instance HS.Show Ref
 
instance RTS.DDL Ref where
 
instance HS.HasField "obj" Ref HS.Integer where
  getField (Ref x _) = x
 
instance HS.HasField "gen" Ref HS.Integer where
  getField (Ref _ x) = x
 
data Value
  = Value_array (Vector.Vector Value)
  | Value_bool HS.Bool
  | Value_dict (Map.Map (Vector.Vector (RTS.UInt 8)) Value)
  | Value_name (Vector.Vector (RTS.UInt 8))
  | Value_null ()
  | Value_number Number
  | Value_ref Ref
  | Value_string (Vector.Vector (RTS.UInt 8))
  
 
deriving instance HS.Eq Value
 
deriving instance HS.Ord Value
 
deriving instance HS.Show Value
 
instance RTS.DDL Value where
 
instance HS.HasField "array" Value
           (HS.Maybe (Vector.Vector Value)) where
  getField (Value_array x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "bool" Value (HS.Maybe HS.Bool) where
  getField (Value_bool x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "dict" Value
           (HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) Value)) where
  getField (Value_dict x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "name" Value
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (Value_name x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "null" Value (HS.Maybe ()) where
  getField (Value_null x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "number" Value (HS.Maybe Number) where
  getField (Value_number x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "ref" Value (HS.Maybe Ref) where
  getField (Value_ref x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "string" Value
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (Value_string x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Dict_0
  = Dict_0 (Vector.Vector (RTS.UInt 8)) Value
  
 
deriving instance HS.Eq Dict_0
 
deriving instance HS.Ord Dict_0
 
deriving instance HS.Show Dict_0
 
instance RTS.DDL Dict_0 where
 
instance HS.HasField "key" Dict_0
           (Vector.Vector (RTS.UInt 8)) where
  getField (Dict_0 x _) = x
 
instance HS.HasField "value" Dict_0 Value where
  getField (Dict_0 _ x) = x
 
data Sign
  = Sign_neg ()
  | Sign_pos ()
  
 
deriving instance HS.Eq Sign
 
deriving instance HS.Ord Sign
 
deriving instance HS.Show Sign
 
instance RTS.DDL Sign where
 
instance HS.HasField "neg" Sign (HS.Maybe ()) where
  getField (Sign_neg x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "pos" Sign (HS.Maybe ()) where
  getField (Sign_pos x) = HS.Just x
   
  getField _ = HS.Nothing
 
cs_lf :: RTS.ClassVal
 
cs_lf = RTS.bcSingle (RTS.uint8 10)
 
cs_cr :: RTS.ClassVal
 
cs_cr = RTS.bcSingle (RTS.uint8 13)
 
cs_simpleWS :: RTS.ClassVal
 
cs_simpleWS =
  RTS.bcUnion
    (RTS.bcUnion
       (RTS.bcUnion (RTS.bcSingle (RTS.uint8 0))
          (RTS.bcSingle (RTS.uint8 9)))
       (RTS.bcSingle (RTS.uint8 12)))
    (RTS.bcSingle (RTS.uint8 32))
 
pSimpleEOL :: D.Parser (RTS.UInt 8)
 
pSimpleEOL =
  (RTS.|||)
    (do HS.const () HS.<$> RTS.pMatch1 "12:31--12:33" cs_cr
        (__ :: RTS.UInt 8) <-
          RTS.uint8 HS.<$> RTS.pMatch1 "12:36--12:38" cs_lf
        HS.pure __)
    (RTS.uint8 HS.<$> RTS.pMatch1 "12:44--12:46" cs_lf)
 
pEOL :: D.Parser (RTS.UInt 8)
 
pEOL =
  (RTS.<||) (RTS.pEnter "PdfValue.SimpleEOL" pSimpleEOL)
    (RTS.uint8 HS.<$> RTS.pMatch1 "13:42--13:44" cs_cr)
 
pComment :: D.Parser (RTS.UInt 8)
 
pComment =
  do HS.const ()
       HS.<$> RTS.pMatch "14:31--14:39" (Vector.vecFromRep "%")
     RTS.pSkipMany (RTS.<||)
       (HS.const ()
          HS.<$> RTS.pMatch1 "14:48--14:67"
                   (RTS.bcComplement (RTS.bcUnion cs_lf cs_cr)))
     (__ :: RTS.UInt 8) <- RTS.pEnter "PdfValue.EOL" pEOL
     HS.pure __
 
pAnyWS :: D.Parser (RTS.UInt 8)
 
pAnyWS =
  (RTS.|||)
    ((RTS.|||)
       (RTS.uint8 HS.<$> RTS.pMatch1 "15:29--15:37" cs_simpleWS)
       (RTS.pEnter "PdfValue.Comment" pComment))
    (RTS.pEnter "PdfValue.EOL" pEOL)
 
_SimpleEOL :: D.Parser ()
 
_SimpleEOL =
  (RTS.|||)
    (do HS.const () HS.<$> RTS.pMatch1 "12:31--12:33" cs_cr
        HS.const () HS.<$> RTS.pMatch1 "12:36--12:38" cs_lf)
    (HS.const () HS.<$> RTS.pMatch1 "12:44--12:46" cs_lf)
 
_EOL :: D.Parser ()
 
_EOL =
  (RTS.<||) (RTS.pEnter "PdfValue._SimpleEOL" _SimpleEOL)
    (HS.const () HS.<$> RTS.pMatch1 "13:42--13:44" cs_cr)
 
_Comment :: D.Parser ()
 
_Comment =
  do HS.const ()
       HS.<$> RTS.pMatch "14:31--14:39" (Vector.vecFromRep "%")
     RTS.pSkipMany (RTS.<||)
       (HS.const ()
          HS.<$> RTS.pMatch1 "14:48--14:67"
                   (RTS.bcComplement (RTS.bcUnion cs_lf cs_cr)))
     RTS.pEnter "PdfValue._EOL" _EOL
 
_AnyWS :: D.Parser ()
 
_AnyWS =
  (RTS.|||)
    ((RTS.|||)
       (HS.const () HS.<$> RTS.pMatch1 "15:29--15:37" cs_simpleWS)
       (RTS.pEnter "PdfValue._Comment" _Comment))
    (RTS.pEnter "PdfValue._EOL" _EOL)
 
pToken :: forall b. RTS.DDL b => D.Parser b -> D.Parser b
 
pToken (pP :: D.Parser b) =
  do (__ :: b) <- pP
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._AnyWS" _AnyWS)
     HS.pure __
 
_Token :: forall b. RTS.DDL b => D.Parser () -> D.Parser ()
 
_Token (_P :: D.Parser ()) =
  do _P
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._AnyWS" _AnyWS)
 
pKW :: Vector.Vector (RTS.UInt 8) -> D.Parser ()
 
pKW (x :: Vector.Vector (RTS.UInt 8)) =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (HS.const () HS.<$> RTS.pMatch "23:39--23:45" x))
 
_KW :: Vector.Vector (RTS.UInt 8) -> D.Parser ()
 
_KW (x :: Vector.Vector (RTS.UInt 8)) =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (HS.const () HS.<$> RTS.pMatch "23:39--23:45" x))
 
pBetween ::
  forall d.
    RTS.DDL d =>
      Vector.Vector (RTS.UInt 8)
        -> (Vector.Vector (RTS.UInt 8) -> (D.Parser d -> D.Parser d))
 
pBetween (open :: Vector.Vector (RTS.UInt 8))
  (close :: Vector.Vector (RTS.UInt 8))
  (pP :: D.Parser d) =
  do RTS.pEnter "PdfValue._KW" (_KW open)
     (__ :: d) <- pP
     RTS.pEnter "PdfValue._KW" (_KW close)
     HS.pure __
 
numBase :: HS.Integer -> (Vector.Vector HS.Integer -> HS.Integer)
 
numBase (base :: HS.Integer) (ds :: Vector.Vector HS.Integer) =
  RTS.loopFold
    (\(val :: HS.Integer) (d :: HS.Integer) ->
       RTS.add (RTS.mul val base) d)
    (RTS.lit 0 :: HS.Integer)
    ds
 
pOnly :: forall b. RTS.DDL b => D.Parser b -> D.Parser b
 
pOnly (pP :: D.Parser b) =
  do (__ :: b) <- pP
     RTS.pEnd "26:39--26:41"
     HS.pure __
 
pWhen ::
  forall b f.
    (RTS.DDL b, RTS.DDL f) => D.Parser f -> (b -> D.Parser b)
 
pWhen (pP :: D.Parser f) (x :: b) =
  do do HS.void pP
        HS.pure ()
     (__ :: b) <- HS.pure x
     HS.pure __
 
pGuard :: HS.Bool -> D.Parser ()
 
pGuard (p :: HS.Bool) = RTS.pGuard "28:29--28:37" "guard failed" p
 
pBool :: D.Parser HS.Bool
 
pBool =
  (RTS.|||)
    (RTS.pEnter "PdfValue.When"
       (pWhen @HS.Bool @()
          (RTS.pEnter "PdfValue.KW" (pKW (Vector.vecFromRep "true")))
          HS.True))
    (RTS.pEnter "PdfValue.When"
       (pWhen @HS.Bool @()
          (RTS.pEnter "PdfValue.KW" (pKW (Vector.vecFromRep "false")))
          HS.False))
 
pSign :: D.Parser Sign
 
pSign =
  (RTS.|||)
    (RTS.pEnter "pos"
       (do (_398 :: ()) <-
             (RTS.<||)
               (HS.const ()
                  HS.<$> RTS.pMatch "51:20--51:28" (Vector.vecFromRep "+"))
               (HS.pure ())
           HS.pure (Sign_pos _398)))
    (RTS.pEnter "neg"
       (do (_399 :: ()) <-
             HS.const ()
               HS.<$> RTS.pMatch "52:10--52:18" (Vector.vecFromRep "-")
           HS.pure (Sign_neg _399)))
 
pDigit :: D.Parser HS.Integer
 
pDigit =
  do (_401 :: RTS.UInt 8) <-
       do (_400 :: RTS.UInt 8) <-
            RTS.uint8
              HS.<$> RTS.pMatch1 "77:17--77:34"
                       (RTS.bcRange (RTS.uint8 48) (RTS.uint8 57))
          HS.pure (RTS.sub _400 (RTS.uint8 48))
     RTS.pIsJust "77:17--77:48" "Value does not fit in target type"
       (RTS.convertMaybe _401 :: HS.Maybe HS.Integer)
 
pNatural :: D.Parser HS.Integer
 
pNatural =
  do (_402 :: Vector.Vector HS.Integer) <-
       RTS.pMinLength "67:27--67:42" (RTS.lit 1 :: RTS.UInt 64)
         (RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.Digit" pDigit))
     HS.pure (numBase (RTS.lit 10 :: HS.Integer) _402)
 
pFrac :: RTS.UInt 64 -> (Number -> D.Parser Number)
 
pFrac (n :: RTS.UInt 64) (w :: Number) =
  do HS.const ()
       HS.<$> RTS.pMatch "70:3--70:11" (Vector.vecFromRep ".")
     (ds :: Vector.Vector HS.Integer) <-
       RTS.pMinLength "71:9--71:25" n
         (RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.Digit" pDigit))
     (__ :: Number) <-
       HS.pure
         (RTS.loopFold
            (\(val :: Number) (d :: HS.Integer) ->
               Number
                 (RTS.add
                    (RTS.mul (RTS.lit 10 :: HS.Integer) (HS.getField @"num" val))
                    d)
                 (RTS.sub (HS.getField @"exp" val) (RTS.lit 1 :: HS.Integer)))
            w
            ds)
     HS.pure __
 
pUnsignedLeadDigits :: D.Parser Number
 
pUnsignedLeadDigits =
  do (n :: HS.Integer) <- RTS.pEnter "PdfValue.Natural" pNatural
     (val :: Number) <- HS.pure (Number n (RTS.lit 0 :: HS.Integer))
     (__ :: Number) <-
       (RTS.<||)
         (RTS.pEnter "PdfValue.Frac" (pFrac (RTS.lit 0 :: RTS.UInt 64) val))
         (HS.pure val)
     HS.pure __
 
pUnsignedNumber :: D.Parser Number
 
pUnsignedNumber =
  (RTS.|||)
    (RTS.pEnter "PdfValue.UnsignedLeadDigits" pUnsignedLeadDigits)
    (RTS.pEnter "PdfValue.Frac"
       (pFrac (RTS.lit 1 :: RTS.UInt 64)
          (Number (RTS.lit 0 :: HS.Integer) (RTS.lit 0 :: HS.Integer))))
 
pNumber :: D.Parser Number
 
pNumber =
  RTS.pEnter "PdfValue.Token"
    (pToken @Number
       (do (sign :: Sign) <- RTS.pEnter "PdfValue.Sign" pSign
           (n :: Number) <-
             RTS.pEnter "PdfValue.UnsignedNumber" pUnsignedNumber
           (__ :: Number) <-
             (RTS.|||)
               (RTS.pEnter "PdfValue.When"
                  (pWhen @Number @()
                     (case sign of
                        Sign_pos (_404 :: ()) -> HS.pure _404
                        _ -> RTS.pError RTS.FromSystem "46:11--46:21"
                               "Pattern match failure")
                     n))
               (RTS.pEnter "PdfValue.When"
                  (pWhen @Number @()
                     (case sign of
                        Sign_neg (_405 :: ()) -> HS.pure _405
                        _ -> RTS.pError RTS.FromSystem "47:11--47:21"
                               "Pattern match failure")
                     (Number (RTS.sub (RTS.lit 0 :: HS.Integer) (HS.getField @"num" n))
                        (HS.getField @"exp" n))))
           HS.pure __))
 
pOctDigit :: D.Parser HS.Integer
 
pOctDigit =
  do (_409 :: RTS.UInt 8) <-
       do (_408 :: RTS.UInt 8) <-
            RTS.uint8
              HS.<$> RTS.pMatch1 "76:17--76:34"
                       (RTS.bcRange (RTS.uint8 48) (RTS.uint8 55))
          HS.pure (RTS.sub _408 (RTS.uint8 48))
     RTS.pIsJust "76:17--76:48" "Value does not fit in target type"
       (RTS.convertMaybe _409 :: HS.Maybe HS.Integer)
 
pHexDigit :: D.Parser HS.Integer
 
pHexDigit =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue.Digit" pDigit)
       (do (_412 :: RTS.UInt 8) <-
             do (_411 :: RTS.UInt 8) <-
                  do (_410 :: RTS.UInt 8) <-
                       RTS.uint8
                         HS.<$> RTS.pMatch1 "79:22--79:39"
                                  (RTS.bcRange (RTS.uint8 97) (RTS.uint8 102))
                     HS.pure (RTS.add (RTS.lit 10 :: RTS.UInt 8) _410)
                HS.pure (RTS.sub _411 (RTS.uint8 97))
           RTS.pIsJust "79:17--79:53" "Value does not fit in target type"
             (RTS.convertMaybe _412 :: HS.Maybe HS.Integer)))
    (do (_415 :: RTS.UInt 8) <-
          do (_414 :: RTS.UInt 8) <-
               do (_413 :: RTS.UInt 8) <-
                    RTS.uint8
                      HS.<$> RTS.pMatch1 "80:22--80:39"
                               (RTS.bcRange (RTS.uint8 65) (RTS.uint8 70))
                  HS.pure (RTS.add (RTS.lit 10 :: RTS.UInt 8) _413)
             HS.pure (RTS.sub _414 (RTS.uint8 65))
        RTS.pIsJust "80:17--80:53" "Value does not fit in target type"
          (RTS.convertMaybe _415 :: HS.Maybe HS.Integer))
 
_Guard :: HS.Bool -> D.Parser ()
 
_Guard (p :: HS.Bool) = RTS.pGuard "28:29--28:37" "guard failed" p
 
pNumberAsNat :: Number -> D.Parser HS.Integer
 
pNumberAsNat (x :: Number) =
  do RTS.pEnter "PdfValue._Guard"
       (_Guard
          (((RTS.lit 0 :: HS.Integer) HS.<= HS.getField @"num" x)
             HS.&& (HS.getField @"exp" x HS.== (RTS.lit 0 :: HS.Integer))))
     (__ :: HS.Integer) <- HS.pure (HS.getField @"num" x)
     HS.pure __
 
pStringNumEsc :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringNumEsc =
  do (_419 :: RTS.UInt 8) <-
       do (_418 :: HS.Integer) <-
            do (_417 :: Vector.Vector HS.Integer) <-
                 RTS.pMinLength "116:33--116:52" (RTS.lit 1 :: RTS.UInt 64)
                   (RTS.pManyUpTo (RTS.<||) (RTS.lit 3 :: RTS.UInt 64)
                      (RTS.pEnter "PdfValue.OctDigit" pOctDigit))
               HS.pure (numBase (RTS.lit 8 :: HS.Integer) _417)
          HS.pure (RTS.convert _418 :: RTS.UInt 8)
     HS.pure (Vector.fromList [_419])
 
pStringEsc :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "101:4--101:13" (Vector.vecFromRep "\\")
     (__ :: Vector.Vector (RTS.UInt 8)) <-
       (RTS.|||)
         (RTS.pEnter "PdfValue.When"
            (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
               (RTS.pMatch "103:12--103:20" (Vector.vecFromRep "n"))
               (Vector.vecFromRep "\n")))
         ((RTS.|||)
            (RTS.pEnter "PdfValue.When"
               (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                  (RTS.pMatch "104:12--104:20" (Vector.vecFromRep "r"))
                  (Vector.vecFromRep "\r")))
            ((RTS.|||)
               (RTS.pEnter "PdfValue.When"
                  (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                     (RTS.pMatch "105:12--105:20" (Vector.vecFromRep "t"))
                     (Vector.vecFromRep "\t")))
               ((RTS.|||)
                  (RTS.pEnter "PdfValue.When"
                     (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                        (RTS.pMatch "106:12--106:20" (Vector.vecFromRep "b"))
                        (Vector.vecFromRep "\b")))
                  ((RTS.|||)
                     (RTS.pEnter "PdfValue.When"
                        (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                           (RTS.pMatch "107:12--107:20" (Vector.vecFromRep "f"))
                           (Vector.vecFromRep "\f")))
                     ((RTS.|||)
                        (RTS.pEnter "PdfValue.When"
                           (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                              (RTS.pMatch "108:12--108:20" (Vector.vecFromRep "("))
                              (Vector.vecFromRep "(")))
                        ((RTS.|||)
                           (RTS.pEnter "PdfValue.When"
                              (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                 (RTS.pMatch "109:12--109:20" (Vector.vecFromRep ")"))
                                 (Vector.vecFromRep ")")))
                           ((RTS.|||)
                              (RTS.pEnter "PdfValue.When"
                                 (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                    (RTS.pMatch "110:12--110:21" (Vector.vecFromRep "\\"))
                                    (Vector.vecFromRep "\\")))
                              ((RTS.|||)
                                 (RTS.pEnter "PdfValue.When"
                                    (pWhen @(Vector.Vector (RTS.UInt 8)) @(RTS.UInt 8)
                                       (RTS.pEnter "PdfValue.EOL" pEOL)
                                       (Vector.vecFromRep "")))
                                 (RTS.pEnter "PdfValue.StringNumEsc" pStringNumEsc)))))))))
     HS.pure __
 
pStringChars :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringChars =
  do (_421 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
       RTS.pMany (RTS.<||)
         (RTS.pEnter "PdfValue.StringChunk" pStringChunk)
     HS.pure (Vector.concat _421)
 
pStringInParens :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringInParens =
  do (_425 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
       do (_422 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pMatch "98:31--98:39" (Vector.vecFromRep "(")
          (_423 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pEnter "PdfValue.StringChars" pStringChars
          (_424 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pMatch "98:55--98:63" (Vector.vecFromRep ")")
          HS.pure (Vector.fromList [_422, _423, _424])
     HS.pure (Vector.concat _425)
 
pStringChunk :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringChunk =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue.StringInParens" pStringInParens)
       (RTS.pEnter "PdfValue.StringEsc" pStringEsc))
    (RTS.pMinLength "96:5--96:32" (RTS.lit 1 :: RTS.UInt 64)
       (RTS.pMany (RTS.<||)
          (RTS.uint8
             HS.<$> RTS.pMatch1 "96:17--96:32"
                      (RTS.bcComplement (RTS.bcByteString "\\()")))))
 
pString :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pString =
  RTS.pEnter "PdfValue.Between"
    (pBetween @(Vector.Vector (RTS.UInt 8)) (Vector.vecFromRep "(")
       (Vector.vecFromRep ")")
       (RTS.pEnter "PdfValue.StringChars" pStringChars))
 
pHexStringNum2 :: D.Parser (RTS.UInt 8)
 
pHexStringNum2 =
  do (_427 :: HS.Integer) <-
       do (_426 :: Vector.Vector HS.Integer) <-
            Vector.replicateM (RTS.lit 2 :: RTS.UInt 64)
              (RTS.pEnter "PdfValue.Token"
                 (pToken @HS.Integer (RTS.pEnter "PdfValue.HexDigit" pHexDigit)))
          HS.pure (numBase (RTS.lit 16 :: HS.Integer) _426)
     HS.pure (RTS.convert _427 :: RTS.UInt 8)
 
pHexStringNum1 :: D.Parser (RTS.UInt 8)
 
pHexStringNum1 =
  do (_429 :: HS.Integer) <-
       do (_428 :: HS.Integer) <-
            RTS.pEnter "PdfValue.Token"
              (pToken @HS.Integer (RTS.pEnter "PdfValue.HexDigit" pHexDigit))
          HS.pure (RTS.mul (RTS.lit 16 :: HS.Integer) _428)
     HS.pure (RTS.convert _429 :: RTS.UInt 8)
 
pHexString :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pHexString =
  RTS.pEnter "PdfValue.Between"
    (pBetween @(Vector.Vector (RTS.UInt 8)) (Vector.vecFromRep "<")
       (Vector.vecFromRep ">")
       (do (front :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMany (RTS.<||)
               (RTS.pEnter "PdfValue.HexStringNum2" pHexStringNum2)
           (__ :: Vector.Vector (RTS.UInt 8)) <-
             (RTS.|||)
               (do (_433 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
                     do (_432 :: Vector.Vector (RTS.UInt 8)) <-
                          do (_431 :: RTS.UInt 8) <-
                               RTS.pEnter "PdfValue.HexStringNum1" pHexStringNum1
                             HS.pure (Vector.fromList [_431])
                        HS.pure (Vector.fromList [front, _432])
                   HS.pure (Vector.concat _433))
               (HS.pure front)
           HS.pure __))
 
pNameEsc :: D.Parser (RTS.UInt 8)
 
pNameEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "141:3--141:11" (Vector.vecFromRep "#")
     (__ :: RTS.UInt 8) <-
       do (_435 :: HS.Integer) <-
            do (_434 :: Vector.Vector HS.Integer) <-
                 Vector.replicateM (RTS.lit 2 :: RTS.UInt 64)
                   (RTS.pEnter "PdfValue.HexDigit" pHexDigit)
               HS.pure (numBase (RTS.lit 16 :: HS.Integer) _434)
          HS.pure (RTS.convert _435 :: RTS.UInt 8)
     RTS.pEnter "PdfValue._Guard"
       (_Guard ((RTS.lit 0 :: RTS.UInt 8) HS.< __))
     HS.pure __
 
pNameChar :: D.Parser (RTS.UInt 8)
 
pNameChar =
  (RTS.|||)
    (RTS.uint8
       HS.<$> RTS.pMatch1 "137:16--137:53"
                (RTS.bcComplement (RTS.bcByteString "\NUL\t\n\f\r ()<>[]{}/%#")))
    (RTS.pEnter "PdfValue.NameEsc" pNameEsc)
 
pName :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pName =
  RTS.pEnter "PdfValue.Token"
    (pToken @(Vector.Vector (RTS.UInt 8))
       (do HS.const ()
             HS.<$> RTS.pMatch "135:24--135:32" (Vector.vecFromRep "/")
           (__ :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.NameChar" pNameChar)
           HS.pure __))
 
pNull :: D.Parser ()
 
pNull = RTS.pEnter "PdfValue.KW" (pKW (Vector.vecFromRep "null"))
 
pRef :: D.Parser Ref
 
pRef =
  do (obj :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (pToken @HS.Integer (RTS.pEnter "PdfValue.Natural" pNatural))
     (gen :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (pToken @HS.Integer (RTS.pEnter "PdfValue.Natural" pNatural))
     RTS.pEnter "PdfValue._KW" (_KW (Vector.vecFromRep "R"))
     HS.pure (Ref obj gen)
 
pArray :: D.Parser (Vector.Vector Value)
 
pArray =
  RTS.pEnter "PdfValue.Between"
    (pBetween @(Vector.Vector Value) (Vector.vecFromRep "[")
       (Vector.vecFromRep "]")
       (RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.Value" pValue)))
 
pDict :: D.Parser (Map.Map (Vector.Vector (RTS.UInt 8)) Value)
 
pDict =
  do (ents :: Vector.Vector Dict_0) <-
       RTS.pEnter "PdfValue.Between"
         (pBetween @(Vector.Vector Dict_0) (Vector.vecFromRep "<<")
            (Vector.vecFromRep ">>")
            (RTS.pMany (RTS.<||)
               (do (key :: Vector.Vector (RTS.UInt 8)) <-
                     RTS.pEnter "PdfValue.Name" pName
                   (value :: Value) <- RTS.pEnter "PdfValue.Value" pValue
                   HS.pure (Dict_0 key value))))
     (__ :: Map.Map (Vector.Vector (RTS.UInt 8)) Value) <-
       RTS.loopFoldM
         (\(d :: Map.Map (Vector.Vector (RTS.UInt 8))
                   Value) (e :: Dict_0) ->
            RTS.pIsJust "159:31--159:52" "Key already present"
              (Map.insertMaybe (HS.getField @"key" e) (HS.getField @"value" e)
                 d))
         (Map.empty :: Map.Map (Vector.Vector (RTS.UInt 8)) Value)
         ents
     HS.pure __
 
pValue :: D.Parser Value
 
pValue =
  (RTS.<||)
    (RTS.pEnter "null"
       (do (_438 :: ()) <- RTS.pEnter "PdfValue.Null" pNull
           HS.pure (Value_null _438)))
    ((RTS.<||)
       (RTS.pEnter "bool"
          (do (_439 :: HS.Bool) <- RTS.pEnter "PdfValue.Bool" pBool
              HS.pure (Value_bool _439)))
       ((RTS.<||)
          (RTS.pEnter "ref"
             (do (_440 :: Ref) <- RTS.pEnter "PdfValue.Ref" pRef
                 HS.pure (Value_ref _440)))
          ((RTS.<||)
             (RTS.pEnter "name"
                (do (_441 :: Vector.Vector (RTS.UInt 8)) <-
                      RTS.pEnter "PdfValue.Name" pName
                    HS.pure (Value_name _441)))
             ((RTS.<||)
                (RTS.pEnter "string"
                   (do (_442 :: Vector.Vector (RTS.UInt 8)) <-
                         RTS.pEnter "PdfValue.String" pString
                       HS.pure (Value_string _442)))
                ((RTS.<||)
                   (RTS.pEnter "string"
                      (do (_443 :: Vector.Vector (RTS.UInt 8)) <-
                            RTS.pEnter "PdfValue.HexString" pHexString
                          HS.pure (Value_string _443)))
                   ((RTS.<||)
                      (RTS.pEnter "number"
                         (do (_444 :: Number) <- RTS.pEnter "PdfValue.Number" pNumber
                             HS.pure (Value_number _444)))
                      ((RTS.<||)
                         (RTS.pEnter "array"
                            (do (_445 :: Vector.Vector Value) <-
                                  RTS.pEnter "PdfValue.Array" pArray
                                HS.pure (Value_array _445)))
                         (RTS.pEnter "dict"
                            (do (_446 :: Map.Map (Vector.Vector (RTS.UInt 8)) Value) <-
                                  RTS.pEnter "PdfValue.Dict" pDict
                                HS.pure (Value_dict _446))))))))))
 
pNatValue :: Value -> D.Parser HS.Integer
 
pNatValue (v :: Value) =
  do (n :: Number) <-
       case v of
         Value_number (_447 :: Number) -> HS.pure _447
         _ -> RTS.pError RTS.FromSystem "188:8--188:18"
                "Pattern match failure"
     (__ :: HS.Integer) <-
       RTS.pEnter "PdfValue.NumberAsNat" (pNumberAsNat n)
     HS.pure __
 
nullValue :: Value
 
nullValue = Value_null ()
 
_Between ::
  forall d.
    RTS.DDL d =>
      Vector.Vector (RTS.UInt 8)
        -> (Vector.Vector (RTS.UInt 8) -> (D.Parser () -> D.Parser ()))
 
_Between (open :: Vector.Vector (RTS.UInt 8))
  (close :: Vector.Vector (RTS.UInt 8))
  (_P :: D.Parser ()) =
  do RTS.pEnter "PdfValue._KW" (_KW open)
     _P
     RTS.pEnter "PdfValue._KW" (_KW close)
 
_Only :: forall b. RTS.DDL b => D.Parser () -> D.Parser ()
 
_Only (_P :: D.Parser ()) =
  do _P
     RTS.pEnd "26:39--26:41"
 
_When ::
  forall b f. (RTS.DDL b, RTS.DDL f) => D.Parser () -> D.Parser ()
 
_When (_P :: D.Parser ()) = _P
 
_Bool :: D.Parser ()
 
_Bool =
  (RTS.|||)
    (RTS.pEnter "PdfValue._When"
       (_When @HS.Bool @()
          (RTS.pEnter "PdfValue._KW" (_KW (Vector.vecFromRep "true")))))
    (RTS.pEnter "PdfValue._When"
       (_When @HS.Bool @()
          (RTS.pEnter "PdfValue._KW" (_KW (Vector.vecFromRep "false")))))
 
_Sign :: D.Parser ()
 
_Sign =
  (RTS.|||)
    (RTS.pEnter "pos"
       ((RTS.<||)
          (HS.const ()
             HS.<$> RTS.pMatch "51:20--51:28" (Vector.vecFromRep "+"))
          (HS.pure ())))
    (RTS.pEnter "neg"
       (HS.const ()
          HS.<$> RTS.pMatch "52:10--52:18" (Vector.vecFromRep "-")))
 
_Digit :: D.Parser ()
 
_Digit =
  do (_401 :: RTS.UInt 8) <-
       do (_400 :: RTS.UInt 8) <-
            RTS.uint8
              HS.<$> RTS.pMatch1 "77:17--77:34"
                       (RTS.bcRange (RTS.uint8 48) (RTS.uint8 57))
          HS.pure (RTS.sub _400 (RTS.uint8 48))
     RTS.pIsJust_ "77:17--77:48" "Value does not fit in target type"
       (RTS.convertMaybe _401 :: HS.Maybe HS.Integer)
 
_Natural :: D.Parser ()
 
_Natural =
  RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: RTS.UInt 64)
    (RTS.pEnter "PdfValue._Digit" _Digit)
 
_Number :: D.Parser ()
 
_Number =
  RTS.pEnter "PdfValue._Token"
    (_Token @Number
       (do (sign :: Sign) <- RTS.pEnter "PdfValue.Sign" pSign
           do HS.void (RTS.pEnter "PdfValue.UnsignedNumber" pUnsignedNumber)
              HS.pure ()
           (RTS.|||)
             (RTS.pEnter "PdfValue._When"
                (_When @Number @()
                   (case sign of
                      Sign_pos (_404 :: ()) -> HS.pure ()
                      _ -> RTS.pError RTS.FromSystem "46:11--46:21"
                             "Pattern match failure")))
             (RTS.pEnter "PdfValue._When"
                (_When @Number @()
                   (case sign of
                      Sign_neg (_405 :: ()) -> HS.pure ()
                      _ -> RTS.pError RTS.FromSystem "47:11--47:21"
                             "Pattern match failure")))))
 
_Frac :: RTS.UInt 64 -> D.Parser ()
 
_Frac (n :: RTS.UInt 64) =
  do HS.const ()
       HS.<$> RTS.pMatch "70:3--70:11" (Vector.vecFromRep ".")
     RTS.pSkipAtLeast (RTS.<||) n (RTS.pEnter "PdfValue._Digit" _Digit)
 
_UnsignedLeadDigits :: D.Parser ()
 
_UnsignedLeadDigits =
  do RTS.pEnter "PdfValue._Natural" _Natural
     (RTS.<||)
       (RTS.pEnter "PdfValue._Frac" (_Frac (RTS.lit 0 :: RTS.UInt 64)))
       (HS.pure ())
 
_UnsignedNumber :: D.Parser ()
 
_UnsignedNumber =
  (RTS.|||)
    (RTS.pEnter "PdfValue._UnsignedLeadDigits" _UnsignedLeadDigits)
    (RTS.pEnter "PdfValue._Frac" (_Frac (RTS.lit 1 :: RTS.UInt 64)))
 
_OctDigit :: D.Parser ()
 
_OctDigit =
  do (_409 :: RTS.UInt 8) <-
       do (_408 :: RTS.UInt 8) <-
            RTS.uint8
              HS.<$> RTS.pMatch1 "76:17--76:34"
                       (RTS.bcRange (RTS.uint8 48) (RTS.uint8 55))
          HS.pure (RTS.sub _408 (RTS.uint8 48))
     RTS.pIsJust_ "76:17--76:48" "Value does not fit in target type"
       (RTS.convertMaybe _409 :: HS.Maybe HS.Integer)
 
_HexDigit :: D.Parser ()
 
_HexDigit =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue._Digit" _Digit)
       (do (_412 :: RTS.UInt 8) <-
             do (_411 :: RTS.UInt 8) <-
                  do (_410 :: RTS.UInt 8) <-
                       RTS.uint8
                         HS.<$> RTS.pMatch1 "79:22--79:39"
                                  (RTS.bcRange (RTS.uint8 97) (RTS.uint8 102))
                     HS.pure (RTS.add (RTS.lit 10 :: RTS.UInt 8) _410)
                HS.pure (RTS.sub _411 (RTS.uint8 97))
           RTS.pIsJust_ "79:17--79:53" "Value does not fit in target type"
             (RTS.convertMaybe _412 :: HS.Maybe HS.Integer)))
    (do (_415 :: RTS.UInt 8) <-
          do (_414 :: RTS.UInt 8) <-
               do (_413 :: RTS.UInt 8) <-
                    RTS.uint8
                      HS.<$> RTS.pMatch1 "80:22--80:39"
                               (RTS.bcRange (RTS.uint8 65) (RTS.uint8 70))
                  HS.pure (RTS.add (RTS.lit 10 :: RTS.UInt 8) _413)
             HS.pure (RTS.sub _414 (RTS.uint8 65))
        RTS.pIsJust_ "80:17--80:53" "Value does not fit in target type"
          (RTS.convertMaybe _415 :: HS.Maybe HS.Integer))
 
_NumberAsNat :: Number -> D.Parser ()
 
_NumberAsNat (x :: Number) =
  RTS.pEnter "PdfValue._Guard"
    (_Guard
       (((RTS.lit 0 :: HS.Integer) HS.<= HS.getField @"num" x)
          HS.&& (HS.getField @"exp" x HS.== (RTS.lit 0 :: HS.Integer))))
 
_StringNumEsc :: D.Parser ()
 
_StringNumEsc =
  RTS.pSkipWithBounds "116:33--116:52" (RTS.<||)
    (RTS.lit 1 :: RTS.UInt 64)
    (RTS.lit 3 :: RTS.UInt 64)
    (RTS.pEnter "PdfValue._OctDigit" _OctDigit)
 
_StringEsc :: D.Parser ()
 
_StringEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "101:4--101:13" (Vector.vecFromRep "\\")
     (RTS.|||)
       (RTS.pEnter "PdfValue._When"
          (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
             (HS.const ()
                HS.<$> RTS.pMatch "103:12--103:20" (Vector.vecFromRep "n"))))
       ((RTS.|||)
          (RTS.pEnter "PdfValue._When"
             (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                (HS.const ()
                   HS.<$> RTS.pMatch "104:12--104:20" (Vector.vecFromRep "r"))))
          ((RTS.|||)
             (RTS.pEnter "PdfValue._When"
                (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                   (HS.const ()
                      HS.<$> RTS.pMatch "105:12--105:20" (Vector.vecFromRep "t"))))
             ((RTS.|||)
                (RTS.pEnter "PdfValue._When"
                   (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                      (HS.const ()
                         HS.<$> RTS.pMatch "106:12--106:20" (Vector.vecFromRep "b"))))
                ((RTS.|||)
                   (RTS.pEnter "PdfValue._When"
                      (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                         (HS.const ()
                            HS.<$> RTS.pMatch "107:12--107:20" (Vector.vecFromRep "f"))))
                   ((RTS.|||)
                      (RTS.pEnter "PdfValue._When"
                         (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                            (HS.const ()
                               HS.<$> RTS.pMatch "108:12--108:20" (Vector.vecFromRep "("))))
                      ((RTS.|||)
                         (RTS.pEnter "PdfValue._When"
                            (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                               (HS.const ()
                                  HS.<$> RTS.pMatch "109:12--109:20" (Vector.vecFromRep ")"))))
                         ((RTS.|||)
                            (RTS.pEnter "PdfValue._When"
                               (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                  (HS.const ()
                                     HS.<$> RTS.pMatch "110:12--110:21" (Vector.vecFromRep "\\"))))
                            ((RTS.|||)
                               (RTS.pEnter "PdfValue._When"
                                  (_When @(Vector.Vector (RTS.UInt 8)) @(RTS.UInt 8)
                                     (RTS.pEnter "PdfValue._EOL" _EOL)))
                               (RTS.pEnter "PdfValue._StringNumEsc" _StringNumEsc)))))))))
 
_StringChars :: D.Parser ()
 
_StringChars =
  RTS.pSkipMany (RTS.<||)
    (do HS.void (RTS.pEnter "PdfValue.StringChunk" pStringChunk)
        HS.pure ())
 
_StringInParens :: D.Parser ()
 
_StringInParens =
  do HS.const ()
       HS.<$> RTS.pMatch "98:31--98:39" (Vector.vecFromRep "(")
     RTS.pEnter "PdfValue._StringChars" _StringChars
     HS.const ()
       HS.<$> RTS.pMatch "98:55--98:63" (Vector.vecFromRep ")")
 
_StringChunk :: D.Parser ()
 
_StringChunk =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue._StringInParens" _StringInParens)
       (RTS.pEnter "PdfValue._StringEsc" _StringEsc))
    (RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: RTS.UInt 64)
       (HS.const ()
          HS.<$> RTS.pMatch1 "96:17--96:32"
                   (RTS.bcComplement (RTS.bcByteString "\\()"))))
 
_String :: D.Parser ()
 
_String =
  RTS.pEnter "PdfValue._Between"
    (_Between @(Vector.Vector (RTS.UInt 8)) (Vector.vecFromRep "(")
       (Vector.vecFromRep ")")
       (RTS.pEnter "PdfValue._StringChars" _StringChars))
 
_HexStringNum2 :: D.Parser ()
 
_HexStringNum2 =
  RTS.pSkipExact (RTS.lit 2 :: RTS.UInt 64)
    (RTS.pEnter "PdfValue._Token"
       (_Token @HS.Integer (RTS.pEnter "PdfValue._HexDigit" _HexDigit)))
 
_HexStringNum1 :: D.Parser ()
 
_HexStringNum1 =
  RTS.pEnter "PdfValue._Token"
    (_Token @HS.Integer (RTS.pEnter "PdfValue._HexDigit" _HexDigit))
 
_HexString :: D.Parser ()
 
_HexString =
  RTS.pEnter "PdfValue._Between"
    (_Between @(Vector.Vector (RTS.UInt 8)) (Vector.vecFromRep "<")
       (Vector.vecFromRep ">")
       (do RTS.pSkipMany (RTS.<||)
             (RTS.pEnter "PdfValue._HexStringNum2" _HexStringNum2)
           (RTS.|||) (RTS.pEnter "PdfValue._HexStringNum1" _HexStringNum1)
             (HS.pure ())))
 
_NameEsc :: D.Parser ()
 
_NameEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "141:3--141:11" (Vector.vecFromRep "#")
     (__ :: RTS.UInt 8) <-
       do (_435 :: HS.Integer) <-
            do (_434 :: Vector.Vector HS.Integer) <-
                 Vector.replicateM (RTS.lit 2 :: RTS.UInt 64)
                   (RTS.pEnter "PdfValue.HexDigit" pHexDigit)
               HS.pure (numBase (RTS.lit 16 :: HS.Integer) _434)
          HS.pure (RTS.convert _435 :: RTS.UInt 8)
     RTS.pEnter "PdfValue._Guard"
       (_Guard ((RTS.lit 0 :: RTS.UInt 8) HS.< __))
 
_NameChar :: D.Parser ()
 
_NameChar =
  (RTS.|||)
    (HS.const ()
       HS.<$> RTS.pMatch1 "137:16--137:53"
                (RTS.bcComplement (RTS.bcByteString "\NUL\t\n\f\r ()<>[]{}/%#")))
    (RTS.pEnter "PdfValue._NameEsc" _NameEsc)
 
_Name :: D.Parser ()
 
_Name =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (do HS.const ()
             HS.<$> RTS.pMatch "135:24--135:32" (Vector.vecFromRep "/")
           RTS.pSkipMany (RTS.<||)
             (RTS.pEnter "PdfValue._NameChar" _NameChar)))
 
_Null :: D.Parser ()
 
_Null = RTS.pEnter "PdfValue._KW" (_KW (Vector.vecFromRep "null"))
 
_Ref :: D.Parser ()
 
_Ref =
  do RTS.pEnter "PdfValue._Token"
       (_Token @HS.Integer (RTS.pEnter "PdfValue._Natural" _Natural))
     RTS.pEnter "PdfValue._Token"
       (_Token @HS.Integer (RTS.pEnter "PdfValue._Natural" _Natural))
     RTS.pEnter "PdfValue._KW" (_KW (Vector.vecFromRep "R"))
 
_Array :: D.Parser ()
 
_Array =
  RTS.pEnter "PdfValue._Between"
    (_Between @(Vector.Vector Value) (Vector.vecFromRep "[")
       (Vector.vecFromRep "]")
       (RTS.pSkipMany (RTS.<||)
          (do HS.void (RTS.pEnter "PdfValue.Value" pValue)
              HS.pure ())))
 
_Dict :: D.Parser ()
 
_Dict =
  do (ents :: Vector.Vector Dict_0) <-
       RTS.pEnter "PdfValue.Between"
         (pBetween @(Vector.Vector Dict_0) (Vector.vecFromRep "<<")
            (Vector.vecFromRep ">>")
            (RTS.pMany (RTS.<||)
               (do (key :: Vector.Vector (RTS.UInt 8)) <-
                     RTS.pEnter "PdfValue.Name" pName
                   (value :: Value) <- RTS.pEnter "PdfValue.Value" pValue
                   HS.pure (Dict_0 key value))))
     HS.void
       (RTS.loopFoldM
          (\(d :: Map.Map (Vector.Vector (RTS.UInt 8))
                    Value) (e :: Dict_0) ->
             RTS.pIsJust "159:31--159:52" "Key already present"
               (Map.insertMaybe (HS.getField @"key" e) (HS.getField @"value" e)
                  d))
          (Map.empty :: Map.Map (Vector.Vector (RTS.UInt 8)) Value)
          ents)
     HS.pure ()
 
_Value :: D.Parser ()
 
_Value =
  (RTS.<||) (RTS.pEnter "null" (RTS.pEnter "PdfValue._Null" _Null))
    ((RTS.<||) (RTS.pEnter "bool" (RTS.pEnter "PdfValue._Bool" _Bool))
       ((RTS.<||) (RTS.pEnter "ref" (RTS.pEnter "PdfValue._Ref" _Ref))
          ((RTS.<||) (RTS.pEnter "name" (RTS.pEnter "PdfValue._Name" _Name))
             ((RTS.<||)
                (RTS.pEnter "string" (RTS.pEnter "PdfValue._String" _String))
                ((RTS.<||)
                   (RTS.pEnter "string" (RTS.pEnter "PdfValue._HexString" _HexString))
                   ((RTS.<||)
                      (RTS.pEnter "number" (RTS.pEnter "PdfValue._Number" _Number))
                      ((RTS.<||)
                         (RTS.pEnter "array" (RTS.pEnter "PdfValue._Array" _Array))
                         (RTS.pEnter "dict" (RTS.pEnter "PdfValue._Dict" _Dict)))))))))
 
_NatValue :: Value -> D.Parser ()
 
_NatValue (v :: Value) =
  do (n :: Number) <-
       case v of
         Value_number (_447 :: Number) -> HS.pure _447
         _ -> RTS.pError RTS.FromSystem "188:8--188:18"
                "Pattern match failure"
     RTS.pEnter "PdfValue._NumberAsNat" (_NumberAsNat n)
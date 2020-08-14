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
 
cs_cr :: RTS.ClassVal
 
cs_cr = RTS.bcSingle (RTS.uint8 13)
 
cs_lf :: RTS.ClassVal
 
cs_lf = RTS.bcSingle (RTS.uint8 10)
 
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
 
_Token :: forall b. RTS.DDL b => D.Parser () -> D.Parser ()
 
_Token (_P :: D.Parser ()) =
  do _P
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._AnyWS" _AnyWS)
 
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
 
pKW :: Vector.Vector (RTS.UInt 8) -> D.Parser ()
 
pKW (x :: Vector.Vector (RTS.UInt 8)) =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (HS.const () HS.<$> RTS.pMatch "23:39--23:45" x))
 
pWhen ::
  forall b f.
    (RTS.DDL b, RTS.DDL f) => D.Parser f -> (b -> D.Parser b)
 
pWhen (pP :: D.Parser f) (x :: b) =
  do do HS.void pP
        HS.pure ()
     (__ :: b) <- HS.pure x
     HS.pure __
 
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
 
pDigit :: D.Parser HS.Integer
 
pDigit =
  do (_1 :: RTS.UInt 8) <-
       do (_0 :: RTS.UInt 8) <-
            RTS.uint8
              HS.<$> RTS.pMatch1 "77:17--77:34"
                       (RTS.bcRange (RTS.uint8 48) (RTS.uint8 57))
          HS.pure (RTS.sub _0 (RTS.uint8 48))
     RTS.pIsJust "77:17--77:48" "Value does not fit in target type"
       (RTS.convertMaybe _1 :: HS.Maybe HS.Integer)
 
pHexDigit :: D.Parser HS.Integer
 
pHexDigit =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue.Digit" pDigit)
       (do (_4 :: RTS.UInt 8) <-
             do (_3 :: RTS.UInt 8) <-
                  do (_2 :: RTS.UInt 8) <-
                       RTS.uint8
                         HS.<$> RTS.pMatch1 "79:22--79:39"
                                  (RTS.bcRange (RTS.uint8 97) (RTS.uint8 102))
                     HS.pure (RTS.add (RTS.lit 10 :: RTS.UInt 8) _2)
                HS.pure (RTS.sub _3 (RTS.uint8 97))
           RTS.pIsJust "79:17--79:53" "Value does not fit in target type"
             (RTS.convertMaybe _4 :: HS.Maybe HS.Integer)))
    (do (_7 :: RTS.UInt 8) <-
          do (_6 :: RTS.UInt 8) <-
               do (_5 :: RTS.UInt 8) <-
                    RTS.uint8
                      HS.<$> RTS.pMatch1 "80:22--80:39"
                               (RTS.bcRange (RTS.uint8 65) (RTS.uint8 70))
                  HS.pure (RTS.add (RTS.lit 10 :: RTS.UInt 8) _5)
             HS.pure (RTS.sub _6 (RTS.uint8 65))
        RTS.pIsJust "80:17--80:53" "Value does not fit in target type"
          (RTS.convertMaybe _7 :: HS.Maybe HS.Integer))
 
_Guard :: HS.Bool -> D.Parser ()
 
_Guard (p :: HS.Bool) = RTS.pGuard "28:29--28:37" "guard failed" p
 
numBase ::
  forall b d e.
    (RTS.DDL b, RTS.DDL d, RTS.DDL e, RTS.Literal 0 e, RTS.IsLoop b,
     RTS.ColElType b d, RTS.ColElType b e, RTS.Numeric e) =>
      e -> (b -> e)
 
numBase (base :: e) (ds :: b) =
  RTS.loopFold (\(val :: e) (d :: e) -> RTS.add (RTS.mul val base) d)
    (RTS.lit 0 :: e)
    ds
 
pNameEsc :: D.Parser (RTS.UInt 8)
 
pNameEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "141:3--141:11" (Vector.vecFromRep "#")
     (__ :: RTS.UInt 8) <-
       do (_9 :: HS.Integer) <-
            do (_8 :: Vector.Vector HS.Integer) <-
                 Vector.replicateM (RTS.lit 2 :: HS.Integer)
                   (RTS.pEnter "PdfValue.HexDigit" pHexDigit)
               HS.pure
                 (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
                    (RTS.lit 16 :: HS.Integer)
                    _8)
          HS.pure (RTS.convert _9 :: RTS.UInt 8)
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
 
pToken :: forall b. RTS.DDL b => D.Parser b -> D.Parser b
 
pToken (pP :: D.Parser b) =
  do (__ :: b) <- pP
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._AnyWS" _AnyWS)
     HS.pure __
 
pName :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pName =
  RTS.pEnter "PdfValue.Token"
    (pToken @(Vector.Vector (RTS.UInt 8))
       (do HS.const ()
             HS.<$> RTS.pMatch "135:24--135:32" (Vector.vecFromRep "/")
           (__ :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.NameChar" pNameChar)
           HS.pure __))
 
pHexStringNum1 :: D.Parser (RTS.UInt 8)
 
pHexStringNum1 =
  do (_11 :: HS.Integer) <-
       do (_10 :: HS.Integer) <-
            RTS.pEnter "PdfValue.Token"
              (pToken @HS.Integer (RTS.pEnter "PdfValue.HexDigit" pHexDigit))
          HS.pure (RTS.mul (RTS.lit 16 :: HS.Integer) _10)
     HS.pure (RTS.convert _11 :: RTS.UInt 8)
 
pHexStringNum2 :: D.Parser (RTS.UInt 8)
 
pHexStringNum2 =
  do (_13 :: HS.Integer) <-
       do (_12 :: Vector.Vector HS.Integer) <-
            Vector.replicateM (RTS.lit 2 :: HS.Integer)
              (RTS.pEnter "PdfValue.Token"
                 (pToken @HS.Integer (RTS.pEnter "PdfValue.HexDigit" pHexDigit)))
          HS.pure
            (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
               (RTS.lit 16 :: HS.Integer)
               _12)
     HS.pure (RTS.convert _13 :: RTS.UInt 8)
 
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
               (do (_16 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
                     do (_15 :: Vector.Vector (RTS.UInt 8)) <-
                          do (_14 :: RTS.UInt 8) <-
                               RTS.pEnter "PdfValue.HexStringNum1" pHexStringNum1
                             HS.pure (Vector.fromList [_14])
                        HS.pure (Vector.fromList [front, _15])
                   HS.pure (Vector.concat _16))
               (HS.pure front)
           HS.pure __))
 
pNull :: D.Parser ()
 
pNull = RTS.pEnter "PdfValue.KW" (pKW (Vector.vecFromRep "null"))
 
pSign :: D.Parser Sign
 
pSign =
  (RTS.|||)
    (RTS.pEnter "pos"
       (do (_17 :: ()) <-
             (RTS.<||)
               (HS.const ()
                  HS.<$> RTS.pMatch "51:20--51:28" (Vector.vecFromRep "+"))
               (HS.pure ())
           HS.pure (Sign_pos _17)))
    (RTS.pEnter "neg"
       (do (_18 :: ()) <-
             HS.const ()
               HS.<$> RTS.pMatch "52:10--52:18" (Vector.vecFromRep "-")
           HS.pure (Sign_neg _18)))
 
pFrac :: HS.Integer -> (Number -> D.Parser Number)
 
pFrac (n :: HS.Integer) (w :: Number) =
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
 
pNatural :: D.Parser HS.Integer
 
pNatural =
  do (_19 :: Vector.Vector HS.Integer) <-
       RTS.pMinLength "67:27--67:42" (RTS.lit 1 :: HS.Integer)
         (RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.Digit" pDigit))
     HS.pure
       (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
          (RTS.lit 10 :: HS.Integer)
          _19)
 
pUnsignedLeadDigits :: D.Parser Number
 
pUnsignedLeadDigits =
  do (n :: HS.Integer) <- RTS.pEnter "PdfValue.Natural" pNatural
     (val :: Number) <- HS.pure (Number n (RTS.lit 0 :: HS.Integer))
     (__ :: Number) <-
       (RTS.<||)
         (RTS.pEnter "PdfValue.Frac" (pFrac (RTS.lit 0 :: HS.Integer) val))
         (HS.pure val)
     HS.pure __
 
pUnsignedNumber :: D.Parser Number
 
pUnsignedNumber =
  (RTS.|||)
    (RTS.pEnter "PdfValue.UnsignedLeadDigits" pUnsignedLeadDigits)
    (RTS.pEnter "PdfValue.Frac"
       (pFrac (RTS.lit 1 :: HS.Integer)
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
                     (RTS.pIsJust "46:11--46:21" "Expected `pos`"
                        (HS.getField @"pos" sign))
                     n))
               (RTS.pEnter "PdfValue.When"
                  (pWhen @Number @()
                     (RTS.pIsJust "47:11--47:21" "Expected `neg`"
                        (HS.getField @"neg" sign))
                     (Number (RTS.sub (RTS.lit 0 :: HS.Integer) (HS.getField @"num" n))
                        (HS.getField @"exp" n))))
           HS.pure __))
 
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
 
pOctDigit :: D.Parser HS.Integer
 
pOctDigit =
  do (_21 :: RTS.UInt 8) <-
       do (_20 :: RTS.UInt 8) <-
            RTS.uint8
              HS.<$> RTS.pMatch1 "76:17--76:34"
                       (RTS.bcRange (RTS.uint8 48) (RTS.uint8 55))
          HS.pure (RTS.sub _20 (RTS.uint8 48))
     RTS.pIsJust "76:17--76:48" "Value does not fit in target type"
       (RTS.convertMaybe _21 :: HS.Maybe HS.Integer)
 
pStringNumEsc :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringNumEsc =
  do (_24 :: RTS.UInt 8) <-
       do (_23 :: HS.Integer) <-
            do (_22 :: Vector.Vector HS.Integer) <-
                 RTS.pMinLength "116:33--116:52" (RTS.lit 1 :: HS.Integer)
                   (RTS.pManyUpTo (RTS.<||) (RTS.lit 3 :: HS.Integer)
                      (RTS.pEnter "PdfValue.OctDigit" pOctDigit))
               HS.pure
                 (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
                    (RTS.lit 8 :: HS.Integer)
                    _22)
          HS.pure (RTS.convert _23 :: RTS.UInt 8)
     HS.pure (Vector.fromList [_24])
 
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
  do (_25 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
       RTS.pMany (RTS.<||)
         (RTS.pEnter "PdfValue.StringChunk" pStringChunk)
     HS.pure (Vector.concat _25)
 
pStringInParens :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringInParens =
  do (_29 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
       do (_26 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pMatch "98:31--98:39" (Vector.vecFromRep "(")
          (_27 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pEnter "PdfValue.StringChars" pStringChars
          (_28 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pMatch "98:55--98:63" (Vector.vecFromRep ")")
          HS.pure (Vector.fromList [_26, _27, _28])
     HS.pure (Vector.concat _29)
 
pStringChunk :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringChunk =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue.StringInParens" pStringInParens)
       (RTS.pEnter "PdfValue.StringEsc" pStringEsc))
    (RTS.pMinLength "96:5--96:32" (RTS.lit 1 :: HS.Integer)
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
       (do (_30 :: ()) <- RTS.pEnter "PdfValue.Null" pNull
           HS.pure (Value_null _30)))
    ((RTS.<||)
       (RTS.pEnter "bool"
          (do (_31 :: HS.Bool) <- RTS.pEnter "PdfValue.Bool" pBool
              HS.pure (Value_bool _31)))
       ((RTS.<||)
          (RTS.pEnter "ref"
             (do (_32 :: Ref) <- RTS.pEnter "PdfValue.Ref" pRef
                 HS.pure (Value_ref _32)))
          ((RTS.<||)
             (RTS.pEnter "name"
                (do (_33 :: Vector.Vector (RTS.UInt 8)) <-
                      RTS.pEnter "PdfValue.Name" pName
                    HS.pure (Value_name _33)))
             ((RTS.<||)
                (RTS.pEnter "string"
                   (do (_34 :: Vector.Vector (RTS.UInt 8)) <-
                         RTS.pEnter "PdfValue.String" pString
                       HS.pure (Value_string _34)))
                ((RTS.<||)
                   (RTS.pEnter "string"
                      (do (_35 :: Vector.Vector (RTS.UInt 8)) <-
                            RTS.pEnter "PdfValue.HexString" pHexString
                          HS.pure (Value_string _35)))
                   ((RTS.<||)
                      (RTS.pEnter "number"
                         (do (_36 :: Number) <- RTS.pEnter "PdfValue.Number" pNumber
                             HS.pure (Value_number _36)))
                      ((RTS.<||)
                         (RTS.pEnter "array"
                            (do (_37 :: Vector.Vector Value) <-
                                  RTS.pEnter "PdfValue.Array" pArray
                                HS.pure (Value_array _37)))
                         (RTS.pEnter "dict"
                            (do (_38 :: Map.Map (Vector.Vector (RTS.UInt 8)) Value) <-
                                  RTS.pEnter "PdfValue.Dict" pDict
                                HS.pure (Value_dict _38))))))))))
 
pGuard :: HS.Bool -> D.Parser ()
 
pGuard (p :: HS.Bool) = RTS.pGuard "28:29--28:37" "guard failed" p
 
pNumberAsNat :: Number -> D.Parser HS.Integer
 
pNumberAsNat (x :: Number) =
  do RTS.pEnter "PdfValue._Guard"
       (_Guard
          (((RTS.lit 0 :: HS.Integer) HS.<= HS.getField @"num" x)
             HS.&& (HS.getField @"exp" x HS.== (RTS.lit 0 :: HS.Integer))))
     (__ :: HS.Integer) <- HS.pure (HS.getField @"num" x)
     HS.pure __
 
pNatValue :: Value -> D.Parser HS.Integer
 
pNatValue (v :: Value) =
  do (n :: Number) <-
       RTS.pIsJust "188:8--188:18" "Expected `number`"
         (HS.getField @"number" v)
     (__ :: HS.Integer) <-
       RTS.pEnter "PdfValue.NumberAsNat" (pNumberAsNat n)
     HS.pure __
 
pOnly :: forall b. RTS.DDL b => D.Parser b -> D.Parser b
 
pOnly (pP :: D.Parser b) =
  do (__ :: b) <- pP
     RTS.pEnd "26:39--26:41"
     HS.pure __
 
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
 
_Array :: D.Parser ()
 
_Array =
  RTS.pEnter "PdfValue._Between"
    (_Between @(Vector.Vector Value) (Vector.vecFromRep "[")
       (Vector.vecFromRep "]")
       (RTS.pSkipMany (RTS.<||)
          (do HS.void (RTS.pEnter "PdfValue.Value" pValue)
              HS.pure ())))
 
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
 
_Digit :: D.Parser ()
 
_Digit =
  do (_1 :: RTS.UInt 8) <-
       do (_0 :: RTS.UInt 8) <-
            RTS.uint8
              HS.<$> RTS.pMatch1 "77:17--77:34"
                       (RTS.bcRange (RTS.uint8 48) (RTS.uint8 57))
          HS.pure (RTS.sub _0 (RTS.uint8 48))
     RTS.pIsJust_ "77:17--77:48" "Value does not fit in target type"
       (RTS.convertMaybe _1 :: HS.Maybe HS.Integer)
 
_Frac :: HS.Integer -> D.Parser ()
 
_Frac (n :: HS.Integer) =
  do HS.const ()
       HS.<$> RTS.pMatch "70:3--70:11" (Vector.vecFromRep ".")
     RTS.pSkipAtLeast (RTS.<||) n (RTS.pEnter "PdfValue._Digit" _Digit)
 
_HexDigit :: D.Parser ()
 
_HexDigit =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue._Digit" _Digit)
       (do (_4 :: RTS.UInt 8) <-
             do (_3 :: RTS.UInt 8) <-
                  do (_2 :: RTS.UInt 8) <-
                       RTS.uint8
                         HS.<$> RTS.pMatch1 "79:22--79:39"
                                  (RTS.bcRange (RTS.uint8 97) (RTS.uint8 102))
                     HS.pure (RTS.add (RTS.lit 10 :: RTS.UInt 8) _2)
                HS.pure (RTS.sub _3 (RTS.uint8 97))
           RTS.pIsJust_ "79:17--79:53" "Value does not fit in target type"
             (RTS.convertMaybe _4 :: HS.Maybe HS.Integer)))
    (do (_7 :: RTS.UInt 8) <-
          do (_6 :: RTS.UInt 8) <-
               do (_5 :: RTS.UInt 8) <-
                    RTS.uint8
                      HS.<$> RTS.pMatch1 "80:22--80:39"
                               (RTS.bcRange (RTS.uint8 65) (RTS.uint8 70))
                  HS.pure (RTS.add (RTS.lit 10 :: RTS.UInt 8) _5)
             HS.pure (RTS.sub _6 (RTS.uint8 65))
        RTS.pIsJust_ "80:17--80:53" "Value does not fit in target type"
          (RTS.convertMaybe _7 :: HS.Maybe HS.Integer))
 
_HexStringNum1 :: D.Parser ()
 
_HexStringNum1 =
  RTS.pEnter "PdfValue._Token"
    (_Token @HS.Integer (RTS.pEnter "PdfValue._HexDigit" _HexDigit))
 
_HexStringNum2 :: D.Parser ()
 
_HexStringNum2 =
  RTS.pSkipExact (RTS.lit 2 :: HS.Integer)
    (RTS.pEnter "PdfValue._Token"
       (_Token @HS.Integer (RTS.pEnter "PdfValue._HexDigit" _HexDigit)))
 
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
       do (_9 :: HS.Integer) <-
            do (_8 :: Vector.Vector HS.Integer) <-
                 Vector.replicateM (RTS.lit 2 :: HS.Integer)
                   (RTS.pEnter "PdfValue.HexDigit" pHexDigit)
               HS.pure
                 (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
                    (RTS.lit 16 :: HS.Integer)
                    _8)
          HS.pure (RTS.convert _9 :: RTS.UInt 8)
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
 
_NumberAsNat :: Number -> D.Parser ()
 
_NumberAsNat (x :: Number) =
  RTS.pEnter "PdfValue._Guard"
    (_Guard
       (((RTS.lit 0 :: HS.Integer) HS.<= HS.getField @"num" x)
          HS.&& (HS.getField @"exp" x HS.== (RTS.lit 0 :: HS.Integer))))
 
_NatValue :: Value -> D.Parser ()
 
_NatValue (v :: Value) =
  do (n :: Number) <-
       RTS.pIsJust "188:8--188:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pEnter "PdfValue._NumberAsNat" (_NumberAsNat n)
 
_Natural :: D.Parser ()
 
_Natural =
  RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: HS.Integer)
    (RTS.pEnter "PdfValue._Digit" _Digit)
 
_Null :: D.Parser ()
 
_Null = RTS.pEnter "PdfValue._KW" (_KW (Vector.vecFromRep "null"))
 
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
                   (RTS.pIsJust_ "46:11--46:21" "Expected `pos`"
                      (HS.getField @"pos" sign))))
             (RTS.pEnter "PdfValue._When"
                (_When @Number @()
                   (RTS.pIsJust_ "47:11--47:21" "Expected `neg`"
                      (HS.getField @"neg" sign))))))
 
_OctDigit :: D.Parser ()
 
_OctDigit =
  do (_21 :: RTS.UInt 8) <-
       do (_20 :: RTS.UInt 8) <-
            RTS.uint8
              HS.<$> RTS.pMatch1 "76:17--76:34"
                       (RTS.bcRange (RTS.uint8 48) (RTS.uint8 55))
          HS.pure (RTS.sub _20 (RTS.uint8 48))
     RTS.pIsJust_ "76:17--76:48" "Value does not fit in target type"
       (RTS.convertMaybe _21 :: HS.Maybe HS.Integer)
 
_Only :: forall b. RTS.DDL b => D.Parser () -> D.Parser ()
 
_Only (_P :: D.Parser ()) =
  do _P
     RTS.pEnd "26:39--26:41"
 
_Ref :: D.Parser ()
 
_Ref =
  do RTS.pEnter "PdfValue._Token"
       (_Token @HS.Integer (RTS.pEnter "PdfValue._Natural" _Natural))
     RTS.pEnter "PdfValue._Token"
       (_Token @HS.Integer (RTS.pEnter "PdfValue._Natural" _Natural))
     RTS.pEnter "PdfValue._KW" (_KW (Vector.vecFromRep "R"))
 
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
 
_StringChars :: D.Parser ()
 
_StringChars =
  RTS.pSkipMany (RTS.<||)
    (do HS.void (RTS.pEnter "PdfValue.StringChunk" pStringChunk)
        HS.pure ())
 
_String :: D.Parser ()
 
_String =
  RTS.pEnter "PdfValue._Between"
    (_Between @(Vector.Vector (RTS.UInt 8)) (Vector.vecFromRep "(")
       (Vector.vecFromRep ")")
       (RTS.pEnter "PdfValue._StringChars" _StringChars))
 
_StringNumEsc :: D.Parser ()
 
_StringNumEsc =
  RTS.pSkipWithBounds "116:33--116:52" (RTS.<||)
    (RTS.lit 1 :: HS.Integer)
    (RTS.lit 3 :: HS.Integer)
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
    (RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: HS.Integer)
       (HS.const ()
          HS.<$> RTS.pMatch1 "96:17--96:32"
                   (RTS.bcComplement (RTS.bcByteString "\\()"))))
 
_UnsignedLeadDigits :: D.Parser ()
 
_UnsignedLeadDigits =
  do (n :: HS.Integer) <- RTS.pEnter "PdfValue.Natural" pNatural
     (val :: Number) <- HS.pure (Number n (RTS.lit 0 :: HS.Integer))
     (RTS.<||)
       (do HS.void
             (RTS.pEnter "PdfValue.Frac" (pFrac (RTS.lit 0 :: HS.Integer) val))
           HS.pure ())
       (HS.pure ())
 
_UnsignedNumber :: D.Parser ()
 
_UnsignedNumber =
  (RTS.|||)
    (RTS.pEnter "PdfValue._UnsignedLeadDigits" _UnsignedLeadDigits)
    (RTS.pEnter "PdfValue._Frac" (_Frac (RTS.lit 1 :: HS.Integer)))
 
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
 
nullValue :: Value
 
nullValue = Value_null ()
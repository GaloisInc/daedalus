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
       HS.<$> RTS.pMatch "14:31--14:33" (Vector.vecFromRep "%")
     RTS.pSkipMany (RTS.<||)
       (HS.const ()
          HS.<$> RTS.pMatch1 "14:42--14:53"
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
       HS.<$> RTS.pMatch "14:31--14:33" (Vector.vecFromRep "%")
     RTS.pSkipMany (RTS.<||)
       (HS.const ()
          HS.<$> RTS.pMatch1 "14:42--14:53"
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
 
_KW :: forall b. RTS.DDL b => D.Parser () -> D.Parser ()
 
_KW (_P :: D.Parser ()) =
  RTS.pEnter "PdfValue._Token" (_Token @b _P)
 
pBetween ::
  forall e.
    RTS.DDL e =>
      Vector.Vector (RTS.UInt 8)
        -> (Vector.Vector (RTS.UInt 8) -> (D.Parser e -> D.Parser e))
 
pBetween (open :: Vector.Vector (RTS.UInt 8))
  (close :: Vector.Vector (RTS.UInt 8))
  (pP :: D.Parser e) =
  do RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const () HS.<$> RTS.pMatch "24:34--24:37" open))
     (__ :: e) <- pP
     RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const () HS.<$> RTS.pMatch "24:51--24:55" close))
     HS.pure __
 
pKW :: forall b. RTS.DDL b => D.Parser b -> D.Parser ()
 
pKW (pP :: D.Parser b) =
  RTS.pEnter "PdfValue._Token"
    (_Token @b
       (do HS.void pP
           HS.pure ()))
 
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
          (RTS.pEnter "PdfValue.KW"
             (pKW @(Vector.Vector (RTS.UInt 8))
                (RTS.pMatch "35:14--35:19" (Vector.vecFromRep "true"))))
          HS.True))
    (RTS.pEnter "PdfValue.When"
       (pWhen @HS.Bool @()
          (RTS.pEnter "PdfValue.KW"
             (pKW @(Vector.Vector (RTS.UInt 8))
                (RTS.pMatch "36:14--36:20" (Vector.vecFromRep "false"))))
          HS.False))
 
pDigit :: D.Parser HS.Integer
 
pDigit =
  do (d :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "77:24--77:33"
                  (RTS.bcRange (RTS.uint8 48) (RTS.uint8 57))
     (__ :: HS.Integer) <-
       HS.pure (RTS.convert (RTS.sub d (RTS.uint8 48)) :: HS.Integer)
     HS.pure __
 
pHexDigit :: D.Parser HS.Integer
 
pHexDigit =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue.Digit" pDigit)
       (do (d :: RTS.UInt 8) <-
             RTS.uint8
               HS.<$> RTS.pMatch1 "80:24--80:33"
                        (RTS.bcRange (RTS.uint8 97) (RTS.uint8 102))
           (__ :: HS.Integer) <-
             HS.pure
               (RTS.add (RTS.lit 10 :: HS.Integer)
                  (RTS.convert (RTS.sub d (RTS.uint8 97)) :: HS.Integer))
           HS.pure __))
    (do (d :: RTS.UInt 8) <-
          RTS.uint8
            HS.<$> RTS.pMatch1 "81:24--81:33"
                     (RTS.bcRange (RTS.uint8 65) (RTS.uint8 70))
        (__ :: HS.Integer) <-
          HS.pure
            (RTS.add (RTS.lit 10 :: HS.Integer)
               (RTS.convert (RTS.sub d (RTS.uint8 65)) :: HS.Integer))
        HS.pure __)
 
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
       HS.<$> RTS.pMatch "153:3--153:5" (Vector.vecFromRep "#")
     (ds :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 2 :: HS.Integer)
         (RTS.pEnter "PdfValue.HexDigit" pHexDigit)
     (__ :: RTS.UInt 8) <-
       HS.pure
         (RTS.convert
            (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
               (RTS.lit 16 :: HS.Integer)
               ds)
            :: RTS.UInt 8)
     RTS.pGuard "156:3--156:8" "guard failed"
       ((RTS.lit 0 :: RTS.UInt 8) HS.< __)
     HS.pure __
 
pNameChar :: D.Parser (RTS.UInt 8)
 
pNameChar =
  (RTS.|||)
    (RTS.uint8
       HS.<$> RTS.pMatch1 "149:16--149:45"
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
             HS.<$> RTS.pMatch "147:24--147:26" (Vector.vecFromRep "/")
           (__ :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.NameChar" pNameChar)
           HS.pure __))
 
pHexStringNum1 :: D.Parser (RTS.UInt 8)
 
pHexStringNum1 =
  do (d :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (pToken @HS.Integer (RTS.pEnter "PdfValue.HexDigit" pHexDigit))
     (__ :: RTS.UInt 8) <-
       HS.pure
         (RTS.convert (RTS.mul (RTS.lit 16 :: HS.Integer) d) :: RTS.UInt 8)
     HS.pure __
 
pHexStringNum2 :: D.Parser (RTS.UInt 8)
 
pHexStringNum2 =
  do (ds :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 2 :: HS.Integer)
         (RTS.pEnter "PdfValue.Token"
            (pToken @HS.Integer (RTS.pEnter "PdfValue.HexDigit" pHexDigit)))
     (__ :: RTS.UInt 8) <-
       HS.pure
         (RTS.convert
            (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
               (RTS.lit 16 :: HS.Integer)
               ds)
            :: RTS.UInt 8)
     HS.pure __
 
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
               (do (_3 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
                     do (_0 :: Vector.Vector (RTS.UInt 8)) <- HS.pure front
                        (_2 :: Vector.Vector (RTS.UInt 8)) <-
                          do (_1 :: RTS.UInt 8) <-
                               RTS.pEnter "PdfValue.HexStringNum1" pHexStringNum1
                             HS.pure (Vector.fromList [_1])
                        HS.pure (Vector.fromList [_0, _2])
                   HS.pure (Vector.concat _3))
               (HS.pure front)
           HS.pure __))
 
pNull :: D.Parser ()
 
pNull =
  RTS.pEnter "PdfValue.KW"
    (pKW @(Vector.Vector (RTS.UInt 8))
       (RTS.pMatch "176:16--176:21" (Vector.vecFromRep "null")))
 
pSign :: D.Parser Sign
 
pSign =
  (RTS.|||)
    (RTS.pEnter "pos"
       (do (_4 :: ()) <-
             (RTS.|||)
               (HS.const ()
                  HS.<$> RTS.pMatch "50:11--50:13" (Vector.vecFromRep "+"))
               (HS.const ()
                  HS.<$> RTS.pMatch "50:17--50:18" (Vector.vecFromRep ""))
           HS.pure (Sign_pos _4)))
    (RTS.pEnter "neg"
       (do (_5 :: ()) <-
             HS.const ()
               HS.<$> RTS.pMatch "51:10--51:12" (Vector.vecFromRep "-")
           HS.pure (Sign_neg _5)))
 
pFrac :: HS.Integer -> (Number -> D.Parser Number)
 
pFrac (n :: HS.Integer) (w :: Number) =
  do (ds :: Vector.Vector HS.Integer) <-
       do HS.const ()
            HS.<$> RTS.pMatch "72:11--72:13" (Vector.vecFromRep ".")
          (__ :: Vector.Vector HS.Integer) <-
            RTS.pMinLength "72:16--72:32" n
              (RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.Digit" pDigit))
          HS.pure __
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
  do (ds :: Vector.Vector HS.Integer) <-
       RTS.pMinLength "67:9--67:24" (RTS.lit 1 :: HS.Integer)
         (RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.Digit" pDigit))
     (__ :: HS.Integer) <-
       HS.pure
         (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
            (RTS.lit 10 :: HS.Integer)
            ds)
     HS.pure __
 
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
                     (RTS.pIsJust "45:11--45:21" "Expected `pos`"
                        (HS.getField @"pos" sign))
                     n))
               (RTS.pEnter "PdfValue.When"
                  (pWhen @Number @()
                     (RTS.pIsJust "46:11--46:21" "Expected `neg`"
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
     RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const ()
             HS.<$> RTS.pMatch "181:6--181:8" (Vector.vecFromRep "R")))
     HS.pure (Ref obj gen)
 
pOctDigit :: D.Parser HS.Integer
 
pOctDigit =
  do (d :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "78:24--78:33"
                  (RTS.bcRange (RTS.uint8 48) (RTS.uint8 55))
     (__ :: HS.Integer) <-
       HS.pure (RTS.convert (RTS.sub d (RTS.uint8 48)) :: HS.Integer)
     HS.pure __
 
pStringNumEsc :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringNumEsc =
  do (ds :: Vector.Vector HS.Integer) <-
       RTS.pMinLength "118:9--118:28" (RTS.lit 1 :: HS.Integer)
         (RTS.pManyUpTo (RTS.<||) (RTS.lit 3 :: HS.Integer)
            (RTS.pEnter "PdfValue.OctDigit" pOctDigit))
     (__ :: Vector.Vector (RTS.UInt 8)) <-
       HS.pure
         (Vector.fromList
            [RTS.convert
               (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
                  (RTS.lit 8 :: HS.Integer)
                  ds)
               :: RTS.UInt 8])
     HS.pure __
 
pStringEsc :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "102:4--102:7" (Vector.vecFromRep "\\")
     (__ :: Vector.Vector (RTS.UInt 8)) <-
       (RTS.|||)
         (RTS.pEnter "PdfValue.When"
            (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
               (RTS.pMatch "104:11--104:13" (Vector.vecFromRep "n"))
               (Vector.vecFromRep "\n")))
         ((RTS.|||)
            (RTS.pEnter "PdfValue.When"
               (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                  (RTS.pMatch "105:11--105:13" (Vector.vecFromRep "r"))
                  (Vector.vecFromRep "\r")))
            ((RTS.|||)
               (RTS.pEnter "PdfValue.When"
                  (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                     (RTS.pMatch "106:11--106:13" (Vector.vecFromRep "t"))
                     (Vector.vecFromRep "\t")))
               ((RTS.|||)
                  (RTS.pEnter "PdfValue.When"
                     (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                        (RTS.pMatch "107:11--107:13" (Vector.vecFromRep "b"))
                        (Vector.vecFromRep "\b")))
                  ((RTS.|||)
                     (RTS.pEnter "PdfValue.When"
                        (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                           (RTS.pMatch "108:11--108:13" (Vector.vecFromRep "f"))
                           (Vector.vecFromRep "\f")))
                     ((RTS.|||)
                        (RTS.pEnter "PdfValue.When"
                           (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                              (RTS.pMatch "109:11--109:13" (Vector.vecFromRep "("))
                              (Vector.vecFromRep "(")))
                        ((RTS.|||)
                           (RTS.pEnter "PdfValue.When"
                              (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                 (RTS.pMatch "110:11--110:13" (Vector.vecFromRep ")"))
                                 (Vector.vecFromRep ")")))
                           ((RTS.|||)
                              (RTS.pEnter "PdfValue.When"
                                 (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                    (RTS.pMatch "111:11--111:14" (Vector.vecFromRep "\\"))
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
  do (_6 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
       RTS.pMany (RTS.<||)
         (RTS.pEnter "PdfValue.StringChunk" pStringChunk)
     HS.pure (Vector.concat _6)
 
pStringInParens :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringInParens =
  do (_10 :: Vector.Vector (Vector.Vector (RTS.UInt 8))) <-
       do (_7 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pMatch "99:31--99:33" (Vector.vecFromRep "(")
          (_8 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pEnter "PdfValue.StringChars" pStringChars
          (_9 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pMatch "99:49--99:51" (Vector.vecFromRep ")")
          HS.pure (Vector.fromList [_7, _8, _9])
     HS.pure (Vector.concat _10)
 
pStringChunk :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringChunk =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue.StringInParens" pStringInParens)
       (RTS.pEnter "PdfValue.StringEsc" pStringEsc))
    (RTS.pMinLength "97:5--97:24" (RTS.lit 1 :: HS.Integer)
       (RTS.pMany (RTS.<||)
          (RTS.uint8
             HS.<$> RTS.pMatch1 "97:17--97:24"
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
            RTS.pIsJust "172:31--172:52" "Key already present"
              (Map.insertMaybe (HS.getField @"key" e) (HS.getField @"value" e)
                 d))
         (Map.empty :: Map.Map (Vector.Vector (RTS.UInt 8)) Value)
         ents
     HS.pure __
 
pValue :: D.Parser Value
 
pValue =
  (RTS.<||)
    (RTS.pEnter "null"
       (do (_11 :: ()) <- RTS.pEnter "PdfValue.Null" pNull
           HS.pure (Value_null _11)))
    ((RTS.<||)
       (RTS.pEnter "bool"
          (do (_12 :: HS.Bool) <- RTS.pEnter "PdfValue.Bool" pBool
              HS.pure (Value_bool _12)))
       ((RTS.<||)
          (RTS.pEnter "ref"
             (do (_13 :: Ref) <- RTS.pEnter "PdfValue.Ref" pRef
                 HS.pure (Value_ref _13)))
          ((RTS.<||)
             (RTS.pEnter "name"
                (do (_14 :: Vector.Vector (RTS.UInt 8)) <-
                      RTS.pEnter "PdfValue.Name" pName
                    HS.pure (Value_name _14)))
             ((RTS.<||)
                (RTS.pEnter "string"
                   (do (_15 :: Vector.Vector (RTS.UInt 8)) <-
                         RTS.pEnter "PdfValue.String" pString
                       HS.pure (Value_string _15)))
                ((RTS.<||)
                   (RTS.pEnter "string"
                      (do (_16 :: Vector.Vector (RTS.UInt 8)) <-
                            RTS.pEnter "PdfValue.HexString" pHexString
                          HS.pure (Value_string _16)))
                   ((RTS.<||)
                      (RTS.pEnter "number"
                         (do (_17 :: Number) <- RTS.pEnter "PdfValue.Number" pNumber
                             HS.pure (Value_number _17)))
                      ((RTS.<||)
                         (RTS.pEnter "array"
                            (do (_18 :: Vector.Vector Value) <-
                                  RTS.pEnter "PdfValue.Array" pArray
                                HS.pure (Value_array _18)))
                         (RTS.pEnter "dict"
                            (do (_19 :: Map.Map (Vector.Vector (RTS.UInt 8)) Value) <-
                                  RTS.pEnter "PdfValue.Dict" pDict
                                HS.pure (Value_dict _19))))))))))
 
pNumberAsNat :: Number -> D.Parser HS.Integer
 
pNumberAsNat (x :: Number) =
  do RTS.pGuard "83:34--83:43" "guard failed"
       ((RTS.lit 0 :: HS.Integer) HS.<= HS.getField @"num" x)
     RTS.pGuard "83:46--83:55" "guard failed"
       (HS.getField @"exp" x HS.== (RTS.lit 0 :: HS.Integer))
     (__ :: HS.Integer) <- HS.pure (HS.getField @"num" x)
     HS.pure __
 
pNatValue :: Value -> D.Parser HS.Integer
 
pNatValue (v :: Value) =
  do (n :: Number) <-
       RTS.pIsJust "201:8--201:18" "Expected `number`"
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
  forall e.
    RTS.DDL e =>
      Vector.Vector (RTS.UInt 8)
        -> (Vector.Vector (RTS.UInt 8) -> (D.Parser () -> D.Parser ()))
 
_Between (open :: Vector.Vector (RTS.UInt 8))
  (close :: Vector.Vector (RTS.UInt 8))
  (_P :: D.Parser ()) =
  do RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const () HS.<$> RTS.pMatch "24:34--24:37" open))
     _P
     RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const () HS.<$> RTS.pMatch "24:51--24:55" close))
 
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
          (RTS.pEnter "PdfValue._KW"
             (_KW @(Vector.Vector (RTS.UInt 8))
                (HS.const ()
                   HS.<$> RTS.pMatch "35:14--35:19" (Vector.vecFromRep "true"))))))
    (RTS.pEnter "PdfValue._When"
       (_When @HS.Bool @()
          (RTS.pEnter "PdfValue._KW"
             (_KW @(Vector.Vector (RTS.UInt 8))
                (HS.const ()
                   HS.<$> RTS.pMatch "36:14--36:20" (Vector.vecFromRep "false"))))))
 
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
             RTS.pIsJust "172:31--172:52" "Key already present"
               (Map.insertMaybe (HS.getField @"key" e) (HS.getField @"value" e)
                  d))
          (Map.empty :: Map.Map (Vector.Vector (RTS.UInt 8)) Value)
          ents)
     HS.pure ()
 
_Digit :: D.Parser ()
 
_Digit =
  HS.const ()
    HS.<$> RTS.pMatch1 "77:24--77:33"
             (RTS.bcRange (RTS.uint8 48) (RTS.uint8 57))
 
_Frac :: HS.Integer -> D.Parser ()
 
_Frac (n :: HS.Integer) =
  do HS.const ()
       HS.<$> RTS.pMatch "72:11--72:13" (Vector.vecFromRep ".")
     RTS.pSkipAtLeast (RTS.<||) n (RTS.pEnter "PdfValue._Digit" _Digit)
 
_HexDigit :: D.Parser ()
 
_HexDigit =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue._Digit" _Digit)
       (HS.const ()
          HS.<$> RTS.pMatch1 "80:24--80:33"
                   (RTS.bcRange (RTS.uint8 97) (RTS.uint8 102))))
    (HS.const ()
       HS.<$> RTS.pMatch1 "81:24--81:33"
                (RTS.bcRange (RTS.uint8 65) (RTS.uint8 70)))
 
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
       HS.<$> RTS.pMatch "153:3--153:5" (Vector.vecFromRep "#")
     (ds :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 2 :: HS.Integer)
         (RTS.pEnter "PdfValue.HexDigit" pHexDigit)
     (__ :: RTS.UInt 8) <-
       HS.pure
         (RTS.convert
            (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
               (RTS.lit 16 :: HS.Integer)
               ds)
            :: RTS.UInt 8)
     RTS.pGuard "156:3--156:8" "guard failed"
       ((RTS.lit 0 :: RTS.UInt 8) HS.< __)
 
_NameChar :: D.Parser ()
 
_NameChar =
  (RTS.|||)
    (HS.const ()
       HS.<$> RTS.pMatch1 "149:16--149:45"
                (RTS.bcComplement (RTS.bcByteString "\NUL\t\n\f\r ()<>[]{}/%#")))
    (RTS.pEnter "PdfValue._NameEsc" _NameEsc)
 
_Name :: D.Parser ()
 
_Name =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (do HS.const ()
             HS.<$> RTS.pMatch "147:24--147:26" (Vector.vecFromRep "/")
           RTS.pSkipMany (RTS.<||)
             (RTS.pEnter "PdfValue._NameChar" _NameChar)))
 
_NumberAsNat :: Number -> D.Parser ()
 
_NumberAsNat (x :: Number) =
  do RTS.pGuard "83:34--83:43" "guard failed"
       ((RTS.lit 0 :: HS.Integer) HS.<= HS.getField @"num" x)
     RTS.pGuard "83:46--83:55" "guard failed"
       (HS.getField @"exp" x HS.== (RTS.lit 0 :: HS.Integer))
 
_NatValue :: Value -> D.Parser ()
 
_NatValue (v :: Value) =
  do (n :: Number) <-
       RTS.pIsJust "201:8--201:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pEnter "PdfValue._NumberAsNat" (_NumberAsNat n)
 
_Natural :: D.Parser ()
 
_Natural =
  RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: HS.Integer)
    (RTS.pEnter "PdfValue._Digit" _Digit)
 
_Null :: D.Parser ()
 
_Null =
  RTS.pEnter "PdfValue._KW"
    (_KW @(Vector.Vector (RTS.UInt 8))
       (HS.const ()
          HS.<$> RTS.pMatch "176:16--176:21" (Vector.vecFromRep "null")))
 
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
                   (RTS.pIsJust_ "45:11--45:21" "Expected `pos`"
                      (HS.getField @"pos" sign))))
             (RTS.pEnter "PdfValue._When"
                (_When @Number @()
                   (RTS.pIsJust_ "46:11--46:21" "Expected `neg`"
                      (HS.getField @"neg" sign))))))
 
_OctDigit :: D.Parser ()
 
_OctDigit =
  HS.const ()
    HS.<$> RTS.pMatch1 "78:24--78:33"
             (RTS.bcRange (RTS.uint8 48) (RTS.uint8 55))
 
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
     RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const ()
             HS.<$> RTS.pMatch "181:6--181:8" (Vector.vecFromRep "R")))
 
_Sign :: D.Parser ()
 
_Sign =
  (RTS.|||)
    (RTS.pEnter "pos"
       ((RTS.|||)
          (HS.const ()
             HS.<$> RTS.pMatch "50:11--50:13" (Vector.vecFromRep "+"))
          (HS.const ()
             HS.<$> RTS.pMatch "50:17--50:18" (Vector.vecFromRep ""))))
    (RTS.pEnter "neg"
       (HS.const ()
          HS.<$> RTS.pMatch "51:10--51:12" (Vector.vecFromRep "-")))
 
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
  RTS.pSkipWithBounds "118:9--118:28" (RTS.<||)
    (RTS.lit 1 :: HS.Integer)
    (RTS.lit 3 :: HS.Integer)
    (RTS.pEnter "PdfValue._OctDigit" _OctDigit)
 
_StringEsc :: D.Parser ()
 
_StringEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "102:4--102:7" (Vector.vecFromRep "\\")
     (RTS.|||)
       (RTS.pEnter "PdfValue._When"
          (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
             (HS.const ()
                HS.<$> RTS.pMatch "104:11--104:13" (Vector.vecFromRep "n"))))
       ((RTS.|||)
          (RTS.pEnter "PdfValue._When"
             (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                (HS.const ()
                   HS.<$> RTS.pMatch "105:11--105:13" (Vector.vecFromRep "r"))))
          ((RTS.|||)
             (RTS.pEnter "PdfValue._When"
                (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                   (HS.const ()
                      HS.<$> RTS.pMatch "106:11--106:13" (Vector.vecFromRep "t"))))
             ((RTS.|||)
                (RTS.pEnter "PdfValue._When"
                   (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                      (HS.const ()
                         HS.<$> RTS.pMatch "107:11--107:13" (Vector.vecFromRep "b"))))
                ((RTS.|||)
                   (RTS.pEnter "PdfValue._When"
                      (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                         (HS.const ()
                            HS.<$> RTS.pMatch "108:11--108:13" (Vector.vecFromRep "f"))))
                   ((RTS.|||)
                      (RTS.pEnter "PdfValue._When"
                         (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                            (HS.const ()
                               HS.<$> RTS.pMatch "109:11--109:13" (Vector.vecFromRep "("))))
                      ((RTS.|||)
                         (RTS.pEnter "PdfValue._When"
                            (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                               (HS.const ()
                                  HS.<$> RTS.pMatch "110:11--110:13" (Vector.vecFromRep ")"))))
                         ((RTS.|||)
                            (RTS.pEnter "PdfValue._When"
                               (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                  (HS.const ()
                                     HS.<$> RTS.pMatch "111:11--111:14" (Vector.vecFromRep "\\"))))
                            ((RTS.|||)
                               (RTS.pEnter "PdfValue._When"
                                  (_When @(Vector.Vector (RTS.UInt 8)) @(RTS.UInt 8)
                                     (RTS.pEnter "PdfValue._EOL" _EOL)))
                               (RTS.pEnter "PdfValue._StringNumEsc" _StringNumEsc)))))))))
 
_StringInParens :: D.Parser ()
 
_StringInParens =
  do HS.const ()
       HS.<$> RTS.pMatch "99:31--99:33" (Vector.vecFromRep "(")
     RTS.pEnter "PdfValue._StringChars" _StringChars
     HS.const ()
       HS.<$> RTS.pMatch "99:49--99:51" (Vector.vecFromRep ")")
 
_StringChunk :: D.Parser ()
 
_StringChunk =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue._StringInParens" _StringInParens)
       (RTS.pEnter "PdfValue._StringEsc" _StringEsc))
    (RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: HS.Integer)
       (HS.const ()
          HS.<$> RTS.pMatch1 "97:17--97:24"
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
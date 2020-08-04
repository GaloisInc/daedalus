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
module PdfText where
 
import qualified PdfMonad as D
import qualified PdfValue
import qualified ISOCodes
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Input as RTS
import qualified RTS.Map as Map
import qualified RTS.Vector as Vector
 
 
data ContentStreamOp
  = ContentStreamOp_appendCurvedFinalPt ()
  | ContentStreamOp_appendCurvedInitPtRepl ()
  | ContentStreamOp_appendCurvedThreePoints ()
  | ContentStreamOp_appendLine ()
  | ContentStreamOp_appendRect ()
  | ContentStreamOp_beginInline ()
  | ContentStreamOp_beginInlineImageData ()
  | ContentStreamOp_beginMarkedContent ()
  | ContentStreamOp_beginNewSuppath ()
  | ContentStreamOp_beginText ()
  | ContentStreamOp_closeFillStrokeEvenOdd ()
  | ContentStreamOp_closeFillStrokeNzWinding ()
  | ContentStreamOp_closeStrokePath ()
  | ContentStreamOp_closeSubpath ()
  | ContentStreamOp_concatMatrix ()
  | ContentStreamOp_defMarkedContentPoint ()
  | ContentStreamOp_defineMarkedContent ()
  | ContentStreamOp_endInline ()
  | ContentStreamOp_endMarkedContent ()
  | ContentStreamOp_endPath ()
  | ContentStreamOp_endTextObj ()
  | ContentStreamOp_fillPathEvenOdd ()
  | ContentStreamOp_fillPathNzWinding ()
  | ContentStreamOp_fillPathNzWindingOld ()
  | ContentStreamOp_fillStroke ()
  | ContentStreamOp_fillStrokeEvenOdd ()
  | ContentStreamOp_invokeXObj ()
  | ContentStreamOp_moveShow ()
  | ContentStreamOp_moveStartText ()
  | ContentStreamOp_moveTextPos ()
  | ContentStreamOp_moveTextPosSetLeading ()
  | ContentStreamOp_paintShadingPattern ()
  | ContentStreamOp_restoreGraphicsState ()
  | ContentStreamOp_saveGraphicsState ()
  | ContentStreamOp_setCMYKNonStroking ()
  | ContentStreamOp_setCMYKStroking ()
  | ContentStreamOp_setCharSpacing ()
  | ContentStreamOp_setClippingEvenOdd ()
  | ContentStreamOp_setClippingNzWinding ()
  | ContentStreamOp_setColorNonStroking ()
  | ContentStreamOp_setColorNonStrokingICC ()
  | ContentStreamOp_setColorRenderingIntent ()
  | ContentStreamOp_setColorSpaceNonStroking ()
  | ContentStreamOp_setColorSpaceStroking ()
  | ContentStreamOp_setColorStroking ()
  | ContentStreamOp_setColorStrokingICC ()
  | ContentStreamOp_setFlat ()
  | ContentStreamOp_setGlpyhWidthBoundingBox ()
  | ContentStreamOp_setGlyphWidth ()
  | ContentStreamOp_setGraphicsStateParams ()
  | ContentStreamOp_setGrayNonStroking ()
  | ContentStreamOp_setGrayStroking ()
  | ContentStreamOp_setHorizontalTextScaling ()
  | ContentStreamOp_setLineCapStyle ()
  | ContentStreamOp_setLineDash ()
  | ContentStreamOp_setLineJoinStyle ()
  | ContentStreamOp_setLineWidth ()
  | ContentStreamOp_setMiterLimit ()
  | ContentStreamOp_setRGBNonStroking ()
  | ContentStreamOp_setRGBStroking ()
  | ContentStreamOp_setSpacing ()
  | ContentStreamOp_setTextFont ()
  | ContentStreamOp_setTextLeading ()
  | ContentStreamOp_setTextMatrix ()
  | ContentStreamOp_setTextRendering ()
  | ContentStreamOp_setTextRise ()
  | ContentStreamOp_setWordSpacing ()
  | ContentStreamOp_showText ()
  | ContentStreamOp_showTextIndGlyph ()
  | ContentStreamOp_stroke ()
  
 
deriving instance HS.Eq ContentStreamOp
 
deriving instance HS.Ord ContentStreamOp
 
deriving instance HS.Show ContentStreamOp
 
instance RTS.DDL ContentStreamOp where
 
instance HS.HasField "appendCurvedFinalPt" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_appendCurvedFinalPt x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "appendCurvedInitPtRepl" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_appendCurvedInitPtRepl x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "appendCurvedThreePoints" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_appendCurvedThreePoints x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "appendLine" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_appendLine x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "appendRect" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_appendRect x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "beginInline" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_beginInline x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "beginInlineImageData" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_beginInlineImageData x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "beginMarkedContent" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_beginMarkedContent x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "beginNewSuppath" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_beginNewSuppath x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "beginText" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_beginText x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "closeFillStrokeEvenOdd" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_closeFillStrokeEvenOdd x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "closeFillStrokeNzWinding" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_closeFillStrokeNzWinding x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "closeStrokePath" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_closeStrokePath x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "closeSubpath" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_closeSubpath x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "concatMatrix" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_concatMatrix x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "defMarkedContentPoint" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_defMarkedContentPoint x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "defineMarkedContent" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_defineMarkedContent x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "endInline" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_endInline x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "endMarkedContent" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_endMarkedContent x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "endPath" ContentStreamOp (HS.Maybe ()) where
  getField (ContentStreamOp_endPath x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "endTextObj" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_endTextObj x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "fillPathEvenOdd" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_fillPathEvenOdd x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "fillPathNzWinding" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_fillPathNzWinding x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "fillPathNzWindingOld" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_fillPathNzWindingOld x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "fillStroke" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_fillStroke x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "fillStrokeEvenOdd" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_fillStrokeEvenOdd x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "invokeXObj" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_invokeXObj x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "moveShow" ContentStreamOp (HS.Maybe ()) where
  getField (ContentStreamOp_moveShow x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "moveStartText" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_moveStartText x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "moveTextPos" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_moveTextPos x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "moveTextPosSetLeading" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_moveTextPosSetLeading x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "paintShadingPattern" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_paintShadingPattern x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "restoreGraphicsState" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_restoreGraphicsState x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "saveGraphicsState" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_saveGraphicsState x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setCMYKNonStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setCMYKNonStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setCMYKStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setCMYKStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setCharSpacing" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setCharSpacing x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setClippingEvenOdd" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setClippingEvenOdd x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setClippingNzWinding" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setClippingNzWinding x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setColorNonStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setColorNonStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setColorNonStrokingICC" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setColorNonStrokingICC x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setColorRenderingIntent" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setColorRenderingIntent x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setColorSpaceNonStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setColorSpaceNonStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setColorSpaceStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setColorSpaceStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setColorStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setColorStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setColorStrokingICC" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setColorStrokingICC x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setFlat" ContentStreamOp (HS.Maybe ()) where
  getField (ContentStreamOp_setFlat x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setGlpyhWidthBoundingBox" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setGlpyhWidthBoundingBox x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setGlyphWidth" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setGlyphWidth x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setGraphicsStateParams" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setGraphicsStateParams x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setGrayNonStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setGrayNonStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setGrayStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setGrayStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setHorizontalTextScaling" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setHorizontalTextScaling x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setLineCapStyle" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setLineCapStyle x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setLineDash" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setLineDash x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setLineJoinStyle" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setLineJoinStyle x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setLineWidth" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setLineWidth x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setMiterLimit" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setMiterLimit x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setRGBNonStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setRGBNonStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setRGBStroking" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setRGBStroking x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setSpacing" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setSpacing x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setTextFont" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setTextFont x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setTextLeading" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setTextLeading x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setTextMatrix" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setTextMatrix x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setTextRendering" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setTextRendering x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setTextRise" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setTextRise x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "setWordSpacing" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_setWordSpacing x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "showText" ContentStreamOp (HS.Maybe ()) where
  getField (ContentStreamOp_showText x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "showTextIndGlyph" ContentStreamOp
           (HS.Maybe ()) where
  getField (ContentStreamOp_showTextIndGlyph x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "stroke" ContentStreamOp (HS.Maybe ()) where
  getField (ContentStreamOp_stroke x) = HS.Just x
   
  getField _ = HS.Nothing
 
data ContentStream_0
  = ContentStream_0_compatSect (Vector.Vector (RTS.UInt 8))
  | ContentStream_0_operand PdfValue.Value
  | ContentStream_0_operation ContentStreamOp
  
 
deriving instance HS.Eq ContentStream_0
 
deriving instance HS.Ord ContentStream_0
 
deriving instance HS.Show ContentStream_0
 
instance RTS.DDL ContentStream_0 where
 
instance HS.HasField "compatSect" ContentStream_0
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (ContentStream_0_compatSect x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "operand" ContentStream_0
           (HS.Maybe PdfValue.Value) where
  getField (ContentStream_0_operand x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "operation" ContentStream_0
           (HS.Maybe ContentStreamOp) where
  getField (ContentStream_0_operation x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Date_0
  = Date_0_bigZ ()
  | Date_0_minus ()
  | Date_0_plus ()
  
 
deriving instance HS.Eq Date_0
 
deriving instance HS.Ord Date_0
 
deriving instance HS.Show Date_0
 
instance RTS.DDL Date_0 where
 
instance HS.HasField "bigZ" Date_0 (HS.Maybe ()) where
  getField (Date_0_bigZ x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "minus" Date_0 (HS.Maybe ()) where
  getField (Date_0_minus x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "plus" Date_0 (HS.Maybe ()) where
  getField (Date_0_plus x) = HS.Just x
   
  getField _ = HS.Nothing
 
data UniChar_0
  = UniChar_0 ISOCodes.LanguageCode (HS.Maybe ISOCodes.CountryCode)
  
 
deriving instance HS.Eq UniChar_0
 
deriving instance HS.Ord UniChar_0
 
deriving instance HS.Show UniChar_0
 
instance RTS.DDL UniChar_0 where
 
instance HS.HasField "lang" UniChar_0 ISOCodes.LanguageCode where
  getField (UniChar_0 x _) = x
 
instance HS.HasField "ctry" UniChar_0
           (HS.Maybe ISOCodes.CountryCode) where
  getField (UniChar_0 _ x) = x
 
data UniChar_1
  = UniChar_1 (RTS.UInt 8) (RTS.UInt 8)
  
 
deriving instance HS.Eq UniChar_1
 
deriving instance HS.Ord UniChar_1
 
deriving instance HS.Show UniChar_1
 
instance RTS.DDL UniChar_1 where
 
instance HS.HasField "big" UniChar_1 (RTS.UInt 8) where
  getField (UniChar_1 x _) = x
 
instance HS.HasField "small" UniChar_1 (RTS.UInt 8) where
  getField (UniChar_1 _ x) = x
 
data UniChar_2
  = UniChar_2 (RTS.UInt 8) (RTS.UInt 8)
  
 
deriving instance HS.Eq UniChar_2
 
deriving instance HS.Ord UniChar_2
 
deriving instance HS.Show UniChar_2
 
instance RTS.DDL UniChar_2 where
 
instance HS.HasField "big" UniChar_2 (RTS.UInt 8) where
  getField (UniChar_2 x _) = x
 
instance HS.HasField "small" UniChar_2 (RTS.UInt 8) where
  getField (UniChar_2 _ x) = x
 
data UniChar
  = UniChar_escape UniChar_0
  | UniChar_supplementary UniChar_1
  | UniChar_twoByte UniChar_2
  
 
deriving instance HS.Eq UniChar
 
deriving instance HS.Ord UniChar
 
deriving instance HS.Show UniChar
 
instance RTS.DDL UniChar where
 
instance HS.HasField "escape" UniChar (HS.Maybe UniChar_0) where
  getField (UniChar_escape x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "supplementary" UniChar
           (HS.Maybe UniChar_1) where
  getField (UniChar_supplementary x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "twoByte" UniChar (HS.Maybe UniChar_2) where
  getField (UniChar_twoByte x) = HS.Just x
   
  getField _ = HS.Nothing
 
data TextObj_0
  = TextObj_0_isPdfDoc (Vector.Vector (RTS.UInt 8))
  | TextObj_0_isUnicode (Vector.Vector UniChar)
  
 
deriving instance HS.Eq TextObj_0
 
deriving instance HS.Ord TextObj_0
 
deriving instance HS.Show TextObj_0
 
instance RTS.DDL TextObj_0 where
 
instance HS.HasField "isPdfDoc" TextObj_0
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (TextObj_0_isPdfDoc x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "isUnicode" TextObj_0
           (HS.Maybe (Vector.Vector UniChar)) where
  getField (TextObj_0_isUnicode x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Date
  = Date (Vector.Vector HS.Integer) HS.Integer HS.Integer HS.Integer
      HS.Integer
      HS.Integer
      Date_0
      HS.Integer
      HS.Integer
  
 
deriving instance HS.Eq Date
 
deriving instance HS.Ord Date
 
deriving instance HS.Show Date
 
instance RTS.DDL Date where
 
instance HS.HasField "year" Date (Vector.Vector HS.Integer) where
  getField (Date x _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "month" Date HS.Integer where
  getField (Date _ x _ _ _ _ _ _ _) = x
 
instance HS.HasField "day" Date HS.Integer where
  getField (Date _ _ x _ _ _ _ _ _) = x
 
instance HS.HasField "hour" Date HS.Integer where
  getField (Date _ _ _ x _ _ _ _ _) = x
 
instance HS.HasField "minute" Date HS.Integer where
  getField (Date _ _ _ _ x _ _ _ _) = x
 
instance HS.HasField "second" Date HS.Integer where
  getField (Date _ _ _ _ _ x _ _ _) = x
 
instance HS.HasField "relLocalTime" Date Date_0 where
  getField (Date _ _ _ _ _ _ x _ _) = x
 
instance HS.HasField "utOffsetHours" Date HS.Integer where
  getField (Date _ _ _ _ _ _ _ x _) = x
 
instance HS.HasField "utOffsetMins" Date HS.Integer where
  getField (Date _ _ _ _ _ _ _ _ x) = x
 
pBeginCompat :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pBeginCompat =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @(Vector.Vector (RTS.UInt 8))
       (RTS.pMatch "90:25--90:28" (Vector.vecFromRep "BX")))
 
pBmpByte :: D.Parser (RTS.UInt 8)
 
pBmpByte =
  (RTS.|||)
    (RTS.uint8
       HS.<$> RTS.pMatch1 "137:15--137:26"
                (RTS.bcRange (RTS.lit 0 :: RTS.UInt 8)
                   (RTS.lit 215 :: RTS.UInt 8)))
    (RTS.uint8
       HS.<$> RTS.pMatch1 "137:30--137:41"
                (RTS.bcRange (RTS.lit 224 :: RTS.UInt 8)
                   (RTS.lit 255 :: RTS.UInt 8)))
 
pBoundedTwoDigits ::
      HS.Integer -> (HS.Integer -> D.Parser HS.Integer)
 
pBoundedTwoDigits (lb :: HS.Integer) (ub :: HS.Integer) =
  do (digs :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 2 :: HS.Integer)
         (RTS.pEnter "PdfValue.Digit" PdfValue.pDigit)
     (__ :: HS.Integer) <-
       HS.pure
         (PdfValue.numBase @(Vector.Vector HS.Integer) @HS.Integer
            @HS.Integer
            (RTS.lit 10 :: HS.Integer)
            digs)
     RTS.pGuard "187:3--187:10" "guard failed" (lb HS.<= __)
     RTS.pGuard "187:14--187:21" "guard failed" (__ HS.<= ub)
     HS.pure __
 
pByte :: D.Parser (RTS.UInt 8)
 
pByte =
  RTS.uint8
    HS.<$> RTS.pMatch1 "121:12--121:19"
             (RTS.bcRange (RTS.lit 0 :: RTS.UInt 8) (RTS.lit 255 :: RTS.UInt 8))
 
pEndCompat :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pEndCompat =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @(Vector.Vector (RTS.UInt 8))
       (RTS.pMatch "93:23--93:26" (Vector.vecFromRep "EX")))
 
pContentStreamOp :: D.Parser ContentStreamOp
 
pContentStreamOp =
  (RTS.|||)
    (RTS.pEnter "closeFillStrokeNzWinding"
       (do (_0 :: ()) <-
             HS.const ()
               HS.<$> RTS.pMatch "13:31--13:33" (Vector.vecFromRep "b")
           HS.pure (ContentStreamOp_closeFillStrokeNzWinding _0)))
    ((RTS.|||)
       (RTS.pEnter "fillStroke"
          (do (_1 :: ()) <-
                HS.const ()
                  HS.<$> RTS.pMatch "14:17--14:19" (Vector.vecFromRep "B")
              HS.pure (ContentStreamOp_fillStroke _1)))
       ((RTS.|||)
          (RTS.pEnter "closeFillStrokeEvenOdd"
             (do (_2 :: ()) <-
                   HS.const ()
                     HS.<$> RTS.pMatch "15:29--15:32" (Vector.vecFromRep "b*")
                 HS.pure (ContentStreamOp_closeFillStrokeEvenOdd _2)))
          ((RTS.|||)
             (RTS.pEnter "fillStrokeEvenOdd"
                (do (_3 :: ()) <-
                      HS.const ()
                        HS.<$> RTS.pMatch "16:24--16:27" (Vector.vecFromRep "B*")
                    HS.pure (ContentStreamOp_fillStrokeEvenOdd _3)))
             ((RTS.|||)
                (RTS.pEnter "beginMarkedContent"
                   (do (_4 :: ()) <-
                         HS.const ()
                           HS.<$> RTS.pMatch "17:25--17:29" (Vector.vecFromRep "BDC")
                       HS.pure (ContentStreamOp_beginMarkedContent _4)))
                ((RTS.|||)
                   (RTS.pEnter "beginInline"
                      (do (_5 :: ()) <-
                            HS.const ()
                              HS.<$> RTS.pMatch "18:18--18:21" (Vector.vecFromRep "Bl")
                          HS.pure (ContentStreamOp_beginInline _5)))
                   ((RTS.|||)
                      (RTS.pEnter "beginMarkedContent"
                         (do (_6 :: ()) <-
                               HS.const ()
                                 HS.<$> RTS.pMatch "19:25--19:29" (Vector.vecFromRep "BMC")
                             HS.pure (ContentStreamOp_beginMarkedContent _6)))
                      ((RTS.|||)
                         (RTS.pEnter "beginText"
                            (do (_7 :: ()) <-
                                  HS.const ()
                                    HS.<$> RTS.pMatch "20:16--20:19" (Vector.vecFromRep "BT")
                                HS.pure (ContentStreamOp_beginText _7)))
                         ((RTS.|||)
                            (RTS.pEnter "appendCurvedThreePoints"
                               (do (_8 :: ()) <-
                                     HS.const ()
                                       HS.<$> RTS.pMatch "21:30--21:32" (Vector.vecFromRep "c")
                                   HS.pure (ContentStreamOp_appendCurvedThreePoints _8)))
                            ((RTS.|||)
                               (RTS.pEnter "concatMatrix"
                                  (do (_9 :: ()) <-
                                        HS.const ()
                                          HS.<$> RTS.pMatch "22:19--22:22" (Vector.vecFromRep "cm")
                                      HS.pure (ContentStreamOp_concatMatrix _9)))
                               ((RTS.|||)
                                  (RTS.pEnter "setColorSpaceStroking"
                                     (do (_10 :: ()) <-
                                           HS.const ()
                                             HS.<$> RTS.pMatch "23:28--23:31"
                                                      (Vector.vecFromRep "CS")
                                         HS.pure (ContentStreamOp_setColorSpaceStroking _10)))
                                  ((RTS.|||)
                                     (RTS.pEnter "setColorSpaceNonStroking"
                                        (do (_11 :: ()) <-
                                              HS.const ()
                                                HS.<$> RTS.pMatch "24:31--24:34"
                                                         (Vector.vecFromRep "cs")
                                            HS.pure (ContentStreamOp_setColorSpaceNonStroking _11)))
                                     ((RTS.|||)
                                        (RTS.pEnter "setLineDash"
                                           (do (_12 :: ()) <-
                                                 HS.const ()
                                                   HS.<$> RTS.pMatch "25:18--25:20"
                                                            (Vector.vecFromRep "d")
                                               HS.pure (ContentStreamOp_setLineDash _12)))
                                        ((RTS.|||)
                                           (RTS.pEnter "setGlyphWidth"
                                              (do (_13 :: ()) <-
                                                    HS.const ()
                                                      HS.<$> RTS.pMatch "26:20--26:23"
                                                               (Vector.vecFromRep "d0")
                                                  HS.pure (ContentStreamOp_setGlyphWidth _13)))
                                           ((RTS.|||)
                                              (RTS.pEnter "setGlpyhWidthBoundingBox"
                                                 (do (_14 :: ()) <-
                                                       HS.const ()
                                                         HS.<$> RTS.pMatch "27:31--27:34"
                                                                  (Vector.vecFromRep "d1")
                                                     HS.pure
                                                       (ContentStreamOp_setGlpyhWidthBoundingBox
                                                          _14)))
                                              ((RTS.|||)
                                                 (RTS.pEnter "invokeXObj"
                                                    (do (_15 :: ()) <-
                                                          HS.const ()
                                                            HS.<$> RTS.pMatch "28:17--28:20"
                                                                     (Vector.vecFromRep "Do")
                                                        HS.pure (ContentStreamOp_invokeXObj _15)))
                                                 ((RTS.|||)
                                                    (RTS.pEnter "defMarkedContentPoint"
                                                       (do (_16 :: ()) <-
                                                             HS.const ()
                                                               HS.<$> RTS.pMatch "29:28--29:31"
                                                                        (Vector.vecFromRep "DP")
                                                           HS.pure
                                                             (ContentStreamOp_defMarkedContentPoint
                                                                _16)))
                                                    ((RTS.|||)
                                                       (RTS.pEnter "endInline"
                                                          (do (_17 :: ()) <-
                                                                HS.const ()
                                                                  HS.<$> RTS.pMatch "30:16--30:19"
                                                                           (Vector.vecFromRep "El")
                                                              HS.pure
                                                                (ContentStreamOp_endInline _17)))
                                                       ((RTS.|||)
                                                          (RTS.pEnter "endMarkedContent"
                                                             (do (_18 :: ()) <-
                                                                   HS.const ()
                                                                     HS.<$> RTS.pMatch
                                                                              "31:23--31:27"
                                                                              (Vector.vecFromRep
                                                                                 "EMC")
                                                                 HS.pure
                                                                   (ContentStreamOp_endMarkedContent
                                                                      _18)))
                                                          ((RTS.|||)
                                                             (RTS.pEnter "endTextObj"
                                                                (do (_19 :: ()) <-
                                                                      HS.const ()
                                                                        HS.<$> RTS.pMatch
                                                                                 "32:17--32:20"
                                                                                 (Vector.vecFromRep
                                                                                    "ET")
                                                                    HS.pure
                                                                      (ContentStreamOp_endTextObj
                                                                         _19)))
                                                             ((RTS.|||)
                                                                (RTS.pEnter "fillPathNzWinding"
                                                                   (do (_20 :: ()) <-
                                                                         HS.const ()
                                                                           HS.<$> RTS.pMatch
                                                                                    "33:24--33:26"
                                                                                    (Vector.vecFromRep
                                                                                       "f")
                                                                       HS.pure
                                                                         (ContentStreamOp_fillPathNzWinding
                                                                            _20)))
                                                                ((RTS.|||)
                                                                   (RTS.pEnter
                                                                      "fillPathNzWindingOld"
                                                                      (do (_21 :: ()) <-
                                                                            HS.const ()
                                                                              HS.<$> RTS.pMatch
                                                                                       "34:27--34:29"
                                                                                       (Vector.vecFromRep
                                                                                          "F")
                                                                          HS.pure
                                                                            (ContentStreamOp_fillPathNzWindingOld
                                                                               _21)))
                                                                   ((RTS.|||)
                                                                      (RTS.pEnter "fillPathEvenOdd"
                                                                         (do (_22 :: ()) <-
                                                                               HS.const ()
                                                                                 HS.<$> RTS.pMatch
                                                                                          "35:22--35:25"
                                                                                          (Vector.vecFromRep
                                                                                             "f*")
                                                                             HS.pure
                                                                               (ContentStreamOp_fillPathEvenOdd
                                                                                  _22)))
                                                                      ((RTS.|||)
                                                                         (RTS.pEnter
                                                                            "setGrayStroking"
                                                                            (do (_23 :: ()) <-
                                                                                  HS.const ()
                                                                                    HS.<$> RTS.pMatch
                                                                                             "36:22--36:24"
                                                                                             (Vector.vecFromRep
                                                                                                "G")
                                                                                HS.pure
                                                                                  (ContentStreamOp_setGrayStroking
                                                                                     _23)))
                                                                         ((RTS.|||)
                                                                            (RTS.pEnter
                                                                               "setGrayNonStroking"
                                                                               (do (_24 :: ()) <-
                                                                                     HS.const ()
                                                                                       HS.<$> RTS.pMatch
                                                                                                "37:25--37:27"
                                                                                                (Vector.vecFromRep
                                                                                                   "g")
                                                                                   HS.pure
                                                                                     (ContentStreamOp_setGrayNonStroking
                                                                                        _24)))
                                                                            ((RTS.|||)
                                                                               (RTS.pEnter
                                                                                  "setGraphicsStateParams"
                                                                                  (do (_25 :: ()) <-
                                                                                        HS.const ()
                                                                                          HS.<$> RTS.pMatch
                                                                                                   "38:29--38:32"
                                                                                                   (Vector.vecFromRep
                                                                                                      "gs")
                                                                                      HS.pure
                                                                                        (ContentStreamOp_setGraphicsStateParams
                                                                                           _25)))
                                                                               ((RTS.|||)
                                                                                  (RTS.pEnter
                                                                                     "closeSubpath"
                                                                                     (do (_26
                                                                                            :: ()) <-
                                                                                           HS.const
                                                                                             ()
                                                                                             HS.<$> RTS.pMatch
                                                                                                      "39:19--39:21"
                                                                                                      (Vector.vecFromRep
                                                                                                         "h")
                                                                                         HS.pure
                                                                                           (ContentStreamOp_closeSubpath
                                                                                              _26)))
                                                                                  ((RTS.|||)
                                                                                     (RTS.pEnter
                                                                                        "setFlat"
                                                                                        (do (_27
                                                                                               :: ()) <-
                                                                                              HS.const
                                                                                                ()
                                                                                                HS.<$> RTS.pMatch
                                                                                                         "40:14--40:16"
                                                                                                         (Vector.vecFromRep
                                                                                                            "i")
                                                                                            HS.pure
                                                                                              (ContentStreamOp_setFlat
                                                                                                 _27)))
                                                                                     ((RTS.|||)
                                                                                        (RTS.pEnter
                                                                                           "beginInlineImageData"
                                                                                           (do (_28
                                                                                                  :: ()) <-
                                                                                                 HS.const
                                                                                                   ()
                                                                                                   HS.<$> RTS.pMatch
                                                                                                            "41:27--41:30"
                                                                                                            (Vector.vecFromRep
                                                                                                               "ID")
                                                                                               HS.pure
                                                                                                 (ContentStreamOp_beginInlineImageData
                                                                                                    _28)))
                                                                                        ((RTS.|||)
                                                                                           (RTS.pEnter
                                                                                              "setLineJoinStyle"
                                                                                              (do (_29
                                                                                                     :: ()) <-
                                                                                                    HS.const
                                                                                                      ()
                                                                                                      HS.<$> RTS.pMatch
                                                                                                               "42:23--42:25"
                                                                                                               (Vector.vecFromRep
                                                                                                                  "j")
                                                                                                  HS.pure
                                                                                                    (ContentStreamOp_setLineJoinStyle
                                                                                                       _29)))
                                                                                           ((RTS.|||)
                                                                                              (RTS.pEnter
                                                                                                 "setLineCapStyle"
                                                                                                 (do (_30
                                                                                                        :: ()) <-
                                                                                                       HS.const
                                                                                                         ()
                                                                                                         HS.<$> RTS.pMatch
                                                                                                                  "43:22--43:24"
                                                                                                                  (Vector.vecFromRep
                                                                                                                     "J")
                                                                                                     HS.pure
                                                                                                       (ContentStreamOp_setLineCapStyle
                                                                                                          _30)))
                                                                                              ((RTS.|||)
                                                                                                 (RTS.pEnter
                                                                                                    "setCMYKStroking"
                                                                                                    (do (_31
                                                                                                           :: ()) <-
                                                                                                          HS.const
                                                                                                            ()
                                                                                                            HS.<$> RTS.pMatch
                                                                                                                     "44:22--44:24"
                                                                                                                     (Vector.vecFromRep
                                                                                                                        "K")
                                                                                                        HS.pure
                                                                                                          (ContentStreamOp_setCMYKStroking
                                                                                                             _31)))
                                                                                                 ((RTS.|||)
                                                                                                    (RTS.pEnter
                                                                                                       "setCMYKNonStroking"
                                                                                                       (do (_32
                                                                                                              :: ()) <-
                                                                                                             HS.const
                                                                                                               ()
                                                                                                               HS.<$> RTS.pMatch
                                                                                                                        "45:25--45:27"
                                                                                                                        (Vector.vecFromRep
                                                                                                                           "k")
                                                                                                           HS.pure
                                                                                                             (ContentStreamOp_setCMYKNonStroking
                                                                                                                _32)))
                                                                                                    ((RTS.|||)
                                                                                                       (RTS.pEnter
                                                                                                          "appendLine"
                                                                                                          (do (_33
                                                                                                                 :: ()) <-
                                                                                                                HS.const
                                                                                                                  ()
                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                           "46:17--46:19"
                                                                                                                           (Vector.vecFromRep
                                                                                                                              "l")
                                                                                                              HS.pure
                                                                                                                (ContentStreamOp_appendLine
                                                                                                                   _33)))
                                                                                                       ((RTS.|||)
                                                                                                          (RTS.pEnter
                                                                                                             "beginNewSuppath"
                                                                                                             (do (_34
                                                                                                                    :: ()) <-
                                                                                                                   HS.const
                                                                                                                     ()
                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                              "47:22--47:24"
                                                                                                                              (Vector.vecFromRep
                                                                                                                                 "m")
                                                                                                                 HS.pure
                                                                                                                   (ContentStreamOp_beginNewSuppath
                                                                                                                      _34)))
                                                                                                          ((RTS.|||)
                                                                                                             (RTS.pEnter
                                                                                                                "setMiterLimit"
                                                                                                                (do (_35
                                                                                                                       :: ()) <-
                                                                                                                      HS.const
                                                                                                                        ()
                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                 "48:20--48:22"
                                                                                                                                 (Vector.vecFromRep
                                                                                                                                    "M")
                                                                                                                    HS.pure
                                                                                                                      (ContentStreamOp_setMiterLimit
                                                                                                                         _35)))
                                                                                                             ((RTS.|||)
                                                                                                                (RTS.pEnter
                                                                                                                   "defineMarkedContent"
                                                                                                                   (do (_36
                                                                                                                          :: ()) <-
                                                                                                                         HS.const
                                                                                                                           ()
                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                    "49:26--49:29"
                                                                                                                                    (Vector.vecFromRep
                                                                                                                                       "MP")
                                                                                                                       HS.pure
                                                                                                                         (ContentStreamOp_defineMarkedContent
                                                                                                                            _36)))
                                                                                                                ((RTS.|||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "endPath"
                                                                                                                      (do (_37
                                                                                                                             :: ()) <-
                                                                                                                            HS.const
                                                                                                                              ()
                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                       "50:14--50:16"
                                                                                                                                       (Vector.vecFromRep
                                                                                                                                          "n")
                                                                                                                          HS.pure
                                                                                                                            (ContentStreamOp_endPath
                                                                                                                               _37)))
                                                                                                                   ((RTS.|||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "saveGraphicsState"
                                                                                                                         (do (_38
                                                                                                                                :: ()) <-
                                                                                                                               HS.const
                                                                                                                                 ()
                                                                                                                                 HS.<$> RTS.pMatch
                                                                                                                                          "51:24--51:26"
                                                                                                                                          (Vector.vecFromRep
                                                                                                                                             "q")
                                                                                                                             HS.pure
                                                                                                                               (ContentStreamOp_saveGraphicsState
                                                                                                                                  _38)))
                                                                                                                      ((RTS.|||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "restoreGraphicsState"
                                                                                                                            (do (_39
                                                                                                                                   :: ()) <-
                                                                                                                                  HS.const
                                                                                                                                    ()
                                                                                                                                    HS.<$> RTS.pMatch
                                                                                                                                             "52:27--52:29"
                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                "Q")
                                                                                                                                HS.pure
                                                                                                                                  (ContentStreamOp_restoreGraphicsState
                                                                                                                                     _39)))
                                                                                                                         ((RTS.|||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "appendRect"
                                                                                                                               (do (_40
                                                                                                                                      :: ()) <-
                                                                                                                                     HS.const
                                                                                                                                       ()
                                                                                                                                       HS.<$> RTS.pMatch
                                                                                                                                                "53:17--53:20"
                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                   "re")
                                                                                                                                   HS.pure
                                                                                                                                     (ContentStreamOp_appendRect
                                                                                                                                        _40)))
                                                                                                                            ((RTS.|||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "setRGBStroking"
                                                                                                                                  (do (_41
                                                                                                                                         :: ()) <-
                                                                                                                                        HS.const
                                                                                                                                          ()
                                                                                                                                          HS.<$> RTS.pMatch
                                                                                                                                                   "54:21--54:24"
                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                      "RG")
                                                                                                                                      HS.pure
                                                                                                                                        (ContentStreamOp_setRGBStroking
                                                                                                                                           _41)))
                                                                                                                               ((RTS.|||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "setRGBNonStroking"
                                                                                                                                     (do (_42
                                                                                                                                            :: ()) <-
                                                                                                                                           HS.const
                                                                                                                                             ()
                                                                                                                                             HS.<$> RTS.pMatch
                                                                                                                                                      "55:24--55:27"
                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                         "rg")
                                                                                                                                         HS.pure
                                                                                                                                           (ContentStreamOp_setRGBNonStroking
                                                                                                                                              _42)))
                                                                                                                                  ((RTS.|||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "setColorRenderingIntent"
                                                                                                                                        (do (_43
                                                                                                                                               :: ()) <-
                                                                                                                                              HS.const
                                                                                                                                                ()
                                                                                                                                                HS.<$> RTS.pMatch
                                                                                                                                                         "56:30--56:33"
                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                            "ri")
                                                                                                                                            HS.pure
                                                                                                                                              (ContentStreamOp_setColorRenderingIntent
                                                                                                                                                 _43)))
                                                                                                                                     ((RTS.|||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "closeStrokePath"
                                                                                                                                           (do (_44
                                                                                                                                                  :: ()) <-
                                                                                                                                                 HS.const
                                                                                                                                                   ()
                                                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                                                            "57:22--57:24"
                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                               "s")
                                                                                                                                               HS.pure
                                                                                                                                                 (ContentStreamOp_closeStrokePath
                                                                                                                                                    _44)))
                                                                                                                                        ((RTS.|||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "stroke"
                                                                                                                                              (do (_45
                                                                                                                                                     :: ()) <-
                                                                                                                                                    HS.const
                                                                                                                                                      ()
                                                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                                                               "58:13--58:15"
                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                  "S")
                                                                                                                                                  HS.pure
                                                                                                                                                    (ContentStreamOp_stroke
                                                                                                                                                       _45)))
                                                                                                                                           ((RTS.|||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "setColorStroking"
                                                                                                                                                 (do (_46
                                                                                                                                                        :: ()) <-
                                                                                                                                                       HS.const
                                                                                                                                                         ()
                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                  "59:23--59:26"
                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                     "SC")
                                                                                                                                                     HS.pure
                                                                                                                                                       (ContentStreamOp_setColorStroking
                                                                                                                                                          _46)))
                                                                                                                                              ((RTS.|||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "setColorNonStroking"
                                                                                                                                                    (do (_47
                                                                                                                                                           :: ()) <-
                                                                                                                                                          HS.const
                                                                                                                                                            ()
                                                                                                                                                            HS.<$> RTS.pMatch
                                                                                                                                                                     "60:26--60:29"
                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                        "sc")
                                                                                                                                                        HS.pure
                                                                                                                                                          (ContentStreamOp_setColorNonStroking
                                                                                                                                                             _47)))
                                                                                                                                                 ((RTS.|||)
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "setColorStrokingICC"
                                                                                                                                                       (do (_48
                                                                                                                                                              :: ()) <-
                                                                                                                                                             HS.const
                                                                                                                                                               ()
                                                                                                                                                               HS.<$> RTS.pMatch
                                                                                                                                                                        "61:26--61:30"
                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                           "SCN")
                                                                                                                                                           HS.pure
                                                                                                                                                             (ContentStreamOp_setColorStrokingICC
                                                                                                                                                                _48)))
                                                                                                                                                    ((RTS.|||)
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "setColorNonStrokingICC"
                                                                                                                                                          (do (_49
                                                                                                                                                                 :: ()) <-
                                                                                                                                                                HS.const
                                                                                                                                                                  ()
                                                                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                                                                           "62:29--62:33"
                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                              "scn")
                                                                                                                                                              HS.pure
                                                                                                                                                                (ContentStreamOp_setColorNonStrokingICC
                                                                                                                                                                   _49)))
                                                                                                                                                       ((RTS.|||)
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "paintShadingPattern"
                                                                                                                                                             (do (_50
                                                                                                                                                                    :: ()) <-
                                                                                                                                                                   HS.const
                                                                                                                                                                     ()
                                                                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                                                                              "63:26--63:29"
                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                 "sh")
                                                                                                                                                                 HS.pure
                                                                                                                                                                   (ContentStreamOp_paintShadingPattern
                                                                                                                                                                      _50)))
                                                                                                                                                          ((RTS.|||)
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "moveStartText"
                                                                                                                                                                (do (_51
                                                                                                                                                                       :: ()) <-
                                                                                                                                                                      HS.const
                                                                                                                                                                        ()
                                                                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                                                                 "64:20--64:23"
                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                    "T*")
                                                                                                                                                                    HS.pure
                                                                                                                                                                      (ContentStreamOp_moveStartText
                                                                                                                                                                         _51)))
                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "setCharSpacing"
                                                                                                                                                                   (do (_52
                                                                                                                                                                          :: ()) <-
                                                                                                                                                                         HS.const
                                                                                                                                                                           ()
                                                                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                                                                    "65:21--65:24"
                                                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                                                       "Tc")
                                                                                                                                                                       HS.pure
                                                                                                                                                                         (ContentStreamOp_setCharSpacing
                                                                                                                                                                            _52)))
                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "moveTextPos"
                                                                                                                                                                      (do (_53
                                                                                                                                                                             :: ()) <-
                                                                                                                                                                            HS.const
                                                                                                                                                                              ()
                                                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                                                       "66:18--66:21"
                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                          "Td")
                                                                                                                                                                          HS.pure
                                                                                                                                                                            (ContentStreamOp_moveTextPos
                                                                                                                                                                               _53)))
                                                                                                                                                                   ((RTS.|||)
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "moveTextPosSetLeading"
                                                                                                                                                                         (do (_54
                                                                                                                                                                                :: ()) <-
                                                                                                                                                                               HS.const
                                                                                                                                                                                 ()
                                                                                                                                                                                 HS.<$> RTS.pMatch
                                                                                                                                                                                          "67:28--67:31"
                                                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                                                             "TD")
                                                                                                                                                                             HS.pure
                                                                                                                                                                               (ContentStreamOp_moveTextPosSetLeading
                                                                                                                                                                                  _54)))
                                                                                                                                                                      ((RTS.|||)
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "setTextFont"
                                                                                                                                                                            (do (_55
                                                                                                                                                                                   :: ()) <-
                                                                                                                                                                                  HS.const
                                                                                                                                                                                    ()
                                                                                                                                                                                    HS.<$> RTS.pMatch
                                                                                                                                                                                             "68:18--68:21"
                                                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                                                "Tf")
                                                                                                                                                                                HS.pure
                                                                                                                                                                                  (ContentStreamOp_setTextFont
                                                                                                                                                                                     _55)))
                                                                                                                                                                         ((RTS.|||)
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "showText"
                                                                                                                                                                               (do (_56
                                                                                                                                                                                      :: ()) <-
                                                                                                                                                                                     HS.const
                                                                                                                                                                                       ()
                                                                                                                                                                                       HS.<$> RTS.pMatch
                                                                                                                                                                                                "69:15--69:18"
                                                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                                                   "Tj")
                                                                                                                                                                                   HS.pure
                                                                                                                                                                                     (ContentStreamOp_showText
                                                                                                                                                                                        _56)))
                                                                                                                                                                            ((RTS.|||)
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "showTextIndGlyph"
                                                                                                                                                                                  (do (_57
                                                                                                                                                                                         :: ()) <-
                                                                                                                                                                                        HS.const
                                                                                                                                                                                          ()
                                                                                                                                                                                          HS.<$> RTS.pMatch
                                                                                                                                                                                                   "70:23--70:26"
                                                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                                                      "TJ")
                                                                                                                                                                                      HS.pure
                                                                                                                                                                                        (ContentStreamOp_showTextIndGlyph
                                                                                                                                                                                           _57)))
                                                                                                                                                                               ((RTS.|||)
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "setTextLeading"
                                                                                                                                                                                     (do (_58
                                                                                                                                                                                            :: ()) <-
                                                                                                                                                                                           HS.const
                                                                                                                                                                                             ()
                                                                                                                                                                                             HS.<$> RTS.pMatch
                                                                                                                                                                                                      "71:21--71:24"
                                                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                                                         "TL")
                                                                                                                                                                                         HS.pure
                                                                                                                                                                                           (ContentStreamOp_setTextLeading
                                                                                                                                                                                              _58)))
                                                                                                                                                                                  ((RTS.|||)
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "setTextMatrix"
                                                                                                                                                                                        (do (_59
                                                                                                                                                                                               :: ()) <-
                                                                                                                                                                                              HS.const
                                                                                                                                                                                                ()
                                                                                                                                                                                                HS.<$> RTS.pMatch
                                                                                                                                                                                                         "72:20--72:23"
                                                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                                                            "Tm")
                                                                                                                                                                                            HS.pure
                                                                                                                                                                                              (ContentStreamOp_setTextMatrix
                                                                                                                                                                                                 _59)))
                                                                                                                                                                                     ((RTS.|||)
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "setTextRendering"
                                                                                                                                                                                           (do (_60
                                                                                                                                                                                                  :: ()) <-
                                                                                                                                                                                                 HS.const
                                                                                                                                                                                                   ()
                                                                                                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                                                                                                            "73:23--73:26"
                                                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                                                               "Tr")
                                                                                                                                                                                               HS.pure
                                                                                                                                                                                                 (ContentStreamOp_setTextRendering
                                                                                                                                                                                                    _60)))
                                                                                                                                                                                        ((RTS.|||)
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "setTextRise"
                                                                                                                                                                                              (do (_61
                                                                                                                                                                                                     :: ()) <-
                                                                                                                                                                                                    HS.const
                                                                                                                                                                                                      ()
                                                                                                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                                                                                                               "74:18--74:21"
                                                                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                                                                  "Ts")
                                                                                                                                                                                                  HS.pure
                                                                                                                                                                                                    (ContentStreamOp_setTextRise
                                                                                                                                                                                                       _61)))
                                                                                                                                                                                           ((RTS.|||)
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "setWordSpacing"
                                                                                                                                                                                                 (do (_62
                                                                                                                                                                                                        :: ()) <-
                                                                                                                                                                                                       HS.const
                                                                                                                                                                                                         ()
                                                                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                                                                  "75:21--75:24"
                                                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                                                     "Tw")
                                                                                                                                                                                                     HS.pure
                                                                                                                                                                                                       (ContentStreamOp_setWordSpacing
                                                                                                                                                                                                          _62)))
                                                                                                                                                                                              ((RTS.|||)
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "setHorizontalTextScaling"
                                                                                                                                                                                                    (do (_63
                                                                                                                                                                                                           :: ()) <-
                                                                                                                                                                                                          HS.const
                                                                                                                                                                                                            ()
                                                                                                                                                                                                            HS.<$> RTS.pMatch
                                                                                                                                                                                                                     "76:31--76:34"
                                                                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                                                                        "Tz")
                                                                                                                                                                                                        HS.pure
                                                                                                                                                                                                          (ContentStreamOp_setHorizontalTextScaling
                                                                                                                                                                                                             _63)))
                                                                                                                                                                                                 ((RTS.|||)
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "appendCurvedInitPtRepl"
                                                                                                                                                                                                       (do (_64
                                                                                                                                                                                                              :: ()) <-
                                                                                                                                                                                                             HS.const
                                                                                                                                                                                                               ()
                                                                                                                                                                                                               HS.<$> RTS.pMatch
                                                                                                                                                                                                                        "77:29--77:31"
                                                                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                                                                           "v")
                                                                                                                                                                                                           HS.pure
                                                                                                                                                                                                             (ContentStreamOp_appendCurvedInitPtRepl
                                                                                                                                                                                                                _64)))
                                                                                                                                                                                                    ((RTS.|||)
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "setLineWidth"
                                                                                                                                                                                                          (do (_65
                                                                                                                                                                                                                 :: ()) <-
                                                                                                                                                                                                                HS.const
                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                                                                                                                           "78:19--78:21"
                                                                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                                                                              "w")
                                                                                                                                                                                                              HS.pure
                                                                                                                                                                                                                (ContentStreamOp_setLineWidth
                                                                                                                                                                                                                   _65)))
                                                                                                                                                                                                       ((RTS.|||)
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "setClippingNzWinding"
                                                                                                                                                                                                             (do (_66
                                                                                                                                                                                                                    :: ()) <-
                                                                                                                                                                                                                   HS.const
                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                                                                                                                              "79:27--79:29"
                                                                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                                                                 "W")
                                                                                                                                                                                                                 HS.pure
                                                                                                                                                                                                                   (ContentStreamOp_setClippingNzWinding
                                                                                                                                                                                                                      _66)))
                                                                                                                                                                                                          ((RTS.|||)
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "setClippingEvenOdd"
                                                                                                                                                                                                                (do (_67
                                                                                                                                                                                                                       :: ()) <-
                                                                                                                                                                                                                      HS.const
                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                                                                                                                 "80:25--80:28"
                                                                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                                                                    "W*")
                                                                                                                                                                                                                    HS.pure
                                                                                                                                                                                                                      (ContentStreamOp_setClippingEvenOdd
                                                                                                                                                                                                                         _67)))
                                                                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "appendCurvedFinalPt"
                                                                                                                                                                                                                   (do (_68
                                                                                                                                                                                                                          :: ()) <-
                                                                                                                                                                                                                         HS.const
                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                                                                                                                    "81:26--81:28"
                                                                                                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                                                                                                       "y")
                                                                                                                                                                                                                       HS.pure
                                                                                                                                                                                                                         (ContentStreamOp_appendCurvedFinalPt
                                                                                                                                                                                                                            _68)))
                                                                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "moveShow"
                                                                                                                                                                                                                      (do (_69
                                                                                                                                                                                                                             :: ()) <-
                                                                                                                                                                                                                            HS.const
                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                                                                                                       "82:15--82:17"
                                                                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                                                                          "'")
                                                                                                                                                                                                                          HS.pure
                                                                                                                                                                                                                            (ContentStreamOp_moveShow
                                                                                                                                                                                                                               _69)))
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "setSpacing"
                                                                                                                                                                                                                      (do (_70
                                                                                                                                                                                                                             :: ()) <-
                                                                                                                                                                                                                            HS.const
                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                                                                                                       "83:17--83:20"
                                                                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                                                                          "\"")
                                                                                                                                                                                                                          HS.pure
                                                                                                                                                                                                                            (ContentStreamOp_setSpacing
                                                                                                                                                                                                                               _70))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
pOperationObj :: D.Parser ContentStreamOp
 
pOperationObj =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @ContentStreamOp
       (RTS.pEnter "PdfText.ContentStreamOp" pContentStreamOp))
 
_BeginCompat :: D.Parser ()
 
_BeginCompat =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @(Vector.Vector (RTS.UInt 8))
       (HS.const ()
          HS.<$> RTS.pMatch "90:25--90:28" (Vector.vecFromRep "BX")))
 
_OpName :: D.Parser ()
 
_OpName =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @(Vector.Vector (RTS.UInt 8))
       (RTS.pSkipMany (RTS.<||)
          (RTS.pEnter "PdfValue._NameChar" PdfValue._NameChar)))
 
pContentStream :: D.Parser (Vector.Vector ContentStream_0)
 
pContentStream =
  do (__ :: Vector.Vector ContentStream_0) <-
       RTS.pMany (RTS.<||)
         (do (__ :: ContentStream_0) <-
               (RTS.|||)
                 (RTS.pEnter "operand"
                    (do (_71 :: PdfValue.Value) <-
                          RTS.pEnter "PdfValue.Value" PdfValue.pValue
                        HS.pure (ContentStream_0_operand _71)))
                 ((RTS.|||)
                    (RTS.pEnter "operation"
                       (do (_72 :: ContentStreamOp) <-
                             RTS.pEnter "PdfText.OperationObj" pOperationObj
                           HS.pure (ContentStream_0_operation _72)))
                    (RTS.pEnter "compatSect"
                       (do (_73 :: Vector.Vector (RTS.UInt 8)) <-
                             do RTS.pEnter "PdfText._BeginCompat" _BeginCompat
                                RTS.pSkipMany (RTS.<||)
                                  ((RTS.|||) (RTS.pEnter "PdfValue._Value" PdfValue._Value)
                                     (RTS.pEnter "PdfText._OpName" _OpName))
                                (__ :: Vector.Vector (RTS.UInt 8)) <-
                                  RTS.pEnter "PdfText.EndCompat" pEndCompat
                                HS.pure __
                           HS.pure (ContentStream_0_compatSect _73))))
             HS.pure __)
     HS.pure __
 
pDay :: D.Parser HS.Integer
 
pDay =
  RTS.pEnter "PdfText.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 1 :: HS.Integer)
       (RTS.lit 31 :: HS.Integer))
 
pHour :: D.Parser HS.Integer
 
pHour =
  RTS.pEnter "PdfText.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
pMinute :: D.Parser HS.Integer
 
pMinute =
  RTS.pEnter "PdfText.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
pMonth :: D.Parser HS.Integer
 
pMonth =
  RTS.pEnter "PdfText.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 1 :: HS.Integer)
       (RTS.lit 12 :: HS.Integer))
 
pSecond :: D.Parser HS.Integer
 
pSecond =
  RTS.pEnter "PdfText.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
pDate :: D.Parser Date
 
pDate =
  do HS.const ()
       HS.<$> RTS.pMatch "201:5--201:8" (Vector.vecFromRep "D:")
     (year :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 4 :: HS.Integer)
         (RTS.pEnter "PdfValue.Digit" PdfValue.pDigit)
     (month :: HS.Integer) <- RTS.pEnter "PdfText.Month" pMonth
     (day :: HS.Integer) <- RTS.pEnter "PdfText.Day" pDay
     (hour :: HS.Integer) <- RTS.pEnter "PdfText.Hour" pHour
     (minute :: HS.Integer) <- RTS.pEnter "PdfText.Minute" pMinute
     (second :: HS.Integer) <- RTS.pEnter "PdfText.Second" pSecond
     (relLocalTime :: Date_0) <-
       (RTS.|||)
         (RTS.pEnter "plus"
            (do (_74 :: ()) <-
                  HS.const ()
                    HS.<$> RTS.pMatch1 "209:15--209:17" (RTS.bcSingle (RTS.uint8 43))
                HS.pure (Date_0_plus _74)))
         ((RTS.|||)
            (RTS.pEnter "minus"
               (do (_75 :: ()) <-
                     HS.const ()
                       HS.<$> RTS.pMatch1 "210:16--210:18" (RTS.bcSingle (RTS.uint8 45))
                   HS.pure (Date_0_minus _75)))
            (RTS.pEnter "bigZ"
               (do (_76 :: ()) <-
                     HS.const ()
                       HS.<$> RTS.pMatch1 "211:15--211:17" (RTS.bcSingle (RTS.uint8 90))
                   HS.pure (Date_0_bigZ _76))))
     (utOffsetHours :: HS.Integer) <- RTS.pEnter "PdfText.Hour" pHour
     HS.const ()
       HS.<$> RTS.pMatch1 "214:5--214:8" (RTS.bcSingle (RTS.uint8 39))
     (utOffsetMins :: HS.Integer) <- RTS.pEnter "PdfText.Minute" pMinute
     HS.pure
       (Date year month day hour minute second relLocalTime utOffsetHours
          utOffsetMins)
 
pHighSurrogate :: D.Parser (RTS.UInt 8)
 
pHighSurrogate =
  do HS.const ()
       HS.<$> RTS.pMatch1 "127:3--127:14"
                (RTS.bcRange (RTS.lit 216 :: RTS.UInt 8)
                   (RTS.lit 219 :: RTS.UInt 8))
     (__ :: RTS.UInt 8) <- RTS.pEnter "PdfText.Byte" pByte
     HS.pure __
 
pLowSurrogate :: D.Parser (RTS.UInt 8)
 
pLowSurrogate =
  do HS.const ()
       HS.<$> RTS.pMatch1 "132:3--132:14"
                (RTS.bcRange (RTS.lit 220 :: RTS.UInt 8)
                   (RTS.lit 223 :: RTS.UInt 8))
     (__ :: RTS.UInt 8) <- RTS.pEnter "PdfText.Byte" pByte
     HS.pure __
 
pOpName :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pOpName =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @(Vector.Vector (RTS.UInt 8))
       (do (__ :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMany (RTS.<||)
               (RTS.pEnter "PdfValue.NameChar" PdfValue.pNameChar)
           HS.pure __))
 
_Byte :: D.Parser ()
 
_Byte =
  HS.const ()
    HS.<$> RTS.pMatch1 "121:12--121:19"
             (RTS.bcRange (RTS.lit 0 :: RTS.UInt 8) (RTS.lit 255 :: RTS.UInt 8))
 
_UFEFF :: D.Parser ()
 
_UFEFF =
  do HS.const ()
       HS.<$> RTS.pMatch1 "163:15--163:17" (RTS.bcSingle (RTS.uint8 254))
     HS.const ()
       HS.<$> RTS.pMatch1 "163:21--163:23" (RTS.bcSingle (RTS.uint8 255))
 
_BmpByte :: D.Parser ()
 
_BmpByte =
  (RTS.|||)
    (HS.const ()
       HS.<$> RTS.pMatch1 "137:15--137:26"
                (RTS.bcRange (RTS.lit 0 :: RTS.UInt 8)
                   (RTS.lit 215 :: RTS.UInt 8)))
    (HS.const ()
       HS.<$> RTS.pMatch1 "137:30--137:41"
                (RTS.bcRange (RTS.lit 224 :: RTS.UInt 8)
                   (RTS.lit 255 :: RTS.UInt 8)))
 
_HighSurrogate :: D.Parser ()
 
_HighSurrogate =
  do HS.const ()
       HS.<$> RTS.pMatch1 "127:3--127:14"
                (RTS.bcRange (RTS.lit 216 :: RTS.UInt 8)
                   (RTS.lit 219 :: RTS.UInt 8))
     RTS.pEnter "PdfText._Byte" _Byte
 
_LowSurrogate :: D.Parser ()
 
_LowSurrogate =
  do HS.const ()
       HS.<$> RTS.pMatch1 "132:3--132:14"
                (RTS.bcRange (RTS.lit 220 :: RTS.UInt 8)
                   (RTS.lit 223 :: RTS.UInt 8))
     RTS.pEnter "PdfText._Byte" _Byte
 
_U001B :: D.Parser ()
 
_U001B =
  do HS.const ()
       HS.<$> RTS.pMatch1 "124:15--124:15" (RTS.bcSingle (RTS.uint8 0))
     HS.const ()
       HS.<$> RTS.pMatch1 "124:19--124:20" (RTS.bcSingle (RTS.uint8 27))
 
_UniChar :: D.Parser ()
 
_UniChar =
  (RTS.<||)
    (RTS.pEnter "escape"
       (do RTS.pEnter "PdfText._U001B" _U001B
           RTS.pEnter "ISOCodes._LanguageCode" ISOCodes._LanguageCode
           (RTS.<||)
             (RTS.pEnter "ISOCodes._CountryCode" ISOCodes._CountryCode)
             (HS.pure ())
           RTS.pEnter "PdfText._U001B" _U001B))
    ((RTS.<||)
       (RTS.pEnter "supplementary"
          (do RTS.pEnter "PdfText._HighSurrogate" _HighSurrogate
              RTS.pEnter "PdfText._LowSurrogate" _LowSurrogate))
       (RTS.pEnter "twoByte"
          (do RTS.pEnter "PdfText._BmpByte" _BmpByte
              RTS.pEnter "PdfText._BmpByte" _BmpByte)))
 
pTextObj :: D.Parser ()
 
pTextObj =
  do (RTS.<||)
       (RTS.pEnter "isUnicode"
          (do RTS.pEnter "PdfText._UFEFF" _UFEFF
              RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfText._UniChar" _UniChar)))
       (RTS.pEnter "isPdfDoc"
          (RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfText._Byte" _Byte)))
     (__ :: ()) <- RTS.pEnd "178:3--178:5"
     HS.pure __
 
pU001B :: D.Parser (RTS.UInt 8)
 
pU001B =
  do HS.const ()
       HS.<$> RTS.pMatch1 "124:15--124:15" (RTS.bcSingle (RTS.uint8 0))
     (__ :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "124:19--124:20" (RTS.bcSingle (RTS.uint8 27))
     HS.pure __
 
pUFEFF :: D.Parser (RTS.UInt 8)
 
pUFEFF =
  do HS.const ()
       HS.<$> RTS.pMatch1 "163:15--163:17" (RTS.bcSingle (RTS.uint8 254))
     (__ :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "163:21--163:23" (RTS.bcSingle (RTS.uint8 255))
     HS.pure __
 
pUniChar :: D.Parser UniChar
 
pUniChar =
  (RTS.<||)
    (RTS.pEnter "escape"
       (do (_77 :: UniChar_0) <-
             do RTS.pEnter "PdfText._U001B" _U001B
                (lang :: ISOCodes.LanguageCode) <-
                  RTS.pEnter "ISOCodes.LanguageCode" ISOCodes.pLanguageCode
                (ctry :: HS.Maybe ISOCodes.CountryCode) <-
                  RTS.pOptional (RTS.<||) HS.Just
                    (RTS.pEnter "ISOCodes.CountryCode" ISOCodes.pCountryCode)
                RTS.pEnter "PdfText._U001B" _U001B
                HS.pure (UniChar_0 lang ctry)
           HS.pure (UniChar_escape _77)))
    ((RTS.<||)
       (RTS.pEnter "supplementary"
          (do (_78 :: UniChar_1) <-
                do (big :: RTS.UInt 8) <-
                     RTS.pEnter "PdfText.HighSurrogate" pHighSurrogate
                   (small :: RTS.UInt 8) <-
                     RTS.pEnter "PdfText.LowSurrogate" pLowSurrogate
                   HS.pure (UniChar_1 big small)
              HS.pure (UniChar_supplementary _78)))
       (RTS.pEnter "twoByte"
          (do (_79 :: UniChar_2) <-
                do (big :: RTS.UInt 8) <- RTS.pEnter "PdfText.BmpByte" pBmpByte
                   (small :: RTS.UInt 8) <- RTS.pEnter "PdfText.BmpByte" pBmpByte
                   HS.pure (UniChar_2 big small)
              HS.pure (UniChar_twoByte _79))))
 
_BoundedTwoDigits :: HS.Integer -> (HS.Integer -> D.Parser ())
 
_BoundedTwoDigits (lb :: HS.Integer) (ub :: HS.Integer) =
  do (digs :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 2 :: HS.Integer)
         (RTS.pEnter "PdfValue.Digit" PdfValue.pDigit)
     (__ :: HS.Integer) <-
       HS.pure
         (PdfValue.numBase @(Vector.Vector HS.Integer) @HS.Integer
            @HS.Integer
            (RTS.lit 10 :: HS.Integer)
            digs)
     RTS.pGuard "187:3--187:10" "guard failed" (lb HS.<= __)
     RTS.pGuard "187:14--187:21" "guard failed" (__ HS.<= ub)
 
_EndCompat :: D.Parser ()
 
_EndCompat =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @(Vector.Vector (RTS.UInt 8))
       (HS.const ()
          HS.<$> RTS.pMatch "93:23--93:26" (Vector.vecFromRep "EX")))
 
_ContentStreamOp :: D.Parser ()
 
_ContentStreamOp =
  (RTS.|||)
    (RTS.pEnter "closeFillStrokeNzWinding"
       (HS.const ()
          HS.<$> RTS.pMatch "13:31--13:33" (Vector.vecFromRep "b")))
    ((RTS.|||)
       (RTS.pEnter "fillStroke"
          (HS.const ()
             HS.<$> RTS.pMatch "14:17--14:19" (Vector.vecFromRep "B")))
       ((RTS.|||)
          (RTS.pEnter "closeFillStrokeEvenOdd"
             (HS.const ()
                HS.<$> RTS.pMatch "15:29--15:32" (Vector.vecFromRep "b*")))
          ((RTS.|||)
             (RTS.pEnter "fillStrokeEvenOdd"
                (HS.const ()
                   HS.<$> RTS.pMatch "16:24--16:27" (Vector.vecFromRep "B*")))
             ((RTS.|||)
                (RTS.pEnter "beginMarkedContent"
                   (HS.const ()
                      HS.<$> RTS.pMatch "17:25--17:29" (Vector.vecFromRep "BDC")))
                ((RTS.|||)
                   (RTS.pEnter "beginInline"
                      (HS.const ()
                         HS.<$> RTS.pMatch "18:18--18:21" (Vector.vecFromRep "Bl")))
                   ((RTS.|||)
                      (RTS.pEnter "beginMarkedContent"
                         (HS.const ()
                            HS.<$> RTS.pMatch "19:25--19:29" (Vector.vecFromRep "BMC")))
                      ((RTS.|||)
                         (RTS.pEnter "beginText"
                            (HS.const ()
                               HS.<$> RTS.pMatch "20:16--20:19" (Vector.vecFromRep "BT")))
                         ((RTS.|||)
                            (RTS.pEnter "appendCurvedThreePoints"
                               (HS.const ()
                                  HS.<$> RTS.pMatch "21:30--21:32" (Vector.vecFromRep "c")))
                            ((RTS.|||)
                               (RTS.pEnter "concatMatrix"
                                  (HS.const ()
                                     HS.<$> RTS.pMatch "22:19--22:22" (Vector.vecFromRep "cm")))
                               ((RTS.|||)
                                  (RTS.pEnter "setColorSpaceStroking"
                                     (HS.const ()
                                        HS.<$> RTS.pMatch "23:28--23:31" (Vector.vecFromRep "CS")))
                                  ((RTS.|||)
                                     (RTS.pEnter "setColorSpaceNonStroking"
                                        (HS.const ()
                                           HS.<$> RTS.pMatch "24:31--24:34"
                                                    (Vector.vecFromRep "cs")))
                                     ((RTS.|||)
                                        (RTS.pEnter "setLineDash"
                                           (HS.const ()
                                              HS.<$> RTS.pMatch "25:18--25:20"
                                                       (Vector.vecFromRep "d")))
                                        ((RTS.|||)
                                           (RTS.pEnter "setGlyphWidth"
                                              (HS.const ()
                                                 HS.<$> RTS.pMatch "26:20--26:23"
                                                          (Vector.vecFromRep "d0")))
                                           ((RTS.|||)
                                              (RTS.pEnter "setGlpyhWidthBoundingBox"
                                                 (HS.const ()
                                                    HS.<$> RTS.pMatch "27:31--27:34"
                                                             (Vector.vecFromRep "d1")))
                                              ((RTS.|||)
                                                 (RTS.pEnter "invokeXObj"
                                                    (HS.const ()
                                                       HS.<$> RTS.pMatch "28:17--28:20"
                                                                (Vector.vecFromRep "Do")))
                                                 ((RTS.|||)
                                                    (RTS.pEnter "defMarkedContentPoint"
                                                       (HS.const ()
                                                          HS.<$> RTS.pMatch "29:28--29:31"
                                                                   (Vector.vecFromRep "DP")))
                                                    ((RTS.|||)
                                                       (RTS.pEnter "endInline"
                                                          (HS.const ()
                                                             HS.<$> RTS.pMatch "30:16--30:19"
                                                                      (Vector.vecFromRep "El")))
                                                       ((RTS.|||)
                                                          (RTS.pEnter "endMarkedContent"
                                                             (HS.const ()
                                                                HS.<$> RTS.pMatch "31:23--31:27"
                                                                         (Vector.vecFromRep "EMC")))
                                                          ((RTS.|||)
                                                             (RTS.pEnter "endTextObj"
                                                                (HS.const ()
                                                                   HS.<$> RTS.pMatch "32:17--32:20"
                                                                            (Vector.vecFromRep
                                                                               "ET")))
                                                             ((RTS.|||)
                                                                (RTS.pEnter "fillPathNzWinding"
                                                                   (HS.const ()
                                                                      HS.<$> RTS.pMatch
                                                                               "33:24--33:26"
                                                                               (Vector.vecFromRep
                                                                                  "f")))
                                                                ((RTS.|||)
                                                                   (RTS.pEnter
                                                                      "fillPathNzWindingOld"
                                                                      (HS.const ()
                                                                         HS.<$> RTS.pMatch
                                                                                  "34:27--34:29"
                                                                                  (Vector.vecFromRep
                                                                                     "F")))
                                                                   ((RTS.|||)
                                                                      (RTS.pEnter "fillPathEvenOdd"
                                                                         (HS.const ()
                                                                            HS.<$> RTS.pMatch
                                                                                     "35:22--35:25"
                                                                                     (Vector.vecFromRep
                                                                                        "f*")))
                                                                      ((RTS.|||)
                                                                         (RTS.pEnter
                                                                            "setGrayStroking"
                                                                            (HS.const ()
                                                                               HS.<$> RTS.pMatch
                                                                                        "36:22--36:24"
                                                                                        (Vector.vecFromRep
                                                                                           "G")))
                                                                         ((RTS.|||)
                                                                            (RTS.pEnter
                                                                               "setGrayNonStroking"
                                                                               (HS.const ()
                                                                                  HS.<$> RTS.pMatch
                                                                                           "37:25--37:27"
                                                                                           (Vector.vecFromRep
                                                                                              "g")))
                                                                            ((RTS.|||)
                                                                               (RTS.pEnter
                                                                                  "setGraphicsStateParams"
                                                                                  (HS.const ()
                                                                                     HS.<$> RTS.pMatch
                                                                                              "38:29--38:32"
                                                                                              (Vector.vecFromRep
                                                                                                 "gs")))
                                                                               ((RTS.|||)
                                                                                  (RTS.pEnter
                                                                                     "closeSubpath"
                                                                                     (HS.const ()
                                                                                        HS.<$> RTS.pMatch
                                                                                                 "39:19--39:21"
                                                                                                 (Vector.vecFromRep
                                                                                                    "h")))
                                                                                  ((RTS.|||)
                                                                                     (RTS.pEnter
                                                                                        "setFlat"
                                                                                        (HS.const ()
                                                                                           HS.<$> RTS.pMatch
                                                                                                    "40:14--40:16"
                                                                                                    (Vector.vecFromRep
                                                                                                       "i")))
                                                                                     ((RTS.|||)
                                                                                        (RTS.pEnter
                                                                                           "beginInlineImageData"
                                                                                           (HS.const
                                                                                              ()
                                                                                              HS.<$> RTS.pMatch
                                                                                                       "41:27--41:30"
                                                                                                       (Vector.vecFromRep
                                                                                                          "ID")))
                                                                                        ((RTS.|||)
                                                                                           (RTS.pEnter
                                                                                              "setLineJoinStyle"
                                                                                              (HS.const
                                                                                                 ()
                                                                                                 HS.<$> RTS.pMatch
                                                                                                          "42:23--42:25"
                                                                                                          (Vector.vecFromRep
                                                                                                             "j")))
                                                                                           ((RTS.|||)
                                                                                              (RTS.pEnter
                                                                                                 "setLineCapStyle"
                                                                                                 (HS.const
                                                                                                    ()
                                                                                                    HS.<$> RTS.pMatch
                                                                                                             "43:22--43:24"
                                                                                                             (Vector.vecFromRep
                                                                                                                "J")))
                                                                                              ((RTS.|||)
                                                                                                 (RTS.pEnter
                                                                                                    "setCMYKStroking"
                                                                                                    (HS.const
                                                                                                       ()
                                                                                                       HS.<$> RTS.pMatch
                                                                                                                "44:22--44:24"
                                                                                                                (Vector.vecFromRep
                                                                                                                   "K")))
                                                                                                 ((RTS.|||)
                                                                                                    (RTS.pEnter
                                                                                                       "setCMYKNonStroking"
                                                                                                       (HS.const
                                                                                                          ()
                                                                                                          HS.<$> RTS.pMatch
                                                                                                                   "45:25--45:27"
                                                                                                                   (Vector.vecFromRep
                                                                                                                      "k")))
                                                                                                    ((RTS.|||)
                                                                                                       (RTS.pEnter
                                                                                                          "appendLine"
                                                                                                          (HS.const
                                                                                                             ()
                                                                                                             HS.<$> RTS.pMatch
                                                                                                                      "46:17--46:19"
                                                                                                                      (Vector.vecFromRep
                                                                                                                         "l")))
                                                                                                       ((RTS.|||)
                                                                                                          (RTS.pEnter
                                                                                                             "beginNewSuppath"
                                                                                                             (HS.const
                                                                                                                ()
                                                                                                                HS.<$> RTS.pMatch
                                                                                                                         "47:22--47:24"
                                                                                                                         (Vector.vecFromRep
                                                                                                                            "m")))
                                                                                                          ((RTS.|||)
                                                                                                             (RTS.pEnter
                                                                                                                "setMiterLimit"
                                                                                                                (HS.const
                                                                                                                   ()
                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                            "48:20--48:22"
                                                                                                                            (Vector.vecFromRep
                                                                                                                               "M")))
                                                                                                             ((RTS.|||)
                                                                                                                (RTS.pEnter
                                                                                                                   "defineMarkedContent"
                                                                                                                   (HS.const
                                                                                                                      ()
                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                               "49:26--49:29"
                                                                                                                               (Vector.vecFromRep
                                                                                                                                  "MP")))
                                                                                                                ((RTS.|||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "endPath"
                                                                                                                      (HS.const
                                                                                                                         ()
                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                  "50:14--50:16"
                                                                                                                                  (Vector.vecFromRep
                                                                                                                                     "n")))
                                                                                                                   ((RTS.|||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "saveGraphicsState"
                                                                                                                         (HS.const
                                                                                                                            ()
                                                                                                                            HS.<$> RTS.pMatch
                                                                                                                                     "51:24--51:26"
                                                                                                                                     (Vector.vecFromRep
                                                                                                                                        "q")))
                                                                                                                      ((RTS.|||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "restoreGraphicsState"
                                                                                                                            (HS.const
                                                                                                                               ()
                                                                                                                               HS.<$> RTS.pMatch
                                                                                                                                        "52:27--52:29"
                                                                                                                                        (Vector.vecFromRep
                                                                                                                                           "Q")))
                                                                                                                         ((RTS.|||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "appendRect"
                                                                                                                               (HS.const
                                                                                                                                  ()
                                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                                           "53:17--53:20"
                                                                                                                                           (Vector.vecFromRep
                                                                                                                                              "re")))
                                                                                                                            ((RTS.|||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "setRGBStroking"
                                                                                                                                  (HS.const
                                                                                                                                     ()
                                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                                              "54:21--54:24"
                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                 "RG")))
                                                                                                                               ((RTS.|||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "setRGBNonStroking"
                                                                                                                                     (HS.const
                                                                                                                                        ()
                                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                                 "55:24--55:27"
                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                    "rg")))
                                                                                                                                  ((RTS.|||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "setColorRenderingIntent"
                                                                                                                                        (HS.const
                                                                                                                                           ()
                                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                                    "56:30--56:33"
                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                       "ri")))
                                                                                                                                     ((RTS.|||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "closeStrokePath"
                                                                                                                                           (HS.const
                                                                                                                                              ()
                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                       "57:22--57:24"
                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                          "s")))
                                                                                                                                        ((RTS.|||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "stroke"
                                                                                                                                              (HS.const
                                                                                                                                                 ()
                                                                                                                                                 HS.<$> RTS.pMatch
                                                                                                                                                          "58:13--58:15"
                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                             "S")))
                                                                                                                                           ((RTS.|||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "setColorStroking"
                                                                                                                                                 (HS.const
                                                                                                                                                    ()
                                                                                                                                                    HS.<$> RTS.pMatch
                                                                                                                                                             "59:23--59:26"
                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                "SC")))
                                                                                                                                              ((RTS.|||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "setColorNonStroking"
                                                                                                                                                    (HS.const
                                                                                                                                                       ()
                                                                                                                                                       HS.<$> RTS.pMatch
                                                                                                                                                                "60:26--60:29"
                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                   "sc")))
                                                                                                                                                 ((RTS.|||)
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "setColorStrokingICC"
                                                                                                                                                       (HS.const
                                                                                                                                                          ()
                                                                                                                                                          HS.<$> RTS.pMatch
                                                                                                                                                                   "61:26--61:30"
                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                      "SCN")))
                                                                                                                                                    ((RTS.|||)
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "setColorNonStrokingICC"
                                                                                                                                                          (HS.const
                                                                                                                                                             ()
                                                                                                                                                             HS.<$> RTS.pMatch
                                                                                                                                                                      "62:29--62:33"
                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                         "scn")))
                                                                                                                                                       ((RTS.|||)
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "paintShadingPattern"
                                                                                                                                                             (HS.const
                                                                                                                                                                ()
                                                                                                                                                                HS.<$> RTS.pMatch
                                                                                                                                                                         "63:26--63:29"
                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                            "sh")))
                                                                                                                                                          ((RTS.|||)
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "moveStartText"
                                                                                                                                                                (HS.const
                                                                                                                                                                   ()
                                                                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                                                                            "64:20--64:23"
                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                               "T*")))
                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "setCharSpacing"
                                                                                                                                                                   (HS.const
                                                                                                                                                                      ()
                                                                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                                                                               "65:21--65:24"
                                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                                  "Tc")))
                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "moveTextPos"
                                                                                                                                                                      (HS.const
                                                                                                                                                                         ()
                                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                                  "66:18--66:21"
                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                     "Td")))
                                                                                                                                                                   ((RTS.|||)
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "moveTextPosSetLeading"
                                                                                                                                                                         (HS.const
                                                                                                                                                                            ()
                                                                                                                                                                            HS.<$> RTS.pMatch
                                                                                                                                                                                     "67:28--67:31"
                                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                                        "TD")))
                                                                                                                                                                      ((RTS.|||)
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "setTextFont"
                                                                                                                                                                            (HS.const
                                                                                                                                                                               ()
                                                                                                                                                                               HS.<$> RTS.pMatch
                                                                                                                                                                                        "68:18--68:21"
                                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                                           "Tf")))
                                                                                                                                                                         ((RTS.|||)
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "showText"
                                                                                                                                                                               (HS.const
                                                                                                                                                                                  ()
                                                                                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                                                                                           "69:15--69:18"
                                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                                              "Tj")))
                                                                                                                                                                            ((RTS.|||)
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "showTextIndGlyph"
                                                                                                                                                                                  (HS.const
                                                                                                                                                                                     ()
                                                                                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                                                                                              "70:23--70:26"
                                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                                 "TJ")))
                                                                                                                                                                               ((RTS.|||)
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "setTextLeading"
                                                                                                                                                                                     (HS.const
                                                                                                                                                                                        ()
                                                                                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                                                                                 "71:21--71:24"
                                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                                    "TL")))
                                                                                                                                                                                  ((RTS.|||)
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "setTextMatrix"
                                                                                                                                                                                        (HS.const
                                                                                                                                                                                           ()
                                                                                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                                                                                    "72:20--72:23"
                                                                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                                                                       "Tm")))
                                                                                                                                                                                     ((RTS.|||)
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "setTextRendering"
                                                                                                                                                                                           (HS.const
                                                                                                                                                                                              ()
                                                                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                                                                       "73:23--73:26"
                                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                                          "Tr")))
                                                                                                                                                                                        ((RTS.|||)
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "setTextRise"
                                                                                                                                                                                              (HS.const
                                                                                                                                                                                                 ()
                                                                                                                                                                                                 HS.<$> RTS.pMatch
                                                                                                                                                                                                          "74:18--74:21"
                                                                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                                                                             "Ts")))
                                                                                                                                                                                           ((RTS.|||)
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "setWordSpacing"
                                                                                                                                                                                                 (HS.const
                                                                                                                                                                                                    ()
                                                                                                                                                                                                    HS.<$> RTS.pMatch
                                                                                                                                                                                                             "75:21--75:24"
                                                                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                                                                "Tw")))
                                                                                                                                                                                              ((RTS.|||)
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "setHorizontalTextScaling"
                                                                                                                                                                                                    (HS.const
                                                                                                                                                                                                       ()
                                                                                                                                                                                                       HS.<$> RTS.pMatch
                                                                                                                                                                                                                "76:31--76:34"
                                                                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                                                                   "Tz")))
                                                                                                                                                                                                 ((RTS.|||)
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "appendCurvedInitPtRepl"
                                                                                                                                                                                                       (HS.const
                                                                                                                                                                                                          ()
                                                                                                                                                                                                          HS.<$> RTS.pMatch
                                                                                                                                                                                                                   "77:29--77:31"
                                                                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                                                                      "v")))
                                                                                                                                                                                                    ((RTS.|||)
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "setLineWidth"
                                                                                                                                                                                                          (HS.const
                                                                                                                                                                                                             ()
                                                                                                                                                                                                             HS.<$> RTS.pMatch
                                                                                                                                                                                                                      "78:19--78:21"
                                                                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                                                                         "w")))
                                                                                                                                                                                                       ((RTS.|||)
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "setClippingNzWinding"
                                                                                                                                                                                                             (HS.const
                                                                                                                                                                                                                ()
                                                                                                                                                                                                                HS.<$> RTS.pMatch
                                                                                                                                                                                                                         "79:27--79:29"
                                                                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                                                                            "W")))
                                                                                                                                                                                                          ((RTS.|||)
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "setClippingEvenOdd"
                                                                                                                                                                                                                (HS.const
                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                                                                                                                            "80:25--80:28"
                                                                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                                                                               "W*")))
                                                                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "appendCurvedFinalPt"
                                                                                                                                                                                                                   (HS.const
                                                                                                                                                                                                                      ()
                                                                                                                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                                                                                                                               "81:26--81:28"
                                                                                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                                                                                  "y")))
                                                                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "moveShow"
                                                                                                                                                                                                                      (HS.const
                                                                                                                                                                                                                         ()
                                                                                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                                                                                  "82:15--82:17"
                                                                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                                                                     "'")))
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "setSpacing"
                                                                                                                                                                                                                      (HS.const
                                                                                                                                                                                                                         ()
                                                                                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                                                                                  "83:17--83:20"
                                                                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                                                                     "\""))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
_OperationObj :: D.Parser ()
 
_OperationObj =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @ContentStreamOp
       (RTS.pEnter "PdfText._ContentStreamOp" _ContentStreamOp))
 
_ContentStream :: D.Parser ()
 
_ContentStream =
  RTS.pSkipMany (RTS.<||)
    ((RTS.|||)
       (RTS.pEnter "operand"
          (RTS.pEnter "PdfValue._Value" PdfValue._Value))
       ((RTS.|||)
          (RTS.pEnter "operation"
             (RTS.pEnter "PdfText._OperationObj" _OperationObj))
          (RTS.pEnter "compatSect"
             (do RTS.pEnter "PdfText._BeginCompat" _BeginCompat
                 RTS.pSkipMany (RTS.<||)
                   ((RTS.|||) (RTS.pEnter "PdfValue._Value" PdfValue._Value)
                      (RTS.pEnter "PdfText._OpName" _OpName))
                 RTS.pEnter "PdfText._EndCompat" _EndCompat))))
 
_Day :: D.Parser ()
 
_Day =
  RTS.pEnter "PdfText._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 1 :: HS.Integer)
       (RTS.lit 31 :: HS.Integer))
 
_Hour :: D.Parser ()
 
_Hour =
  RTS.pEnter "PdfText._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
_Minute :: D.Parser ()
 
_Minute =
  RTS.pEnter "PdfText._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
_Month :: D.Parser ()
 
_Month =
  RTS.pEnter "PdfText._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 1 :: HS.Integer)
       (RTS.lit 12 :: HS.Integer))
 
_Second :: D.Parser ()
 
_Second =
  RTS.pEnter "PdfText._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
_Date :: D.Parser ()
 
_Date =
  do HS.const ()
       HS.<$> RTS.pMatch "201:5--201:8" (Vector.vecFromRep "D:")
     RTS.pSkipExact (RTS.lit 4 :: HS.Integer)
       (RTS.pEnter "PdfValue._Digit" PdfValue._Digit)
     RTS.pEnter "PdfText._Month" _Month
     RTS.pEnter "PdfText._Day" _Day
     RTS.pEnter "PdfText._Hour" _Hour
     RTS.pEnter "PdfText._Minute" _Minute
     RTS.pEnter "PdfText._Second" _Second
     (RTS.|||)
       (RTS.pEnter "plus"
          (HS.const ()
             HS.<$> RTS.pMatch1 "209:15--209:17" (RTS.bcSingle (RTS.uint8 43))))
       ((RTS.|||)
          (RTS.pEnter "minus"
             (HS.const ()
                HS.<$> RTS.pMatch1 "210:16--210:18" (RTS.bcSingle (RTS.uint8 45))))
          (RTS.pEnter "bigZ"
             (HS.const ()
                HS.<$> RTS.pMatch1 "211:15--211:17"
                         (RTS.bcSingle (RTS.uint8 90)))))
     RTS.pEnter "PdfText._Hour" _Hour
     HS.const ()
       HS.<$> RTS.pMatch1 "214:5--214:8" (RTS.bcSingle (RTS.uint8 39))
     RTS.pEnter "PdfText._Minute" _Minute
 
_TextObj :: D.Parser ()
 
_TextObj =
  do (RTS.<||)
       (RTS.pEnter "isUnicode"
          (do RTS.pEnter "PdfText._UFEFF" _UFEFF
              RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfText._UniChar" _UniChar)))
       (RTS.pEnter "isPdfDoc"
          (RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfText._Byte" _Byte)))
     RTS.pEnd "178:3--178:5"
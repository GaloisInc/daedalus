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
module PdfContentStream where
 
import qualified PdfMonad as D
import qualified PdfValue
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
 
data ContentOp
  = ContentOp_futureOp (Vector.Vector (RTS.UInt 8))
  | ContentOp_knownOp ContentStreamOp
  
 
deriving instance HS.Eq ContentOp
 
deriving instance HS.Ord ContentOp
 
deriving instance HS.Show ContentOp
 
instance RTS.DDL ContentOp where
 
instance HS.HasField "futureOp" ContentOp
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (ContentOp_futureOp x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "knownOp" ContentOp
           (HS.Maybe ContentStreamOp) where
  getField (ContentOp_knownOp x) = HS.Just x
   
  getField _ = HS.Nothing
 
data DirectObj
  = DirectObj_array (Vector.Vector PdfValue.Value)
  | DirectObj_bool HS.Bool
  | DirectObj_dict
      (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  | DirectObj_name (Vector.Vector (RTS.UInt 8))
  | DirectObj_null ()
  | DirectObj_number PdfValue.Number
  | DirectObj_string (Vector.Vector (RTS.UInt 8))
  
 
deriving instance HS.Eq DirectObj
 
deriving instance HS.Ord DirectObj
 
deriving instance HS.Show DirectObj
 
instance RTS.DDL DirectObj where
 
instance HS.HasField "array" DirectObj
           (HS.Maybe (Vector.Vector PdfValue.Value)) where
  getField (DirectObj_array x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "bool" DirectObj (HS.Maybe HS.Bool) where
  getField (DirectObj_bool x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "dict" DirectObj
           (HS.Maybe
              (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) where
  getField (DirectObj_dict x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "name" DirectObj
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (DirectObj_name x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "null" DirectObj (HS.Maybe ()) where
  getField (DirectObj_null x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "number" DirectObj
           (HS.Maybe PdfValue.Number) where
  getField (DirectObj_number x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "string" DirectObj
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (DirectObj_string x) = HS.Just x
   
  getField _ = HS.Nothing
 
data ContentStreamBody_0
  = ContentStreamBody_0_compatSect ()
  | ContentStreamBody_0_operand DirectObj
  | ContentStreamBody_0_operation ContentOp
  
 
deriving instance HS.Eq ContentStreamBody_0
 
deriving instance HS.Ord ContentStreamBody_0
 
deriving instance HS.Show ContentStreamBody_0
 
instance RTS.DDL ContentStreamBody_0 where
 
instance HS.HasField "compatSect" ContentStreamBody_0
           (HS.Maybe ()) where
  getField (ContentStreamBody_0_compatSect x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "operand" ContentStreamBody_0
           (HS.Maybe DirectObj) where
  getField (ContentStreamBody_0_operand x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "operation" ContentStreamBody_0
           (HS.Maybe ContentOp) where
  getField (ContentStreamBody_0_operation x) = HS.Just x
   
  getField _ = HS.Nothing
 
pBeginCompat :: D.Parser ()
 
pBeginCompat =
  RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "BX"))
 
pContentStreamOp :: D.Parser ContentStreamOp
 
pContentStreamOp =
  (RTS.<||)
    (RTS.pEnter "closeFillStrokeEvenOdd"
       (do (_0 :: ()) <-
             RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "b*"))
           HS.pure (ContentStreamOp_closeFillStrokeEvenOdd _0)))
    ((RTS.<||)
       (RTS.pEnter "closeFillStrokeNzWinding"
          (do (_1 :: ()) <-
                RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "b"))
              HS.pure (ContentStreamOp_closeFillStrokeNzWinding _1)))
       ((RTS.<||)
          (RTS.pEnter "fillStrokeEvenOdd"
             (do (_2 :: ()) <-
                   RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "B*"))
                 HS.pure (ContentStreamOp_fillStrokeEvenOdd _2)))
          ((RTS.<||)
             (RTS.pEnter "beginMarkedContent"
                (do (_3 :: ()) <-
                      RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "BDC"))
                    HS.pure (ContentStreamOp_beginMarkedContent _3)))
             ((RTS.<||)
                (RTS.pEnter "beginInline"
                   (do (_4 :: ()) <-
                         RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "Bl"))
                       HS.pure (ContentStreamOp_beginInline _4)))
                ((RTS.<||)
                   (RTS.pEnter "beginMarkedContent"
                      (do (_5 :: ()) <-
                            RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "BMC"))
                          HS.pure (ContentStreamOp_beginMarkedContent _5)))
                   ((RTS.<||)
                      (RTS.pEnter "beginText"
                         (do (_6 :: ()) <-
                               RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "BT"))
                             HS.pure (ContentStreamOp_beginText _6)))
                      ((RTS.<||)
                         (RTS.pEnter "fillStroke"
                            (do (_7 :: ()) <-
                                  RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "B"))
                                HS.pure (ContentStreamOp_fillStroke _7)))
                         ((RTS.<||)
                            (RTS.pEnter "concatMatrix"
                               (do (_8 :: ()) <-
                                     RTS.pEnter "PdfValue.KW"
                                       (PdfValue.pKW (Vector.vecFromRep "cm"))
                                   HS.pure (ContentStreamOp_concatMatrix _8)))
                            ((RTS.<||)
                               (RTS.pEnter "setColorSpaceStroking"
                                  (do (_9 :: ()) <-
                                        RTS.pEnter "PdfValue.KW"
                                          (PdfValue.pKW (Vector.vecFromRep "CS"))
                                      HS.pure (ContentStreamOp_setColorSpaceStroking _9)))
                               ((RTS.<||)
                                  (RTS.pEnter "setColorSpaceNonStroking"
                                     (do (_10 :: ()) <-
                                           RTS.pEnter "PdfValue.KW"
                                             (PdfValue.pKW (Vector.vecFromRep "cs"))
                                         HS.pure (ContentStreamOp_setColorSpaceNonStroking _10)))
                                  ((RTS.<||)
                                     (RTS.pEnter "appendCurvedThreePoints"
                                        (do (_11 :: ()) <-
                                              RTS.pEnter "PdfValue.KW"
                                                (PdfValue.pKW (Vector.vecFromRep "c"))
                                            HS.pure (ContentStreamOp_appendCurvedThreePoints _11)))
                                     ((RTS.<||)
                                        (RTS.pEnter "setGlyphWidth"
                                           (do (_12 :: ()) <-
                                                 RTS.pEnter "PdfValue.KW"
                                                   (PdfValue.pKW (Vector.vecFromRep "d0"))
                                               HS.pure (ContentStreamOp_setGlyphWidth _12)))
                                        ((RTS.<||)
                                           (RTS.pEnter "setGlpyhWidthBoundingBox"
                                              (do (_13 :: ()) <-
                                                    RTS.pEnter "PdfValue.KW"
                                                      (PdfValue.pKW (Vector.vecFromRep "d1"))
                                                  HS.pure
                                                    (ContentStreamOp_setGlpyhWidthBoundingBox _13)))
                                           ((RTS.<||)
                                              (RTS.pEnter "setLineDash"
                                                 (do (_14 :: ()) <-
                                                       RTS.pEnter "PdfValue.KW"
                                                         (PdfValue.pKW (Vector.vecFromRep "d"))
                                                     HS.pure (ContentStreamOp_setLineDash _14)))
                                              ((RTS.<||)
                                                 (RTS.pEnter "invokeXObj"
                                                    (do (_15 :: ()) <-
                                                          RTS.pEnter "PdfValue.KW"
                                                            (PdfValue.pKW (Vector.vecFromRep "Do"))
                                                        HS.pure (ContentStreamOp_invokeXObj _15)))
                                                 ((RTS.<||)
                                                    (RTS.pEnter "defMarkedContentPoint"
                                                       (do (_16 :: ()) <-
                                                             RTS.pEnter "PdfValue.KW"
                                                               (PdfValue.pKW
                                                                  (Vector.vecFromRep "DP"))
                                                           HS.pure
                                                             (ContentStreamOp_defMarkedContentPoint
                                                                _16)))
                                                    ((RTS.<||)
                                                       (RTS.pEnter "endInline"
                                                          (do (_17 :: ()) <-
                                                                RTS.pEnter "PdfValue.KW"
                                                                  (PdfValue.pKW
                                                                     (Vector.vecFromRep "El"))
                                                              HS.pure
                                                                (ContentStreamOp_endInline _17)))
                                                       ((RTS.<||)
                                                          (RTS.pEnter "endMarkedContent"
                                                             (do (_18 :: ()) <-
                                                                   RTS.pEnter "PdfValue.KW"
                                                                     (PdfValue.pKW
                                                                        (Vector.vecFromRep "EMC"))
                                                                 HS.pure
                                                                   (ContentStreamOp_endMarkedContent
                                                                      _18)))
                                                          ((RTS.<||)
                                                             (RTS.pEnter "endTextObj"
                                                                (do (_19 :: ()) <-
                                                                      RTS.pEnter "PdfValue.KW"
                                                                        (PdfValue.pKW
                                                                           (Vector.vecFromRep "ET"))
                                                                    HS.pure
                                                                      (ContentStreamOp_endTextObj
                                                                         _19)))
                                                             ((RTS.<||)
                                                                (RTS.pEnter "fillPathEvenOdd"
                                                                   (do (_20 :: ()) <-
                                                                         RTS.pEnter "PdfValue.KW"
                                                                           (PdfValue.pKW
                                                                              (Vector.vecFromRep
                                                                                 "f*"))
                                                                       HS.pure
                                                                         (ContentStreamOp_fillPathEvenOdd
                                                                            _20)))
                                                                ((RTS.<||)
                                                                   (RTS.pEnter "fillPathNzWinding"
                                                                      (do (_21 :: ()) <-
                                                                            RTS.pEnter "PdfValue.KW"
                                                                              (PdfValue.pKW
                                                                                 (Vector.vecFromRep
                                                                                    "f"))
                                                                          HS.pure
                                                                            (ContentStreamOp_fillPathNzWinding
                                                                               _21)))
                                                                   ((RTS.<||)
                                                                      (RTS.pEnter
                                                                         "fillPathNzWindingOld"
                                                                         (do (_22 :: ()) <-
                                                                               RTS.pEnter
                                                                                 "PdfValue.KW"
                                                                                 (PdfValue.pKW
                                                                                    (Vector.vecFromRep
                                                                                       "F"))
                                                                             HS.pure
                                                                               (ContentStreamOp_fillPathNzWindingOld
                                                                                  _22)))
                                                                      ((RTS.<||)
                                                                         (RTS.pEnter
                                                                            "setGrayStroking"
                                                                            (do (_23 :: ()) <-
                                                                                  RTS.pEnter
                                                                                    "PdfValue.KW"
                                                                                    (PdfValue.pKW
                                                                                       (Vector.vecFromRep
                                                                                          "G"))
                                                                                HS.pure
                                                                                  (ContentStreamOp_setGrayStroking
                                                                                     _23)))
                                                                         ((RTS.<||)
                                                                            (RTS.pEnter
                                                                               "setGraphicsStateParams"
                                                                               (do (_24 :: ()) <-
                                                                                     RTS.pEnter
                                                                                       "PdfValue.KW"
                                                                                       (PdfValue.pKW
                                                                                          (Vector.vecFromRep
                                                                                             "gs"))
                                                                                   HS.pure
                                                                                     (ContentStreamOp_setGraphicsStateParams
                                                                                        _24)))
                                                                            ((RTS.<||)
                                                                               (RTS.pEnter
                                                                                  "setGrayNonStroking"
                                                                                  (do (_25 :: ()) <-
                                                                                        RTS.pEnter
                                                                                          "PdfValue.KW"
                                                                                          (PdfValue.pKW
                                                                                             (Vector.vecFromRep
                                                                                                "g"))
                                                                                      HS.pure
                                                                                        (ContentStreamOp_setGrayNonStroking
                                                                                           _25)))
                                                                               ((RTS.<||)
                                                                                  (RTS.pEnter
                                                                                     "closeSubpath"
                                                                                     (do (_26
                                                                                            :: ()) <-
                                                                                           RTS.pEnter
                                                                                             "PdfValue.KW"
                                                                                             (PdfValue.pKW
                                                                                                (Vector.vecFromRep
                                                                                                   "h"))
                                                                                         HS.pure
                                                                                           (ContentStreamOp_closeSubpath
                                                                                              _26)))
                                                                                  ((RTS.<||)
                                                                                     (RTS.pEnter
                                                                                        "setFlat"
                                                                                        (do (_27
                                                                                               :: ()) <-
                                                                                              RTS.pEnter
                                                                                                "PdfValue.KW"
                                                                                                (PdfValue.pKW
                                                                                                   (Vector.vecFromRep
                                                                                                      "i"))
                                                                                            HS.pure
                                                                                              (ContentStreamOp_setFlat
                                                                                                 _27)))
                                                                                     ((RTS.<||)
                                                                                        (RTS.pEnter
                                                                                           "beginInlineImageData"
                                                                                           (do (_28
                                                                                                  :: ()) <-
                                                                                                 RTS.pEnter
                                                                                                   "PdfValue.KW"
                                                                                                   (PdfValue.pKW
                                                                                                      (Vector.vecFromRep
                                                                                                         "ID"))
                                                                                               HS.pure
                                                                                                 (ContentStreamOp_beginInlineImageData
                                                                                                    _28)))
                                                                                        ((RTS.<||)
                                                                                           (RTS.pEnter
                                                                                              "setLineJoinStyle"
                                                                                              (do (_29
                                                                                                     :: ()) <-
                                                                                                    RTS.pEnter
                                                                                                      "PdfValue.KW"
                                                                                                      (PdfValue.pKW
                                                                                                         (Vector.vecFromRep
                                                                                                            "j"))
                                                                                                  HS.pure
                                                                                                    (ContentStreamOp_setLineJoinStyle
                                                                                                       _29)))
                                                                                           ((RTS.<||)
                                                                                              (RTS.pEnter
                                                                                                 "setLineCapStyle"
                                                                                                 (do (_30
                                                                                                        :: ()) <-
                                                                                                       RTS.pEnter
                                                                                                         "PdfValue.KW"
                                                                                                         (PdfValue.pKW
                                                                                                            (Vector.vecFromRep
                                                                                                               "J"))
                                                                                                     HS.pure
                                                                                                       (ContentStreamOp_setLineCapStyle
                                                                                                          _30)))
                                                                                              ((RTS.<||)
                                                                                                 (RTS.pEnter
                                                                                                    "setCMYKStroking"
                                                                                                    (do (_31
                                                                                                           :: ()) <-
                                                                                                          RTS.pEnter
                                                                                                            "PdfValue.KW"
                                                                                                            (PdfValue.pKW
                                                                                                               (Vector.vecFromRep
                                                                                                                  "K"))
                                                                                                        HS.pure
                                                                                                          (ContentStreamOp_setCMYKStroking
                                                                                                             _31)))
                                                                                                 ((RTS.<||)
                                                                                                    (RTS.pEnter
                                                                                                       "setCMYKNonStroking"
                                                                                                       (do (_32
                                                                                                              :: ()) <-
                                                                                                             RTS.pEnter
                                                                                                               "PdfValue.KW"
                                                                                                               (PdfValue.pKW
                                                                                                                  (Vector.vecFromRep
                                                                                                                     "k"))
                                                                                                           HS.pure
                                                                                                             (ContentStreamOp_setCMYKNonStroking
                                                                                                                _32)))
                                                                                                    ((RTS.<||)
                                                                                                       (RTS.pEnter
                                                                                                          "appendLine"
                                                                                                          (do (_33
                                                                                                                 :: ()) <-
                                                                                                                RTS.pEnter
                                                                                                                  "PdfValue.KW"
                                                                                                                  (PdfValue.pKW
                                                                                                                     (Vector.vecFromRep
                                                                                                                        "l"))
                                                                                                              HS.pure
                                                                                                                (ContentStreamOp_appendLine
                                                                                                                   _33)))
                                                                                                       ((RTS.<||)
                                                                                                          (RTS.pEnter
                                                                                                             "beginNewSuppath"
                                                                                                             (do (_34
                                                                                                                    :: ()) <-
                                                                                                                   RTS.pEnter
                                                                                                                     "PdfValue.KW"
                                                                                                                     (PdfValue.pKW
                                                                                                                        (Vector.vecFromRep
                                                                                                                           "m"))
                                                                                                                 HS.pure
                                                                                                                   (ContentStreamOp_beginNewSuppath
                                                                                                                      _34)))
                                                                                                          ((RTS.<||)
                                                                                                             (RTS.pEnter
                                                                                                                "defineMarkedContent"
                                                                                                                (do (_35
                                                                                                                       :: ()) <-
                                                                                                                      RTS.pEnter
                                                                                                                        "PdfValue.KW"
                                                                                                                        (PdfValue.pKW
                                                                                                                           (Vector.vecFromRep
                                                                                                                              "MP"))
                                                                                                                    HS.pure
                                                                                                                      (ContentStreamOp_defineMarkedContent
                                                                                                                         _35)))
                                                                                                             ((RTS.<||)
                                                                                                                (RTS.pEnter
                                                                                                                   "setMiterLimit"
                                                                                                                   (do (_36
                                                                                                                          :: ()) <-
                                                                                                                         RTS.pEnter
                                                                                                                           "PdfValue.KW"
                                                                                                                           (PdfValue.pKW
                                                                                                                              (Vector.vecFromRep
                                                                                                                                 "M"))
                                                                                                                       HS.pure
                                                                                                                         (ContentStreamOp_setMiterLimit
                                                                                                                            _36)))
                                                                                                                ((RTS.<||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "endPath"
                                                                                                                      (do (_37
                                                                                                                             :: ()) <-
                                                                                                                            RTS.pEnter
                                                                                                                              "PdfValue.KW"
                                                                                                                              (PdfValue.pKW
                                                                                                                                 (Vector.vecFromRep
                                                                                                                                    "n"))
                                                                                                                          HS.pure
                                                                                                                            (ContentStreamOp_endPath
                                                                                                                               _37)))
                                                                                                                   ((RTS.<||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "saveGraphicsState"
                                                                                                                         (do (_38
                                                                                                                                :: ()) <-
                                                                                                                               RTS.pEnter
                                                                                                                                 "PdfValue.KW"
                                                                                                                                 (PdfValue.pKW
                                                                                                                                    (Vector.vecFromRep
                                                                                                                                       "q"))
                                                                                                                             HS.pure
                                                                                                                               (ContentStreamOp_saveGraphicsState
                                                                                                                                  _38)))
                                                                                                                      ((RTS.<||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "restoreGraphicsState"
                                                                                                                            (do (_39
                                                                                                                                   :: ()) <-
                                                                                                                                  RTS.pEnter
                                                                                                                                    "PdfValue.KW"
                                                                                                                                    (PdfValue.pKW
                                                                                                                                       (Vector.vecFromRep
                                                                                                                                          "Q"))
                                                                                                                                HS.pure
                                                                                                                                  (ContentStreamOp_restoreGraphicsState
                                                                                                                                     _39)))
                                                                                                                         ((RTS.<||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "setRGBStroking"
                                                                                                                               (do (_40
                                                                                                                                      :: ()) <-
                                                                                                                                     RTS.pEnter
                                                                                                                                       "PdfValue.KW"
                                                                                                                                       (PdfValue.pKW
                                                                                                                                          (Vector.vecFromRep
                                                                                                                                             "RG"))
                                                                                                                                   HS.pure
                                                                                                                                     (ContentStreamOp_setRGBStroking
                                                                                                                                        _40)))
                                                                                                                            ((RTS.<||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "appendRect"
                                                                                                                                  (do (_41
                                                                                                                                         :: ()) <-
                                                                                                                                        RTS.pEnter
                                                                                                                                          "PdfValue.KW"
                                                                                                                                          (PdfValue.pKW
                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                "re"))
                                                                                                                                      HS.pure
                                                                                                                                        (ContentStreamOp_appendRect
                                                                                                                                           _41)))
                                                                                                                               ((RTS.<||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "setRGBNonStroking"
                                                                                                                                     (do (_42
                                                                                                                                            :: ()) <-
                                                                                                                                           RTS.pEnter
                                                                                                                                             "PdfValue.KW"
                                                                                                                                             (PdfValue.pKW
                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                   "rg"))
                                                                                                                                         HS.pure
                                                                                                                                           (ContentStreamOp_setRGBNonStroking
                                                                                                                                              _42)))
                                                                                                                                  ((RTS.<||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "setColorRenderingIntent"
                                                                                                                                        (do (_43
                                                                                                                                               :: ()) <-
                                                                                                                                              RTS.pEnter
                                                                                                                                                "PdfValue.KW"
                                                                                                                                                (PdfValue.pKW
                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                      "ri"))
                                                                                                                                            HS.pure
                                                                                                                                              (ContentStreamOp_setColorRenderingIntent
                                                                                                                                                 _43)))
                                                                                                                                     ((RTS.<||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "setColorNonStrokingICC"
                                                                                                                                           (do (_44
                                                                                                                                                  :: ()) <-
                                                                                                                                                 RTS.pEnter
                                                                                                                                                   "PdfValue.KW"
                                                                                                                                                   (PdfValue.pKW
                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                         "scn"))
                                                                                                                                               HS.pure
                                                                                                                                                 (ContentStreamOp_setColorNonStrokingICC
                                                                                                                                                    _44)))
                                                                                                                                        ((RTS.<||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "setColorNonStroking"
                                                                                                                                              (do (_45
                                                                                                                                                     :: ()) <-
                                                                                                                                                    RTS.pEnter
                                                                                                                                                      "PdfValue.KW"
                                                                                                                                                      (PdfValue.pKW
                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                            "sc"))
                                                                                                                                                  HS.pure
                                                                                                                                                    (ContentStreamOp_setColorNonStroking
                                                                                                                                                       _45)))
                                                                                                                                           ((RTS.<||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "closeStrokePath"
                                                                                                                                                 (do (_46
                                                                                                                                                        :: ()) <-
                                                                                                                                                       RTS.pEnter
                                                                                                                                                         "PdfValue.KW"
                                                                                                                                                         (PdfValue.pKW
                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                               "s"))
                                                                                                                                                     HS.pure
                                                                                                                                                       (ContentStreamOp_closeStrokePath
                                                                                                                                                          _46)))
                                                                                                                                              ((RTS.<||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "setColorStrokingICC"
                                                                                                                                                    (do (_47
                                                                                                                                                           :: ()) <-
                                                                                                                                                          RTS.pEnter
                                                                                                                                                            "PdfValue.KW"
                                                                                                                                                            (PdfValue.pKW
                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                  "SCN"))
                                                                                                                                                        HS.pure
                                                                                                                                                          (ContentStreamOp_setColorStrokingICC
                                                                                                                                                             _47)))
                                                                                                                                                 ((RTS.<||)
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "setColorStroking"
                                                                                                                                                       (do (_48
                                                                                                                                                              :: ()) <-
                                                                                                                                                             RTS.pEnter
                                                                                                                                                               "PdfValue.KW"
                                                                                                                                                               (PdfValue.pKW
                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                     "SC"))
                                                                                                                                                           HS.pure
                                                                                                                                                             (ContentStreamOp_setColorStroking
                                                                                                                                                                _48)))
                                                                                                                                                    ((RTS.<||)
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "stroke"
                                                                                                                                                          (do (_49
                                                                                                                                                                 :: ()) <-
                                                                                                                                                                RTS.pEnter
                                                                                                                                                                  "PdfValue.KW"
                                                                                                                                                                  (PdfValue.pKW
                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                        "S"))
                                                                                                                                                              HS.pure
                                                                                                                                                                (ContentStreamOp_stroke
                                                                                                                                                                   _49)))
                                                                                                                                                       ((RTS.<||)
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "paintShadingPattern"
                                                                                                                                                             (do (_50
                                                                                                                                                                    :: ()) <-
                                                                                                                                                                   RTS.pEnter
                                                                                                                                                                     "PdfValue.KW"
                                                                                                                                                                     (PdfValue.pKW
                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                           "sh"))
                                                                                                                                                                 HS.pure
                                                                                                                                                                   (ContentStreamOp_paintShadingPattern
                                                                                                                                                                      _50)))
                                                                                                                                                          ((RTS.<||)
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "moveStartText"
                                                                                                                                                                (do (_51
                                                                                                                                                                       :: ()) <-
                                                                                                                                                                      RTS.pEnter
                                                                                                                                                                        "PdfValue.KW"
                                                                                                                                                                        (PdfValue.pKW
                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                              "T*"))
                                                                                                                                                                    HS.pure
                                                                                                                                                                      (ContentStreamOp_moveStartText
                                                                                                                                                                         _51)))
                                                                                                                                                             ((RTS.<||)
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "setCharSpacing"
                                                                                                                                                                   (do (_52
                                                                                                                                                                          :: ()) <-
                                                                                                                                                                         RTS.pEnter
                                                                                                                                                                           "PdfValue.KW"
                                                                                                                                                                           (PdfValue.pKW
                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                 "Tc"))
                                                                                                                                                                       HS.pure
                                                                                                                                                                         (ContentStreamOp_setCharSpacing
                                                                                                                                                                            _52)))
                                                                                                                                                                ((RTS.<||)
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "moveTextPos"
                                                                                                                                                                      (do (_53
                                                                                                                                                                             :: ()) <-
                                                                                                                                                                            RTS.pEnter
                                                                                                                                                                              "PdfValue.KW"
                                                                                                                                                                              (PdfValue.pKW
                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                    "Td"))
                                                                                                                                                                          HS.pure
                                                                                                                                                                            (ContentStreamOp_moveTextPos
                                                                                                                                                                               _53)))
                                                                                                                                                                   ((RTS.<||)
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "moveTextPosSetLeading"
                                                                                                                                                                         (do (_54
                                                                                                                                                                                :: ()) <-
                                                                                                                                                                               RTS.pEnter
                                                                                                                                                                                 "PdfValue.KW"
                                                                                                                                                                                 (PdfValue.pKW
                                                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                                                       "TD"))
                                                                                                                                                                             HS.pure
                                                                                                                                                                               (ContentStreamOp_moveTextPosSetLeading
                                                                                                                                                                                  _54)))
                                                                                                                                                                      ((RTS.<||)
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "setTextFont"
                                                                                                                                                                            (do (_55
                                                                                                                                                                                   :: ()) <-
                                                                                                                                                                                  RTS.pEnter
                                                                                                                                                                                    "PdfValue.KW"
                                                                                                                                                                                    (PdfValue.pKW
                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                          "Tf"))
                                                                                                                                                                                HS.pure
                                                                                                                                                                                  (ContentStreamOp_setTextFont
                                                                                                                                                                                     _55)))
                                                                                                                                                                         ((RTS.<||)
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "showText"
                                                                                                                                                                               (do (_56
                                                                                                                                                                                      :: ()) <-
                                                                                                                                                                                     RTS.pEnter
                                                                                                                                                                                       "PdfValue.KW"
                                                                                                                                                                                       (PdfValue.pKW
                                                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                                                             "Tj"))
                                                                                                                                                                                   HS.pure
                                                                                                                                                                                     (ContentStreamOp_showText
                                                                                                                                                                                        _56)))
                                                                                                                                                                            ((RTS.<||)
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "showTextIndGlyph"
                                                                                                                                                                                  (do (_57
                                                                                                                                                                                         :: ()) <-
                                                                                                                                                                                        RTS.pEnter
                                                                                                                                                                                          "PdfValue.KW"
                                                                                                                                                                                          (PdfValue.pKW
                                                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                                                "TJ"))
                                                                                                                                                                                      HS.pure
                                                                                                                                                                                        (ContentStreamOp_showTextIndGlyph
                                                                                                                                                                                           _57)))
                                                                                                                                                                               ((RTS.<||)
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "setTextLeading"
                                                                                                                                                                                     (do (_58
                                                                                                                                                                                            :: ()) <-
                                                                                                                                                                                           RTS.pEnter
                                                                                                                                                                                             "PdfValue.KW"
                                                                                                                                                                                             (PdfValue.pKW
                                                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                                                   "TL"))
                                                                                                                                                                                         HS.pure
                                                                                                                                                                                           (ContentStreamOp_setTextLeading
                                                                                                                                                                                              _58)))
                                                                                                                                                                                  ((RTS.<||)
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "setTextMatrix"
                                                                                                                                                                                        (do (_59
                                                                                                                                                                                               :: ()) <-
                                                                                                                                                                                              RTS.pEnter
                                                                                                                                                                                                "PdfValue.KW"
                                                                                                                                                                                                (PdfValue.pKW
                                                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                                                      "Tm"))
                                                                                                                                                                                            HS.pure
                                                                                                                                                                                              (ContentStreamOp_setTextMatrix
                                                                                                                                                                                                 _59)))
                                                                                                                                                                                     ((RTS.<||)
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "setTextRendering"
                                                                                                                                                                                           (do (_60
                                                                                                                                                                                                  :: ()) <-
                                                                                                                                                                                                 RTS.pEnter
                                                                                                                                                                                                   "PdfValue.KW"
                                                                                                                                                                                                   (PdfValue.pKW
                                                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                                                         "Tr"))
                                                                                                                                                                                               HS.pure
                                                                                                                                                                                                 (ContentStreamOp_setTextRendering
                                                                                                                                                                                                    _60)))
                                                                                                                                                                                        ((RTS.<||)
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "setTextRise"
                                                                                                                                                                                              (do (_61
                                                                                                                                                                                                     :: ()) <-
                                                                                                                                                                                                    RTS.pEnter
                                                                                                                                                                                                      "PdfValue.KW"
                                                                                                                                                                                                      (PdfValue.pKW
                                                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                                                            "Ts"))
                                                                                                                                                                                                  HS.pure
                                                                                                                                                                                                    (ContentStreamOp_setTextRise
                                                                                                                                                                                                       _61)))
                                                                                                                                                                                           ((RTS.<||)
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "setWordSpacing"
                                                                                                                                                                                                 (do (_62
                                                                                                                                                                                                        :: ()) <-
                                                                                                                                                                                                       RTS.pEnter
                                                                                                                                                                                                         "PdfValue.KW"
                                                                                                                                                                                                         (PdfValue.pKW
                                                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                                                               "Tw"))
                                                                                                                                                                                                     HS.pure
                                                                                                                                                                                                       (ContentStreamOp_setWordSpacing
                                                                                                                                                                                                          _62)))
                                                                                                                                                                                              ((RTS.<||)
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "setHorizontalTextScaling"
                                                                                                                                                                                                    (do (_63
                                                                                                                                                                                                           :: ()) <-
                                                                                                                                                                                                          RTS.pEnter
                                                                                                                                                                                                            "PdfValue.KW"
                                                                                                                                                                                                            (PdfValue.pKW
                                                                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                                                                  "Tz"))
                                                                                                                                                                                                        HS.pure
                                                                                                                                                                                                          (ContentStreamOp_setHorizontalTextScaling
                                                                                                                                                                                                             _63)))
                                                                                                                                                                                                 ((RTS.<||)
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "appendCurvedInitPtRepl"
                                                                                                                                                                                                       (do (_64
                                                                                                                                                                                                              :: ()) <-
                                                                                                                                                                                                             RTS.pEnter
                                                                                                                                                                                                               "PdfValue.KW"
                                                                                                                                                                                                               (PdfValue.pKW
                                                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                                                     "v"))
                                                                                                                                                                                                           HS.pure
                                                                                                                                                                                                             (ContentStreamOp_appendCurvedInitPtRepl
                                                                                                                                                                                                                _64)))
                                                                                                                                                                                                    ((RTS.<||)
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "setLineWidth"
                                                                                                                                                                                                          (do (_65
                                                                                                                                                                                                                 :: ()) <-
                                                                                                                                                                                                                RTS.pEnter
                                                                                                                                                                                                                  "PdfValue.KW"
                                                                                                                                                                                                                  (PdfValue.pKW
                                                                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                                                                        "w"))
                                                                                                                                                                                                              HS.pure
                                                                                                                                                                                                                (ContentStreamOp_setLineWidth
                                                                                                                                                                                                                   _65)))
                                                                                                                                                                                                       ((RTS.<||)
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "setClippingEvenOdd"
                                                                                                                                                                                                             (do (_66
                                                                                                                                                                                                                    :: ()) <-
                                                                                                                                                                                                                   RTS.pEnter
                                                                                                                                                                                                                     "PdfValue.KW"
                                                                                                                                                                                                                     (PdfValue.pKW
                                                                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                                                                           "W*"))
                                                                                                                                                                                                                 HS.pure
                                                                                                                                                                                                                   (ContentStreamOp_setClippingEvenOdd
                                                                                                                                                                                                                      _66)))
                                                                                                                                                                                                          ((RTS.<||)
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "setClippingNzWinding"
                                                                                                                                                                                                                (do (_67
                                                                                                                                                                                                                       :: ()) <-
                                                                                                                                                                                                                      RTS.pEnter
                                                                                                                                                                                                                        "PdfValue.KW"
                                                                                                                                                                                                                        (PdfValue.pKW
                                                                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                                                                              "W"))
                                                                                                                                                                                                                    HS.pure
                                                                                                                                                                                                                      (ContentStreamOp_setClippingNzWinding
                                                                                                                                                                                                                         _67)))
                                                                                                                                                                                                             ((RTS.<||)
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "appendCurvedFinalPt"
                                                                                                                                                                                                                   (do (_68
                                                                                                                                                                                                                          :: ()) <-
                                                                                                                                                                                                                         RTS.pEnter
                                                                                                                                                                                                                           "PdfValue.KW"
                                                                                                                                                                                                                           (PdfValue.pKW
                                                                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                                                                 "y"))
                                                                                                                                                                                                                       HS.pure
                                                                                                                                                                                                                         (ContentStreamOp_appendCurvedFinalPt
                                                                                                                                                                                                                            _68)))
                                                                                                                                                                                                                ((RTS.<||)
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "moveShow"
                                                                                                                                                                                                                      (do (_69
                                                                                                                                                                                                                             :: ()) <-
                                                                                                                                                                                                                            RTS.pEnter
                                                                                                                                                                                                                              "PdfValue.KW"
                                                                                                                                                                                                                              (PdfValue.pKW
                                                                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                                                                    "'"))
                                                                                                                                                                                                                          HS.pure
                                                                                                                                                                                                                            (ContentStreamOp_moveShow
                                                                                                                                                                                                                               _69)))
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "setSpacing"
                                                                                                                                                                                                                      (do (_70
                                                                                                                                                                                                                             :: ()) <-
                                                                                                                                                                                                                            RTS.pEnter
                                                                                                                                                                                                                              "PdfValue.KW"
                                                                                                                                                                                                                              (PdfValue.pKW
                                                                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                                                                    "\""))
                                                                                                                                                                                                                          HS.pure
                                                                                                                                                                                                                            (ContentStreamOp_setSpacing
                                                                                                                                                                                                                               _70))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
pContentStreamOpObj :: D.Parser ContentStreamOp
 
pContentStreamOpObj =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @ContentStreamOp
       (RTS.pEnter "PdfContentStream.ContentStreamOp" pContentStreamOp))
 
pOpName :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pOpName =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @(Vector.Vector (RTS.UInt 8))
       (do RTS.pEnter "PdfValue._NameChar" PdfValue._NameChar
           (__ :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMany (RTS.<||)
               (RTS.pEnter "PdfValue.NameChar" PdfValue.pNameChar)
           HS.pure __))
 
pContentOp :: D.Parser ContentOp
 
pContentOp =
  (RTS.<||)
    (RTS.pEnter "knownOp"
       (do (_71 :: ContentStreamOp) <-
             RTS.pEnter "PdfContentStream.ContentStreamOpObj"
               pContentStreamOpObj
           HS.pure (ContentOp_knownOp _71)))
    (RTS.pEnter "futureOp"
       (do (_72 :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pEnter "PdfContentStream.OpName" pOpName
           HS.pure (ContentOp_futureOp _72)))
 
pDirectObj :: D.Parser DirectObj
 
pDirectObj =
  (RTS.<||)
    (RTS.pEnter "null"
       (do (_73 :: ()) <- RTS.pEnter "PdfValue.Null" PdfValue.pNull
           HS.pure (DirectObj_null _73)))
    ((RTS.<||)
       (RTS.pEnter "bool"
          (do (_74 :: HS.Bool) <- RTS.pEnter "PdfValue.Bool" PdfValue.pBool
              HS.pure (DirectObj_bool _74)))
       ((RTS.<||)
          (RTS.pEnter "name"
             (do (_75 :: Vector.Vector (RTS.UInt 8)) <-
                   RTS.pEnter "PdfValue.Name" PdfValue.pName
                 HS.pure (DirectObj_name _75)))
          ((RTS.<||)
             (RTS.pEnter "string"
                (do (_76 :: Vector.Vector (RTS.UInt 8)) <-
                      RTS.pEnter "PdfValue.String" PdfValue.pString
                    HS.pure (DirectObj_string _76)))
             ((RTS.<||)
                (RTS.pEnter "string"
                   (do (_77 :: Vector.Vector (RTS.UInt 8)) <-
                         RTS.pEnter "PdfValue.HexString" PdfValue.pHexString
                       HS.pure (DirectObj_string _77)))
                ((RTS.<||)
                   (RTS.pEnter "number"
                      (do (_78 :: PdfValue.Number) <-
                            RTS.pEnter "PdfValue.Number" PdfValue.pNumber
                          HS.pure (DirectObj_number _78)))
                   ((RTS.<||)
                      (RTS.pEnter "array"
                         (do (_79 :: Vector.Vector PdfValue.Value) <-
                               RTS.pEnter "PdfValue.Array" PdfValue.pArray
                             HS.pure (DirectObj_array _79)))
                      (RTS.pEnter "dict"
                         (do (_80 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
                               RTS.pEnter "PdfValue.Dict" PdfValue.pDict
                             HS.pure (DirectObj_dict _80)))))))))
 
pContentStreamOperandObj :: D.Parser DirectObj
 
pContentStreamOperandObj =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @DirectObj
       (RTS.pEnter "PdfContentStream.DirectObj" pDirectObj))
 
pEndCompat :: D.Parser ()
 
pEndCompat =
  RTS.pEnter "PdfValue.KW" (PdfValue.pKW (Vector.vecFromRep "EX"))
 
_BeginCompat :: D.Parser ()
 
_BeginCompat =
  RTS.pEnter "PdfValue._KW" (PdfValue._KW (Vector.vecFromRep "BX"))
 
_ContentStreamOp :: D.Parser ()
 
_ContentStreamOp =
  (RTS.<||)
    (RTS.pEnter "closeFillStrokeEvenOdd"
       (RTS.pEnter "PdfValue._KW"
          (PdfValue._KW (Vector.vecFromRep "b*"))))
    ((RTS.<||)
       (RTS.pEnter "closeFillStrokeNzWinding"
          (RTS.pEnter "PdfValue._KW" (PdfValue._KW (Vector.vecFromRep "b"))))
       ((RTS.<||)
          (RTS.pEnter "fillStrokeEvenOdd"
             (RTS.pEnter "PdfValue._KW"
                (PdfValue._KW (Vector.vecFromRep "B*"))))
          ((RTS.<||)
             (RTS.pEnter "beginMarkedContent"
                (RTS.pEnter "PdfValue._KW"
                   (PdfValue._KW (Vector.vecFromRep "BDC"))))
             ((RTS.<||)
                (RTS.pEnter "beginInline"
                   (RTS.pEnter "PdfValue._KW"
                      (PdfValue._KW (Vector.vecFromRep "Bl"))))
                ((RTS.<||)
                   (RTS.pEnter "beginMarkedContent"
                      (RTS.pEnter "PdfValue._KW"
                         (PdfValue._KW (Vector.vecFromRep "BMC"))))
                   ((RTS.<||)
                      (RTS.pEnter "beginText"
                         (RTS.pEnter "PdfValue._KW"
                            (PdfValue._KW (Vector.vecFromRep "BT"))))
                      ((RTS.<||)
                         (RTS.pEnter "fillStroke"
                            (RTS.pEnter "PdfValue._KW" (PdfValue._KW (Vector.vecFromRep "B"))))
                         ((RTS.<||)
                            (RTS.pEnter "concatMatrix"
                               (RTS.pEnter "PdfValue._KW"
                                  (PdfValue._KW (Vector.vecFromRep "cm"))))
                            ((RTS.<||)
                               (RTS.pEnter "setColorSpaceStroking"
                                  (RTS.pEnter "PdfValue._KW"
                                     (PdfValue._KW (Vector.vecFromRep "CS"))))
                               ((RTS.<||)
                                  (RTS.pEnter "setColorSpaceNonStroking"
                                     (RTS.pEnter "PdfValue._KW"
                                        (PdfValue._KW (Vector.vecFromRep "cs"))))
                                  ((RTS.<||)
                                     (RTS.pEnter "appendCurvedThreePoints"
                                        (RTS.pEnter "PdfValue._KW"
                                           (PdfValue._KW (Vector.vecFromRep "c"))))
                                     ((RTS.<||)
                                        (RTS.pEnter "setGlyphWidth"
                                           (RTS.pEnter "PdfValue._KW"
                                              (PdfValue._KW (Vector.vecFromRep "d0"))))
                                        ((RTS.<||)
                                           (RTS.pEnter "setGlpyhWidthBoundingBox"
                                              (RTS.pEnter "PdfValue._KW"
                                                 (PdfValue._KW (Vector.vecFromRep "d1"))))
                                           ((RTS.<||)
                                              (RTS.pEnter "setLineDash"
                                                 (RTS.pEnter "PdfValue._KW"
                                                    (PdfValue._KW (Vector.vecFromRep "d"))))
                                              ((RTS.<||)
                                                 (RTS.pEnter "invokeXObj"
                                                    (RTS.pEnter "PdfValue._KW"
                                                       (PdfValue._KW (Vector.vecFromRep "Do"))))
                                                 ((RTS.<||)
                                                    (RTS.pEnter "defMarkedContentPoint"
                                                       (RTS.pEnter "PdfValue._KW"
                                                          (PdfValue._KW (Vector.vecFromRep "DP"))))
                                                    ((RTS.<||)
                                                       (RTS.pEnter "endInline"
                                                          (RTS.pEnter "PdfValue._KW"
                                                             (PdfValue._KW
                                                                (Vector.vecFromRep "El"))))
                                                       ((RTS.<||)
                                                          (RTS.pEnter "endMarkedContent"
                                                             (RTS.pEnter "PdfValue._KW"
                                                                (PdfValue._KW
                                                                   (Vector.vecFromRep "EMC"))))
                                                          ((RTS.<||)
                                                             (RTS.pEnter "endTextObj"
                                                                (RTS.pEnter "PdfValue._KW"
                                                                   (PdfValue._KW
                                                                      (Vector.vecFromRep "ET"))))
                                                             ((RTS.<||)
                                                                (RTS.pEnter "fillPathEvenOdd"
                                                                   (RTS.pEnter "PdfValue._KW"
                                                                      (PdfValue._KW
                                                                         (Vector.vecFromRep "f*"))))
                                                                ((RTS.<||)
                                                                   (RTS.pEnter "fillPathNzWinding"
                                                                      (RTS.pEnter "PdfValue._KW"
                                                                         (PdfValue._KW
                                                                            (Vector.vecFromRep
                                                                               "f"))))
                                                                   ((RTS.<||)
                                                                      (RTS.pEnter
                                                                         "fillPathNzWindingOld"
                                                                         (RTS.pEnter "PdfValue._KW"
                                                                            (PdfValue._KW
                                                                               (Vector.vecFromRep
                                                                                  "F"))))
                                                                      ((RTS.<||)
                                                                         (RTS.pEnter
                                                                            "setGrayStroking"
                                                                            (RTS.pEnter
                                                                               "PdfValue._KW"
                                                                               (PdfValue._KW
                                                                                  (Vector.vecFromRep
                                                                                     "G"))))
                                                                         ((RTS.<||)
                                                                            (RTS.pEnter
                                                                               "setGraphicsStateParams"
                                                                               (RTS.pEnter
                                                                                  "PdfValue._KW"
                                                                                  (PdfValue._KW
                                                                                     (Vector.vecFromRep
                                                                                        "gs"))))
                                                                            ((RTS.<||)
                                                                               (RTS.pEnter
                                                                                  "setGrayNonStroking"
                                                                                  (RTS.pEnter
                                                                                     "PdfValue._KW"
                                                                                     (PdfValue._KW
                                                                                        (Vector.vecFromRep
                                                                                           "g"))))
                                                                               ((RTS.<||)
                                                                                  (RTS.pEnter
                                                                                     "closeSubpath"
                                                                                     (RTS.pEnter
                                                                                        "PdfValue._KW"
                                                                                        (PdfValue._KW
                                                                                           (Vector.vecFromRep
                                                                                              "h"))))
                                                                                  ((RTS.<||)
                                                                                     (RTS.pEnter
                                                                                        "setFlat"
                                                                                        (RTS.pEnter
                                                                                           "PdfValue._KW"
                                                                                           (PdfValue._KW
                                                                                              (Vector.vecFromRep
                                                                                                 "i"))))
                                                                                     ((RTS.<||)
                                                                                        (RTS.pEnter
                                                                                           "beginInlineImageData"
                                                                                           (RTS.pEnter
                                                                                              "PdfValue._KW"
                                                                                              (PdfValue._KW
                                                                                                 (Vector.vecFromRep
                                                                                                    "ID"))))
                                                                                        ((RTS.<||)
                                                                                           (RTS.pEnter
                                                                                              "setLineJoinStyle"
                                                                                              (RTS.pEnter
                                                                                                 "PdfValue._KW"
                                                                                                 (PdfValue._KW
                                                                                                    (Vector.vecFromRep
                                                                                                       "j"))))
                                                                                           ((RTS.<||)
                                                                                              (RTS.pEnter
                                                                                                 "setLineCapStyle"
                                                                                                 (RTS.pEnter
                                                                                                    "PdfValue._KW"
                                                                                                    (PdfValue._KW
                                                                                                       (Vector.vecFromRep
                                                                                                          "J"))))
                                                                                              ((RTS.<||)
                                                                                                 (RTS.pEnter
                                                                                                    "setCMYKStroking"
                                                                                                    (RTS.pEnter
                                                                                                       "PdfValue._KW"
                                                                                                       (PdfValue._KW
                                                                                                          (Vector.vecFromRep
                                                                                                             "K"))))
                                                                                                 ((RTS.<||)
                                                                                                    (RTS.pEnter
                                                                                                       "setCMYKNonStroking"
                                                                                                       (RTS.pEnter
                                                                                                          "PdfValue._KW"
                                                                                                          (PdfValue._KW
                                                                                                             (Vector.vecFromRep
                                                                                                                "k"))))
                                                                                                    ((RTS.<||)
                                                                                                       (RTS.pEnter
                                                                                                          "appendLine"
                                                                                                          (RTS.pEnter
                                                                                                             "PdfValue._KW"
                                                                                                             (PdfValue._KW
                                                                                                                (Vector.vecFromRep
                                                                                                                   "l"))))
                                                                                                       ((RTS.<||)
                                                                                                          (RTS.pEnter
                                                                                                             "beginNewSuppath"
                                                                                                             (RTS.pEnter
                                                                                                                "PdfValue._KW"
                                                                                                                (PdfValue._KW
                                                                                                                   (Vector.vecFromRep
                                                                                                                      "m"))))
                                                                                                          ((RTS.<||)
                                                                                                             (RTS.pEnter
                                                                                                                "defineMarkedContent"
                                                                                                                (RTS.pEnter
                                                                                                                   "PdfValue._KW"
                                                                                                                   (PdfValue._KW
                                                                                                                      (Vector.vecFromRep
                                                                                                                         "MP"))))
                                                                                                             ((RTS.<||)
                                                                                                                (RTS.pEnter
                                                                                                                   "setMiterLimit"
                                                                                                                   (RTS.pEnter
                                                                                                                      "PdfValue._KW"
                                                                                                                      (PdfValue._KW
                                                                                                                         (Vector.vecFromRep
                                                                                                                            "M"))))
                                                                                                                ((RTS.<||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "endPath"
                                                                                                                      (RTS.pEnter
                                                                                                                         "PdfValue._KW"
                                                                                                                         (PdfValue._KW
                                                                                                                            (Vector.vecFromRep
                                                                                                                               "n"))))
                                                                                                                   ((RTS.<||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "saveGraphicsState"
                                                                                                                         (RTS.pEnter
                                                                                                                            "PdfValue._KW"
                                                                                                                            (PdfValue._KW
                                                                                                                               (Vector.vecFromRep
                                                                                                                                  "q"))))
                                                                                                                      ((RTS.<||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "restoreGraphicsState"
                                                                                                                            (RTS.pEnter
                                                                                                                               "PdfValue._KW"
                                                                                                                               (PdfValue._KW
                                                                                                                                  (Vector.vecFromRep
                                                                                                                                     "Q"))))
                                                                                                                         ((RTS.<||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "setRGBStroking"
                                                                                                                               (RTS.pEnter
                                                                                                                                  "PdfValue._KW"
                                                                                                                                  (PdfValue._KW
                                                                                                                                     (Vector.vecFromRep
                                                                                                                                        "RG"))))
                                                                                                                            ((RTS.<||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "appendRect"
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "PdfValue._KW"
                                                                                                                                     (PdfValue._KW
                                                                                                                                        (Vector.vecFromRep
                                                                                                                                           "re"))))
                                                                                                                               ((RTS.<||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "setRGBNonStroking"
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "PdfValue._KW"
                                                                                                                                        (PdfValue._KW
                                                                                                                                           (Vector.vecFromRep
                                                                                                                                              "rg"))))
                                                                                                                                  ((RTS.<||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "setColorRenderingIntent"
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "PdfValue._KW"
                                                                                                                                           (PdfValue._KW
                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                 "ri"))))
                                                                                                                                     ((RTS.<||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "setColorNonStrokingICC"
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "PdfValue._KW"
                                                                                                                                              (PdfValue._KW
                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                    "scn"))))
                                                                                                                                        ((RTS.<||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "setColorNonStroking"
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "PdfValue._KW"
                                                                                                                                                 (PdfValue._KW
                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                       "sc"))))
                                                                                                                                           ((RTS.<||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "closeStrokePath"
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "PdfValue._KW"
                                                                                                                                                    (PdfValue._KW
                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                          "s"))))
                                                                                                                                              ((RTS.<||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "setColorStrokingICC"
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "PdfValue._KW"
                                                                                                                                                       (PdfValue._KW
                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                             "SCN"))))
                                                                                                                                                 ((RTS.<||)
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "setColorStroking"
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "PdfValue._KW"
                                                                                                                                                          (PdfValue._KW
                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                "SC"))))
                                                                                                                                                    ((RTS.<||)
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "stroke"
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "PdfValue._KW"
                                                                                                                                                             (PdfValue._KW
                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                   "S"))))
                                                                                                                                                       ((RTS.<||)
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "paintShadingPattern"
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "PdfValue._KW"
                                                                                                                                                                (PdfValue._KW
                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                      "sh"))))
                                                                                                                                                          ((RTS.<||)
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "moveStartText"
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "PdfValue._KW"
                                                                                                                                                                   (PdfValue._KW
                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                         "T*"))))
                                                                                                                                                             ((RTS.<||)
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "setCharSpacing"
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "PdfValue._KW"
                                                                                                                                                                      (PdfValue._KW
                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                            "Tc"))))
                                                                                                                                                                ((RTS.<||)
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "moveTextPos"
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "PdfValue._KW"
                                                                                                                                                                         (PdfValue._KW
                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                               "Td"))))
                                                                                                                                                                   ((RTS.<||)
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "moveTextPosSetLeading"
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "PdfValue._KW"
                                                                                                                                                                            (PdfValue._KW
                                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                                  "TD"))))
                                                                                                                                                                      ((RTS.<||)
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "setTextFont"
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "PdfValue._KW"
                                                                                                                                                                               (PdfValue._KW
                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                     "Tf"))))
                                                                                                                                                                         ((RTS.<||)
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "showText"
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "PdfValue._KW"
                                                                                                                                                                                  (PdfValue._KW
                                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                                        "Tj"))))
                                                                                                                                                                            ((RTS.<||)
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "showTextIndGlyph"
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "PdfValue._KW"
                                                                                                                                                                                     (PdfValue._KW
                                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                                           "TJ"))))
                                                                                                                                                                               ((RTS.<||)
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "setTextLeading"
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "PdfValue._KW"
                                                                                                                                                                                        (PdfValue._KW
                                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                                              "TL"))))
                                                                                                                                                                                  ((RTS.<||)
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "setTextMatrix"
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "PdfValue._KW"
                                                                                                                                                                                           (PdfValue._KW
                                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                                 "Tm"))))
                                                                                                                                                                                     ((RTS.<||)
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "setTextRendering"
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "PdfValue._KW"
                                                                                                                                                                                              (PdfValue._KW
                                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                                    "Tr"))))
                                                                                                                                                                                        ((RTS.<||)
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "setTextRise"
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "PdfValue._KW"
                                                                                                                                                                                                 (PdfValue._KW
                                                                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                                                                       "Ts"))))
                                                                                                                                                                                           ((RTS.<||)
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "setWordSpacing"
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "PdfValue._KW"
                                                                                                                                                                                                    (PdfValue._KW
                                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                                          "Tw"))))
                                                                                                                                                                                              ((RTS.<||)
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "setHorizontalTextScaling"
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "PdfValue._KW"
                                                                                                                                                                                                       (PdfValue._KW
                                                                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                                                                             "Tz"))))
                                                                                                                                                                                                 ((RTS.<||)
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "appendCurvedInitPtRepl"
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "PdfValue._KW"
                                                                                                                                                                                                          (PdfValue._KW
                                                                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                                                                "v"))))
                                                                                                                                                                                                    ((RTS.<||)
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "setLineWidth"
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "PdfValue._KW"
                                                                                                                                                                                                             (PdfValue._KW
                                                                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                                                                   "w"))))
                                                                                                                                                                                                       ((RTS.<||)
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "setClippingEvenOdd"
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "PdfValue._KW"
                                                                                                                                                                                                                (PdfValue._KW
                                                                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                                                                      "W*"))))
                                                                                                                                                                                                          ((RTS.<||)
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "setClippingNzWinding"
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "PdfValue._KW"
                                                                                                                                                                                                                   (PdfValue._KW
                                                                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                                                                         "W"))))
                                                                                                                                                                                                             ((RTS.<||)
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "appendCurvedFinalPt"
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "PdfValue._KW"
                                                                                                                                                                                                                      (PdfValue._KW
                                                                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                                                                            "y"))))
                                                                                                                                                                                                                ((RTS.<||)
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "moveShow"
                                                                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                                                                         "PdfValue._KW"
                                                                                                                                                                                                                         (PdfValue._KW
                                                                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                                                                               "'"))))
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "setSpacing"
                                                                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                                                                         "PdfValue._KW"
                                                                                                                                                                                                                         (PdfValue._KW
                                                                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                                                                               "\"")))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
_ContentStreamOpObj :: D.Parser ()
 
_ContentStreamOpObj =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @ContentStreamOp
       (RTS.pEnter "PdfContentStream._ContentStreamOp" _ContentStreamOp))
 
_OpName :: D.Parser ()
 
_OpName =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @(Vector.Vector (RTS.UInt 8))
       (do RTS.pEnter "PdfValue._NameChar" PdfValue._NameChar
           RTS.pSkipMany (RTS.<||)
             (RTS.pEnter "PdfValue._NameChar" PdfValue._NameChar)))
 
_ContentOp :: D.Parser ()
 
_ContentOp =
  (RTS.<||)
    (RTS.pEnter "knownOp"
       (RTS.pEnter "PdfContentStream._ContentStreamOpObj"
          _ContentStreamOpObj))
    (RTS.pEnter "futureOp"
       (RTS.pEnter "PdfContentStream._OpName" _OpName))
 
_DirectObj :: D.Parser ()
 
_DirectObj =
  (RTS.<||)
    (RTS.pEnter "null" (RTS.pEnter "PdfValue._Null" PdfValue._Null))
    ((RTS.<||)
       (RTS.pEnter "bool" (RTS.pEnter "PdfValue._Bool" PdfValue._Bool))
       ((RTS.<||)
          (RTS.pEnter "name" (RTS.pEnter "PdfValue._Name" PdfValue._Name))
          ((RTS.<||)
             (RTS.pEnter "string"
                (RTS.pEnter "PdfValue._String" PdfValue._String))
             ((RTS.<||)
                (RTS.pEnter "string"
                   (RTS.pEnter "PdfValue._HexString" PdfValue._HexString))
                ((RTS.<||)
                   (RTS.pEnter "number"
                      (RTS.pEnter "PdfValue._Number" PdfValue._Number))
                   ((RTS.<||)
                      (RTS.pEnter "array" (RTS.pEnter "PdfValue._Array" PdfValue._Array))
                      (RTS.pEnter "dict"
                         (RTS.pEnter "PdfValue._Dict" PdfValue._Dict))))))))
 
_ContentStreamOperandObj :: D.Parser ()
 
_ContentStreamOperandObj =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @DirectObj
       (RTS.pEnter "PdfContentStream._DirectObj" _DirectObj))
 
_EndCompat :: D.Parser ()
 
_EndCompat =
  RTS.pEnter "PdfValue._KW" (PdfValue._KW (Vector.vecFromRep "EX"))
 
pContentStreamBody ::
      D.Parser ContentOp -> D.Parser (Vector.Vector ContentStreamBody_0)
 
pContentStreamBody (pOp :: D.Parser ContentOp) =
  do RTS.pSkipMany (RTS.<||)
       (RTS.pEnter "PdfValue._AnyWS" PdfValue._AnyWS)
     (__ :: Vector.Vector ContentStreamBody_0) <-
       RTS.pMany (RTS.<||)
         (do (__ :: ContentStreamBody_0) <-
               (RTS.<||)
                 (RTS.pEnter "compatSect"
                    (do (_81 :: ()) <-
                          do RTS.pEnter "PdfContentStream._BeginCompat" _BeginCompat
                             RTS.pErrorMode RTS.Abort
                               (do RTS.pEnter "PdfContentStream._ContentStreamBody"
                                     (_ContentStreamBody
                                        (RTS.pEnter "PdfContentStream._ContentOp" _ContentOp))
                                   (__ :: ()) <- RTS.pEnter "PdfContentStream.EndCompat" pEndCompat
                                   HS.pure __)
                        HS.pure (ContentStreamBody_0_compatSect _81)))
                 ((RTS.<||)
                    (RTS.pEnter "operand"
                       (do (_82 :: DirectObj) <-
                             RTS.pEnter "PdfContentStream.ContentStreamOperandObj"
                               pContentStreamOperandObj
                           HS.pure (ContentStreamBody_0_operand _82)))
                    (RTS.pEnter "operation"
                       (do (_83 :: ContentOp) <- pOp
                           HS.pure (ContentStreamBody_0_operation _83))))
             HS.pure __)
     HS.pure __
 
_ContentStreamBody :: D.Parser () -> D.Parser ()
 
_ContentStreamBody (_Op :: D.Parser ()) =
  do RTS.pSkipMany (RTS.<||)
       (RTS.pEnter "PdfValue._AnyWS" PdfValue._AnyWS)
     RTS.pSkipMany (RTS.<||)
       ((RTS.<||)
          (RTS.pEnter "compatSect"
             (do RTS.pEnter "PdfContentStream._BeginCompat" _BeginCompat
                 RTS.pErrorMode RTS.Abort
                   (do do HS.void
                            (RTS.pEnter "PdfContentStream.ContentStreamBody"
                               (pContentStreamBody
                                  (RTS.pEnter "PdfContentStream.ContentOp" pContentOp)))
                          HS.pure ()
                       RTS.pEnter "PdfContentStream._EndCompat" _EndCompat)))
          ((RTS.<||)
             (RTS.pEnter "operand"
                (RTS.pEnter "PdfContentStream._ContentStreamOperandObj"
                   _ContentStreamOperandObj))
             (RTS.pEnter "operation" _Op)))
 
pContentStream :: D.Parser (Vector.Vector ContentStreamBody_0)
 
pContentStream =
  RTS.pEnter "PdfContentStream.ContentStreamBody"
    (pContentStreamBody
       (do (_84 :: ContentStreamOp) <-
             RTS.pEnter "PdfContentStream.ContentStreamOpObj"
               pContentStreamOpObj
           HS.pure (ContentOp_knownOp _84)))
 
_ContentStream :: D.Parser ()
 
_ContentStream =
  RTS.pEnter "PdfContentStream._ContentStreamBody"
    (_ContentStreamBody
       (RTS.pEnter "PdfContentStream._ContentStreamOpObj"
          _ContentStreamOpObj))
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
  = ContentStreamBody_0_compatSect (Vector.Vector (RTS.UInt 8))
  | ContentStreamBody_0_operand DirectObj
  | ContentStreamBody_0_operation ContentStreamOp
  
 
deriving instance HS.Eq ContentStreamBody_0
 
deriving instance HS.Ord ContentStreamBody_0
 
deriving instance HS.Show ContentStreamBody_0
 
instance RTS.DDL ContentStreamBody_0 where
 
instance HS.HasField "compatSect" ContentStreamBody_0
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (ContentStreamBody_0_compatSect x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "operand" ContentStreamBody_0
           (HS.Maybe DirectObj) where
  getField (ContentStreamBody_0_operand x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "operation" ContentStreamBody_0
           (HS.Maybe ContentStreamOp) where
  getField (ContentStreamBody_0_operation x) = HS.Just x
   
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
 
pBeginCompat :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pBeginCompat =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @(Vector.Vector (RTS.UInt 8))
       (HS.pure (Vector.vecFromRep "BX")))
 
pContentStreamOp :: D.Parser ContentStreamOp
 
pContentStreamOp =
  (RTS.|||)
    (RTS.pEnter "closeFillStrokeNzWinding"
       (do (_0 :: ()) <- HS.pure ()
           HS.pure (ContentStreamOp_closeFillStrokeNzWinding _0)))
    ((RTS.|||)
       (RTS.pEnter "fillStroke"
          (do (_1 :: ()) <- HS.pure ()
              HS.pure (ContentStreamOp_fillStroke _1)))
       ((RTS.|||)
          (RTS.pEnter "closeFillStrokeEvenOdd"
             (do (_2 :: ()) <- HS.pure ()
                 HS.pure (ContentStreamOp_closeFillStrokeEvenOdd _2)))
          ((RTS.|||)
             (RTS.pEnter "fillStrokeEvenOdd"
                (do (_3 :: ()) <- HS.pure ()
                    HS.pure (ContentStreamOp_fillStrokeEvenOdd _3)))
             ((RTS.|||)
                (RTS.pEnter "beginMarkedContent"
                   (do (_4 :: ()) <- HS.pure ()
                       HS.pure (ContentStreamOp_beginMarkedContent _4)))
                ((RTS.|||)
                   (RTS.pEnter "beginInline"
                      (do (_5 :: ()) <- HS.pure ()
                          HS.pure (ContentStreamOp_beginInline _5)))
                   ((RTS.|||)
                      (RTS.pEnter "beginMarkedContent"
                         (do (_6 :: ()) <- HS.pure ()
                             HS.pure (ContentStreamOp_beginMarkedContent _6)))
                      ((RTS.|||)
                         (RTS.pEnter "beginText"
                            (do (_7 :: ()) <- HS.pure ()
                                HS.pure (ContentStreamOp_beginText _7)))
                         ((RTS.|||)
                            (RTS.pEnter "appendCurvedThreePoints"
                               (do (_8 :: ()) <- HS.pure ()
                                   HS.pure (ContentStreamOp_appendCurvedThreePoints _8)))
                            ((RTS.|||)
                               (RTS.pEnter "concatMatrix"
                                  (do (_9 :: ()) <- HS.pure ()
                                      HS.pure (ContentStreamOp_concatMatrix _9)))
                               ((RTS.|||)
                                  (RTS.pEnter "setColorSpaceStroking"
                                     (do (_10 :: ()) <- HS.pure ()
                                         HS.pure (ContentStreamOp_setColorSpaceStroking _10)))
                                  ((RTS.|||)
                                     (RTS.pEnter "setColorSpaceNonStroking"
                                        (do (_11 :: ()) <- HS.pure ()
                                            HS.pure (ContentStreamOp_setColorSpaceNonStroking _11)))
                                     ((RTS.|||)
                                        (RTS.pEnter "setLineDash"
                                           (do (_12 :: ()) <- HS.pure ()
                                               HS.pure (ContentStreamOp_setLineDash _12)))
                                        ((RTS.|||)
                                           (RTS.pEnter "setGlyphWidth"
                                              (do (_13 :: ()) <- HS.pure ()
                                                  HS.pure (ContentStreamOp_setGlyphWidth _13)))
                                           ((RTS.|||)
                                              (RTS.pEnter "setGlpyhWidthBoundingBox"
                                                 (do (_14 :: ()) <- HS.pure ()
                                                     HS.pure
                                                       (ContentStreamOp_setGlpyhWidthBoundingBox
                                                          _14)))
                                              ((RTS.|||)
                                                 (RTS.pEnter "invokeXObj"
                                                    (do (_15 :: ()) <- HS.pure ()
                                                        HS.pure (ContentStreamOp_invokeXObj _15)))
                                                 ((RTS.|||)
                                                    (RTS.pEnter "defMarkedContentPoint"
                                                       (do (_16 :: ()) <- HS.pure ()
                                                           HS.pure
                                                             (ContentStreamOp_defMarkedContentPoint
                                                                _16)))
                                                    ((RTS.|||)
                                                       (RTS.pEnter "endInline"
                                                          (do (_17 :: ()) <- HS.pure ()
                                                              HS.pure
                                                                (ContentStreamOp_endInline _17)))
                                                       ((RTS.|||)
                                                          (RTS.pEnter "endMarkedContent"
                                                             (do (_18 :: ()) <- HS.pure ()
                                                                 HS.pure
                                                                   (ContentStreamOp_endMarkedContent
                                                                      _18)))
                                                          ((RTS.|||)
                                                             (RTS.pEnter "endTextObj"
                                                                (do (_19 :: ()) <- HS.pure ()
                                                                    HS.pure
                                                                      (ContentStreamOp_endTextObj
                                                                         _19)))
                                                             ((RTS.|||)
                                                                (RTS.pEnter "fillPathNzWinding"
                                                                   (do (_20 :: ()) <- HS.pure ()
                                                                       HS.pure
                                                                         (ContentStreamOp_fillPathNzWinding
                                                                            _20)))
                                                                ((RTS.|||)
                                                                   (RTS.pEnter
                                                                      "fillPathNzWindingOld"
                                                                      (do (_21 :: ()) <- HS.pure ()
                                                                          HS.pure
                                                                            (ContentStreamOp_fillPathNzWindingOld
                                                                               _21)))
                                                                   ((RTS.|||)
                                                                      (RTS.pEnter "fillPathEvenOdd"
                                                                         (do (_22 :: ()) <-
                                                                               HS.pure ()
                                                                             HS.pure
                                                                               (ContentStreamOp_fillPathEvenOdd
                                                                                  _22)))
                                                                      ((RTS.|||)
                                                                         (RTS.pEnter
                                                                            "setGrayStroking"
                                                                            (do (_23 :: ()) <-
                                                                                  HS.pure ()
                                                                                HS.pure
                                                                                  (ContentStreamOp_setGrayStroking
                                                                                     _23)))
                                                                         ((RTS.|||)
                                                                            (RTS.pEnter
                                                                               "setGrayNonStroking"
                                                                               (do (_24 :: ()) <-
                                                                                     HS.pure ()
                                                                                   HS.pure
                                                                                     (ContentStreamOp_setGrayNonStroking
                                                                                        _24)))
                                                                            ((RTS.|||)
                                                                               (RTS.pEnter
                                                                                  "setGraphicsStateParams"
                                                                                  (do (_25 :: ()) <-
                                                                                        HS.pure ()
                                                                                      HS.pure
                                                                                        (ContentStreamOp_setGraphicsStateParams
                                                                                           _25)))
                                                                               ((RTS.|||)
                                                                                  (RTS.pEnter
                                                                                     "closeSubpath"
                                                                                     (do (_26
                                                                                            :: ()) <-
                                                                                           HS.pure
                                                                                             ()
                                                                                         HS.pure
                                                                                           (ContentStreamOp_closeSubpath
                                                                                              _26)))
                                                                                  ((RTS.|||)
                                                                                     (RTS.pEnter
                                                                                        "setFlat"
                                                                                        (do (_27
                                                                                               :: ()) <-
                                                                                              HS.pure
                                                                                                ()
                                                                                            HS.pure
                                                                                              (ContentStreamOp_setFlat
                                                                                                 _27)))
                                                                                     ((RTS.|||)
                                                                                        (RTS.pEnter
                                                                                           "beginInlineImageData"
                                                                                           (do (_28
                                                                                                  :: ()) <-
                                                                                                 HS.pure
                                                                                                   ()
                                                                                               HS.pure
                                                                                                 (ContentStreamOp_beginInlineImageData
                                                                                                    _28)))
                                                                                        ((RTS.|||)
                                                                                           (RTS.pEnter
                                                                                              "setLineJoinStyle"
                                                                                              (do (_29
                                                                                                     :: ()) <-
                                                                                                    HS.pure
                                                                                                      ()
                                                                                                  HS.pure
                                                                                                    (ContentStreamOp_setLineJoinStyle
                                                                                                       _29)))
                                                                                           ((RTS.|||)
                                                                                              (RTS.pEnter
                                                                                                 "setLineCapStyle"
                                                                                                 (do (_30
                                                                                                        :: ()) <-
                                                                                                       HS.pure
                                                                                                         ()
                                                                                                     HS.pure
                                                                                                       (ContentStreamOp_setLineCapStyle
                                                                                                          _30)))
                                                                                              ((RTS.|||)
                                                                                                 (RTS.pEnter
                                                                                                    "setCMYKStroking"
                                                                                                    (do (_31
                                                                                                           :: ()) <-
                                                                                                          HS.pure
                                                                                                            ()
                                                                                                        HS.pure
                                                                                                          (ContentStreamOp_setCMYKStroking
                                                                                                             _31)))
                                                                                                 ((RTS.|||)
                                                                                                    (RTS.pEnter
                                                                                                       "setCMYKNonStroking"
                                                                                                       (do (_32
                                                                                                              :: ()) <-
                                                                                                             HS.pure
                                                                                                               ()
                                                                                                           HS.pure
                                                                                                             (ContentStreamOp_setCMYKNonStroking
                                                                                                                _32)))
                                                                                                    ((RTS.|||)
                                                                                                       (RTS.pEnter
                                                                                                          "appendLine"
                                                                                                          (do (_33
                                                                                                                 :: ()) <-
                                                                                                                HS.pure
                                                                                                                  ()
                                                                                                              HS.pure
                                                                                                                (ContentStreamOp_appendLine
                                                                                                                   _33)))
                                                                                                       ((RTS.|||)
                                                                                                          (RTS.pEnter
                                                                                                             "beginNewSuppath"
                                                                                                             (do (_34
                                                                                                                    :: ()) <-
                                                                                                                   HS.pure
                                                                                                                     ()
                                                                                                                 HS.pure
                                                                                                                   (ContentStreamOp_beginNewSuppath
                                                                                                                      _34)))
                                                                                                          ((RTS.|||)
                                                                                                             (RTS.pEnter
                                                                                                                "setMiterLimit"
                                                                                                                (do (_35
                                                                                                                       :: ()) <-
                                                                                                                      HS.pure
                                                                                                                        ()
                                                                                                                    HS.pure
                                                                                                                      (ContentStreamOp_setMiterLimit
                                                                                                                         _35)))
                                                                                                             ((RTS.|||)
                                                                                                                (RTS.pEnter
                                                                                                                   "defineMarkedContent"
                                                                                                                   (do (_36
                                                                                                                          :: ()) <-
                                                                                                                         HS.pure
                                                                                                                           ()
                                                                                                                       HS.pure
                                                                                                                         (ContentStreamOp_defineMarkedContent
                                                                                                                            _36)))
                                                                                                                ((RTS.|||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "endPath"
                                                                                                                      (do (_37
                                                                                                                             :: ()) <-
                                                                                                                            HS.pure
                                                                                                                              ()
                                                                                                                          HS.pure
                                                                                                                            (ContentStreamOp_endPath
                                                                                                                               _37)))
                                                                                                                   ((RTS.|||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "saveGraphicsState"
                                                                                                                         (do (_38
                                                                                                                                :: ()) <-
                                                                                                                               HS.pure
                                                                                                                                 ()
                                                                                                                             HS.pure
                                                                                                                               (ContentStreamOp_saveGraphicsState
                                                                                                                                  _38)))
                                                                                                                      ((RTS.|||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "restoreGraphicsState"
                                                                                                                            (do (_39
                                                                                                                                   :: ()) <-
                                                                                                                                  HS.pure
                                                                                                                                    ()
                                                                                                                                HS.pure
                                                                                                                                  (ContentStreamOp_restoreGraphicsState
                                                                                                                                     _39)))
                                                                                                                         ((RTS.|||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "appendRect"
                                                                                                                               (do (_40
                                                                                                                                      :: ()) <-
                                                                                                                                     HS.pure
                                                                                                                                       ()
                                                                                                                                   HS.pure
                                                                                                                                     (ContentStreamOp_appendRect
                                                                                                                                        _40)))
                                                                                                                            ((RTS.|||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "setRGBStroking"
                                                                                                                                  (do (_41
                                                                                                                                         :: ()) <-
                                                                                                                                        HS.pure
                                                                                                                                          ()
                                                                                                                                      HS.pure
                                                                                                                                        (ContentStreamOp_setRGBStroking
                                                                                                                                           _41)))
                                                                                                                               ((RTS.|||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "setRGBNonStroking"
                                                                                                                                     (do (_42
                                                                                                                                            :: ()) <-
                                                                                                                                           HS.pure
                                                                                                                                             ()
                                                                                                                                         HS.pure
                                                                                                                                           (ContentStreamOp_setRGBNonStroking
                                                                                                                                              _42)))
                                                                                                                                  ((RTS.|||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "setColorRenderingIntent"
                                                                                                                                        (do (_43
                                                                                                                                               :: ()) <-
                                                                                                                                              HS.pure
                                                                                                                                                ()
                                                                                                                                            HS.pure
                                                                                                                                              (ContentStreamOp_setColorRenderingIntent
                                                                                                                                                 _43)))
                                                                                                                                     ((RTS.|||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "closeStrokePath"
                                                                                                                                           (do (_44
                                                                                                                                                  :: ()) <-
                                                                                                                                                 HS.pure
                                                                                                                                                   ()
                                                                                                                                               HS.pure
                                                                                                                                                 (ContentStreamOp_closeStrokePath
                                                                                                                                                    _44)))
                                                                                                                                        ((RTS.|||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "stroke"
                                                                                                                                              (do (_45
                                                                                                                                                     :: ()) <-
                                                                                                                                                    HS.pure
                                                                                                                                                      ()
                                                                                                                                                  HS.pure
                                                                                                                                                    (ContentStreamOp_stroke
                                                                                                                                                       _45)))
                                                                                                                                           ((RTS.|||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "setColorStroking"
                                                                                                                                                 (do (_46
                                                                                                                                                        :: ()) <-
                                                                                                                                                       HS.pure
                                                                                                                                                         ()
                                                                                                                                                     HS.pure
                                                                                                                                                       (ContentStreamOp_setColorStroking
                                                                                                                                                          _46)))
                                                                                                                                              ((RTS.|||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "setColorNonStroking"
                                                                                                                                                    (do (_47
                                                                                                                                                           :: ()) <-
                                                                                                                                                          HS.pure
                                                                                                                                                            ()
                                                                                                                                                        HS.pure
                                                                                                                                                          (ContentStreamOp_setColorNonStroking
                                                                                                                                                             _47)))
                                                                                                                                                 ((RTS.|||)
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "setColorStrokingICC"
                                                                                                                                                       (do (_48
                                                                                                                                                              :: ()) <-
                                                                                                                                                             HS.pure
                                                                                                                                                               ()
                                                                                                                                                           HS.pure
                                                                                                                                                             (ContentStreamOp_setColorStrokingICC
                                                                                                                                                                _48)))
                                                                                                                                                    ((RTS.|||)
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "setColorNonStrokingICC"
                                                                                                                                                          (do (_49
                                                                                                                                                                 :: ()) <-
                                                                                                                                                                HS.pure
                                                                                                                                                                  ()
                                                                                                                                                              HS.pure
                                                                                                                                                                (ContentStreamOp_setColorNonStrokingICC
                                                                                                                                                                   _49)))
                                                                                                                                                       ((RTS.|||)
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "paintShadingPattern"
                                                                                                                                                             (do (_50
                                                                                                                                                                    :: ()) <-
                                                                                                                                                                   HS.pure
                                                                                                                                                                     ()
                                                                                                                                                                 HS.pure
                                                                                                                                                                   (ContentStreamOp_paintShadingPattern
                                                                                                                                                                      _50)))
                                                                                                                                                          ((RTS.|||)
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "moveStartText"
                                                                                                                                                                (do (_51
                                                                                                                                                                       :: ()) <-
                                                                                                                                                                      HS.pure
                                                                                                                                                                        ()
                                                                                                                                                                    HS.pure
                                                                                                                                                                      (ContentStreamOp_moveStartText
                                                                                                                                                                         _51)))
                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "setCharSpacing"
                                                                                                                                                                   (do (_52
                                                                                                                                                                          :: ()) <-
                                                                                                                                                                         HS.pure
                                                                                                                                                                           ()
                                                                                                                                                                       HS.pure
                                                                                                                                                                         (ContentStreamOp_setCharSpacing
                                                                                                                                                                            _52)))
                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "moveTextPos"
                                                                                                                                                                      (do (_53
                                                                                                                                                                             :: ()) <-
                                                                                                                                                                            HS.pure
                                                                                                                                                                              ()
                                                                                                                                                                          HS.pure
                                                                                                                                                                            (ContentStreamOp_moveTextPos
                                                                                                                                                                               _53)))
                                                                                                                                                                   ((RTS.|||)
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "moveTextPosSetLeading"
                                                                                                                                                                         (do (_54
                                                                                                                                                                                :: ()) <-
                                                                                                                                                                               HS.pure
                                                                                                                                                                                 ()
                                                                                                                                                                             HS.pure
                                                                                                                                                                               (ContentStreamOp_moveTextPosSetLeading
                                                                                                                                                                                  _54)))
                                                                                                                                                                      ((RTS.|||)
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "setTextFont"
                                                                                                                                                                            (do (_55
                                                                                                                                                                                   :: ()) <-
                                                                                                                                                                                  HS.pure
                                                                                                                                                                                    ()
                                                                                                                                                                                HS.pure
                                                                                                                                                                                  (ContentStreamOp_setTextFont
                                                                                                                                                                                     _55)))
                                                                                                                                                                         ((RTS.|||)
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "showText"
                                                                                                                                                                               (do (_56
                                                                                                                                                                                      :: ()) <-
                                                                                                                                                                                     HS.pure
                                                                                                                                                                                       ()
                                                                                                                                                                                   HS.pure
                                                                                                                                                                                     (ContentStreamOp_showText
                                                                                                                                                                                        _56)))
                                                                                                                                                                            ((RTS.|||)
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "showTextIndGlyph"
                                                                                                                                                                                  (do (_57
                                                                                                                                                                                         :: ()) <-
                                                                                                                                                                                        HS.pure
                                                                                                                                                                                          ()
                                                                                                                                                                                      HS.pure
                                                                                                                                                                                        (ContentStreamOp_showTextIndGlyph
                                                                                                                                                                                           _57)))
                                                                                                                                                                               ((RTS.|||)
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "setTextLeading"
                                                                                                                                                                                     (do (_58
                                                                                                                                                                                            :: ()) <-
                                                                                                                                                                                           HS.pure
                                                                                                                                                                                             ()
                                                                                                                                                                                         HS.pure
                                                                                                                                                                                           (ContentStreamOp_setTextLeading
                                                                                                                                                                                              _58)))
                                                                                                                                                                                  ((RTS.|||)
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "setTextMatrix"
                                                                                                                                                                                        (do (_59
                                                                                                                                                                                               :: ()) <-
                                                                                                                                                                                              HS.pure
                                                                                                                                                                                                ()
                                                                                                                                                                                            HS.pure
                                                                                                                                                                                              (ContentStreamOp_setTextMatrix
                                                                                                                                                                                                 _59)))
                                                                                                                                                                                     ((RTS.|||)
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "setTextRendering"
                                                                                                                                                                                           (do (_60
                                                                                                                                                                                                  :: ()) <-
                                                                                                                                                                                                 HS.pure
                                                                                                                                                                                                   ()
                                                                                                                                                                                               HS.pure
                                                                                                                                                                                                 (ContentStreamOp_setTextRendering
                                                                                                                                                                                                    _60)))
                                                                                                                                                                                        ((RTS.|||)
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "setTextRise"
                                                                                                                                                                                              (do (_61
                                                                                                                                                                                                     :: ()) <-
                                                                                                                                                                                                    HS.pure
                                                                                                                                                                                                      ()
                                                                                                                                                                                                  HS.pure
                                                                                                                                                                                                    (ContentStreamOp_setTextRise
                                                                                                                                                                                                       _61)))
                                                                                                                                                                                           ((RTS.|||)
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "setWordSpacing"
                                                                                                                                                                                                 (do (_62
                                                                                                                                                                                                        :: ()) <-
                                                                                                                                                                                                       HS.pure
                                                                                                                                                                                                         ()
                                                                                                                                                                                                     HS.pure
                                                                                                                                                                                                       (ContentStreamOp_setWordSpacing
                                                                                                                                                                                                          _62)))
                                                                                                                                                                                              ((RTS.|||)
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "setHorizontalTextScaling"
                                                                                                                                                                                                    (do (_63
                                                                                                                                                                                                           :: ()) <-
                                                                                                                                                                                                          HS.pure
                                                                                                                                                                                                            ()
                                                                                                                                                                                                        HS.pure
                                                                                                                                                                                                          (ContentStreamOp_setHorizontalTextScaling
                                                                                                                                                                                                             _63)))
                                                                                                                                                                                                 ((RTS.|||)
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "appendCurvedInitPtRepl"
                                                                                                                                                                                                       (do (_64
                                                                                                                                                                                                              :: ()) <-
                                                                                                                                                                                                             HS.pure
                                                                                                                                                                                                               ()
                                                                                                                                                                                                           HS.pure
                                                                                                                                                                                                             (ContentStreamOp_appendCurvedInitPtRepl
                                                                                                                                                                                                                _64)))
                                                                                                                                                                                                    ((RTS.|||)
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "setLineWidth"
                                                                                                                                                                                                          (do (_65
                                                                                                                                                                                                                 :: ()) <-
                                                                                                                                                                                                                HS.pure
                                                                                                                                                                                                                  ()
                                                                                                                                                                                                              HS.pure
                                                                                                                                                                                                                (ContentStreamOp_setLineWidth
                                                                                                                                                                                                                   _65)))
                                                                                                                                                                                                       ((RTS.|||)
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "setClippingNzWinding"
                                                                                                                                                                                                             (do (_66
                                                                                                                                                                                                                    :: ()) <-
                                                                                                                                                                                                                   HS.pure
                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                 HS.pure
                                                                                                                                                                                                                   (ContentStreamOp_setClippingNzWinding
                                                                                                                                                                                                                      _66)))
                                                                                                                                                                                                          ((RTS.|||)
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "setClippingEvenOdd"
                                                                                                                                                                                                                (do (_67
                                                                                                                                                                                                                       :: ()) <-
                                                                                                                                                                                                                      HS.pure
                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                    HS.pure
                                                                                                                                                                                                                      (ContentStreamOp_setClippingEvenOdd
                                                                                                                                                                                                                         _67)))
                                                                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "appendCurvedFinalPt"
                                                                                                                                                                                                                   (do (_68
                                                                                                                                                                                                                          :: ()) <-
                                                                                                                                                                                                                         HS.pure
                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                       HS.pure
                                                                                                                                                                                                                         (ContentStreamOp_appendCurvedFinalPt
                                                                                                                                                                                                                            _68)))
                                                                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "moveShow"
                                                                                                                                                                                                                      (do (_69
                                                                                                                                                                                                                             :: ()) <-
                                                                                                                                                                                                                            HS.pure
                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                          HS.pure
                                                                                                                                                                                                                            (ContentStreamOp_moveShow
                                                                                                                                                                                                                               _69)))
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "setSpacing"
                                                                                                                                                                                                                      (do (_70
                                                                                                                                                                                                                             :: ()) <-
                                                                                                                                                                                                                            HS.pure
                                                                                                                                                                                                                              ()
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
       (do (__ :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMany (RTS.<||)
               (RTS.pEnter "PdfValue.NameChar" PdfValue.pNameChar)
           HS.pure __))
 
pContentOp :: D.Parser ContentOp
 
pContentOp =
  (RTS.|||)
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
 
pEndCompat :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pEndCompat =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @(Vector.Vector (RTS.UInt 8))
       (HS.pure (Vector.vecFromRep "EX")))
 
_BeginCompat :: D.Parser ()
 
_BeginCompat =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @(Vector.Vector (RTS.UInt 8)) (HS.pure ()))
 
_ContentStreamOp :: D.Parser ()
 
_ContentStreamOp =
  (RTS.|||) (RTS.pEnter "closeFillStrokeNzWinding" (HS.pure ()))
    ((RTS.|||) (RTS.pEnter "fillStroke" (HS.pure ()))
       ((RTS.|||) (RTS.pEnter "closeFillStrokeEvenOdd" (HS.pure ()))
          ((RTS.|||) (RTS.pEnter "fillStrokeEvenOdd" (HS.pure ()))
             ((RTS.|||) (RTS.pEnter "beginMarkedContent" (HS.pure ()))
                ((RTS.|||) (RTS.pEnter "beginInline" (HS.pure ()))
                   ((RTS.|||) (RTS.pEnter "beginMarkedContent" (HS.pure ()))
                      ((RTS.|||) (RTS.pEnter "beginText" (HS.pure ()))
                         ((RTS.|||) (RTS.pEnter "appendCurvedThreePoints" (HS.pure ()))
                            ((RTS.|||) (RTS.pEnter "concatMatrix" (HS.pure ()))
                               ((RTS.|||) (RTS.pEnter "setColorSpaceStroking" (HS.pure ()))
                                  ((RTS.|||) (RTS.pEnter "setColorSpaceNonStroking" (HS.pure ()))
                                     ((RTS.|||) (RTS.pEnter "setLineDash" (HS.pure ()))
                                        ((RTS.|||) (RTS.pEnter "setGlyphWidth" (HS.pure ()))
                                           ((RTS.|||)
                                              (RTS.pEnter "setGlpyhWidthBoundingBox" (HS.pure ()))
                                              ((RTS.|||) (RTS.pEnter "invokeXObj" (HS.pure ()))
                                                 ((RTS.|||)
                                                    (RTS.pEnter "defMarkedContentPoint"
                                                       (HS.pure ()))
                                                    ((RTS.|||) (RTS.pEnter "endInline" (HS.pure ()))
                                                       ((RTS.|||)
                                                          (RTS.pEnter "endMarkedContent"
                                                             (HS.pure ()))
                                                          ((RTS.|||)
                                                             (RTS.pEnter "endTextObj" (HS.pure ()))
                                                             ((RTS.|||)
                                                                (RTS.pEnter "fillPathNzWinding"
                                                                   (HS.pure ()))
                                                                ((RTS.|||)
                                                                   (RTS.pEnter
                                                                      "fillPathNzWindingOld"
                                                                      (HS.pure ()))
                                                                   ((RTS.|||)
                                                                      (RTS.pEnter "fillPathEvenOdd"
                                                                         (HS.pure ()))
                                                                      ((RTS.|||)
                                                                         (RTS.pEnter
                                                                            "setGrayStroking"
                                                                            (HS.pure ()))
                                                                         ((RTS.|||)
                                                                            (RTS.pEnter
                                                                               "setGrayNonStroking"
                                                                               (HS.pure ()))
                                                                            ((RTS.|||)
                                                                               (RTS.pEnter
                                                                                  "setGraphicsStateParams"
                                                                                  (HS.pure ()))
                                                                               ((RTS.|||)
                                                                                  (RTS.pEnter
                                                                                     "closeSubpath"
                                                                                     (HS.pure ()))
                                                                                  ((RTS.|||)
                                                                                     (RTS.pEnter
                                                                                        "setFlat"
                                                                                        (HS.pure
                                                                                           ()))
                                                                                     ((RTS.|||)
                                                                                        (RTS.pEnter
                                                                                           "beginInlineImageData"
                                                                                           (HS.pure
                                                                                              ()))
                                                                                        ((RTS.|||)
                                                                                           (RTS.pEnter
                                                                                              "setLineJoinStyle"
                                                                                              (HS.pure
                                                                                                 ()))
                                                                                           ((RTS.|||)
                                                                                              (RTS.pEnter
                                                                                                 "setLineCapStyle"
                                                                                                 (HS.pure
                                                                                                    ()))
                                                                                              ((RTS.|||)
                                                                                                 (RTS.pEnter
                                                                                                    "setCMYKStroking"
                                                                                                    (HS.pure
                                                                                                       ()))
                                                                                                 ((RTS.|||)
                                                                                                    (RTS.pEnter
                                                                                                       "setCMYKNonStroking"
                                                                                                       (HS.pure
                                                                                                          ()))
                                                                                                    ((RTS.|||)
                                                                                                       (RTS.pEnter
                                                                                                          "appendLine"
                                                                                                          (HS.pure
                                                                                                             ()))
                                                                                                       ((RTS.|||)
                                                                                                          (RTS.pEnter
                                                                                                             "beginNewSuppath"
                                                                                                             (HS.pure
                                                                                                                ()))
                                                                                                          ((RTS.|||)
                                                                                                             (RTS.pEnter
                                                                                                                "setMiterLimit"
                                                                                                                (HS.pure
                                                                                                                   ()))
                                                                                                             ((RTS.|||)
                                                                                                                (RTS.pEnter
                                                                                                                   "defineMarkedContent"
                                                                                                                   (HS.pure
                                                                                                                      ()))
                                                                                                                ((RTS.|||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "endPath"
                                                                                                                      (HS.pure
                                                                                                                         ()))
                                                                                                                   ((RTS.|||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "saveGraphicsState"
                                                                                                                         (HS.pure
                                                                                                                            ()))
                                                                                                                      ((RTS.|||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "restoreGraphicsState"
                                                                                                                            (HS.pure
                                                                                                                               ()))
                                                                                                                         ((RTS.|||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "appendRect"
                                                                                                                               (HS.pure
                                                                                                                                  ()))
                                                                                                                            ((RTS.|||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "setRGBStroking"
                                                                                                                                  (HS.pure
                                                                                                                                     ()))
                                                                                                                               ((RTS.|||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "setRGBNonStroking"
                                                                                                                                     (HS.pure
                                                                                                                                        ()))
                                                                                                                                  ((RTS.|||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "setColorRenderingIntent"
                                                                                                                                        (HS.pure
                                                                                                                                           ()))
                                                                                                                                     ((RTS.|||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "closeStrokePath"
                                                                                                                                           (HS.pure
                                                                                                                                              ()))
                                                                                                                                        ((RTS.|||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "stroke"
                                                                                                                                              (HS.pure
                                                                                                                                                 ()))
                                                                                                                                           ((RTS.|||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "setColorStroking"
                                                                                                                                                 (HS.pure
                                                                                                                                                    ()))
                                                                                                                                              ((RTS.|||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "setColorNonStroking"
                                                                                                                                                    (HS.pure
                                                                                                                                                       ()))
                                                                                                                                                 ((RTS.|||)
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "setColorStrokingICC"
                                                                                                                                                       (HS.pure
                                                                                                                                                          ()))
                                                                                                                                                    ((RTS.|||)
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "setColorNonStrokingICC"
                                                                                                                                                          (HS.pure
                                                                                                                                                             ()))
                                                                                                                                                       ((RTS.|||)
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "paintShadingPattern"
                                                                                                                                                             (HS.pure
                                                                                                                                                                ()))
                                                                                                                                                          ((RTS.|||)
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "moveStartText"
                                                                                                                                                                (HS.pure
                                                                                                                                                                   ()))
                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "setCharSpacing"
                                                                                                                                                                   (HS.pure
                                                                                                                                                                      ()))
                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "moveTextPos"
                                                                                                                                                                      (HS.pure
                                                                                                                                                                         ()))
                                                                                                                                                                   ((RTS.|||)
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "moveTextPosSetLeading"
                                                                                                                                                                         (HS.pure
                                                                                                                                                                            ()))
                                                                                                                                                                      ((RTS.|||)
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "setTextFont"
                                                                                                                                                                            (HS.pure
                                                                                                                                                                               ()))
                                                                                                                                                                         ((RTS.|||)
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "showText"
                                                                                                                                                                               (HS.pure
                                                                                                                                                                                  ()))
                                                                                                                                                                            ((RTS.|||)
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "showTextIndGlyph"
                                                                                                                                                                                  (HS.pure
                                                                                                                                                                                     ()))
                                                                                                                                                                               ((RTS.|||)
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "setTextLeading"
                                                                                                                                                                                     (HS.pure
                                                                                                                                                                                        ()))
                                                                                                                                                                                  ((RTS.|||)
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "setTextMatrix"
                                                                                                                                                                                        (HS.pure
                                                                                                                                                                                           ()))
                                                                                                                                                                                     ((RTS.|||)
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "setTextRendering"
                                                                                                                                                                                           (HS.pure
                                                                                                                                                                                              ()))
                                                                                                                                                                                        ((RTS.|||)
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "setTextRise"
                                                                                                                                                                                              (HS.pure
                                                                                                                                                                                                 ()))
                                                                                                                                                                                           ((RTS.|||)
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "setWordSpacing"
                                                                                                                                                                                                 (HS.pure
                                                                                                                                                                                                    ()))
                                                                                                                                                                                              ((RTS.|||)
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "setHorizontalTextScaling"
                                                                                                                                                                                                    (HS.pure
                                                                                                                                                                                                       ()))
                                                                                                                                                                                                 ((RTS.|||)
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "appendCurvedInitPtRepl"
                                                                                                                                                                                                       (HS.pure
                                                                                                                                                                                                          ()))
                                                                                                                                                                                                    ((RTS.|||)
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "setLineWidth"
                                                                                                                                                                                                          (HS.pure
                                                                                                                                                                                                             ()))
                                                                                                                                                                                                       ((RTS.|||)
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "setClippingNzWinding"
                                                                                                                                                                                                             (HS.pure
                                                                                                                                                                                                                ()))
                                                                                                                                                                                                          ((RTS.|||)
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "setClippingEvenOdd"
                                                                                                                                                                                                                (HS.pure
                                                                                                                                                                                                                   ()))
                                                                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "appendCurvedFinalPt"
                                                                                                                                                                                                                   (HS.pure
                                                                                                                                                                                                                      ()))
                                                                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "moveShow"
                                                                                                                                                                                                                      (HS.pure
                                                                                                                                                                                                                         ()))
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "setSpacing"
                                                                                                                                                                                                                      (HS.pure
                                                                                                                                                                                                                         ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
_ContentStreamOpObj :: D.Parser ()
 
_ContentStreamOpObj =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @ContentStreamOp
       (RTS.pEnter "PdfContentStream._ContentStreamOp" _ContentStreamOp))
 
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
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @(Vector.Vector (RTS.UInt 8)) (HS.pure ()))
 
pContentStreamBody ::
      D.Parser ContentOp -> D.Parser (Vector.Vector ContentStreamBody_0)
 
pContentStreamBody (pOp :: D.Parser ContentOp) =
  RTS.pMany (RTS.<||)
    (do (__ :: ContentStreamBody_0) <-
          (RTS.|||)
            (RTS.pEnter "operand"
               (do (_81 :: DirectObj) <-
                     RTS.pEnter "PdfContentStream.ContentStreamOperandObj"
                       pContentStreamOperandObj
                   HS.pure (ContentStreamBody_0_operand _81)))
            ((RTS.|||)
               (RTS.pEnter "operation"
                  (do (_82 :: ContentStreamOp) <-
                        RTS.pEnter "PdfContentStream.ContentStreamOpObj"
                          pContentStreamOpObj
                      HS.pure (ContentStreamBody_0_operation _82)))
               (RTS.pEnter "compatSect"
                  (do (_83 :: Vector.Vector (RTS.UInt 8)) <-
                        do RTS.pEnter "PdfContentStream._BeginCompat" _BeginCompat
                           RTS.pEnter "PdfContentStream._ContentStreamBody" _ContentStreamBody
                           (__ :: Vector.Vector (RTS.UInt 8)) <-
                             RTS.pEnter "PdfContentStream.EndCompat" pEndCompat
                           HS.pure __
                      HS.pure (ContentStreamBody_0_compatSect _83))))
        HS.pure __)
 
_ContentStreamBody :: D.Parser ()
 
_ContentStreamBody =
  RTS.pSkipMany (RTS.<||)
    ((RTS.|||)
       (RTS.pEnter "operand"
          (RTS.pEnter "PdfContentStream._ContentStreamOperandObj"
             _ContentStreamOperandObj))
       ((RTS.|||)
          (RTS.pEnter "operation"
             (RTS.pEnter "PdfContentStream._ContentStreamOpObj"
                _ContentStreamOpObj))
          (RTS.pEnter "compatSect"
             (do RTS.pEnter "PdfContentStream._BeginCompat" _BeginCompat
                 do HS.void
                      (RTS.pEnter "PdfContentStream.ContentStreamBody"
                         (pContentStreamBody
                            (RTS.pEnter "PdfContentStream.ContentOp" pContentOp)))
                    HS.pure ()
                 RTS.pEnter "PdfContentStream._EndCompat" _EndCompat))))
 
pContentStream :: D.Parser (Vector.Vector ContentStreamBody_0)
 
pContentStream =
  RTS.pEnter "PdfContentStream.ContentStreamBody"
    (pContentStreamBody
       (do (_84 :: ContentStreamOp) <-
             RTS.pEnter "PdfContentStream.ContentStreamOpObj"
               pContentStreamOpObj
           HS.pure (ContentOp_knownOp _84)))
 
_OpName :: D.Parser ()
 
_OpName =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @(Vector.Vector (RTS.UInt 8))
       (RTS.pSkipMany (RTS.<||)
          (RTS.pEnter "PdfValue._NameChar" PdfValue._NameChar)))
 
_ContentOp :: D.Parser ()
 
_ContentOp =
  (RTS.|||)
    (RTS.pEnter "knownOp"
       (RTS.pEnter "PdfContentStream._ContentStreamOpObj"
          _ContentStreamOpObj))
    (RTS.pEnter "futureOp"
       (RTS.pEnter "PdfContentStream._OpName" _OpName))
 
_ContentStream :: D.Parser ()
 
_ContentStream =
  RTS.pEnter "PdfContentStream._ContentStreamBody" _ContentStreamBody
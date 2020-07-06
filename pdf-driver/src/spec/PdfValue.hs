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
import qualified ISOCodes
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Vector as Vector
import qualified RTS.Map as Map
 
 
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
 
data ContentStream_0
  = ContentStream_0_compatSect (Vector.Vector (RTS.UInt 8))
  | ContentStream_0_operand Value
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
           (HS.Maybe Value) where
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
    (do HS.const () HS.<$> RTS.pMatch1 "14:31--14:33" cs_cr
        (__ :: RTS.UInt 8) <-
          RTS.uint8 HS.<$> RTS.pMatch1 "14:36--14:38" cs_lf
        HS.pure __)
    (RTS.uint8 HS.<$> RTS.pMatch1 "14:44--14:46" cs_lf)
 
pEOL :: D.Parser (RTS.UInt 8)
 
pEOL =
  (RTS.<||) (RTS.pEnter "PdfValue.SimpleEOL" pSimpleEOL)
    (RTS.uint8 HS.<$> RTS.pMatch1 "15:42--15:44" cs_cr)
 
pComment :: D.Parser (RTS.UInt 8)
 
pComment =
  do HS.const ()
       HS.<$> RTS.pMatch "16:31--16:33" (Vector.vecFromRep "%")
     RTS.pSkipMany (RTS.<||)
       (HS.const ()
          HS.<$> RTS.pMatch1 "16:42--16:53"
                   (RTS.bcComplement (RTS.bcUnion cs_lf cs_cr)))
     (__ :: RTS.UInt 8) <- RTS.pEnter "PdfValue.EOL" pEOL
     HS.pure __
 
pAnyWS :: D.Parser (RTS.UInt 8)
 
pAnyWS =
  (RTS.|||)
    ((RTS.|||)
       (RTS.uint8 HS.<$> RTS.pMatch1 "17:29--17:37" cs_simpleWS)
       (RTS.pEnter "PdfValue.Comment" pComment))
    (RTS.pEnter "PdfValue.EOL" pEOL)
 
_SimpleEOL :: D.Parser ()
 
_SimpleEOL =
  (RTS.|||)
    (do HS.const () HS.<$> RTS.pMatch1 "14:31--14:33" cs_cr
        HS.const () HS.<$> RTS.pMatch1 "14:36--14:38" cs_lf)
    (HS.const () HS.<$> RTS.pMatch1 "14:44--14:46" cs_lf)
 
_EOL :: D.Parser ()
 
_EOL =
  (RTS.<||) (RTS.pEnter "PdfValue._SimpleEOL" _SimpleEOL)
    (HS.const () HS.<$> RTS.pMatch1 "15:42--15:44" cs_cr)
 
_Comment :: D.Parser ()
 
_Comment =
  do HS.const ()
       HS.<$> RTS.pMatch "16:31--16:33" (Vector.vecFromRep "%")
     RTS.pSkipMany (RTS.<||)
       (HS.const ()
          HS.<$> RTS.pMatch1 "16:42--16:53"
                   (RTS.bcComplement (RTS.bcUnion cs_lf cs_cr)))
     RTS.pEnter "PdfValue._EOL" _EOL
 
_AnyWS :: D.Parser ()
 
_AnyWS =
  (RTS.|||)
    ((RTS.|||)
       (HS.const () HS.<$> RTS.pMatch1 "17:29--17:37" cs_simpleWS)
       (RTS.pEnter "PdfValue._Comment" _Comment))
    (RTS.pEnter "PdfValue._EOL" _EOL)
 
_Token :: forall a. RTS.DDL a => D.Parser () -> D.Parser ()
 
_Token (_P :: D.Parser ()) =
  do _P
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._AnyWS" _AnyWS)
 
_KW :: forall a. RTS.DDL a => D.Parser () -> D.Parser ()
 
_KW (_P :: D.Parser ()) =
  RTS.pEnter "PdfValue._Token" (_Token @a _P)
 
pBetween ::
  forall c.
    RTS.DDL c =>
      Vector.Vector (RTS.UInt 8)
        -> (Vector.Vector (RTS.UInt 8) -> (D.Parser c -> D.Parser c))
 
pBetween (open :: Vector.Vector (RTS.UInt 8))
  (close :: Vector.Vector (RTS.UInt 8))
  (pP :: D.Parser c) =
  do RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const () HS.<$> RTS.pMatch "26:34--26:37" open))
     (__ :: c) <- pP
     RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const () HS.<$> RTS.pMatch "26:51--26:55" close))
     HS.pure __
 
pKW :: forall a. RTS.DDL a => D.Parser a -> D.Parser ()
 
pKW (pP :: D.Parser a) =
  RTS.pEnter "PdfValue._Token"
    (_Token @a
       (do HS.void pP
           HS.pure ()))
 
pWhen ::
  forall a b.
    (RTS.DDL a, RTS.DDL b) => D.Parser a -> (b -> D.Parser b)
 
pWhen (pP :: D.Parser a) (x :: b) =
  do do HS.void pP
        HS.pure ()
     (__ :: b) <- HS.pure x
     HS.pure __
 
pBool :: D.Parser HS.Bool
 
pBool =
  (RTS.|||)
    (RTS.pEnter "PdfValue.When"
       (pWhen @() @HS.Bool
          (RTS.pEnter "PdfValue.KW"
             (pKW @(Vector.Vector (RTS.UInt 8))
                (RTS.pMatch "37:14--37:19" (Vector.vecFromRep "true"))))
          HS.True))
    (RTS.pEnter "PdfValue.When"
       (pWhen @() @HS.Bool
          (RTS.pEnter "PdfValue.KW"
             (pKW @(Vector.Vector (RTS.UInt 8))
                (RTS.pMatch "38:14--38:20" (Vector.vecFromRep "false"))))
          HS.False))
 
pDigit :: D.Parser HS.Integer
 
pDigit =
  do (d :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "79:24--79:33"
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
               HS.<$> RTS.pMatch1 "82:24--82:33"
                        (RTS.bcRange (RTS.uint8 97) (RTS.uint8 102))
           (__ :: HS.Integer) <-
             HS.pure
               (RTS.add (RTS.lit 10 :: HS.Integer)
                  (RTS.convert (RTS.sub d (RTS.uint8 97)) :: HS.Integer))
           HS.pure __))
    (do (d :: RTS.UInt 8) <-
          RTS.uint8
            HS.<$> RTS.pMatch1 "83:24--83:33"
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
       HS.<$> RTS.pMatch "155:3--155:5" (Vector.vecFromRep "#")
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
     RTS.pGuard "158:3--158:8" "guard failed"
       ((RTS.lit 0 :: RTS.UInt 8) HS.< __)
     HS.pure __
 
pNameChar :: D.Parser (RTS.UInt 8)
 
pNameChar =
  (RTS.|||)
    (RTS.uint8
       HS.<$> RTS.pMatch1 "151:16--151:45"
                (RTS.bcComplement (RTS.bcByteString "\NUL\t\n\f\r ()<>[]{}/%#")))
    (RTS.pEnter "PdfValue.NameEsc" pNameEsc)
 
pToken :: forall a. RTS.DDL a => D.Parser a -> D.Parser a
 
pToken (pP :: D.Parser a) =
  do (__ :: a) <- pP
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._AnyWS" _AnyWS)
     HS.pure __
 
pName :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pName =
  RTS.pEnter "PdfValue.Token"
    (pToken @(Vector.Vector (RTS.UInt 8))
       (do HS.const ()
             HS.<$> RTS.pMatch "149:24--149:26" (Vector.vecFromRep "/")
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
       (RTS.pMatch "178:16--178:21" (Vector.vecFromRep "null")))
 
pSign :: D.Parser Sign
 
pSign =
  (RTS.|||)
    (RTS.pEnter "pos"
       (do (_4 :: ()) <-
             (RTS.|||)
               (HS.const ()
                  HS.<$> RTS.pMatch "52:11--52:13" (Vector.vecFromRep "+"))
               (HS.const ()
                  HS.<$> RTS.pMatch "52:17--52:18" (Vector.vecFromRep ""))
           HS.pure (Sign_pos _4)))
    (RTS.pEnter "neg"
       (do (_5 :: ()) <-
             HS.const ()
               HS.<$> RTS.pMatch "53:10--53:12" (Vector.vecFromRep "-")
           HS.pure (Sign_neg _5)))
 
pFrac :: HS.Integer -> (Number -> D.Parser Number)
 
pFrac (n :: HS.Integer) (w :: Number) =
  do (ds :: Vector.Vector HS.Integer) <-
       do HS.const ()
            HS.<$> RTS.pMatch "74:11--74:13" (Vector.vecFromRep ".")
          (__ :: Vector.Vector HS.Integer) <-
            RTS.pMinLength "74:16--74:32" n
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
       RTS.pMinLength "69:9--69:24" (RTS.lit 1 :: HS.Integer)
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
                  (pWhen @() @Number
                     (RTS.pIsJust "47:11--47:21" "Expected `pos`"
                        (HS.getField @"pos" sign))
                     n))
               (RTS.pEnter "PdfValue.When"
                  (pWhen @() @Number
                     (RTS.pIsJust "48:11--48:21" "Expected `neg`"
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
             HS.<$> RTS.pMatch "183:6--183:8" (Vector.vecFromRep "R")))
     HS.pure (Ref obj gen)
 
pOctDigit :: D.Parser HS.Integer
 
pOctDigit =
  do (d :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "80:24--80:33"
                  (RTS.bcRange (RTS.uint8 48) (RTS.uint8 55))
     (__ :: HS.Integer) <-
       HS.pure (RTS.convert (RTS.sub d (RTS.uint8 48)) :: HS.Integer)
     HS.pure __
 
pStringNumEsc :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringNumEsc =
  do (ds :: Vector.Vector HS.Integer) <-
       RTS.pMinLength "120:9--120:28" (RTS.lit 1 :: HS.Integer)
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
       HS.<$> RTS.pMatch "104:4--104:7" (Vector.vecFromRep "\\")
     (__ :: Vector.Vector (RTS.UInt 8)) <-
       (RTS.|||)
         (RTS.pEnter "PdfValue.When"
            (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
               (RTS.pMatch "106:11--106:13" (Vector.vecFromRep "n"))
               (Vector.vecFromRep "\n")))
         ((RTS.|||)
            (RTS.pEnter "PdfValue.When"
               (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                  (RTS.pMatch "107:11--107:13" (Vector.vecFromRep "r"))
                  (Vector.vecFromRep "\r")))
            ((RTS.|||)
               (RTS.pEnter "PdfValue.When"
                  (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                     (RTS.pMatch "108:11--108:13" (Vector.vecFromRep "t"))
                     (Vector.vecFromRep "\t")))
               ((RTS.|||)
                  (RTS.pEnter "PdfValue.When"
                     (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                        (RTS.pMatch "109:11--109:13" (Vector.vecFromRep "b"))
                        (Vector.vecFromRep "\b")))
                  ((RTS.|||)
                     (RTS.pEnter "PdfValue.When"
                        (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                           (RTS.pMatch "110:11--110:13" (Vector.vecFromRep "f"))
                           (Vector.vecFromRep "\f")))
                     ((RTS.|||)
                        (RTS.pEnter "PdfValue.When"
                           (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                              (RTS.pMatch "111:11--111:13" (Vector.vecFromRep "("))
                              (Vector.vecFromRep "(")))
                        ((RTS.|||)
                           (RTS.pEnter "PdfValue.When"
                              (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                 (RTS.pMatch "112:11--112:13" (Vector.vecFromRep ")"))
                                 (Vector.vecFromRep ")")))
                           ((RTS.|||)
                              (RTS.pEnter "PdfValue.When"
                                 (pWhen @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                    (RTS.pMatch "113:11--113:14" (Vector.vecFromRep "\\"))
                                    (Vector.vecFromRep "\\")))
                              ((RTS.|||)
                                 (RTS.pEnter "PdfValue.When"
                                    (pWhen @(RTS.UInt 8) @(Vector.Vector (RTS.UInt 8))
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
            RTS.pMatch "101:31--101:33" (Vector.vecFromRep "(")
          (_8 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pEnter "PdfValue.StringChars" pStringChars
          (_9 :: Vector.Vector (RTS.UInt 8)) <-
            RTS.pMatch "101:49--101:51" (Vector.vecFromRep ")")
          HS.pure (Vector.fromList [_7, _8, _9])
     HS.pure (Vector.concat _10)
 
pStringChunk :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pStringChunk =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue.StringInParens" pStringInParens)
       (RTS.pEnter "PdfValue.StringEsc" pStringEsc))
    (RTS.pMinLength "99:5--99:24" (RTS.lit 1 :: HS.Integer)
       (RTS.pMany (RTS.<||)
          (RTS.uint8
             HS.<$> RTS.pMatch1 "99:17--99:24"
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
            RTS.pIsJust "174:31--174:52" "Key already present"
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
 
pBeginCompat :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pBeginCompat =
  RTS.pEnter "PdfValue.Token"
    (pToken @(Vector.Vector (RTS.UInt 8))
       (RTS.pMatch "269:25--269:28" (Vector.vecFromRep "BX")))
 
pBmpByte :: D.Parser (RTS.UInt 8)
 
pBmpByte =
  (RTS.|||)
    (RTS.uint8
       HS.<$> RTS.pMatch1 "316:15--316:26"
                (RTS.bcRange (RTS.lit 0 :: RTS.UInt 8)
                   (RTS.lit 215 :: RTS.UInt 8)))
    (RTS.uint8
       HS.<$> RTS.pMatch1 "316:30--316:41"
                (RTS.bcRange (RTS.lit 224 :: RTS.UInt 8)
                   (RTS.lit 255 :: RTS.UInt 8)))
 
pBoundedTwoDigits ::
      HS.Integer -> (HS.Integer -> D.Parser HS.Integer)
 
pBoundedTwoDigits (lb :: HS.Integer) (ub :: HS.Integer) =
  do (digs :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 2 :: HS.Integer)
         (RTS.pEnter "PdfValue.Digit" pDigit)
     (__ :: HS.Integer) <-
       HS.pure
         (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
            (RTS.lit 10 :: HS.Integer)
            digs)
     RTS.pGuard "366:3--366:10" "guard failed" (lb HS.<= __)
     RTS.pGuard "366:14--366:21" "guard failed" (__ HS.<= ub)
     HS.pure __
 
pByte :: D.Parser (RTS.UInt 8)
 
pByte =
  RTS.uint8
    HS.<$> RTS.pMatch1 "300:12--300:19"
             (RTS.bcRange (RTS.lit 0 :: RTS.UInt 8) (RTS.lit 255 :: RTS.UInt 8))
 
pEndCompat :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pEndCompat =
  RTS.pEnter "PdfValue.Token"
    (pToken @(Vector.Vector (RTS.UInt 8))
       (RTS.pMatch "272:23--272:26" (Vector.vecFromRep "EX")))
 
pContentStreamOp :: D.Parser ContentStreamOp
 
pContentStreamOp =
  (RTS.|||)
    (RTS.pEnter "closeFillStrokeNzWinding"
       (do (_20 :: ()) <-
             HS.const ()
               HS.<$> RTS.pMatch "192:31--192:33" (Vector.vecFromRep "b")
           HS.pure (ContentStreamOp_closeFillStrokeNzWinding _20)))
    ((RTS.|||)
       (RTS.pEnter "fillStroke"
          (do (_21 :: ()) <-
                HS.const ()
                  HS.<$> RTS.pMatch "193:17--193:19" (Vector.vecFromRep "B")
              HS.pure (ContentStreamOp_fillStroke _21)))
       ((RTS.|||)
          (RTS.pEnter "closeFillStrokeEvenOdd"
             (do (_22 :: ()) <-
                   HS.const ()
                     HS.<$> RTS.pMatch "194:29--194:32" (Vector.vecFromRep "b*")
                 HS.pure (ContentStreamOp_closeFillStrokeEvenOdd _22)))
          ((RTS.|||)
             (RTS.pEnter "fillStrokeEvenOdd"
                (do (_23 :: ()) <-
                      HS.const ()
                        HS.<$> RTS.pMatch "195:24--195:27" (Vector.vecFromRep "B*")
                    HS.pure (ContentStreamOp_fillStrokeEvenOdd _23)))
             ((RTS.|||)
                (RTS.pEnter "beginMarkedContent"
                   (do (_24 :: ()) <-
                         HS.const ()
                           HS.<$> RTS.pMatch "196:25--196:29" (Vector.vecFromRep "BDC")
                       HS.pure (ContentStreamOp_beginMarkedContent _24)))
                ((RTS.|||)
                   (RTS.pEnter "beginInline"
                      (do (_25 :: ()) <-
                            HS.const ()
                              HS.<$> RTS.pMatch "197:18--197:21" (Vector.vecFromRep "Bl")
                          HS.pure (ContentStreamOp_beginInline _25)))
                   ((RTS.|||)
                      (RTS.pEnter "beginMarkedContent"
                         (do (_26 :: ()) <-
                               HS.const ()
                                 HS.<$> RTS.pMatch "198:25--198:29" (Vector.vecFromRep "BMC")
                             HS.pure (ContentStreamOp_beginMarkedContent _26)))
                      ((RTS.|||)
                         (RTS.pEnter "beginText"
                            (do (_27 :: ()) <-
                                  HS.const ()
                                    HS.<$> RTS.pMatch "199:16--199:19" (Vector.vecFromRep "BT")
                                HS.pure (ContentStreamOp_beginText _27)))
                         ((RTS.|||)
                            (RTS.pEnter "appendCurvedThreePoints"
                               (do (_28 :: ()) <-
                                     HS.const ()
                                       HS.<$> RTS.pMatch "200:30--200:32" (Vector.vecFromRep "c")
                                   HS.pure (ContentStreamOp_appendCurvedThreePoints _28)))
                            ((RTS.|||)
                               (RTS.pEnter "concatMatrix"
                                  (do (_29 :: ()) <-
                                        HS.const ()
                                          HS.<$> RTS.pMatch "201:19--201:22"
                                                   (Vector.vecFromRep "cm")
                                      HS.pure (ContentStreamOp_concatMatrix _29)))
                               ((RTS.|||)
                                  (RTS.pEnter "setColorSpaceStroking"
                                     (do (_30 :: ()) <-
                                           HS.const ()
                                             HS.<$> RTS.pMatch "202:28--202:31"
                                                      (Vector.vecFromRep "CS")
                                         HS.pure (ContentStreamOp_setColorSpaceStroking _30)))
                                  ((RTS.|||)
                                     (RTS.pEnter "setColorSpaceNonStroking"
                                        (do (_31 :: ()) <-
                                              HS.const ()
                                                HS.<$> RTS.pMatch "203:31--203:34"
                                                         (Vector.vecFromRep "cs")
                                            HS.pure (ContentStreamOp_setColorSpaceNonStroking _31)))
                                     ((RTS.|||)
                                        (RTS.pEnter "setLineDash"
                                           (do (_32 :: ()) <-
                                                 HS.const ()
                                                   HS.<$> RTS.pMatch "204:18--204:20"
                                                            (Vector.vecFromRep "d")
                                               HS.pure (ContentStreamOp_setLineDash _32)))
                                        ((RTS.|||)
                                           (RTS.pEnter "setGlyphWidth"
                                              (do (_33 :: ()) <-
                                                    HS.const ()
                                                      HS.<$> RTS.pMatch "205:20--205:23"
                                                               (Vector.vecFromRep "d0")
                                                  HS.pure (ContentStreamOp_setGlyphWidth _33)))
                                           ((RTS.|||)
                                              (RTS.pEnter "setGlpyhWidthBoundingBox"
                                                 (do (_34 :: ()) <-
                                                       HS.const ()
                                                         HS.<$> RTS.pMatch "206:31--206:34"
                                                                  (Vector.vecFromRep "d1")
                                                     HS.pure
                                                       (ContentStreamOp_setGlpyhWidthBoundingBox
                                                          _34)))
                                              ((RTS.|||)
                                                 (RTS.pEnter "invokeXObj"
                                                    (do (_35 :: ()) <-
                                                          HS.const ()
                                                            HS.<$> RTS.pMatch "207:17--207:20"
                                                                     (Vector.vecFromRep "Do")
                                                        HS.pure (ContentStreamOp_invokeXObj _35)))
                                                 ((RTS.|||)
                                                    (RTS.pEnter "defMarkedContentPoint"
                                                       (do (_36 :: ()) <-
                                                             HS.const ()
                                                               HS.<$> RTS.pMatch "208:28--208:31"
                                                                        (Vector.vecFromRep "DP")
                                                           HS.pure
                                                             (ContentStreamOp_defMarkedContentPoint
                                                                _36)))
                                                    ((RTS.|||)
                                                       (RTS.pEnter "endInline"
                                                          (do (_37 :: ()) <-
                                                                HS.const ()
                                                                  HS.<$> RTS.pMatch "209:16--209:19"
                                                                           (Vector.vecFromRep "El")
                                                              HS.pure
                                                                (ContentStreamOp_endInline _37)))
                                                       ((RTS.|||)
                                                          (RTS.pEnter "endMarkedContent"
                                                             (do (_38 :: ()) <-
                                                                   HS.const ()
                                                                     HS.<$> RTS.pMatch
                                                                              "210:23--210:27"
                                                                              (Vector.vecFromRep
                                                                                 "EMC")
                                                                 HS.pure
                                                                   (ContentStreamOp_endMarkedContent
                                                                      _38)))
                                                          ((RTS.|||)
                                                             (RTS.pEnter "endTextObj"
                                                                (do (_39 :: ()) <-
                                                                      HS.const ()
                                                                        HS.<$> RTS.pMatch
                                                                                 "211:17--211:20"
                                                                                 (Vector.vecFromRep
                                                                                    "ET")
                                                                    HS.pure
                                                                      (ContentStreamOp_endTextObj
                                                                         _39)))
                                                             ((RTS.|||)
                                                                (RTS.pEnter "fillPathNzWinding"
                                                                   (do (_40 :: ()) <-
                                                                         HS.const ()
                                                                           HS.<$> RTS.pMatch
                                                                                    "212:24--212:26"
                                                                                    (Vector.vecFromRep
                                                                                       "f")
                                                                       HS.pure
                                                                         (ContentStreamOp_fillPathNzWinding
                                                                            _40)))
                                                                ((RTS.|||)
                                                                   (RTS.pEnter
                                                                      "fillPathNzWindingOld"
                                                                      (do (_41 :: ()) <-
                                                                            HS.const ()
                                                                              HS.<$> RTS.pMatch
                                                                                       "213:27--213:29"
                                                                                       (Vector.vecFromRep
                                                                                          "F")
                                                                          HS.pure
                                                                            (ContentStreamOp_fillPathNzWindingOld
                                                                               _41)))
                                                                   ((RTS.|||)
                                                                      (RTS.pEnter "fillPathEvenOdd"
                                                                         (do (_42 :: ()) <-
                                                                               HS.const ()
                                                                                 HS.<$> RTS.pMatch
                                                                                          "214:22--214:25"
                                                                                          (Vector.vecFromRep
                                                                                             "f*")
                                                                             HS.pure
                                                                               (ContentStreamOp_fillPathEvenOdd
                                                                                  _42)))
                                                                      ((RTS.|||)
                                                                         (RTS.pEnter
                                                                            "setGrayStroking"
                                                                            (do (_43 :: ()) <-
                                                                                  HS.const ()
                                                                                    HS.<$> RTS.pMatch
                                                                                             "215:22--215:24"
                                                                                             (Vector.vecFromRep
                                                                                                "G")
                                                                                HS.pure
                                                                                  (ContentStreamOp_setGrayStroking
                                                                                     _43)))
                                                                         ((RTS.|||)
                                                                            (RTS.pEnter
                                                                               "setGrayNonStroking"
                                                                               (do (_44 :: ()) <-
                                                                                     HS.const ()
                                                                                       HS.<$> RTS.pMatch
                                                                                                "216:25--216:27"
                                                                                                (Vector.vecFromRep
                                                                                                   "g")
                                                                                   HS.pure
                                                                                     (ContentStreamOp_setGrayNonStroking
                                                                                        _44)))
                                                                            ((RTS.|||)
                                                                               (RTS.pEnter
                                                                                  "setGraphicsStateParams"
                                                                                  (do (_45 :: ()) <-
                                                                                        HS.const ()
                                                                                          HS.<$> RTS.pMatch
                                                                                                   "217:29--217:32"
                                                                                                   (Vector.vecFromRep
                                                                                                      "gs")
                                                                                      HS.pure
                                                                                        (ContentStreamOp_setGraphicsStateParams
                                                                                           _45)))
                                                                               ((RTS.|||)
                                                                                  (RTS.pEnter
                                                                                     "closeSubpath"
                                                                                     (do (_46
                                                                                            :: ()) <-
                                                                                           HS.const
                                                                                             ()
                                                                                             HS.<$> RTS.pMatch
                                                                                                      "218:19--218:21"
                                                                                                      (Vector.vecFromRep
                                                                                                         "h")
                                                                                         HS.pure
                                                                                           (ContentStreamOp_closeSubpath
                                                                                              _46)))
                                                                                  ((RTS.|||)
                                                                                     (RTS.pEnter
                                                                                        "setFlat"
                                                                                        (do (_47
                                                                                               :: ()) <-
                                                                                              HS.const
                                                                                                ()
                                                                                                HS.<$> RTS.pMatch
                                                                                                         "219:14--219:16"
                                                                                                         (Vector.vecFromRep
                                                                                                            "i")
                                                                                            HS.pure
                                                                                              (ContentStreamOp_setFlat
                                                                                                 _47)))
                                                                                     ((RTS.|||)
                                                                                        (RTS.pEnter
                                                                                           "beginInlineImageData"
                                                                                           (do (_48
                                                                                                  :: ()) <-
                                                                                                 HS.const
                                                                                                   ()
                                                                                                   HS.<$> RTS.pMatch
                                                                                                            "220:27--220:30"
                                                                                                            (Vector.vecFromRep
                                                                                                               "ID")
                                                                                               HS.pure
                                                                                                 (ContentStreamOp_beginInlineImageData
                                                                                                    _48)))
                                                                                        ((RTS.|||)
                                                                                           (RTS.pEnter
                                                                                              "setLineJoinStyle"
                                                                                              (do (_49
                                                                                                     :: ()) <-
                                                                                                    HS.const
                                                                                                      ()
                                                                                                      HS.<$> RTS.pMatch
                                                                                                               "221:23--221:25"
                                                                                                               (Vector.vecFromRep
                                                                                                                  "j")
                                                                                                  HS.pure
                                                                                                    (ContentStreamOp_setLineJoinStyle
                                                                                                       _49)))
                                                                                           ((RTS.|||)
                                                                                              (RTS.pEnter
                                                                                                 "setLineCapStyle"
                                                                                                 (do (_50
                                                                                                        :: ()) <-
                                                                                                       HS.const
                                                                                                         ()
                                                                                                         HS.<$> RTS.pMatch
                                                                                                                  "222:22--222:24"
                                                                                                                  (Vector.vecFromRep
                                                                                                                     "J")
                                                                                                     HS.pure
                                                                                                       (ContentStreamOp_setLineCapStyle
                                                                                                          _50)))
                                                                                              ((RTS.|||)
                                                                                                 (RTS.pEnter
                                                                                                    "setCMYKStroking"
                                                                                                    (do (_51
                                                                                                           :: ()) <-
                                                                                                          HS.const
                                                                                                            ()
                                                                                                            HS.<$> RTS.pMatch
                                                                                                                     "223:22--223:24"
                                                                                                                     (Vector.vecFromRep
                                                                                                                        "K")
                                                                                                        HS.pure
                                                                                                          (ContentStreamOp_setCMYKStroking
                                                                                                             _51)))
                                                                                                 ((RTS.|||)
                                                                                                    (RTS.pEnter
                                                                                                       "setCMYKNonStroking"
                                                                                                       (do (_52
                                                                                                              :: ()) <-
                                                                                                             HS.const
                                                                                                               ()
                                                                                                               HS.<$> RTS.pMatch
                                                                                                                        "224:25--224:27"
                                                                                                                        (Vector.vecFromRep
                                                                                                                           "k")
                                                                                                           HS.pure
                                                                                                             (ContentStreamOp_setCMYKNonStroking
                                                                                                                _52)))
                                                                                                    ((RTS.|||)
                                                                                                       (RTS.pEnter
                                                                                                          "appendLine"
                                                                                                          (do (_53
                                                                                                                 :: ()) <-
                                                                                                                HS.const
                                                                                                                  ()
                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                           "225:17--225:19"
                                                                                                                           (Vector.vecFromRep
                                                                                                                              "l")
                                                                                                              HS.pure
                                                                                                                (ContentStreamOp_appendLine
                                                                                                                   _53)))
                                                                                                       ((RTS.|||)
                                                                                                          (RTS.pEnter
                                                                                                             "beginNewSuppath"
                                                                                                             (do (_54
                                                                                                                    :: ()) <-
                                                                                                                   HS.const
                                                                                                                     ()
                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                              "226:22--226:24"
                                                                                                                              (Vector.vecFromRep
                                                                                                                                 "m")
                                                                                                                 HS.pure
                                                                                                                   (ContentStreamOp_beginNewSuppath
                                                                                                                      _54)))
                                                                                                          ((RTS.|||)
                                                                                                             (RTS.pEnter
                                                                                                                "setMiterLimit"
                                                                                                                (do (_55
                                                                                                                       :: ()) <-
                                                                                                                      HS.const
                                                                                                                        ()
                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                 "227:20--227:22"
                                                                                                                                 (Vector.vecFromRep
                                                                                                                                    "M")
                                                                                                                    HS.pure
                                                                                                                      (ContentStreamOp_setMiterLimit
                                                                                                                         _55)))
                                                                                                             ((RTS.|||)
                                                                                                                (RTS.pEnter
                                                                                                                   "defineMarkedContent"
                                                                                                                   (do (_56
                                                                                                                          :: ()) <-
                                                                                                                         HS.const
                                                                                                                           ()
                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                    "228:26--228:29"
                                                                                                                                    (Vector.vecFromRep
                                                                                                                                       "MP")
                                                                                                                       HS.pure
                                                                                                                         (ContentStreamOp_defineMarkedContent
                                                                                                                            _56)))
                                                                                                                ((RTS.|||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "endPath"
                                                                                                                      (do (_57
                                                                                                                             :: ()) <-
                                                                                                                            HS.const
                                                                                                                              ()
                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                       "229:14--229:16"
                                                                                                                                       (Vector.vecFromRep
                                                                                                                                          "n")
                                                                                                                          HS.pure
                                                                                                                            (ContentStreamOp_endPath
                                                                                                                               _57)))
                                                                                                                   ((RTS.|||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "saveGraphicsState"
                                                                                                                         (do (_58
                                                                                                                                :: ()) <-
                                                                                                                               HS.const
                                                                                                                                 ()
                                                                                                                                 HS.<$> RTS.pMatch
                                                                                                                                          "230:24--230:26"
                                                                                                                                          (Vector.vecFromRep
                                                                                                                                             "q")
                                                                                                                             HS.pure
                                                                                                                               (ContentStreamOp_saveGraphicsState
                                                                                                                                  _58)))
                                                                                                                      ((RTS.|||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "restoreGraphicsState"
                                                                                                                            (do (_59
                                                                                                                                   :: ()) <-
                                                                                                                                  HS.const
                                                                                                                                    ()
                                                                                                                                    HS.<$> RTS.pMatch
                                                                                                                                             "231:27--231:29"
                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                "Q")
                                                                                                                                HS.pure
                                                                                                                                  (ContentStreamOp_restoreGraphicsState
                                                                                                                                     _59)))
                                                                                                                         ((RTS.|||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "appendRect"
                                                                                                                               (do (_60
                                                                                                                                      :: ()) <-
                                                                                                                                     HS.const
                                                                                                                                       ()
                                                                                                                                       HS.<$> RTS.pMatch
                                                                                                                                                "232:17--232:20"
                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                   "re")
                                                                                                                                   HS.pure
                                                                                                                                     (ContentStreamOp_appendRect
                                                                                                                                        _60)))
                                                                                                                            ((RTS.|||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "setRGBStroking"
                                                                                                                                  (do (_61
                                                                                                                                         :: ()) <-
                                                                                                                                        HS.const
                                                                                                                                          ()
                                                                                                                                          HS.<$> RTS.pMatch
                                                                                                                                                   "233:21--233:24"
                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                      "RG")
                                                                                                                                      HS.pure
                                                                                                                                        (ContentStreamOp_setRGBStroking
                                                                                                                                           _61)))
                                                                                                                               ((RTS.|||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "setRGBNonStroking"
                                                                                                                                     (do (_62
                                                                                                                                            :: ()) <-
                                                                                                                                           HS.const
                                                                                                                                             ()
                                                                                                                                             HS.<$> RTS.pMatch
                                                                                                                                                      "234:24--234:27"
                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                         "rg")
                                                                                                                                         HS.pure
                                                                                                                                           (ContentStreamOp_setRGBNonStroking
                                                                                                                                              _62)))
                                                                                                                                  ((RTS.|||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "setColorRenderingIntent"
                                                                                                                                        (do (_63
                                                                                                                                               :: ()) <-
                                                                                                                                              HS.const
                                                                                                                                                ()
                                                                                                                                                HS.<$> RTS.pMatch
                                                                                                                                                         "235:30--235:33"
                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                            "ri")
                                                                                                                                            HS.pure
                                                                                                                                              (ContentStreamOp_setColorRenderingIntent
                                                                                                                                                 _63)))
                                                                                                                                     ((RTS.|||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "closeStrokePath"
                                                                                                                                           (do (_64
                                                                                                                                                  :: ()) <-
                                                                                                                                                 HS.const
                                                                                                                                                   ()
                                                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                                                            "236:22--236:24"
                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                               "s")
                                                                                                                                               HS.pure
                                                                                                                                                 (ContentStreamOp_closeStrokePath
                                                                                                                                                    _64)))
                                                                                                                                        ((RTS.|||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "stroke"
                                                                                                                                              (do (_65
                                                                                                                                                     :: ()) <-
                                                                                                                                                    HS.const
                                                                                                                                                      ()
                                                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                                                               "237:13--237:15"
                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                  "S")
                                                                                                                                                  HS.pure
                                                                                                                                                    (ContentStreamOp_stroke
                                                                                                                                                       _65)))
                                                                                                                                           ((RTS.|||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "setColorStroking"
                                                                                                                                                 (do (_66
                                                                                                                                                        :: ()) <-
                                                                                                                                                       HS.const
                                                                                                                                                         ()
                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                  "238:23--238:26"
                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                     "SC")
                                                                                                                                                     HS.pure
                                                                                                                                                       (ContentStreamOp_setColorStroking
                                                                                                                                                          _66)))
                                                                                                                                              ((RTS.|||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "setColorNonStroking"
                                                                                                                                                    (do (_67
                                                                                                                                                           :: ()) <-
                                                                                                                                                          HS.const
                                                                                                                                                            ()
                                                                                                                                                            HS.<$> RTS.pMatch
                                                                                                                                                                     "239:26--239:29"
                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                        "sc")
                                                                                                                                                        HS.pure
                                                                                                                                                          (ContentStreamOp_setColorNonStroking
                                                                                                                                                             _67)))
                                                                                                                                                 ((RTS.|||)
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "setColorStrokingICC"
                                                                                                                                                       (do (_68
                                                                                                                                                              :: ()) <-
                                                                                                                                                             HS.const
                                                                                                                                                               ()
                                                                                                                                                               HS.<$> RTS.pMatch
                                                                                                                                                                        "240:26--240:30"
                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                           "SCN")
                                                                                                                                                           HS.pure
                                                                                                                                                             (ContentStreamOp_setColorStrokingICC
                                                                                                                                                                _68)))
                                                                                                                                                    ((RTS.|||)
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "setColorNonStrokingICC"
                                                                                                                                                          (do (_69
                                                                                                                                                                 :: ()) <-
                                                                                                                                                                HS.const
                                                                                                                                                                  ()
                                                                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                                                                           "241:29--241:33"
                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                              "scn")
                                                                                                                                                              HS.pure
                                                                                                                                                                (ContentStreamOp_setColorNonStrokingICC
                                                                                                                                                                   _69)))
                                                                                                                                                       ((RTS.|||)
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "paintShadingPattern"
                                                                                                                                                             (do (_70
                                                                                                                                                                    :: ()) <-
                                                                                                                                                                   HS.const
                                                                                                                                                                     ()
                                                                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                                                                              "242:26--242:29"
                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                 "sh")
                                                                                                                                                                 HS.pure
                                                                                                                                                                   (ContentStreamOp_paintShadingPattern
                                                                                                                                                                      _70)))
                                                                                                                                                          ((RTS.|||)
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "moveStartText"
                                                                                                                                                                (do (_71
                                                                                                                                                                       :: ()) <-
                                                                                                                                                                      HS.const
                                                                                                                                                                        ()
                                                                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                                                                 "243:20--243:23"
                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                    "T*")
                                                                                                                                                                    HS.pure
                                                                                                                                                                      (ContentStreamOp_moveStartText
                                                                                                                                                                         _71)))
                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "setCharSpacing"
                                                                                                                                                                   (do (_72
                                                                                                                                                                          :: ()) <-
                                                                                                                                                                         HS.const
                                                                                                                                                                           ()
                                                                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                                                                    "244:21--244:24"
                                                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                                                       "Tc")
                                                                                                                                                                       HS.pure
                                                                                                                                                                         (ContentStreamOp_setCharSpacing
                                                                                                                                                                            _72)))
                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "moveTextPos"
                                                                                                                                                                      (do (_73
                                                                                                                                                                             :: ()) <-
                                                                                                                                                                            HS.const
                                                                                                                                                                              ()
                                                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                                                       "245:18--245:21"
                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                          "Td")
                                                                                                                                                                          HS.pure
                                                                                                                                                                            (ContentStreamOp_moveTextPos
                                                                                                                                                                               _73)))
                                                                                                                                                                   ((RTS.|||)
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "moveTextPosSetLeading"
                                                                                                                                                                         (do (_74
                                                                                                                                                                                :: ()) <-
                                                                                                                                                                               HS.const
                                                                                                                                                                                 ()
                                                                                                                                                                                 HS.<$> RTS.pMatch
                                                                                                                                                                                          "246:28--246:31"
                                                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                                                             "TD")
                                                                                                                                                                             HS.pure
                                                                                                                                                                               (ContentStreamOp_moveTextPosSetLeading
                                                                                                                                                                                  _74)))
                                                                                                                                                                      ((RTS.|||)
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "setTextFont"
                                                                                                                                                                            (do (_75
                                                                                                                                                                                   :: ()) <-
                                                                                                                                                                                  HS.const
                                                                                                                                                                                    ()
                                                                                                                                                                                    HS.<$> RTS.pMatch
                                                                                                                                                                                             "247:18--247:21"
                                                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                                                "Tf")
                                                                                                                                                                                HS.pure
                                                                                                                                                                                  (ContentStreamOp_setTextFont
                                                                                                                                                                                     _75)))
                                                                                                                                                                         ((RTS.|||)
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "showText"
                                                                                                                                                                               (do (_76
                                                                                                                                                                                      :: ()) <-
                                                                                                                                                                                     HS.const
                                                                                                                                                                                       ()
                                                                                                                                                                                       HS.<$> RTS.pMatch
                                                                                                                                                                                                "248:15--248:18"
                                                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                                                   "Tj")
                                                                                                                                                                                   HS.pure
                                                                                                                                                                                     (ContentStreamOp_showText
                                                                                                                                                                                        _76)))
                                                                                                                                                                            ((RTS.|||)
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "showTextIndGlyph"
                                                                                                                                                                                  (do (_77
                                                                                                                                                                                         :: ()) <-
                                                                                                                                                                                        HS.const
                                                                                                                                                                                          ()
                                                                                                                                                                                          HS.<$> RTS.pMatch
                                                                                                                                                                                                   "249:23--249:26"
                                                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                                                      "TJ")
                                                                                                                                                                                      HS.pure
                                                                                                                                                                                        (ContentStreamOp_showTextIndGlyph
                                                                                                                                                                                           _77)))
                                                                                                                                                                               ((RTS.|||)
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "setTextLeading"
                                                                                                                                                                                     (do (_78
                                                                                                                                                                                            :: ()) <-
                                                                                                                                                                                           HS.const
                                                                                                                                                                                             ()
                                                                                                                                                                                             HS.<$> RTS.pMatch
                                                                                                                                                                                                      "250:21--250:24"
                                                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                                                         "TL")
                                                                                                                                                                                         HS.pure
                                                                                                                                                                                           (ContentStreamOp_setTextLeading
                                                                                                                                                                                              _78)))
                                                                                                                                                                                  ((RTS.|||)
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "setTextMatrix"
                                                                                                                                                                                        (do (_79
                                                                                                                                                                                               :: ()) <-
                                                                                                                                                                                              HS.const
                                                                                                                                                                                                ()
                                                                                                                                                                                                HS.<$> RTS.pMatch
                                                                                                                                                                                                         "251:20--251:23"
                                                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                                                            "Tm")
                                                                                                                                                                                            HS.pure
                                                                                                                                                                                              (ContentStreamOp_setTextMatrix
                                                                                                                                                                                                 _79)))
                                                                                                                                                                                     ((RTS.|||)
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "setTextRendering"
                                                                                                                                                                                           (do (_80
                                                                                                                                                                                                  :: ()) <-
                                                                                                                                                                                                 HS.const
                                                                                                                                                                                                   ()
                                                                                                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                                                                                                            "252:23--252:26"
                                                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                                                               "Tr")
                                                                                                                                                                                               HS.pure
                                                                                                                                                                                                 (ContentStreamOp_setTextRendering
                                                                                                                                                                                                    _80)))
                                                                                                                                                                                        ((RTS.|||)
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "setTextRise"
                                                                                                                                                                                              (do (_81
                                                                                                                                                                                                     :: ()) <-
                                                                                                                                                                                                    HS.const
                                                                                                                                                                                                      ()
                                                                                                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                                                                                                               "253:18--253:21"
                                                                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                                                                  "Ts")
                                                                                                                                                                                                  HS.pure
                                                                                                                                                                                                    (ContentStreamOp_setTextRise
                                                                                                                                                                                                       _81)))
                                                                                                                                                                                           ((RTS.|||)
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "setWordSpacing"
                                                                                                                                                                                                 (do (_82
                                                                                                                                                                                                        :: ()) <-
                                                                                                                                                                                                       HS.const
                                                                                                                                                                                                         ()
                                                                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                                                                  "254:21--254:24"
                                                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                                                     "Tw")
                                                                                                                                                                                                     HS.pure
                                                                                                                                                                                                       (ContentStreamOp_setWordSpacing
                                                                                                                                                                                                          _82)))
                                                                                                                                                                                              ((RTS.|||)
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "setHorizontalTextScaling"
                                                                                                                                                                                                    (do (_83
                                                                                                                                                                                                           :: ()) <-
                                                                                                                                                                                                          HS.const
                                                                                                                                                                                                            ()
                                                                                                                                                                                                            HS.<$> RTS.pMatch
                                                                                                                                                                                                                     "255:31--255:34"
                                                                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                                                                        "Tz")
                                                                                                                                                                                                        HS.pure
                                                                                                                                                                                                          (ContentStreamOp_setHorizontalTextScaling
                                                                                                                                                                                                             _83)))
                                                                                                                                                                                                 ((RTS.|||)
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "appendCurvedInitPtRepl"
                                                                                                                                                                                                       (do (_84
                                                                                                                                                                                                              :: ()) <-
                                                                                                                                                                                                             HS.const
                                                                                                                                                                                                               ()
                                                                                                                                                                                                               HS.<$> RTS.pMatch
                                                                                                                                                                                                                        "256:29--256:31"
                                                                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                                                                           "v")
                                                                                                                                                                                                           HS.pure
                                                                                                                                                                                                             (ContentStreamOp_appendCurvedInitPtRepl
                                                                                                                                                                                                                _84)))
                                                                                                                                                                                                    ((RTS.|||)
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "setLineWidth"
                                                                                                                                                                                                          (do (_85
                                                                                                                                                                                                                 :: ()) <-
                                                                                                                                                                                                                HS.const
                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                                                                                                                           "257:19--257:21"
                                                                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                                                                              "w")
                                                                                                                                                                                                              HS.pure
                                                                                                                                                                                                                (ContentStreamOp_setLineWidth
                                                                                                                                                                                                                   _85)))
                                                                                                                                                                                                       ((RTS.|||)
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "setClippingNzWinding"
                                                                                                                                                                                                             (do (_86
                                                                                                                                                                                                                    :: ()) <-
                                                                                                                                                                                                                   HS.const
                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                                                                                                                              "258:27--258:29"
                                                                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                                                                 "W")
                                                                                                                                                                                                                 HS.pure
                                                                                                                                                                                                                   (ContentStreamOp_setClippingNzWinding
                                                                                                                                                                                                                      _86)))
                                                                                                                                                                                                          ((RTS.|||)
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "setClippingEvenOdd"
                                                                                                                                                                                                                (do (_87
                                                                                                                                                                                                                       :: ()) <-
                                                                                                                                                                                                                      HS.const
                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                                                                                                                 "259:25--259:28"
                                                                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                                                                    "W*")
                                                                                                                                                                                                                    HS.pure
                                                                                                                                                                                                                      (ContentStreamOp_setClippingEvenOdd
                                                                                                                                                                                                                         _87)))
                                                                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "appendCurvedFinalPt"
                                                                                                                                                                                                                   (do (_88
                                                                                                                                                                                                                          :: ()) <-
                                                                                                                                                                                                                         HS.const
                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                                                                                                                    "260:26--260:28"
                                                                                                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                                                                                                       "y")
                                                                                                                                                                                                                       HS.pure
                                                                                                                                                                                                                         (ContentStreamOp_appendCurvedFinalPt
                                                                                                                                                                                                                            _88)))
                                                                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "moveShow"
                                                                                                                                                                                                                      (do (_89
                                                                                                                                                                                                                             :: ()) <-
                                                                                                                                                                                                                            HS.const
                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                                                                                                       "261:15--261:17"
                                                                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                                                                          "'")
                                                                                                                                                                                                                          HS.pure
                                                                                                                                                                                                                            (ContentStreamOp_moveShow
                                                                                                                                                                                                                               _89)))
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "setSpacing"
                                                                                                                                                                                                                      (do (_90
                                                                                                                                                                                                                             :: ()) <-
                                                                                                                                                                                                                            HS.const
                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                                                                                                       "262:17--262:20"
                                                                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                                                                          "\"")
                                                                                                                                                                                                                          HS.pure
                                                                                                                                                                                                                            (ContentStreamOp_setSpacing
                                                                                                                                                                                                                               _90))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
pOperationObj :: D.Parser ContentStreamOp
 
pOperationObj =
  RTS.pEnter "PdfValue.Token"
    (pToken @ContentStreamOp
       (RTS.pEnter "PdfValue.ContentStreamOp" pContentStreamOp))
 
_BeginCompat :: D.Parser ()
 
_BeginCompat =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (HS.const ()
          HS.<$> RTS.pMatch "269:25--269:28" (Vector.vecFromRep "BX")))
 
_NameEsc :: D.Parser ()
 
_NameEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "155:3--155:5" (Vector.vecFromRep "#")
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
     RTS.pGuard "158:3--158:8" "guard failed"
       ((RTS.lit 0 :: RTS.UInt 8) HS.< __)
 
_NameChar :: D.Parser ()
 
_NameChar =
  (RTS.|||)
    (HS.const ()
       HS.<$> RTS.pMatch1 "151:16--151:45"
                (RTS.bcComplement (RTS.bcByteString "\NUL\t\n\f\r ()<>[]{}/%#")))
    (RTS.pEnter "PdfValue._NameEsc" _NameEsc)
 
_OpName :: D.Parser ()
 
_OpName =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (RTS.pSkipMany (RTS.<||)
          (RTS.pEnter "PdfValue._NameChar" _NameChar)))
 
_Between ::
  forall c.
    RTS.DDL c =>
      Vector.Vector (RTS.UInt 8)
        -> (Vector.Vector (RTS.UInt 8) -> (D.Parser () -> D.Parser ()))
 
_Between (open :: Vector.Vector (RTS.UInt 8))
  (close :: Vector.Vector (RTS.UInt 8))
  (_P :: D.Parser ()) =
  do RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const () HS.<$> RTS.pMatch "26:34--26:37" open))
     _P
     RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const () HS.<$> RTS.pMatch "26:51--26:55" close))
 
_Array :: D.Parser ()
 
_Array =
  RTS.pEnter "PdfValue._Between"
    (_Between @(Vector.Vector Value) (Vector.vecFromRep "[")
       (Vector.vecFromRep "]")
       (RTS.pSkipMany (RTS.<||)
          (do HS.void (RTS.pEnter "PdfValue.Value" pValue)
              HS.pure ())))
 
_When ::
  forall a b. (RTS.DDL a, RTS.DDL b) => D.Parser () -> D.Parser ()
 
_When (_P :: D.Parser ()) = _P
 
_Bool :: D.Parser ()
 
_Bool =
  (RTS.|||)
    (RTS.pEnter "PdfValue._When"
       (_When @() @HS.Bool
          (RTS.pEnter "PdfValue._KW"
             (_KW @(Vector.Vector (RTS.UInt 8))
                (HS.const ()
                   HS.<$> RTS.pMatch "37:14--37:19" (Vector.vecFromRep "true"))))))
    (RTS.pEnter "PdfValue._When"
       (_When @() @HS.Bool
          (RTS.pEnter "PdfValue._KW"
             (_KW @(Vector.Vector (RTS.UInt 8))
                (HS.const ()
                   HS.<$> RTS.pMatch "38:14--38:20" (Vector.vecFromRep "false"))))))
 
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
             RTS.pIsJust "174:31--174:52" "Key already present"
               (Map.insertMaybe (HS.getField @"key" e) (HS.getField @"value" e)
                  d))
          (Map.empty :: Map.Map (Vector.Vector (RTS.UInt 8)) Value)
          ents)
     HS.pure ()
 
_Digit :: D.Parser ()
 
_Digit =
  HS.const ()
    HS.<$> RTS.pMatch1 "79:24--79:33"
             (RTS.bcRange (RTS.uint8 48) (RTS.uint8 57))
 
_HexDigit :: D.Parser ()
 
_HexDigit =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue._Digit" _Digit)
       (HS.const ()
          HS.<$> RTS.pMatch1 "82:24--82:33"
                   (RTS.bcRange (RTS.uint8 97) (RTS.uint8 102))))
    (HS.const ()
       HS.<$> RTS.pMatch1 "83:24--83:33"
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
 
_Name :: D.Parser ()
 
_Name =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (do HS.const ()
             HS.<$> RTS.pMatch "149:24--149:26" (Vector.vecFromRep "/")
           RTS.pSkipMany (RTS.<||)
             (RTS.pEnter "PdfValue._NameChar" _NameChar)))
 
_Null :: D.Parser ()
 
_Null =
  RTS.pEnter "PdfValue._KW"
    (_KW @(Vector.Vector (RTS.UInt 8))
       (HS.const ()
          HS.<$> RTS.pMatch "178:16--178:21" (Vector.vecFromRep "null")))
 
_Number :: D.Parser ()
 
_Number =
  RTS.pEnter "PdfValue._Token"
    (_Token @Number
       (do (sign :: Sign) <- RTS.pEnter "PdfValue.Sign" pSign
           do HS.void (RTS.pEnter "PdfValue.UnsignedNumber" pUnsignedNumber)
              HS.pure ()
           (RTS.|||)
             (RTS.pEnter "PdfValue._When"
                (_When @() @Number
                   (RTS.pIsJust_ "47:11--47:21" "Expected `pos`"
                      (HS.getField @"pos" sign))))
             (RTS.pEnter "PdfValue._When"
                (_When @() @Number
                   (RTS.pIsJust_ "48:11--48:21" "Expected `neg`"
                      (HS.getField @"neg" sign))))))
 
_Natural :: D.Parser ()
 
_Natural =
  RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: HS.Integer)
    (RTS.pEnter "PdfValue._Digit" _Digit)
 
_Ref :: D.Parser ()
 
_Ref =
  do RTS.pEnter "PdfValue._Token"
       (_Token @HS.Integer (RTS.pEnter "PdfValue._Natural" _Natural))
     RTS.pEnter "PdfValue._Token"
       (_Token @HS.Integer (RTS.pEnter "PdfValue._Natural" _Natural))
     RTS.pEnter "PdfValue._KW"
       (_KW @(Vector.Vector (RTS.UInt 8))
          (HS.const ()
             HS.<$> RTS.pMatch "183:6--183:8" (Vector.vecFromRep "R")))
 
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
 
pContentStream :: D.Parser (Vector.Vector ContentStream_0)
 
pContentStream =
  do (__ :: Vector.Vector ContentStream_0) <-
       RTS.pMany (RTS.<||)
         (do (__ :: ContentStream_0) <-
               (RTS.|||)
                 (RTS.pEnter "operand"
                    (do (_91 :: Value) <- RTS.pEnter "PdfValue.Value" pValue
                        HS.pure (ContentStream_0_operand _91)))
                 ((RTS.|||)
                    (RTS.pEnter "operation"
                       (do (_92 :: ContentStreamOp) <-
                             RTS.pEnter "PdfValue.OperationObj" pOperationObj
                           HS.pure (ContentStream_0_operation _92)))
                    (RTS.pEnter "compatSect"
                       (do (_93 :: Vector.Vector (RTS.UInt 8)) <-
                             do RTS.pEnter "PdfValue._BeginCompat" _BeginCompat
                                RTS.pSkipMany (RTS.<||)
                                  ((RTS.|||) (RTS.pEnter "PdfValue._Value" _Value)
                                     (RTS.pEnter "PdfValue._OpName" _OpName))
                                (__ :: Vector.Vector (RTS.UInt 8)) <-
                                  RTS.pEnter "PdfValue.EndCompat" pEndCompat
                                HS.pure __
                           HS.pure (ContentStream_0_compatSect _93))))
             HS.pure __)
     HS.pure __
 
pDay :: D.Parser HS.Integer
 
pDay =
  RTS.pEnter "PdfValue.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 1 :: HS.Integer)
       (RTS.lit 31 :: HS.Integer))
 
pHour :: D.Parser HS.Integer
 
pHour =
  RTS.pEnter "PdfValue.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
pMinute :: D.Parser HS.Integer
 
pMinute =
  RTS.pEnter "PdfValue.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
pMonth :: D.Parser HS.Integer
 
pMonth =
  RTS.pEnter "PdfValue.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 1 :: HS.Integer)
       (RTS.lit 12 :: HS.Integer))
 
pSecond :: D.Parser HS.Integer
 
pSecond =
  RTS.pEnter "PdfValue.BoundedTwoDigits"
    (pBoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
pDate :: D.Parser Date
 
pDate =
  do HS.const ()
       HS.<$> RTS.pMatch "380:5--380:8" (Vector.vecFromRep "D:")
     (year :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 4 :: HS.Integer)
         (RTS.pEnter "PdfValue.Digit" pDigit)
     (month :: HS.Integer) <- RTS.pEnter "PdfValue.Month" pMonth
     (day :: HS.Integer) <- RTS.pEnter "PdfValue.Day" pDay
     (hour :: HS.Integer) <- RTS.pEnter "PdfValue.Hour" pHour
     (minute :: HS.Integer) <- RTS.pEnter "PdfValue.Minute" pMinute
     (second :: HS.Integer) <- RTS.pEnter "PdfValue.Second" pSecond
     (relLocalTime :: Date_0) <-
       (RTS.|||)
         (RTS.pEnter "plus"
            (do (_94 :: ()) <-
                  HS.const ()
                    HS.<$> RTS.pMatch1 "388:15--388:17" (RTS.bcSingle (RTS.uint8 43))
                HS.pure (Date_0_plus _94)))
         ((RTS.|||)
            (RTS.pEnter "minus"
               (do (_95 :: ()) <-
                     HS.const ()
                       HS.<$> RTS.pMatch1 "389:16--389:18" (RTS.bcSingle (RTS.uint8 45))
                   HS.pure (Date_0_minus _95)))
            (RTS.pEnter "bigZ"
               (do (_96 :: ()) <-
                     HS.const ()
                       HS.<$> RTS.pMatch1 "390:15--390:17" (RTS.bcSingle (RTS.uint8 90))
                   HS.pure (Date_0_bigZ _96))))
     (utOffsetHours :: HS.Integer) <- RTS.pEnter "PdfValue.Hour" pHour
     HS.const ()
       HS.<$> RTS.pMatch1 "393:5--393:8" (RTS.bcSingle (RTS.uint8 39))
     (utOffsetMins :: HS.Integer) <-
       RTS.pEnter "PdfValue.Minute" pMinute
     HS.pure
       (Date year month day hour minute second relLocalTime utOffsetHours
          utOffsetMins)
 
pHighSurrogate :: D.Parser (RTS.UInt 8)
 
pHighSurrogate =
  do HS.const ()
       HS.<$> RTS.pMatch1 "306:3--306:14"
                (RTS.bcRange (RTS.lit 216 :: RTS.UInt 8)
                   (RTS.lit 219 :: RTS.UInt 8))
     (__ :: RTS.UInt 8) <- RTS.pEnter "PdfValue.Byte" pByte
     HS.pure __
 
pLowSurrogate :: D.Parser (RTS.UInt 8)
 
pLowSurrogate =
  do HS.const ()
       HS.<$> RTS.pMatch1 "311:3--311:14"
                (RTS.bcRange (RTS.lit 220 :: RTS.UInt 8)
                   (RTS.lit 223 :: RTS.UInt 8))
     (__ :: RTS.UInt 8) <- RTS.pEnter "PdfValue.Byte" pByte
     HS.pure __
 
pNumberAsNat :: Number -> D.Parser HS.Integer
 
pNumberAsNat (x :: Number) =
  do RTS.pGuard "85:34--85:43" "guard failed"
       ((RTS.lit 0 :: HS.Integer) HS.<= HS.getField @"num" x)
     RTS.pGuard "85:46--85:55" "guard failed"
       (HS.getField @"exp" x HS.== (RTS.lit 0 :: HS.Integer))
     (__ :: HS.Integer) <- HS.pure (HS.getField @"num" x)
     HS.pure __
 
pNatValue :: Value -> D.Parser HS.Integer
 
pNatValue (v :: Value) =
  do (n :: Number) <-
       RTS.pIsJust "413:8--413:18" "Expected `number`"
         (HS.getField @"number" v)
     (__ :: HS.Integer) <-
       RTS.pEnter "PdfValue.NumberAsNat" (pNumberAsNat n)
     HS.pure __
 
pOnly :: forall a. RTS.DDL a => D.Parser a -> D.Parser a
 
pOnly (pP :: D.Parser a) =
  do (__ :: a) <- pP
     RTS.pEnd "28:39--28:41"
     HS.pure __
 
pOpName :: D.Parser (Vector.Vector (RTS.UInt 8))
 
pOpName =
  RTS.pEnter "PdfValue.Token"
    (pToken @(Vector.Vector (RTS.UInt 8))
       (do (__ :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMany (RTS.<||) (RTS.pEnter "PdfValue.NameChar" pNameChar)
           HS.pure __))
 
_Byte :: D.Parser ()
 
_Byte =
  HS.const ()
    HS.<$> RTS.pMatch1 "300:12--300:19"
             (RTS.bcRange (RTS.lit 0 :: RTS.UInt 8) (RTS.lit 255 :: RTS.UInt 8))
 
_UFEFF :: D.Parser ()
 
_UFEFF =
  do HS.const ()
       HS.<$> RTS.pMatch1 "342:15--342:17" (RTS.bcSingle (RTS.uint8 254))
     HS.const ()
       HS.<$> RTS.pMatch1 "342:21--342:23" (RTS.bcSingle (RTS.uint8 255))
 
_BmpByte :: D.Parser ()
 
_BmpByte =
  (RTS.|||)
    (HS.const ()
       HS.<$> RTS.pMatch1 "316:15--316:26"
                (RTS.bcRange (RTS.lit 0 :: RTS.UInt 8)
                   (RTS.lit 215 :: RTS.UInt 8)))
    (HS.const ()
       HS.<$> RTS.pMatch1 "316:30--316:41"
                (RTS.bcRange (RTS.lit 224 :: RTS.UInt 8)
                   (RTS.lit 255 :: RTS.UInt 8)))
 
_HighSurrogate :: D.Parser ()
 
_HighSurrogate =
  do HS.const ()
       HS.<$> RTS.pMatch1 "306:3--306:14"
                (RTS.bcRange (RTS.lit 216 :: RTS.UInt 8)
                   (RTS.lit 219 :: RTS.UInt 8))
     RTS.pEnter "PdfValue._Byte" _Byte
 
_LowSurrogate :: D.Parser ()
 
_LowSurrogate =
  do HS.const ()
       HS.<$> RTS.pMatch1 "311:3--311:14"
                (RTS.bcRange (RTS.lit 220 :: RTS.UInt 8)
                   (RTS.lit 223 :: RTS.UInt 8))
     RTS.pEnter "PdfValue._Byte" _Byte
 
_U001B :: D.Parser ()
 
_U001B =
  do HS.const ()
       HS.<$> RTS.pMatch1 "303:15--303:15" (RTS.bcSingle (RTS.uint8 0))
     HS.const ()
       HS.<$> RTS.pMatch1 "303:19--303:20" (RTS.bcSingle (RTS.uint8 27))
 
_UniChar :: D.Parser ()
 
_UniChar =
  (RTS.<||)
    (RTS.pEnter "escape"
       (do RTS.pEnter "PdfValue._U001B" _U001B
           RTS.pEnter "ISOCodes._LanguageCode" ISOCodes._LanguageCode
           (RTS.<||)
             (RTS.pEnter "ISOCodes._CountryCode" ISOCodes._CountryCode)
             (HS.pure ())
           RTS.pEnter "PdfValue._U001B" _U001B))
    ((RTS.<||)
       (RTS.pEnter "supplementary"
          (do RTS.pEnter "PdfValue._HighSurrogate" _HighSurrogate
              RTS.pEnter "PdfValue._LowSurrogate" _LowSurrogate))
       (RTS.pEnter "twoByte"
          (do RTS.pEnter "PdfValue._BmpByte" _BmpByte
              RTS.pEnter "PdfValue._BmpByte" _BmpByte)))
 
pTextObj :: D.Parser ()
 
pTextObj =
  do (RTS.<||)
       (RTS.pEnter "isUnicode"
          (do RTS.pEnter "PdfValue._UFEFF" _UFEFF
              RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._UniChar" _UniChar)))
       (RTS.pEnter "isPdfDoc"
          (RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._Byte" _Byte)))
     (__ :: ()) <- RTS.pEnd "357:3--357:5"
     HS.pure __
 
pU001B :: D.Parser (RTS.UInt 8)
 
pU001B =
  do HS.const ()
       HS.<$> RTS.pMatch1 "303:15--303:15" (RTS.bcSingle (RTS.uint8 0))
     (__ :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "303:19--303:20" (RTS.bcSingle (RTS.uint8 27))
     HS.pure __
 
pUFEFF :: D.Parser (RTS.UInt 8)
 
pUFEFF =
  do HS.const ()
       HS.<$> RTS.pMatch1 "342:15--342:17" (RTS.bcSingle (RTS.uint8 254))
     (__ :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "342:21--342:23" (RTS.bcSingle (RTS.uint8 255))
     HS.pure __
 
pUniChar :: D.Parser UniChar
 
pUniChar =
  (RTS.<||)
    (RTS.pEnter "escape"
       (do (_97 :: UniChar_0) <-
             do RTS.pEnter "PdfValue._U001B" _U001B
                (lang :: ISOCodes.LanguageCode) <-
                  RTS.pEnter "ISOCodes.LanguageCode" ISOCodes.pLanguageCode
                (ctry :: HS.Maybe ISOCodes.CountryCode) <-
                  RTS.pOptional (RTS.<||) HS.Just
                    (RTS.pEnter "ISOCodes.CountryCode" ISOCodes.pCountryCode)
                RTS.pEnter "PdfValue._U001B" _U001B
                HS.pure (UniChar_0 lang ctry)
           HS.pure (UniChar_escape _97)))
    ((RTS.<||)
       (RTS.pEnter "supplementary"
          (do (_98 :: UniChar_1) <-
                do (big :: RTS.UInt 8) <-
                     RTS.pEnter "PdfValue.HighSurrogate" pHighSurrogate
                   (small :: RTS.UInt 8) <-
                     RTS.pEnter "PdfValue.LowSurrogate" pLowSurrogate
                   HS.pure (UniChar_1 big small)
              HS.pure (UniChar_supplementary _98)))
       (RTS.pEnter "twoByte"
          (do (_99 :: UniChar_2) <-
                do (big :: RTS.UInt 8) <- RTS.pEnter "PdfValue.BmpByte" pBmpByte
                   (small :: RTS.UInt 8) <- RTS.pEnter "PdfValue.BmpByte" pBmpByte
                   HS.pure (UniChar_2 big small)
              HS.pure (UniChar_twoByte _99))))
 
_BoundedTwoDigits :: HS.Integer -> (HS.Integer -> D.Parser ())
 
_BoundedTwoDigits (lb :: HS.Integer) (ub :: HS.Integer) =
  do (digs :: Vector.Vector HS.Integer) <-
       Vector.replicateM (RTS.lit 2 :: HS.Integer)
         (RTS.pEnter "PdfValue.Digit" pDigit)
     (__ :: HS.Integer) <-
       HS.pure
         (numBase @(Vector.Vector HS.Integer) @HS.Integer @HS.Integer
            (RTS.lit 10 :: HS.Integer)
            digs)
     RTS.pGuard "366:3--366:10" "guard failed" (lb HS.<= __)
     RTS.pGuard "366:14--366:21" "guard failed" (__ HS.<= ub)
 
_EndCompat :: D.Parser ()
 
_EndCompat =
  RTS.pEnter "PdfValue._Token"
    (_Token @(Vector.Vector (RTS.UInt 8))
       (HS.const ()
          HS.<$> RTS.pMatch "272:23--272:26" (Vector.vecFromRep "EX")))
 
_ContentStreamOp :: D.Parser ()
 
_ContentStreamOp =
  (RTS.|||)
    (RTS.pEnter "closeFillStrokeNzWinding"
       (HS.const ()
          HS.<$> RTS.pMatch "192:31--192:33" (Vector.vecFromRep "b")))
    ((RTS.|||)
       (RTS.pEnter "fillStroke"
          (HS.const ()
             HS.<$> RTS.pMatch "193:17--193:19" (Vector.vecFromRep "B")))
       ((RTS.|||)
          (RTS.pEnter "closeFillStrokeEvenOdd"
             (HS.const ()
                HS.<$> RTS.pMatch "194:29--194:32" (Vector.vecFromRep "b*")))
          ((RTS.|||)
             (RTS.pEnter "fillStrokeEvenOdd"
                (HS.const ()
                   HS.<$> RTS.pMatch "195:24--195:27" (Vector.vecFromRep "B*")))
             ((RTS.|||)
                (RTS.pEnter "beginMarkedContent"
                   (HS.const ()
                      HS.<$> RTS.pMatch "196:25--196:29" (Vector.vecFromRep "BDC")))
                ((RTS.|||)
                   (RTS.pEnter "beginInline"
                      (HS.const ()
                         HS.<$> RTS.pMatch "197:18--197:21" (Vector.vecFromRep "Bl")))
                   ((RTS.|||)
                      (RTS.pEnter "beginMarkedContent"
                         (HS.const ()
                            HS.<$> RTS.pMatch "198:25--198:29" (Vector.vecFromRep "BMC")))
                      ((RTS.|||)
                         (RTS.pEnter "beginText"
                            (HS.const ()
                               HS.<$> RTS.pMatch "199:16--199:19" (Vector.vecFromRep "BT")))
                         ((RTS.|||)
                            (RTS.pEnter "appendCurvedThreePoints"
                               (HS.const ()
                                  HS.<$> RTS.pMatch "200:30--200:32" (Vector.vecFromRep "c")))
                            ((RTS.|||)
                               (RTS.pEnter "concatMatrix"
                                  (HS.const ()
                                     HS.<$> RTS.pMatch "201:19--201:22" (Vector.vecFromRep "cm")))
                               ((RTS.|||)
                                  (RTS.pEnter "setColorSpaceStroking"
                                     (HS.const ()
                                        HS.<$> RTS.pMatch "202:28--202:31"
                                                 (Vector.vecFromRep "CS")))
                                  ((RTS.|||)
                                     (RTS.pEnter "setColorSpaceNonStroking"
                                        (HS.const ()
                                           HS.<$> RTS.pMatch "203:31--203:34"
                                                    (Vector.vecFromRep "cs")))
                                     ((RTS.|||)
                                        (RTS.pEnter "setLineDash"
                                           (HS.const ()
                                              HS.<$> RTS.pMatch "204:18--204:20"
                                                       (Vector.vecFromRep "d")))
                                        ((RTS.|||)
                                           (RTS.pEnter "setGlyphWidth"
                                              (HS.const ()
                                                 HS.<$> RTS.pMatch "205:20--205:23"
                                                          (Vector.vecFromRep "d0")))
                                           ((RTS.|||)
                                              (RTS.pEnter "setGlpyhWidthBoundingBox"
                                                 (HS.const ()
                                                    HS.<$> RTS.pMatch "206:31--206:34"
                                                             (Vector.vecFromRep "d1")))
                                              ((RTS.|||)
                                                 (RTS.pEnter "invokeXObj"
                                                    (HS.const ()
                                                       HS.<$> RTS.pMatch "207:17--207:20"
                                                                (Vector.vecFromRep "Do")))
                                                 ((RTS.|||)
                                                    (RTS.pEnter "defMarkedContentPoint"
                                                       (HS.const ()
                                                          HS.<$> RTS.pMatch "208:28--208:31"
                                                                   (Vector.vecFromRep "DP")))
                                                    ((RTS.|||)
                                                       (RTS.pEnter "endInline"
                                                          (HS.const ()
                                                             HS.<$> RTS.pMatch "209:16--209:19"
                                                                      (Vector.vecFromRep "El")))
                                                       ((RTS.|||)
                                                          (RTS.pEnter "endMarkedContent"
                                                             (HS.const ()
                                                                HS.<$> RTS.pMatch "210:23--210:27"
                                                                         (Vector.vecFromRep "EMC")))
                                                          ((RTS.|||)
                                                             (RTS.pEnter "endTextObj"
                                                                (HS.const ()
                                                                   HS.<$> RTS.pMatch
                                                                            "211:17--211:20"
                                                                            (Vector.vecFromRep
                                                                               "ET")))
                                                             ((RTS.|||)
                                                                (RTS.pEnter "fillPathNzWinding"
                                                                   (HS.const ()
                                                                      HS.<$> RTS.pMatch
                                                                               "212:24--212:26"
                                                                               (Vector.vecFromRep
                                                                                  "f")))
                                                                ((RTS.|||)
                                                                   (RTS.pEnter
                                                                      "fillPathNzWindingOld"
                                                                      (HS.const ()
                                                                         HS.<$> RTS.pMatch
                                                                                  "213:27--213:29"
                                                                                  (Vector.vecFromRep
                                                                                     "F")))
                                                                   ((RTS.|||)
                                                                      (RTS.pEnter "fillPathEvenOdd"
                                                                         (HS.const ()
                                                                            HS.<$> RTS.pMatch
                                                                                     "214:22--214:25"
                                                                                     (Vector.vecFromRep
                                                                                        "f*")))
                                                                      ((RTS.|||)
                                                                         (RTS.pEnter
                                                                            "setGrayStroking"
                                                                            (HS.const ()
                                                                               HS.<$> RTS.pMatch
                                                                                        "215:22--215:24"
                                                                                        (Vector.vecFromRep
                                                                                           "G")))
                                                                         ((RTS.|||)
                                                                            (RTS.pEnter
                                                                               "setGrayNonStroking"
                                                                               (HS.const ()
                                                                                  HS.<$> RTS.pMatch
                                                                                           "216:25--216:27"
                                                                                           (Vector.vecFromRep
                                                                                              "g")))
                                                                            ((RTS.|||)
                                                                               (RTS.pEnter
                                                                                  "setGraphicsStateParams"
                                                                                  (HS.const ()
                                                                                     HS.<$> RTS.pMatch
                                                                                              "217:29--217:32"
                                                                                              (Vector.vecFromRep
                                                                                                 "gs")))
                                                                               ((RTS.|||)
                                                                                  (RTS.pEnter
                                                                                     "closeSubpath"
                                                                                     (HS.const ()
                                                                                        HS.<$> RTS.pMatch
                                                                                                 "218:19--218:21"
                                                                                                 (Vector.vecFromRep
                                                                                                    "h")))
                                                                                  ((RTS.|||)
                                                                                     (RTS.pEnter
                                                                                        "setFlat"
                                                                                        (HS.const ()
                                                                                           HS.<$> RTS.pMatch
                                                                                                    "219:14--219:16"
                                                                                                    (Vector.vecFromRep
                                                                                                       "i")))
                                                                                     ((RTS.|||)
                                                                                        (RTS.pEnter
                                                                                           "beginInlineImageData"
                                                                                           (HS.const
                                                                                              ()
                                                                                              HS.<$> RTS.pMatch
                                                                                                       "220:27--220:30"
                                                                                                       (Vector.vecFromRep
                                                                                                          "ID")))
                                                                                        ((RTS.|||)
                                                                                           (RTS.pEnter
                                                                                              "setLineJoinStyle"
                                                                                              (HS.const
                                                                                                 ()
                                                                                                 HS.<$> RTS.pMatch
                                                                                                          "221:23--221:25"
                                                                                                          (Vector.vecFromRep
                                                                                                             "j")))
                                                                                           ((RTS.|||)
                                                                                              (RTS.pEnter
                                                                                                 "setLineCapStyle"
                                                                                                 (HS.const
                                                                                                    ()
                                                                                                    HS.<$> RTS.pMatch
                                                                                                             "222:22--222:24"
                                                                                                             (Vector.vecFromRep
                                                                                                                "J")))
                                                                                              ((RTS.|||)
                                                                                                 (RTS.pEnter
                                                                                                    "setCMYKStroking"
                                                                                                    (HS.const
                                                                                                       ()
                                                                                                       HS.<$> RTS.pMatch
                                                                                                                "223:22--223:24"
                                                                                                                (Vector.vecFromRep
                                                                                                                   "K")))
                                                                                                 ((RTS.|||)
                                                                                                    (RTS.pEnter
                                                                                                       "setCMYKNonStroking"
                                                                                                       (HS.const
                                                                                                          ()
                                                                                                          HS.<$> RTS.pMatch
                                                                                                                   "224:25--224:27"
                                                                                                                   (Vector.vecFromRep
                                                                                                                      "k")))
                                                                                                    ((RTS.|||)
                                                                                                       (RTS.pEnter
                                                                                                          "appendLine"
                                                                                                          (HS.const
                                                                                                             ()
                                                                                                             HS.<$> RTS.pMatch
                                                                                                                      "225:17--225:19"
                                                                                                                      (Vector.vecFromRep
                                                                                                                         "l")))
                                                                                                       ((RTS.|||)
                                                                                                          (RTS.pEnter
                                                                                                             "beginNewSuppath"
                                                                                                             (HS.const
                                                                                                                ()
                                                                                                                HS.<$> RTS.pMatch
                                                                                                                         "226:22--226:24"
                                                                                                                         (Vector.vecFromRep
                                                                                                                            "m")))
                                                                                                          ((RTS.|||)
                                                                                                             (RTS.pEnter
                                                                                                                "setMiterLimit"
                                                                                                                (HS.const
                                                                                                                   ()
                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                            "227:20--227:22"
                                                                                                                            (Vector.vecFromRep
                                                                                                                               "M")))
                                                                                                             ((RTS.|||)
                                                                                                                (RTS.pEnter
                                                                                                                   "defineMarkedContent"
                                                                                                                   (HS.const
                                                                                                                      ()
                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                               "228:26--228:29"
                                                                                                                               (Vector.vecFromRep
                                                                                                                                  "MP")))
                                                                                                                ((RTS.|||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "endPath"
                                                                                                                      (HS.const
                                                                                                                         ()
                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                  "229:14--229:16"
                                                                                                                                  (Vector.vecFromRep
                                                                                                                                     "n")))
                                                                                                                   ((RTS.|||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "saveGraphicsState"
                                                                                                                         (HS.const
                                                                                                                            ()
                                                                                                                            HS.<$> RTS.pMatch
                                                                                                                                     "230:24--230:26"
                                                                                                                                     (Vector.vecFromRep
                                                                                                                                        "q")))
                                                                                                                      ((RTS.|||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "restoreGraphicsState"
                                                                                                                            (HS.const
                                                                                                                               ()
                                                                                                                               HS.<$> RTS.pMatch
                                                                                                                                        "231:27--231:29"
                                                                                                                                        (Vector.vecFromRep
                                                                                                                                           "Q")))
                                                                                                                         ((RTS.|||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "appendRect"
                                                                                                                               (HS.const
                                                                                                                                  ()
                                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                                           "232:17--232:20"
                                                                                                                                           (Vector.vecFromRep
                                                                                                                                              "re")))
                                                                                                                            ((RTS.|||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "setRGBStroking"
                                                                                                                                  (HS.const
                                                                                                                                     ()
                                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                                              "233:21--233:24"
                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                 "RG")))
                                                                                                                               ((RTS.|||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "setRGBNonStroking"
                                                                                                                                     (HS.const
                                                                                                                                        ()
                                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                                 "234:24--234:27"
                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                    "rg")))
                                                                                                                                  ((RTS.|||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "setColorRenderingIntent"
                                                                                                                                        (HS.const
                                                                                                                                           ()
                                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                                    "235:30--235:33"
                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                       "ri")))
                                                                                                                                     ((RTS.|||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "closeStrokePath"
                                                                                                                                           (HS.const
                                                                                                                                              ()
                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                       "236:22--236:24"
                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                          "s")))
                                                                                                                                        ((RTS.|||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "stroke"
                                                                                                                                              (HS.const
                                                                                                                                                 ()
                                                                                                                                                 HS.<$> RTS.pMatch
                                                                                                                                                          "237:13--237:15"
                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                             "S")))
                                                                                                                                           ((RTS.|||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "setColorStroking"
                                                                                                                                                 (HS.const
                                                                                                                                                    ()
                                                                                                                                                    HS.<$> RTS.pMatch
                                                                                                                                                             "238:23--238:26"
                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                "SC")))
                                                                                                                                              ((RTS.|||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "setColorNonStroking"
                                                                                                                                                    (HS.const
                                                                                                                                                       ()
                                                                                                                                                       HS.<$> RTS.pMatch
                                                                                                                                                                "239:26--239:29"
                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                   "sc")))
                                                                                                                                                 ((RTS.|||)
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "setColorStrokingICC"
                                                                                                                                                       (HS.const
                                                                                                                                                          ()
                                                                                                                                                          HS.<$> RTS.pMatch
                                                                                                                                                                   "240:26--240:30"
                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                      "SCN")))
                                                                                                                                                    ((RTS.|||)
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "setColorNonStrokingICC"
                                                                                                                                                          (HS.const
                                                                                                                                                             ()
                                                                                                                                                             HS.<$> RTS.pMatch
                                                                                                                                                                      "241:29--241:33"
                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                         "scn")))
                                                                                                                                                       ((RTS.|||)
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "paintShadingPattern"
                                                                                                                                                             (HS.const
                                                                                                                                                                ()
                                                                                                                                                                HS.<$> RTS.pMatch
                                                                                                                                                                         "242:26--242:29"
                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                            "sh")))
                                                                                                                                                          ((RTS.|||)
                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                "moveStartText"
                                                                                                                                                                (HS.const
                                                                                                                                                                   ()
                                                                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                                                                            "243:20--243:23"
                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                               "T*")))
                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                   "setCharSpacing"
                                                                                                                                                                   (HS.const
                                                                                                                                                                      ()
                                                                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                                                                               "244:21--244:24"
                                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                                  "Tc")))
                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                      "moveTextPos"
                                                                                                                                                                      (HS.const
                                                                                                                                                                         ()
                                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                                  "245:18--245:21"
                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                     "Td")))
                                                                                                                                                                   ((RTS.|||)
                                                                                                                                                                      (RTS.pEnter
                                                                                                                                                                         "moveTextPosSetLeading"
                                                                                                                                                                         (HS.const
                                                                                                                                                                            ()
                                                                                                                                                                            HS.<$> RTS.pMatch
                                                                                                                                                                                     "246:28--246:31"
                                                                                                                                                                                     (Vector.vecFromRep
                                                                                                                                                                                        "TD")))
                                                                                                                                                                      ((RTS.|||)
                                                                                                                                                                         (RTS.pEnter
                                                                                                                                                                            "setTextFont"
                                                                                                                                                                            (HS.const
                                                                                                                                                                               ()
                                                                                                                                                                               HS.<$> RTS.pMatch
                                                                                                                                                                                        "247:18--247:21"
                                                                                                                                                                                        (Vector.vecFromRep
                                                                                                                                                                                           "Tf")))
                                                                                                                                                                         ((RTS.|||)
                                                                                                                                                                            (RTS.pEnter
                                                                                                                                                                               "showText"
                                                                                                                                                                               (HS.const
                                                                                                                                                                                  ()
                                                                                                                                                                                  HS.<$> RTS.pMatch
                                                                                                                                                                                           "248:15--248:18"
                                                                                                                                                                                           (Vector.vecFromRep
                                                                                                                                                                                              "Tj")))
                                                                                                                                                                            ((RTS.|||)
                                                                                                                                                                               (RTS.pEnter
                                                                                                                                                                                  "showTextIndGlyph"
                                                                                                                                                                                  (HS.const
                                                                                                                                                                                     ()
                                                                                                                                                                                     HS.<$> RTS.pMatch
                                                                                                                                                                                              "249:23--249:26"
                                                                                                                                                                                              (Vector.vecFromRep
                                                                                                                                                                                                 "TJ")))
                                                                                                                                                                               ((RTS.|||)
                                                                                                                                                                                  (RTS.pEnter
                                                                                                                                                                                     "setTextLeading"
                                                                                                                                                                                     (HS.const
                                                                                                                                                                                        ()
                                                                                                                                                                                        HS.<$> RTS.pMatch
                                                                                                                                                                                                 "250:21--250:24"
                                                                                                                                                                                                 (Vector.vecFromRep
                                                                                                                                                                                                    "TL")))
                                                                                                                                                                                  ((RTS.|||)
                                                                                                                                                                                     (RTS.pEnter
                                                                                                                                                                                        "setTextMatrix"
                                                                                                                                                                                        (HS.const
                                                                                                                                                                                           ()
                                                                                                                                                                                           HS.<$> RTS.pMatch
                                                                                                                                                                                                    "251:20--251:23"
                                                                                                                                                                                                    (Vector.vecFromRep
                                                                                                                                                                                                       "Tm")))
                                                                                                                                                                                     ((RTS.|||)
                                                                                                                                                                                        (RTS.pEnter
                                                                                                                                                                                           "setTextRendering"
                                                                                                                                                                                           (HS.const
                                                                                                                                                                                              ()
                                                                                                                                                                                              HS.<$> RTS.pMatch
                                                                                                                                                                                                       "252:23--252:26"
                                                                                                                                                                                                       (Vector.vecFromRep
                                                                                                                                                                                                          "Tr")))
                                                                                                                                                                                        ((RTS.|||)
                                                                                                                                                                                           (RTS.pEnter
                                                                                                                                                                                              "setTextRise"
                                                                                                                                                                                              (HS.const
                                                                                                                                                                                                 ()
                                                                                                                                                                                                 HS.<$> RTS.pMatch
                                                                                                                                                                                                          "253:18--253:21"
                                                                                                                                                                                                          (Vector.vecFromRep
                                                                                                                                                                                                             "Ts")))
                                                                                                                                                                                           ((RTS.|||)
                                                                                                                                                                                              (RTS.pEnter
                                                                                                                                                                                                 "setWordSpacing"
                                                                                                                                                                                                 (HS.const
                                                                                                                                                                                                    ()
                                                                                                                                                                                                    HS.<$> RTS.pMatch
                                                                                                                                                                                                             "254:21--254:24"
                                                                                                                                                                                                             (Vector.vecFromRep
                                                                                                                                                                                                                "Tw")))
                                                                                                                                                                                              ((RTS.|||)
                                                                                                                                                                                                 (RTS.pEnter
                                                                                                                                                                                                    "setHorizontalTextScaling"
                                                                                                                                                                                                    (HS.const
                                                                                                                                                                                                       ()
                                                                                                                                                                                                       HS.<$> RTS.pMatch
                                                                                                                                                                                                                "255:31--255:34"
                                                                                                                                                                                                                (Vector.vecFromRep
                                                                                                                                                                                                                   "Tz")))
                                                                                                                                                                                                 ((RTS.|||)
                                                                                                                                                                                                    (RTS.pEnter
                                                                                                                                                                                                       "appendCurvedInitPtRepl"
                                                                                                                                                                                                       (HS.const
                                                                                                                                                                                                          ()
                                                                                                                                                                                                          HS.<$> RTS.pMatch
                                                                                                                                                                                                                   "256:29--256:31"
                                                                                                                                                                                                                   (Vector.vecFromRep
                                                                                                                                                                                                                      "v")))
                                                                                                                                                                                                    ((RTS.|||)
                                                                                                                                                                                                       (RTS.pEnter
                                                                                                                                                                                                          "setLineWidth"
                                                                                                                                                                                                          (HS.const
                                                                                                                                                                                                             ()
                                                                                                                                                                                                             HS.<$> RTS.pMatch
                                                                                                                                                                                                                      "257:19--257:21"
                                                                                                                                                                                                                      (Vector.vecFromRep
                                                                                                                                                                                                                         "w")))
                                                                                                                                                                                                       ((RTS.|||)
                                                                                                                                                                                                          (RTS.pEnter
                                                                                                                                                                                                             "setClippingNzWinding"
                                                                                                                                                                                                             (HS.const
                                                                                                                                                                                                                ()
                                                                                                                                                                                                                HS.<$> RTS.pMatch
                                                                                                                                                                                                                         "258:27--258:29"
                                                                                                                                                                                                                         (Vector.vecFromRep
                                                                                                                                                                                                                            "W")))
                                                                                                                                                                                                          ((RTS.|||)
                                                                                                                                                                                                             (RTS.pEnter
                                                                                                                                                                                                                "setClippingEvenOdd"
                                                                                                                                                                                                                (HS.const
                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                   HS.<$> RTS.pMatch
                                                                                                                                                                                                                            "259:25--259:28"
                                                                                                                                                                                                                            (Vector.vecFromRep
                                                                                                                                                                                                                               "W*")))
                                                                                                                                                                                                             ((RTS.|||)
                                                                                                                                                                                                                (RTS.pEnter
                                                                                                                                                                                                                   "appendCurvedFinalPt"
                                                                                                                                                                                                                   (HS.const
                                                                                                                                                                                                                      ()
                                                                                                                                                                                                                      HS.<$> RTS.pMatch
                                                                                                                                                                                                                               "260:26--260:28"
                                                                                                                                                                                                                               (Vector.vecFromRep
                                                                                                                                                                                                                                  "y")))
                                                                                                                                                                                                                ((RTS.|||)
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "moveShow"
                                                                                                                                                                                                                      (HS.const
                                                                                                                                                                                                                         ()
                                                                                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                                                                                  "261:15--261:17"
                                                                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                                                                     "'")))
                                                                                                                                                                                                                   (RTS.pEnter
                                                                                                                                                                                                                      "setSpacing"
                                                                                                                                                                                                                      (HS.const
                                                                                                                                                                                                                         ()
                                                                                                                                                                                                                         HS.<$> RTS.pMatch
                                                                                                                                                                                                                                  "262:17--262:20"
                                                                                                                                                                                                                                  (Vector.vecFromRep
                                                                                                                                                                                                                                     "\""))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
_OperationObj :: D.Parser ()
 
_OperationObj =
  RTS.pEnter "PdfValue._Token"
    (_Token @ContentStreamOp
       (RTS.pEnter "PdfValue._ContentStreamOp" _ContentStreamOp))
 
_ContentStream :: D.Parser ()
 
_ContentStream =
  RTS.pSkipMany (RTS.<||)
    ((RTS.|||)
       (RTS.pEnter "operand" (RTS.pEnter "PdfValue._Value" _Value))
       ((RTS.|||)
          (RTS.pEnter "operation"
             (RTS.pEnter "PdfValue._OperationObj" _OperationObj))
          (RTS.pEnter "compatSect"
             (do RTS.pEnter "PdfValue._BeginCompat" _BeginCompat
                 RTS.pSkipMany (RTS.<||)
                   ((RTS.|||) (RTS.pEnter "PdfValue._Value" _Value)
                      (RTS.pEnter "PdfValue._OpName" _OpName))
                 RTS.pEnter "PdfValue._EndCompat" _EndCompat))))
 
_Day :: D.Parser ()
 
_Day =
  RTS.pEnter "PdfValue._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 1 :: HS.Integer)
       (RTS.lit 31 :: HS.Integer))
 
_Hour :: D.Parser ()
 
_Hour =
  RTS.pEnter "PdfValue._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
_Minute :: D.Parser ()
 
_Minute =
  RTS.pEnter "PdfValue._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
_Month :: D.Parser ()
 
_Month =
  RTS.pEnter "PdfValue._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 1 :: HS.Integer)
       (RTS.lit 12 :: HS.Integer))
 
_Second :: D.Parser ()
 
_Second =
  RTS.pEnter "PdfValue._BoundedTwoDigits"
    (_BoundedTwoDigits (RTS.lit 0 :: HS.Integer)
       (RTS.lit 60 :: HS.Integer))
 
_Date :: D.Parser ()
 
_Date =
  do HS.const ()
       HS.<$> RTS.pMatch "380:5--380:8" (Vector.vecFromRep "D:")
     RTS.pSkipExact (RTS.lit 4 :: HS.Integer)
       (RTS.pEnter "PdfValue._Digit" _Digit)
     RTS.pEnter "PdfValue._Month" _Month
     RTS.pEnter "PdfValue._Day" _Day
     RTS.pEnter "PdfValue._Hour" _Hour
     RTS.pEnter "PdfValue._Minute" _Minute
     RTS.pEnter "PdfValue._Second" _Second
     (RTS.|||)
       (RTS.pEnter "plus"
          (HS.const ()
             HS.<$> RTS.pMatch1 "388:15--388:17" (RTS.bcSingle (RTS.uint8 43))))
       ((RTS.|||)
          (RTS.pEnter "minus"
             (HS.const ()
                HS.<$> RTS.pMatch1 "389:16--389:18" (RTS.bcSingle (RTS.uint8 45))))
          (RTS.pEnter "bigZ"
             (HS.const ()
                HS.<$> RTS.pMatch1 "390:15--390:17"
                         (RTS.bcSingle (RTS.uint8 90)))))
     RTS.pEnter "PdfValue._Hour" _Hour
     HS.const ()
       HS.<$> RTS.pMatch1 "393:5--393:8" (RTS.bcSingle (RTS.uint8 39))
     RTS.pEnter "PdfValue._Minute" _Minute
 
_Frac :: HS.Integer -> D.Parser ()
 
_Frac (n :: HS.Integer) =
  do HS.const ()
       HS.<$> RTS.pMatch "74:11--74:13" (Vector.vecFromRep ".")
     RTS.pSkipAtLeast (RTS.<||) n (RTS.pEnter "PdfValue._Digit" _Digit)
 
_NumberAsNat :: Number -> D.Parser ()
 
_NumberAsNat (x :: Number) =
  do RTS.pGuard "85:34--85:43" "guard failed"
       ((RTS.lit 0 :: HS.Integer) HS.<= HS.getField @"num" x)
     RTS.pGuard "85:46--85:55" "guard failed"
       (HS.getField @"exp" x HS.== (RTS.lit 0 :: HS.Integer))
 
_NatValue :: Value -> D.Parser ()
 
_NatValue (v :: Value) =
  do (n :: Number) <-
       RTS.pIsJust "413:8--413:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pEnter "PdfValue._NumberAsNat" (_NumberAsNat n)
 
_OctDigit :: D.Parser ()
 
_OctDigit =
  HS.const ()
    HS.<$> RTS.pMatch1 "80:24--80:33"
             (RTS.bcRange (RTS.uint8 48) (RTS.uint8 55))
 
_Only :: forall a. RTS.DDL a => D.Parser () -> D.Parser ()
 
_Only (_P :: D.Parser ()) =
  do _P
     RTS.pEnd "28:39--28:41"
 
_Sign :: D.Parser ()
 
_Sign =
  (RTS.|||)
    (RTS.pEnter "pos"
       ((RTS.|||)
          (HS.const ()
             HS.<$> RTS.pMatch "52:11--52:13" (Vector.vecFromRep "+"))
          (HS.const ()
             HS.<$> RTS.pMatch "52:17--52:18" (Vector.vecFromRep ""))))
    (RTS.pEnter "neg"
       (HS.const ()
          HS.<$> RTS.pMatch "53:10--53:12" (Vector.vecFromRep "-")))
 
_StringNumEsc :: D.Parser ()
 
_StringNumEsc =
  RTS.pSkipWithBounds "120:9--120:28" (RTS.<||)
    (RTS.lit 1 :: HS.Integer)
    (RTS.lit 3 :: HS.Integer)
    (RTS.pEnter "PdfValue._OctDigit" _OctDigit)
 
_StringEsc :: D.Parser ()
 
_StringEsc =
  do HS.const ()
       HS.<$> RTS.pMatch "104:4--104:7" (Vector.vecFromRep "\\")
     (RTS.|||)
       (RTS.pEnter "PdfValue._When"
          (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
             (HS.const ()
                HS.<$> RTS.pMatch "106:11--106:13" (Vector.vecFromRep "n"))))
       ((RTS.|||)
          (RTS.pEnter "PdfValue._When"
             (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                (HS.const ()
                   HS.<$> RTS.pMatch "107:11--107:13" (Vector.vecFromRep "r"))))
          ((RTS.|||)
             (RTS.pEnter "PdfValue._When"
                (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                   (HS.const ()
                      HS.<$> RTS.pMatch "108:11--108:13" (Vector.vecFromRep "t"))))
             ((RTS.|||)
                (RTS.pEnter "PdfValue._When"
                   (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                      (HS.const ()
                         HS.<$> RTS.pMatch "109:11--109:13" (Vector.vecFromRep "b"))))
                ((RTS.|||)
                   (RTS.pEnter "PdfValue._When"
                      (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                         (HS.const ()
                            HS.<$> RTS.pMatch "110:11--110:13" (Vector.vecFromRep "f"))))
                   ((RTS.|||)
                      (RTS.pEnter "PdfValue._When"
                         (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                            (HS.const ()
                               HS.<$> RTS.pMatch "111:11--111:13" (Vector.vecFromRep "("))))
                      ((RTS.|||)
                         (RTS.pEnter "PdfValue._When"
                            (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                               (HS.const ()
                                  HS.<$> RTS.pMatch "112:11--112:13" (Vector.vecFromRep ")"))))
                         ((RTS.|||)
                            (RTS.pEnter "PdfValue._When"
                               (_When @(Vector.Vector (RTS.UInt 8)) @(Vector.Vector (RTS.UInt 8))
                                  (HS.const ()
                                     HS.<$> RTS.pMatch "113:11--113:14" (Vector.vecFromRep "\\"))))
                            ((RTS.|||)
                               (RTS.pEnter "PdfValue._When"
                                  (_When @(RTS.UInt 8) @(Vector.Vector (RTS.UInt 8))
                                     (RTS.pEnter "PdfValue._EOL" _EOL)))
                               (RTS.pEnter "PdfValue._StringNumEsc" _StringNumEsc)))))))))
 
_StringInParens :: D.Parser ()
 
_StringInParens =
  do HS.const ()
       HS.<$> RTS.pMatch "101:31--101:33" (Vector.vecFromRep "(")
     RTS.pEnter "PdfValue._StringChars" _StringChars
     HS.const ()
       HS.<$> RTS.pMatch "101:49--101:51" (Vector.vecFromRep ")")
 
_StringChunk :: D.Parser ()
 
_StringChunk =
  (RTS.|||)
    ((RTS.|||) (RTS.pEnter "PdfValue._StringInParens" _StringInParens)
       (RTS.pEnter "PdfValue._StringEsc" _StringEsc))
    (RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: HS.Integer)
       (HS.const ()
          HS.<$> RTS.pMatch1 "99:17--99:24"
                   (RTS.bcComplement (RTS.bcByteString "\\()"))))
 
_TextObj :: D.Parser ()
 
_TextObj =
  do (RTS.<||)
       (RTS.pEnter "isUnicode"
          (do RTS.pEnter "PdfValue._UFEFF" _UFEFF
              RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._UniChar" _UniChar)))
       (RTS.pEnter "isPdfDoc"
          (RTS.pSkipMany (RTS.<||) (RTS.pEnter "PdfValue._Byte" _Byte)))
     RTS.pEnd "357:3--357:5"
 
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
 
nullValue :: Value
 
nullValue = Value_null ()
-- Type0Font: definition of a Type0 font:
import Stdlib
import Array
import Map
import Pair

import GenPdfValue
import PdfValue
import PdfDecl
import CMap
import FontDesc
import Encoding

import FontCommon

def EncodingRepr = Choose1 {
  predefEnc = PredefEncoding;
  encDict = EncodingP;
}

-- PartialType1Font: partial definition of a Type1 font
def PartialType1Font (com : PartialFontCommon) (pChars : PartialCharSet)
  (bf: maybe FontName) (enc: maybe EncodingRepr) = {
  common = com;
  chars = pChars;

  baseFont0 = bf;
  encoding0 = enc;
}

def InitType1Font = PartialType1Font
  InitCommonFont
  InitCharSet
  nothing
  nothing

def Type1SetCommon (com : PartialFontCommon)
  (f : PartialType1Font) = PartialType1Font
  com
  f.chars
  f.baseFont0
  f.encoding0

def Type1SetChars (pChars : PartialCharSet)
  (f : PartialType1Font) = PartialType1Font
  f.common
  pChars
  f.baseFont0
  f.encoding0

-- AddBaseFont nm f: add a base font to font
def Type1AddBaseFont Subst f = PartialType1Font 
  f.common
  f.chars
  (just (DirectOrRef (Token (GenName (FontName Subst)))))
  f.encoding0

-- AddEncoding: add an encoding
def Type1AddEncoding f = PartialType1Font
  f.common
  f.chars
  f.baseFont0
  (just (DirectOrRef EncodingRepr))

def ExtendType1Font subtypePrefix Subst k font = {
  @cf0 = ExtendCommonFont (append subtypePrefix "Type1") SimpleFontType
           k font.common;
  case cf0 of {
    just cf0 -> just (Type1SetCommon cf0 font)
  ; nothing ->
      case ExtendCharSet k font.chars of {
        just chars0 -> just (Type1SetChars chars0 font)
      ; nothing ->
          if k == "BaseFont" then {
            font.baseFont0 is nothing;
            just (Type1AddBaseFont Subst font)
          }
          else if k == "Encoding" then {
            font.encoding0 is nothing;
            just (Type1AddEncoding font)
          }
          else nothing
      }
  }
}

-- Type1Font f: coerce partial font f into an actual Type1
-- font
def Type1Font (f: PartialType1Font) = {
  toUnicode = CommonFont f.common;
  baseFont = f.baseFont0 is just; -- required 
  charSet = case (baseFont : FontName) of {
    standard _ -> 
      (When {
        f.chars.firstChar0 is nothing;
        f.chars.lastChar0 is nothing;
        f.chars.widths0 is nothing;
        f.chars.fontDesc0 is nothing;
      }
      nothing) <|
      (just (CharSet baseFont f.chars))
  ; nonStandard _ -> just (CharSet baseFont f.chars)
  };

  encoding = f.encoding0;
}

def Type1FontP = GenPdfDict1
  InitType1Font
  (ExtendType1Font "" Void)
  Type1Font

-- Multiple master fonts (Sec. 9.6.2.3)
def MMFontP = GenPdfDict1
  InitType1Font
  (ExtendType1Font "MM" (When (Match1 '_') ' ')) -- sub underscore w space
  Type1Font

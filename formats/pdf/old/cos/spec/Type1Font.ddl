-- Type0Font: definition of a Type0 font:
import Stdlib
import Array
import Map
import Maybe
import Pair

import CMap
import Encoding
import FontCommon
import FontDesc
import GenPdfValue
import Glyph
import PdfValue
import PdfDecl
import StdEncoding
import SymbolEncoding
import ZapfDingbatsEncoding

def EncodsngRepr = First
  predefEnc = (Token (GenName PredefEncodingName))
  encDict = EncodingP


-- partialType1Font: partial definition of a Type1 font
def partialType1Font (com : partialCommonFont) (pChars : partialCharSet)
  (bf: maybe FontName) (enc: maybe EncodingRepr) = {
  common = com;
  chars = pChars;

  baseFont0 = bf;
  encoding0 = enc;
}

def initType1Font = partialType1Font
  initCommonFont
  initCharSet
  nothing
  nothing

def type1SetCommon (com : partialCommonFont)
  (f : partialType1Font) = partialType1Font
  com
  f.chars
  f.baseFont0
  f.encoding0

def type1SetChars (pChars : partialCharSet)
  (f : partialType1Font) = partialType1Font
  f.common
  pChars
  f.baseFont0
  f.encoding0

-- AddBaseFont nm f: add a base font to font
-- TODO: re-enable Subst
def Type1AddBaseFont (Subst : uint 8) (f : partialType1Font) =
  partialType1Font 
    f.common
    f.chars
    (just (DirectOrRef (Token (GenName (FontNameP)))))
    f.encoding0

-- AddEncoding: add an encoding
def Type1AddEncoding f = partialType1Font
  f.common
  f.chars
  f.baseFont0
  (just (DirectOrRef EncodingRepr))

def ExtendType1Font (subType : FontSubty) (Subst : uint 8)
  k (font : partialType1Font) : maybe partialType1Font = {
  @cf0 = ExtendCommonFont subType SimpleFontType
           k font.common;
  case cf0 of {
    just cf0 -> just (type1SetCommon cf0 font)
  ; nothing ->
       case ExtendCharSet k font.chars of {
         just chars0 -> just (type1SetChars chars0 font)
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

def type1StdDesc (std : StandardFont) (fd : maybe CharSet) = {
  type1StdName = std;
  type1StdDesc = fd;
}

def type1NonStdDesc (nm : [ uint 8 ]) (cs : CharSet) = {
  type1NonStdName = nm;
  type1NonStdDesc = cs;
}

-- Type1FontDesc: a font descriptor for Type1 fonts
def Type1FontDesc (std : StandardFont) (cs : CharSet) = First
  stdDesc = type1StdDesc std (just cs)
  nonStdDesc = type1NonStdDesc [ ] cs


def mkType1StdDesc std cs : Type1FontDesc = {|
  stdDesc = type1StdDesc std cs
|}

def mkType1NonStdDesc nm cs : Type1FontDesc = {|
  nonStdDesc = type1NonStdDesc nm cs
|}

-- Type1Font f: coerce partial font f into an actual Type1
-- font
def Type1Font (subTy : FontSubty) (f: partialType1Font) = {
  toUnicode = CommonFont f.common;
  @baseFont = f.baseFont0 is just;

  charSet = case (baseFont : FontName) of {
    standard s -> mkType1StdDesc s 
      ((When {
          f.chars.firstChar0 is nothing;
          f.chars.lastChar0 is nothing;
          f.chars.widths0 is nothing;
          f.chars.fontDesc0 is nothing;
        }
      nothing) <|
      (just (CharSet subTy baseFont f.chars)))
  ; nonStandard ns -> mkType1NonStdDesc ns (CharSet subTy baseFont f.chars)
  };

  encoding = f.encoding0;
}

-- Test1Font: stub value for testing
def Type1FontStub = Type1Font type1Sym
  (partialType1Font
     CommonFontWitness
     initCharSet
     (just helvetica)
     nothing)

-- DBG:
def Type1FontP0 = When Value Type1FontStub
def Type1FontP = GenPdfDict1
  initType1Font
  (ExtendType1Font type1Sym Void)
  (Type1Font type1Sym)

-- Multiple master fonts (Sec. 9.6.2.3)
def MMFontP = GenPdfDict1
  initType1Font
  (ExtendType1Font mmTypeSym (When $['_'] ' ')) -- sub underscore w space
  (Type1Font mmTypeSym)

def LatinEnc (font : Type1Font) =
  case font.encoding of {
    just encRepr ->
      case encRepr of {
        predefEnc nm -> PredefEncoding nm
      ; encDict d ->
          case d.baseEncoding of {
            just nm0 -> PredefEncoding nm0
          ; nothing -> StdEncoding
          }
      }
  ; nothing -> StdEncoding
  }

-- Type1BaseEnc f: the base encoding for font f
-- WARNING: this guesses at navigating very inconsistent info in a PDF
def Type1BaseEnc (f : Type1Font) = case f.charSet of {
  stdDesc s -> case (s.type1StdName : StandardFont) of {
      symbol -> SymbolEncoding
    ; zapfDingbats -> ZapfDingbatsEncoding
    ; _ -> LatinEnc f
    }
; nonStdDesc ns ->
    if ns.type1NonStdDesc.fontDesc.isLatin then LatinEnc f
    else SymbolEncoding
}

def type1EncDiffs (f : Type1Font) : [ uint 8 -> glyph ] = 
  case f.encoding of {
    just repr -> case repr of {
      predefEnc _ -> empty
    ; encDict d -> d.differences
    }
  ; nothing -> empty
  }

-- Type1Enc: the encoding map for a Type1 font
def Type1Enc (f : Type1Font) = 
  MapUnion (Type1BaseEnc f) (type1EncDiffs f)

import Stdlib
import Array
import Map
import Pair

import GenPdfValue
import PdfValue
import PdfDecl
import CIDFont
import CMap
import FontCommon
import FontDesc

def Type0Encoding = Choose1 {
  preDef = DirectOrRef (Token Name);
  cmap = CMapRef SimpleFontType; -- TODO: use Sec. 9.7.6.2
}

def PreDefEncoding (s : [ uint 8 ]) : Type0Encoding = {|
  preDef = s
|}

-- PartialType1Font: partial definition of a Type1 font
def partialType0Font (com : ?partialCommonFont)
  (bf: maybe FontName)
  (enc: maybe Type0Encoding)
  (dfs: maybe CIDFont) = {
  common = com;
  baseFont0 = bf;
  encoding0 = enc;
  descFonts0 = dfs;
}

def initType0Font = partialType0Font
  initCommonFont
  nothing
  nothing
  nothing

def Type0SetCommon (com : ?partialCommonFont) (f : partialType0Font) =
  partialType0Font
    com
    f.baseFont0
    f.encoding0
    f.descFonts0

-- AddBaseFont nm f: add a base font to font
def AddBaseFont f = partialType0Font 
  f.common
  (just (DirectOrRef (Token (GenName (FontName)))))
  f.encoding0
  f.descFonts0

-- AddEncoding: add an encoding
def AddEncoding f = partialType0Font
  f.common
  f.baseFont0
  (just Type0Encoding)
  f.descFonts0

-- AddFontDesc: add a font descriptor
def AddDescFonts f = partialType0Font
  f.common
  f.baseFont0
  f.encoding0
  (just (DirectOrRef (Between "[" "]" (DirectOrRef CIDFontP))))

def ExtendType0Font k font = {
  @cf0 = ExtendCommonFont type0Sym SimpleFontType -- TODO: fix font type
    k font.common;
  case cf0 of {
    just cf1 -> just (Type0SetCommon cf1 font)
  ; nothing -> 
      if k == "BaseFont" then {
        font.baseFont0 is nothing;
        just (AddBaseFont font)
      }
      else if k == "Encoding" then {
        font.encoding0 is nothing;
        just (AddEncoding font)
      }
      else if k == "DescendantFonts" then {
        font.descFonts0 is nothing;
        just (AddDescFonts font)
      }
      else nothing
  }
}

-- Type0Font f: coerce partial font f into an actual Type1
-- font
def Type0Font (f : partialType0Font) = {
  toUnicode = CommonFont f.common;

  encoding = f.encoding0 is just; -- required
  -- DBG:
  --  descFont = f.descFonts0 is just; -- required. TODO: enable
  baseFont = f.baseFont0 is just;

  -- Guard ((f.baseFont0 is just) ==
  --   (append descFont.cidBaseFont
  --     case (descFont.cidSubtype : CIDFontType) of {
  --       cidFontType0 -> append "-"
  --         (case (encoding : Type0Encoding) of {
  --           preDef nm -> nm
  --         ; cmap m -> ""
  --           -- TODO: CMap: extend with field for name
  --         })
  --     ; cidFontType2 -> ""
  --     }));
  -- TODO: enable once CMap parser extracts names
}

def Type0FontP = GenPdfDict1
  initType0Font
  ExtendType0Font
  Type0Font

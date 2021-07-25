import Stdlib
import Array
import Map
import Pair

import GenPdfValue
import PdfValue
import PdfDecl
import CIDFont
import CMap
import FontDesc

def Type0Encoding = Choose1 {
  preDef = DirectOrRef (Token Name);
  cmap = CMapRef SimpleFontType; -- TODO: use Sec. 9.7.6.2
}

def PreDefEncoding (s : [ uint 8 ]) : Type0Encoding = {|
  preDef = s
|}

-- PartialType1Font: partial definition of a Type1 font
def PartialType0Font (t: bool) (st: bool)
  (bf: maybe FontName)
  (enc: maybe Type0Encoding) (dfs: maybe CIDFont)
  (toUni : maybe ToUnicodeCMap0) = {
  type0 = t;
  subtype0 = st;
  baseFont0 = bf;
  encoding0 = enc;
  descFonts0 = dfs;
  toUnicode0 = toUni;
}

def InitType0Font = PartialType0Font
  false
  false
  nothing
  nothing
  nothing
  nothing

-- AddType: note that the required Type field has been seen
def Type0AddType f = PartialType0Font 
  (Holds (DirectOrRef (Token (NameStr "Font"))))
  f.subtype0
  f.baseFont0
  f.encoding0
  f.descFonts0
  f.toUnicode0

-- AddSubtype f: note the subtype field has been seen
def Type0AddSubtype f = PartialType0Font 
  f.type0
  (Holds (DirectOrRef (Token (NameStr "Type0"))))
  f.baseFont0
  f.encoding0
  f.descFonts0
  f.toUnicode0

-- AddBaseFont nm f: add a base font to font
def AddBaseFont f = PartialType0Font 
  f.type0
  f.subtype0
  (just (DirectOrRef (Token (GenName (FontName Void)))))
  f.encoding0
  f.descFonts0
  f.toUnicode0

-- AddEncoding: add an encoding
def AddEncoding f = PartialType0Font
  f.type0
  f.subtype0
  f.baseFont0
  (just Type0Encoding)
  f.descFonts0
  f.toUnicode0

-- AddFontDesc: add a font descriptor
def AddDescFonts f = PartialType0Font
  f.type0
  f.subtype0
  f.baseFont0
  f.encoding0
  (just (DirectOrRef (Between "[" "]" (DirectOrRef CIDFontP))))
  f.toUnicode0

-- AddToUnicode: add a to-unicode map
def AddToUnicode f = PartialType0Font
  f.type0
  f.subtype0
  f.baseFont0
  f.encoding0
  f.descFonts0
  (just (CMapRef SimpleFontType))

def ExtendType0Font k font = {
  if k == "Type" then {
    font.type0 is false;
    just (Type0AddType font)
  }
  else if k == "Subtype" then {
    font.subtype0 is false;
    just (Type0AddSubtype font)
  }
  else if k == "BaseFont" then {
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
  else if k == "ToUnicode" then {
    font.toUnicode0 is nothing;
    just (AddToUnicode font)
  }
  else nothing
}

-- Type0Font f: coerce partial font f into an actual Type1
-- font
def Type0Font (f : PartialType0Font) = {
  Guard f.type0; -- required
  Guard f.subtype0; -- required

  encoding = f.encoding0 is just; -- required
  --  descFont = f.descFonts0 is just; -- required
  -- TODO: enable for testing
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
  -- TODO:

  toUnicode = f.toUnicode0;
}

def Type0FontP = GenPdfDict1
  InitType0Font
  ExtendType0Font
  Type0Font

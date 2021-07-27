-- FontCommon: parsing common to all fonts
import Stdlib
import PdfValue
import GenPdfValue
import PdfDecl

import FontDesc
import CMap

-- CommonFont: fields that are common to all fonts
--------------------------------------------------------------------------------

def PartialCommonFont (ty : bool) (subTy : bool)
  (toUnicode : maybe ToUnicodeCMap0) = {
  fontType = ty;
  fontSubtype = subTy;
  toUnicode = toUnicode;
}

def InitCommonFont = PartialCommonFont
  false
  false
  nothing

-- mainly used for testing
def CommonFontWitness = PartialCommonFont true true nothing

-- AddType: note that the required Type field has been seen
def FontAddType f = PartialCommonFont
  (Holds (DirectOrRef (NameToken "Font")))
  f.fontSubtype
  f.toUnicode

-- AddSubtype f: note the subtype field has been seen
def AddSubtype (subTy : [ uint 8 ]) f = PartialCommonFont
  f.fontType
  (Holds (DirectOrRef (NameToken subTy)))
  f.toUnicode

-- AddToUnicode: add a to-unicode map
def AddToUnicode (fontClass : FontType) f = PartialCommonFont
  f.fontType
  f.fontSubtype
  (When (Token Ref) nothing)
-- DBG:
--  (just (CMapRef fontClass))

def ExtendCommonFont (subTy : [ uint 8]) (fontClass : FontType) (k : [ uint 8 ])
  (font : PartialCommonFont) : maybe PartialCommonFont = 
  if k == "Type" then {
    font.fontType is false;
    just (FontAddType font)
  }
  else if k == "Subtype" then {
    font.fontSubtype is false;
    just (AddSubtype subTy font)
  }
  else if k == "ToUnicode" then {
    font.toUnicode is nothing;
    just (AddToUnicode fontClass font)
  }
  else nothing

-- CommonFont: finalize the common font
def CommonFont (f : PartialCommonFont) = {
  Guard f.fontType;
  Guard f.fontSubtype;
  f.toUnicode
}

-- CharSet: fields that represent a set of characters:
--------------------------------------------------------------------------------

-- fields common to only Type1 and Type3

-- TODO: add name and font desc to this
def PartialCharSet (name: maybe [ uint 8 ]) 
  (first: maybe (uint 64)) (last : maybe (uint 64))
  (pWidths : maybe [ uint 64 ])
  (pFontDesc : maybe stream) = {
  name0 = name;
  firstChar0 = first;
  lastChar0 = last;
  widths0 = pWidths;
  fontDesc0 = pFontDesc;
}

def InitCharSet = PartialCharSet
  nothing
  nothing
  nothing
  nothing
  nothing

-- AddName: add a name
-- TODO: inherit name from resource dict, for checking
def AddName f = PartialCharSet
  (just (Token Name))
  f.firstChar0
  f.lastChar0
  f.widths0
  f.fontDesc0

-- AddFirstChar: add a first character seen
def AddFirstChar f = PartialCharSet
  f.name0
  (just (DirectOrRef (Token UNatural)))
  f.lastChar0
  f.widths0
  f.fontDesc0

-- AddLastChar: add the last character seen
def AddLastChar f = PartialCharSet
  f.name0
  f.firstChar0
  (just (DirectOrRef (Token UNatural)))
  f.widths0
  f.fontDesc0

-- AddWidths: add the last character seen
def AddWidths f = PartialCharSet
  f.name0
  f.firstChar0
  f.lastChar0
  (just (DirectOrRef (GenArray UNatural)))
  f.fontDesc0

-- AddFontDesc: add a font descriptor
def AddFontDesc f = PartialCharSet
  f.name0
  f.firstChar0
  f.lastChar0
  f.widths0
  (just (InputAtRef (Token Ref) is just))

def ExtendCharSet (k : [ uint 8 ]) (chars : PartialCharSet) :
  maybe PartialCharSet = 
  if k == "Name" then {
    chars.name0 is nothing;
    just (AddName chars)
  }
  else if k == "FirstChar" then {
    chars.firstChar0 is nothing;
    just (AddFirstChar chars)
  }
  else if k == "LastChar" then {
    chars.lastChar0 is nothing;
    just (AddLastChar chars)
  }
  else if k == "Widths" then {
    chars.widths0 is nothing;
    just (AddWidths chars)
  }
  else if k == "FontDescriptor" then {
    chars.fontDesc0 is nothing;
    just (AddFontDesc chars)
  }
  else nothing

-- PDFA: what should this be called with for Type3 fonts?
def CharSet (baseFont : FontName) (chars : PartialCharSet) = {
  name = chars.name0; -- required only in PDF 1.0

  firstChar = chars.firstChar0 is just;
  lastChar = chars.lastChar0 is just;
  Guard (firstChar <= lastChar);

  widths = chars.widths0 is just; 
  Guard ((length widths) == inc (lastChar - firstChar));

  fontDesc = WithStream (chars.fontDesc0 is just)
    (GenObj (FontDescP baseFont))
}

-- Type0: type, subtype, basefont, encoding (name or stream), descendants, toUnicode

-- Type1: type, subtype, name, basefont, chars..., fontDesc, encoding (name or dictionary), toUnicode

-- Type3: type, subtype, name, encoding : dict, chars..., fontDesc, toUnicode

-- Common: type, subtype, toUnicode

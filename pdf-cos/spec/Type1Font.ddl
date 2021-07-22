-- Type0Font: definition of a Type0 font:
import Stdlib
import Array
import Map
import Pair

import GenPdfValue
import PdfValue
-- import PdfDecl
import CMap
import FontDesc

import Testing

-- Type1Font0: partial definition of a Type1 font
def Type1Font0 (t: bool) (st: bool)
  (name: maybe string) (bf: maybe string)
  (fc: maybe (uint 64)) (lc: maybe (uint 64))
  (ws: maybe [ int ]) (fd: maybe stream) (enc: maybe Encoding)
  toUni = {
  type0 = t;
  subtype0 = st;
  name0 = name;
  baseFont0 = bf;
  firstChar0 = fc;
  lastChar0 = lc;
  widths0 = ws;
  fontDesc0 = fd;
  encoding0 = enc;
  toUnicode0 = toUni;
}

def InitType1Font = Type1Font0
  false
  false
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing

-- AddType: note that the required Type field has been seen
def Type1AddType f = Type1Font0 
  (Holds (DirectOrRef (Token (NameStr "Font"))))
  f.subtype0
  f.name0
  f.baseFont0
  f.firstChar0
  f.lastChar0
  f.widths0
  f.fontDesc0
  f.encoding0
  f.toUnicode0

-- AddSubtype f: note the subtype field has been seen
def AddSubtype f = Type1Font0 
  f.type0
  (Holds (DirectOrRef (Token (NameStr "Type1"))))
  f.name0
  f.baseFont0
  f.firstChar0
  f.lastChar0
  f.widths0
  f.fontDesc0
  f.encoding0
  f.toUnicode0

-- AddName f nm: 
def AddName f = Type1Font0 
  f.type0
  f.subtype0
  (just (DirectOrRef (Token Name)))
  f.baseFont0
  f.firstChar0
  f.lastChar0
  f.widths0
  f.fontDesc0
  f.encoding0
  f.toUnicode0

-- AddBaseFont nm f: add a base font to font
def AddBaseFont f = Type1Font0 
  f.type0
  f.subtype0
  f.name0
  (just (DirectOrRef (Token Name)))
  f.firstChar0
  f.lastChar0
  f.widths0
  f.fontDesc0
  f.encoding0
  f.toUnicode0

-- AddFirstChar: add a first character seen
def AddFirstChar f = Type1Font0
  f.type0
  f.subtype0
  f.name0
  f.baseFont0
  (just (DirectOrRef (Token (Natural as! uint 64))))
  -- TODO: rework to remove coercion
  f.lastChar0
  f.widths0
  f.fontDesc0
  f.encoding0
  f.toUnicode0

-- AddLastChar: add the last character seen
def AddLastChar f = Type1Font0
  f.type0
  f.subtype0
  f.name0
  f.baseFont0
  f.firstChar0
  (just (DirectOrRef (Token (Natural as! uint 64))))
  f.widths0
  f.fontDesc0
  f.encoding0
  f.toUnicode0

-- AddWidths: add the last character seen
def AddWidths f = Type1Font0
  f.type0
  f.subtype0
  f.name0
  f.baseFont0
  f.firstChar0
  f.lastChar0
  (just (DirectOrRef (GenArray Natural)))
  f.fontDesc0
  f.encoding0
  f.toUnicode0

-- AddFontDesc: add a font descriptor
def AddFontDesc f = Type1Font0
  f.type0
  f.subtype0
  f.name0
  f.baseFont0
  f.firstChar0
  f.lastChar0
  f.widths0
  { @s = GetStream;
    Dict;
    just s
  }
  f.encoding0
  f.toUnicode0

-- AddEncoding: add an encoding
def AddEncoding f = Type1Font0
  f.type0
  f.subtype0
  f.name0
  f.baseFont0
  f.firstChar0
  f.lastChar0
  f.widths0
  f.fontDesc0
  (just (DirectOrRef Encoding))
  f.toUnicode0

-- AddToUnicode: add a to-unicode map
def AddToUnicode f = Type1Font0
  f.type0
  f.subtype0
  f.name0
  f.baseFont0
  f.firstChar0
  f.lastChar0
  f.widths0
  f.fontDesc0
  f.encoding0
  (just
    (WithStream ((ResolveStream {| ref = Token Ref |}).body is ok)
      (ToUnicodeCMap {| simpleFont = {} |})))

-- TODO: refine this defn in another module
def CharEncodingDict = Dict

def Encoding = Choose {
  macRoman = @(NameToken "MacRomanEncoding");
  macExpert = @(NameToken "MacExpertEncoding");
  winAnsi = @(NameToken "WinAnsiEncoding");
  encDict = CharEncodingDict;
}

def ExtendType1Font k font = {
  if k == "Type" then {
    font.type0 is false;
    just (Type1AddType font)
  }
  else if k == "Subtype" then {
    font.subtype0 is false;
    just (AddSubtype font)
  }
  else if k == "Name" then {
    font.name0 is nothing;
    just (AddName font)
  }
  else if k == "BaseFont" then {
    font.baseFont0 is nothing;
    just (AddBaseFont font)
  }
  else if k == "FirstChar" then {
    font.firstChar0 is nothing;
    just (AddFirstChar font)
  }
  else if k == "LastChar" then {
    font.lastChar0 is nothing;
    just (AddLastChar font)
  }
  else if k == "Widths" then {
    font.widths0 is nothing;
    just (AddWidths font)
  }
  else if k == "FontDescriptor" then {
    font.fontDesc0 is nothing;
    just (AddFontDesc font)
  }
  else if k == "Encoding" then {
    font.encoding0 is nothing;
    just (AddEncoding font)
  }
  else if k == "ToUnicode" then {
    font.toUnicode0 is nothing;
    just (AddToUnicode font)
  }
  else nothing
}

-- Type1Font f: coerce partial font f into an actual Type1
-- font
def Type1Font f = {
  Guard f.type0;
  Guard f.subtype0;

  name = f.name0; -- required
  baseFont = f.baseFont0 is just; -- required
  firstChar = f.firstChar0 is just; -- required?
  lastChar = f.lastChar0 is just; -- required
  Guard (firstChar <= lastChar);

  widths = f.widths0 is just; -- required
  Guard ((length widths) == inc (lastChar - firstChar));

  fontDesc = WithStream (f.fontDesc0 is just) (FontDescP baseFont);
  encoding = f.encoding0;
  toUnicode = f.toUnicode0;
}

def Type1FontP = GenPdfDict1
  InitType1Font
  ExtendType1Font
  Type1Font

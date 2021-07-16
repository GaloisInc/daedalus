-- Type0Font: definition of a Type0 font:
import Stdlib
import Array
import Map

import GenPdfValue
import PdfValue
import PdfDecl
import CMap

-- Type1Font0: partial definition of a Type1 font
def Type1Font0 = {
  type0 = ^false;
  subtype0 = ^false;
  name0 = ^nothing;
  baseFont0 = ^nothing;
  firstChar0 = ^nothing;
  lastChar0 = ^nothing;
  widths0 = ^nothing;
  fontDesc0 = ^nothing;
  encoding0 = ^nothing;
  toUnicode0 = ^nothing;
  others0 = ^empty;
}

-- AddType: note that the required Type field has been seen
def AddType f : Type1Font0 = {
  type0 = Holds (DirectOrRef (GenName "Font"));

  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddSubtype f: note the subtype field has been seen
def AddSubtype f : Type1Font0 = {
  type0 = ^f.type0;

  subtype0 = Holds (DirectOrRef (GenName "Type1"));

  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddName f nm: 
def AddName f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;

  name0 = just (DirectOrRef (Token Name));

  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddBaseFont nm f: add a base font to font
def AddBaseFont f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;

  baseFont0 = just (DirectOrRef (Token Name));

  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddFirstChar: add a first character seen
def AddFirstChar f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;

  firstChar0 = just (DirectOrRef (Natural as! uint 64));
  -- TODO: rework to remove coercion

  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddLastChar: add the last character seen
def AddLastChar f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;

  lastChar0 = just (DirectOrRef (Natural as! uint 64));

  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddWidths: add the last character seen
def AddWidths f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;

  widths0 = just (DirectOrRef (GenArray Natural));

  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddFontDesc: add a font desriptor
def AddFontDesc f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;

  fontDesc0 = just (DirectOrRef (FontDescriptor));

  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddEncoding: add an encoding
def AddEncoding f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;

  encoding0 = just (DirectOrRef Encoding);

  toUnicode0 = ^f.toUnicode0;
  others0 = ^f.others0;
}

-- AddToUnicode: add a to-unicode map
def AddToUnicode f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;

  toUnicode0 = just {
    @r = {| ref = Token Ref |};
    @strm = ResolveStream r;
    @fontTy = {| simpleFont = ^{} |};
    ToUnicodeCMap fontTy
  };

  others0 = ^f.others0;
}


-- AddToUnicode: add a to-unicode map
def AddOther k f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;

  others0 = Extend k (Token Value) f.others0;
}


-- CoerceType1Font f: coerce font accumulation into an actual Type1
-- font
def CoerceType1Font f = {
  Guard f.type0;
  Guard f.subtype0;

  name = f.name0 is just; -- required
  baseFont = f.baseFont0 is just; -- required
  firstChar = f.firstChar0 is just; -- required?
  lastChar = f.lastChar0 is just; -- required
  Guard (firstChar <= lastChar);

  widths = f.widths0 is just; -- required
  Guard ((length widths) == inc (lastChar - firstChar));

  fontDesc = f.fontDesc0 is just; -- required
  encoding = f.encoding0;
  toUnicode = f.toUnicode0;
  others = f.others0;
}

-- TODO: refine these two defns:
def CharEncodingDict = Dict

def FontDescriptor = Dict

def Encoding = Choose {
  macRoman = NameToken "MacRomanEncoding";
  macExpert = NameToken "MacExpertEncoding";
  winAnsi = NameToken "WinAnsiEncoding";
  encDict = CharEncodingDict;
}


def Type1FontRec font = Default font {
  @k = Token Name;
  @font0 = if k == "Type" then {
      font.type0 is false;
      AddType font
    }
    else if k == "Subtype" then {
      font.subtype0 is false;
      AddSubtype font
    }
    else if k == "Name" then {
      font.name0 is nothing;
      AddName font
    }
    else if k == "BaseFont" then {
      font.baseFont0 is nothing;
      AddBaseFont font
    }
    else if k == "FirstChar" then {
      font.firstChar0 is nothing;
      AddFirstChar font
    }
    else if k == "LastChar" then {
      font.lastChar0 is nothing;
      AddLastChar font
    }
    else if k == "Widths" then {
      font.widths0 is nothing;
      AddWidths font
    }
    else if k == "FontDescriptor" then {
      font.fontDesc0 is nothing;
      AddFontDesc font
    }
    else if k == "Encoding" then {
      font.encoding0 is nothing;
      AddEncoding font
    }
    else if k == "ToUnicode" then {
      font.toUnicode0 is nothing;
      AddToUnicode font
    }
    else AddOther k font;
  Type1FontRec font0
}

def Type1Font = Between "<<" ">>" {
  @initFont = Type1Font0;
  @fontRec = Type1FontRec initFont;
  CoerceType1Font fontRec
} 

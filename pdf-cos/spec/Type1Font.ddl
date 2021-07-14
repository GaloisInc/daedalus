-- Type0Font: definition of a Type0 font:
import Stdlib

import GenPdfValue
import PdfValue
import PdfDecl
import CMap

-- Type1Font0: accumulating type for font 0
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
def AddType f : Type1Font0= {
  type0 = {
    NameToken "Font";
    ^true
  };

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

  subtype0 = {
    NameToken "Type1";
    ^true
  };

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

  name0 = just (Token Name);

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

  baseFont0 = just (Token Name);

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

  firstChar0 = just (Token Natural as! uint 64);
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

  lastChar0 = just (Token Natural as! uint 64);

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

  widths0 = just (Token (GenArray Natural));

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

  fontDesc0 = just (Token FontDescriptor);

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

  encoding0 = just (Token Encoding);

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

  toUnicode0 = {
    @r = {| ref = Token Ref |};
    @strm = ResolveStream r;
    @fontTy = {| simpleFont = ^{} |};
    @cmap = ToUnicodeCMap fontTy;
    just cmap
  };

  others0 = ^f.others0;
}


-- AddToUnicode: add a to-unicode map
def AddOther f : Type1Font0 = {
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

  others0 = {
    @k = Token Name; -- parse any unspecified fields
    @v = Token Value;
    Insert k v f.others0
  };
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


-- TODO: potentially lookup dictionary fields as references

def Type1FontRec font =
  { @font0 = Choose1 {
      { NameToken "Type";
        AddType font 
      };
      { NameToken "Subtype";
        AddSubtype font 
      };
      { NameToken "Name";
        AddName font
      };
      { NameToken "BaseFont";
        AddBaseFont font
      };
      { NameToken "FirstChar";
        AddFirstChar font
      };
      { NameToken "LastChar";
        AddLastChar font
      };
      { NameToken "Widths";
        AddWidths font
      };
      { NameToken "FontDescriptor";
        AddFontDesc font
      };
      { NameToken "Encoding";
        AddEncoding font
      };
      { NameToken "ToUnicode";
        AddToUnicode font
      };
      AddOther font; -- parse any unspecified fields
    };
    Type1FontRec font0
  } <|
  ^font

def Type1Font = {
  @initFont = Type1Font0;
  @fontRec = Type1FontRec initFont;
  CoerceType1Font fontRec
} 

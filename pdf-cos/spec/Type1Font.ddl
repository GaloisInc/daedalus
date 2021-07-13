-- Type0Font: definition of a Type0 font:
import Stdlib

import GenPdfValue
import PdfValue
import PdfDecl
import CMap

def NameToken s = Token (GenName s)

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
  others = ^empty;
}

-- AddField: template for adding field values. Not actually used.
def AddField f : Type1Font0 = {
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
  others = ^f.others;
}

-- AddType: note that the required Type field has been seen
def AddType f : Type1Font0= {
  type0 = ^true;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddSubtype f: note the subtype field has been seen
def AddSubtype f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^true;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddName f nm: 
def AddName nm f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;

  name0 = just nm;

  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddBaseFont nm f: add a base font to font
def AddBaseFont bf f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;

  baseFont0 = just bf;

  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddFirstChar: add a first character seen
def AddFirstChar fc f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;

  firstChar0 = just fc;

  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddLastChar: add the last character seen
def AddLastChar lc f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;

  lastChar0 = just lc;

  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddWidths: add the last character seen
def AddWidths ws f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;

  widths0 = just ws;

  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddFontDesc: add a font desriptor
def AddFontDesc fd f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;

  fontDesc0 = just fd;

  encoding0 = ^f.encoding0;
  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddEncoding: add an encoding
def AddEncoding enc f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;

  encoding0 = just enc;

  toUnicode0 = ^f.toUnicode0;
  others = ^f.others;
}

-- AddToUnicode: add a to-unicode map
def AddToUnicode toUni f : Type1Font0 = {
  type0 = ^f.type0;
  subtype0 = ^f.subtype0;
  name0 = ^f.name0;
  baseFont0 = ^f.baseFont0;
  firstChar0 = ^f.firstChar0;
  lastChar0 = ^f.lastChar0;
  widths0 = ^f.widths0;
  fontDesc0 = ^f.fontDesc0;
  encoding0 = ^f.encoding0;

  toUnicode0 = just toUni;

  others = ^f.others;
}


-- AddToUnicode: add a to-unicode map
def AddOther k v f : Type1Font0 = {
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

  others = Insert k v f.others;
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
        @ty = NameToken "Font";
        AddType font 
      };
      { NameToken "Subtype";
        @subTy = NameToken "Type1";
        AddSubtype font 
      };
      { NameToken "Name";
        @nm = Token Name;
        AddName nm font
      };
      { NameToken "BaseFont";
        @baseFont = Token Name;
        AddBaseFont baseFont font
      };
      { NameToken "FirstChar";
        @firstChar = Token Natural as! uint 64;
        AddFirstChar firstChar font
      };
      { NameToken "LastChar";
        @lastChar = Token Natural as! uint 64;
        AddLastChar lastChar font
      };
      { NameToken "Widths";
        @widths = Token (GenArray Natural);
        AddWidths widths font
      };
      { NameToken "FontDescriptor";
        @fontDesc = Token FontDescriptor;
        AddFontDesc fontDesc font
      };
      { NameToken "Encoding";
        @encoding = Token Encoding;
        AddEncoding encoding font
      };
      { NameToken "ToUnicode";
        @r = {| ref = Token Ref |};
        @strm = ResolveStream r;
        @fontTy = {| simpleFont = ^{} |};
        @cmap = ToUnicodeCMap fontTy;
        AddToUnicode cmap font
      };
      { @k = Token Name; -- parse any unspecified fields
        @v = Value;
        AddOther k v font
      };
    };
    Type1FontRec font0
  } <|
  ^font

def Type1Font = {
  @initFont = Type1Font0;
  @fontRec = Type1FontRec initFont;
  CoerceType1Font fontRec
} 

-- FontDesc: font descriptor
import Array
import Pair
import Stdlib

import GenPdfValue
import PdfValue
import PdfDecl

-- FontSubty: the subtypes of fonts
def FontSubty = Choose1 {
  cidType0 = @(Match "CIDFontType0");
  cidType2 = @(Match "CIDFontType2");
  fontTy0 = @(Match "Type0");
  fontTy1 = @(Match "Type1");
  fontTy3 = @(Match "Type3");
  fontMM = @(Match "MMType1");
  fontTrueType = @(Match "TrueType");
}

def cidFontType0Sym : FontSubty = {| cidType0 = {} |}
def cidFontType2Sym : FontSubty = {| cidType2 = {} |}
def type0Sym : FontSubty = {| fontTy0 = {} |}
def type1Sym : FontSubty = {| fontTy1 = {} |}
def type3Sym : FontSubty = {| fontTy3 = {} |}
def mmTypeSym : FontSubty = {| fontMM = {} |}
def trueTypeSym : FontSubty = {| fontTrueType = {} |}

-- the 14 standard fonts
-- KEY: prefix order of choices is critical
def StandardFont = Choose1 {
  courierBoldOblique = @(Match "Courier-BoldOblique")
; courierBold = @(Match "Courier-Bold")
; courierOblique = @(Match "Courier-Oblique")
; courier = @(Match "Courier")
; helveticaBoldOblique = @(Match "Helvetica-BoldOblique")
; helveticaBold = @(Match "Helvetica-Bold")
; helveticaOblique = @(Match "Helvetica-Oblique")
; helvetica = @(Match "Helvetica")
; symbol = @(Match "Symbol")
; timesBoldItalic = @(Match "Times-BoldItalic")
; timesBold = @(Match "Times-Bold")
; timesItalic = @(Match "Times-Italic")
; timesRoman = @(Match "Times-Roman")
; zapfDingbats = @(Match "ZapfDingbats")
}

def HelveticaNm : StandardFont = {| helvetica = {} |}

def stdFontIsLatin (f : StandardFont) : bool = case f of {
  symbol -> false
; zapfDingbats -> false
; _ -> true
}

-- Base fonts: the 14 standards and everything else
-- TODO: reenable:
--def FontName (Subst : uint 8) = Choose1 {
def FontName = {
  @nameChars = Many NameChar;
  Choose1 {
    standard = WithStream (arrayStream nameChars) (Only StandardFont);
    nonStandard = nameChars
  }
}

def FontName0 : FontName = {| nonStandard = Many NameChar |}

def FontNameP = FontName

def Helvetica : FontName = {|
  standard = HelveticaNm
|}

def NonStandardFont (nm : [ uint 8]) : FontName = {|
  nonStandard = nm
|}

def PartialFontDesc (pt: bool) (pfn: bool) (mayFlags : maybe (uint 32))
  (ff : maybe Ref)
  (ff2 : maybe Ref)
  (ff3 : maybe Ref) = {
  descType0 = pt;
  descFontName0 = pfn;
  flags0 = mayFlags;
  fontFile = ff;
  fontFile2 = ff2;
  fontFile3 = ff3;
}

def InitFontDesc = PartialFontDesc
  false
  false
  nothing
  nothing
  nothing
  nothing

def AddFontDescType fd = PartialFontDesc
  (Holds (NameToken "FontDescriptor"))
  fd.descFontName0
  fd.flags0
  fd.fontFile
  fd.fontFile2
  fd.fontFile3

def AddFontDescName (parBaseFont : FontName) fd = PartialFontDesc
  fd.descType0
  (Holds (Token (GenName (FontName))))
--  (Holds (Guard ((Token (GenName (FontName Void))) == parBaseFont)))
  fd.flags0
  fd.fontFile
  fd.fontFile2
  fd.fontFile3

def AddFlags fd = PartialFontDesc
  fd.descType0
  fd.descFontName0
  (just (Token Natural as! uint 32))
  fd.fontFile
  fd.fontFile2
  fd.fontFile3

def ExtendFontDesc (parBaseFont : FontName)
  (k: [ uint 8 ]) (fd: PartialFontDesc) = 
  if k == "Type" then {
    fd.descType0 is false;
    just (AddFontDescType fd)
  }
  else if k == "FontName" then {
    fd.descFontName0 is false;
    just (AddFontDescName parBaseFont fd)
  }
  else if k == "Flags" then {
    fd.flags0 is nothing;
    just (AddFlags fd)
  }
  else nothing

def FontTyStream font okFonts mayStrm = case mayStrm of {
  just s -> When (Guard (member font okFonts)) (just s)
; nothing -> nothing
}

-- font flags (Table 121)
def FontDesc (subTy : FontSubty) (fd: PartialFontDesc) = {
  Guard fd.descType0;
  Guard fd.descFontName0;

  @flags = fd.flags0 is just;
  @fontIsSymbolic = bitIsSet32 flags 2;
  @fontIsNonSymbolic = bitIsSet32 flags 5;

  Guard (boolXor fontIsSymbolic fontIsNonSymbolic);
  isLatin = fontIsNonSymbolic;

  -- check the font files are consistent with 
  fontFile = FontTyStream subTy
    [ type1Sym
    , mmTypeSym
    ]
    fd.fontFile;
  fontFile2 = FontTyStream subTy
    [ trueTypeSym
    , mmTypeSym
    ]
    fd.fontFile2;
  fontFile3 = FontTyStream subTy
    [ cidFontType0Sym
    , trueTypeSym
    , cidFontType2Sym
    , type1Sym
    ]
    fd.fontFile3;
}

def StubFontDesc = FontDesc type1Sym (PartialFontDesc
  true
  true
  (just (32 : uint 32))
  nothing
  nothing
  nothing)

-- DBG:
def FontDescP (parBaseFont : FontName ) = When Value StubFontDesc
def FontDescP0 (fontSubty : FontSubty) (parBaseFont : FontName ) = GenPdfDict1
  InitFontDesc
  (ExtendFontDesc parBaseFont)
  (FontDesc fontSubty)

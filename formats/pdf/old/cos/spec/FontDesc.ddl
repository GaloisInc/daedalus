-- FontDesc: font descriptor
import Array
import Pair
import Stdlib

import GenPdfValue
import PdfValue
import PdfDecl

def CIDFontType = First
  cidFontType0 = @(Match "CIDFontType0")
  cidFontType2 = @(Match "CIDFontType2")


def cidFontType0Sym : CIDFontType = {| cidFontType0 = {} |}
def cidFontType2Sym : CIDFontType = {| cidFontType2 = {} |}

-- FontSubty: the subtypes of fonts
def FontSubty = First
  cidTy = CIDFontType
  fontTy0 = @(Match "Type0")
  fontTy1 = @(Match "Type1")
  fontTy3 = @(Match "Type3")
  fontMM = @(Match "MMType1")
  fontTrueType = @(Match "TrueType")


def cidTypeSym (pty : CIDFontType) : FontSubty = {| cidTy = pty |}
def type0Sym : FontSubty = {| fontTy0 = {} |}
def type1Sym : FontSubty = {| fontTy1 = {} |}
def type3Sym : FontSubty = {| fontTy3 = {} |}
def mmTypeSym : FontSubty = {| fontMM = {} |}
def trueTypeSym : FontSubty = {| fontTrueType = {} |}

-- the 14 standard fonts
-- KEY: prefix order of choices is critical
def StandardFont = First
  courierBoldOblique = @(Match "Courier-BoldOblique")
  courierBold = @(Match "Courier-Bold")
  courierOblique = @(Match "Courier-Oblique")
  courier = @(Match "Courier")
  helveticaBoldOblique = @(Match "Helvetica-BoldOblique")
  helveticaBold = @(Match "Helvetica-Bold")
  helveticaOblique = @(Match "Helvetica-Oblique")
  helvetica = @(Match "Helvetica")
  symbol = @(Match "Symbol")
  timesBoldItalic = @(Match "Times-BoldItalic")
  timesBold = @(Match "Times-Bold")
  timesItalic = @(Match "Times-Italic")
  timesRoman = @(Match "Times-Roman")
  zapfDingbats = @(Match "ZapfDingbats")


def helveticaNm : StandardFont = {| helvetica = {} |}

def stdFontIsLatin (f : StandardFont) : bool = case f of {
  symbol -> false
; zapfDingbats -> false
; _ -> true
}

-- Base fonts: the 14 standards and everything else
-- TODO: reenable:
--def FontName (Subst : uint 8) = First
def FontName = {
  @nameChars = Many NameChar;
  First
    -- DBG:
    standard = helveticaNm
--    standard = WithStream (arrayStream nameChars nameChars) (Only StandardFont);
    nonStandard = nameChars
}


def FontName0 : FontName = {| nonStandard = Many NameChar |}

def FontNameP = FontName

def helvetica : FontName = {|
  standard = helveticaNm
|}

def nonStandardFont (nm : [ uint 8]) : FontName = {|
  nonStandard = nm
|}

def partialFontDesc (pt: bool) (pfn: bool) (mayFlags : maybe (uint 32))
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

def initFontDesc = partialFontDesc
  false
  false
  nothing
  nothing
  nothing
  nothing

def AddFontDescType fd = partialFontDesc
  (Holds (NameToken "FontDescriptor"))
  fd.descFontName0
  fd.flags0
  fd.fontFile
  fd.fontFile2
  fd.fontFile3

def AddFontDescName (parBaseFont : FontName) fd = partialFontDesc
  fd.descType0
  (Holds (Token (GenName (FontName))))
--  (Holds (Guard ((Token (GenName (FontName Void))) == parBaseFont)))
  fd.flags0
  fd.fontFile
  fd.fontFile2
  fd.fontFile3

def AddFlags fd = partialFontDesc
  fd.descType0
  fd.descFontName0
  (just (Token Natural as! uint 32))
  fd.fontFile
  fd.fontFile2
  fd.fontFile3

def AddFontFile fd = partialFontDesc
  fd.descType0
  fd.descFontName0
  fd.flags0
  (just (Token Ref))
  fd.fontFile2
  fd.fontFile3

def AddFontFile2 fd = partialFontDesc
  fd.descType0
  fd.descFontName0
  fd.flags0
  fd.fontFile
  (just (Token Ref))
  fd.fontFile3

def AddFontFile3 fd = partialFontDesc
  fd.descType0
  fd.descFontName0
  fd.flags0
  fd.fontFile
  fd.fontFile2
  (just (Token Ref))

def ExtendFontDesc (parBaseFont : FontName)
  (k: [ uint 8 ]) (fd: partialFontDesc) = 
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
  else if k == "FontFile" then {
    fd.fontFile is nothing;
    just (AddFontFile fd)
  }
  else if k == "FontFile2" then {
    fd.fontFile2 is nothing;
    just (AddFontFile2 fd)
  }
  else if k == "FontFile3" then {
    fd.fontFile3 is nothing;
    just (AddFontFile3 fd)
  }
  else nothing

def FontTyStream font okFonts mayStrm = case mayStrm of {
  just s -> When (Guard (member font okFonts)) (just s)
; nothing -> nothing
}

-- font flags (Table 121)
def FontDesc (subTy : FontSubty) (fd: partialFontDesc) = {
  Guard fd.descType0;
  Guard fd.descFontName0;

  @flags = fd.flags0 is just;
  @fontIsSymbolic = bitIsSet32 flags 2;
  @fontIsNonSymbolic = bitIsSet32 flags 5;

  Guard (boolXor fontIsSymbolic fontIsNonSymbolic);
  isLatin = fontIsNonSymbolic;

  -- check the font files are consistent with the font type
  fontFile = FontTyStream subTy
    [ type1Sym
    , mmTypeSym
    ]
    fd.fontFile;
  fontFile2 = FontTyStream subTy
    [ trueTypeSym
    , cidTypeSym cidFontType2Sym
    ]
    fd.fontFile2;
  fontFile3 = FontTyStream subTy
    [ type1Sym
    , mmTypeSym
    , cidTypeSym cidFontType0Sym
    , trueTypeSym
    , cidTypeSym cidFontType2Sym
    ]
    fd.fontFile3;
}

-- type1Sym doesn't actually get used
def StubFontDesc = FontDesc type1Sym (partialFontDesc
  true
  true
  (just (32 : uint 32))
  nothing
  nothing
  nothing)

-- DBG:
def FontDescP0 (parBaseFont : FontName ) = When Value StubFontDesc
def FontDescP1 (parBaseFont : FontName ) = GenPdfDict1
  initFontDesc
  (ExtendFontDesc parBaseFont)
  (FontDesc type1Sym)

def FontDescP (fontSubty : FontSubty) (parBaseFont : FontName ) = GenPdfDict1
  initFontDesc
  (ExtendFontDesc parBaseFont)
  (FontDesc fontSubty)

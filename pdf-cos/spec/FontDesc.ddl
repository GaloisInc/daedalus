-- FontDesc: font descriptor
import Stdlib
import Pair

import GenPdfValue
import PdfValue
import PdfDecl

-- the 14 standard fonts
def StandardFont = Choose1 {
  timesRoman = @(Match "Times-Roman")
; helvetica = @(Match "Helvetica")
; courier = @(Match "Courier")
; symbol = @(Match "Symbol")
; timesBold = @(Match "Times-Bold")
; helveticaBold = @(Match "Helvetica-Bold")
; courierBold = @(Match "Courier-Bold")
; zapfDingbats = @(Match "ZapfDingbats")
; timesItalic = @(Match "Times-Italic")
; helveticaOblique = @(Match "Helvetica-Obique")
; courierOblique = @(Match "Courier-Oblique")
; timesBoldItalic = @(Match "Times-BoldItalic")
; helveticaBoldOblique = @(Match "Helvetica-BoldOblique")
; courierBoldOblique = @(Match "Courier-BoldOblique")
}

-- Base fonts: the 14 standards and everything else
def FontName Subst = Choose1 {
  standard = StandardFont
; nonStandard = Many (Subst <| NameChar)
}

def Helvetica : FontName = {|
  standard = {| helvetica = { } |}
|}

def NonStandardFont (nm : [ uint 8]) : FontName = {|
  nonStandard = nm
|}

def PartialFontDesc (pt: bool) (pfn: bool) (mayFlags : maybe int) = {
  descType0 = pt;
  descFontName0 = pfn;
  flags0 = mayFlags;
}

def InitFontDesc = PartialFontDesc
  false
  false
  nothing

def AddFontDescType fd = PartialFontDesc
  (Holds (Token (NameStr "FontDescriptor")))
  fd.descFontName0
  fd.flags0

def AddFontDescName (parBaseFont : FontName) fd = PartialFontDesc
  fd.descType0
  (Holds (Guard ((Token (GenName (FontName Void))) == parBaseFont)))
  fd.flags0

def AddFlags fd = PartialFontDesc
  fd.descType0
  fd.descFontName0
  (just (Token Integer))

def ExtendFontDesc (parBaseFont : FontName) (k: [ uint 8 ])
  (fd: PartialFontDesc) = 
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

-- font flags (Table 121)
def FontDesc (fd: PartialFontDesc) = {
  Guard fd.descType0;
  Guard fd.descFontName0;

  @flags = fd.flags0 is just;
  @fontIsSymbolic = bitIsSet flags 2;
  @fontIsNonSymbolic = bitIsSet flags 5;

  Guard (boolXor fontIsSymbolic fontIsNonSymbolic);
  isLatin = fontIsNonSymbolic
}

def FontDescP (parBaseFont : FontName ) = GenPdfDict1
  InitFontDesc
  (ExtendFontDesc parBaseFont)
  FontDesc

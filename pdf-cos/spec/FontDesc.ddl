-- FontDesc: font descriptor
import Stdlib
import Pair

import GenPdfValue
import PdfValue
import PdfDecl

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
-- DBG:
--def FontName (Subst : uint 8) = Choose1 {
def FontName = {
  @nameChars = Many NameChar;
  Choose1 {
    standard = WithStream (arrayStream nameChars) (Only StandardFont);
    nonStandard = nameChars
  }
}

def Helvetica : FontName = {|
  standard = HelveticaNm
|}

def NonStandardFont (nm : [ uint 8]) : FontName = {|
  nonStandard = nm
|}

def PartialFontDesc (pt: bool) (pfn: bool) (mayFlags : maybe (uint 32)) = {
  descType0 = pt;
  descFontName0 = pfn;
  flags0 = mayFlags;
}

def InitFontDesc = PartialFontDesc
  false
  false
  nothing

def AddFontDescType fd = PartialFontDesc
  (Holds (NameToken "FontDescriptor"))
  fd.descFontName0
  fd.flags0

def AddFontDescName (parBaseFont : FontName) fd = PartialFontDesc
  fd.descType0
  (Holds (Token (GenName (FontName))))
--  (Holds (Guard ((Token (GenName (FontName Void))) == parBaseFont)))
  fd.flags0

def AddFlags fd = PartialFontDesc
  fd.descType0
  fd.descFontName0
  (just (Token Natural as! uint 32))

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

-- font flags (Table 121)
def FontDesc (fd: PartialFontDesc) = {
  Guard fd.descType0;
  Guard fd.descFontName0;

  @flags = fd.flags0 is just;
  @fontIsSymbolic = bitIsSet32 flags 2;
  @fontIsNonSymbolic = bitIsSet32 flags 5;

  Guard (boolXor fontIsSymbolic fontIsNonSymbolic);
  isLatin = fontIsNonSymbolic
}

def FontDescP (parBaseFont : FontName ) = GenPdfDict1
  InitFontDesc
  (ExtendFontDesc parBaseFont)
  FontDesc

def StubFontDesc = FontDesc (PartialFontDesc
  true
  true
  (just (setBit 2 0)) )

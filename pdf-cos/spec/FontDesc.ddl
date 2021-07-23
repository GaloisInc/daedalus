-- FontDesc: font descriptor
import Stdlib
import Pair

import GenPdfValue
import PdfValue
import PdfDecl

def PartialFontDesc (pt: bool) (pfn: bool) = {
  descType0 = pt;
  descFontName0 = pfn;
}

def InitFontDesc = PartialFontDesc
  false
  false

def AddFontDescType fd = PartialFontDesc
  (Holds (Token (NameStr "FontDescriptor")))
  fd.descFontName0

def AddFontDescName baseFontNm fd = PartialFontDesc
  fd.descType0
  (Holds (Guard (Token Name == baseFontNm)))

def ExtendFontDesc (baseFontNm : string) (k: string) (fd: PartialFontDesc) = 
  if k == "Type" then {
    fd.descType0 is false;
    just (AddFontDescType fd)
  }
  else if k == "FontName" then {
    fd.descFontName0 is false;
    just (AddFontDescName "AGaramond-Semibold" fd)
  }
  else nothing

def FontDesc (fd: PartialFontDesc) = {
  Guard fd.descType0;
  Guard fd.descFontName0;
}

def FontDesc0 d = d

def FontDescP (baseFontNm : string) = GenPdfDict1
  InitFontDesc
  (ExtendFontDesc baseFontNm)
  FontDesc

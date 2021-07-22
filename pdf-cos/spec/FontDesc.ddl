-- FontDesc: font descriptor
import Stdlib
import Pair

import GenPdfValue
import PdfValue
import PdfDecl

def PartialFontDesc (t: bool) (fn: bool) = {
  descType0 = t;
  descFontName0 = fn;
}

def InitFontDesc = PartialFontDesc
  false
  false

def AddFontDescType fd = PartialFontDesc
  (Holds (DirectOrRef (GenName "FontDescriptor")))
  fd.descFontName0

def AddFontDescName baseFontNm fd = PartialFontDesc
  fd.descType0
  (Holds (Guard ((DirectOrRef Name) == baseFontNm)))

def ExtendFontDesc (baseFontNm : string) k fd =
  if k == "Type" then {
    fd.descType0 is false;
    just (AddFontDescType fd)
  }
  else if k == "FontName" then {
    fd.descFontName0 is false;
    just (AddFontDescName baseFontNm fd)
  }
  else nothing

def FontDesc (fd: PartialFontDesc) = {
  Guard fd.descType0;
  Guard fd.descFontName0;
}

def FontDescP (baseFontNm : string) = GenPdfDict1
  InitFontDesc
  (ExtendFontDesc baseFontNm)
  FontDesc

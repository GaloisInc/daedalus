-- FontDesc: font descriptor
import Stdlib
import Pair

import GenPdfValue
import PdfValue
import PdfDecl

def PartialFontDesc = {
  descType0 = false;
  descFontName0 = false;
  flags0 = nothing : maybe int;
}

def MkPartialFontDesc (pt: bool) (pfn: bool) mayFlags : PartialFontDesc = {
  descType0 = pt;
  descFontName0 = pfn;
  flags0 = mayFlags;
}

def InitFontDesc = MkPartialFontDesc
  false
  false
  nothing

def AddFontDescType fd = MkPartialFontDesc
  (Holds (Token (NameStr "FontDescriptor")))
  fd.descFontName0
  fd.flags0

def AddFontDescName baseFontNm fd = MkPartialFontDesc
  fd.descType0
  (Holds (Guard (Token Name == baseFontNm)))
  fd.flags0

def AddFlags fd = MkPartialFontDesc
  fd.descType0
  fd.descFontName0
  (just Integer)

def ExtendFontDesc (baseFontNm : [ uint 8 ]) (k: [ uint 8 ])
  (fd: PartialFontDesc) = 
  if k == "Type" then {
    fd.descType0 is false;
    just (AddFontDescType fd)
  }
  else if k == "FontName" then {
    fd.descFontName0 is false;
    just (AddFontDescName baseFontNm fd)
  }
  else if k == "Flags" then {
    fd.flags0 is nothing;
    just (AddFlags fd)
  }
  else nothing

def FontDesc (fd: PartialFontDesc) = {
  Guard fd.descType0;
  Guard fd.descFontName0;
  -- TODO: add some field here to make this a named type
  flags = fd.flags0 is just;
}

def FontDesc0 d = d

def FontDescP (baseFontNm : [ uint 8] ) = GenPdfDict1
  InitFontDesc
  (ExtendFontDesc baseFontNm)
  FontDesc

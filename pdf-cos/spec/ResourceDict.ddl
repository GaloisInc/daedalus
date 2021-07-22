import Stdlib
import Array
import Map
import Pair

-- import PdfDecl
import PdfValue
import GenPdfValue
import FontDict

import Testing

-- ResourceDict: resource dictionary, with default values in all fields

def ResourceDict0 pGState pCS pPattern pShading pXObj pFont pProcSet pProps = {
  extGState0 = pGState;
  colorSpace0 = pCS;
  pattern0 = pPattern;
  shading0 = pShading;
  xObject0 = pXObj;
  font0 = pFont;
  procSet0 = pProcSet;
  properties0 = pProps;
}

def InitResourceDict = ResourceDict0
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing

def AddExtGState d = ResourceDict0
  (just (DirectOrRef (GenPdfDict Dict)))
  -- TODO: refine using Sec. 8.4.5, if needed
  d.colorSpace0
  d.pattern0
  d.shading0
  d.xObject0
  d.font0
  d.procSet0
  d.properties0

def AddColorSpace d = ResourceDict0
  d.extGState0
  (just (DirectOrRef Dict))
  -- TODO: refine using Sec 8.6, if needed
  d.pattern0
  d.shading0
  d.xObject0
  d.font0
  d.procSet0
  d.properties0

def AddPattern d = ResourceDict0
  d.extGState0
  d.colorSpace0
  (just (DirectOrRef Dict))
  -- TODO: refine using Sec. 8.7, if needed
  d.shading0
  d.xObject0
  d.font0
  d.procSet0
  d.properties0

def AddShading d = ResourceDict0
  d.extGState0
  d.colorSpace0
  d.pattern0
  (just (DirectOrRef (GenPdfDict Dict)))
  -- TODO: refine using Sec. 8.7.4.5, if needed
  d.xObject0
  d.font0
  d.procSet0
  d.properties0

def AddXObject d = ResourceDict0
  d.extGState0
  d.colorSpace0
  d.pattern0
  d.shading0
  (just (DirectOrRef Dict))
  d.font0
  d.procSet0
  d.properties0

def AddFont d = ResourceDict0
  d.extGState0
  d.colorSpace0
  d.pattern0
  d.shading0
  d.xObject0
  (just (DirectOrRef (GenPdfDict FontDict)))
  d.procSet0
  d.properties0

def AddProcSet d = ResourceDict0
  d.extGState0
  d.colorSpace0
  d.pattern0
  d.shading0
  d.xObject0
  d.font0
  (just (DirectOrRef Array))
  d.properties0

def AddProperties d = ResourceDict0
  d.extGState0
  d.colorSpace0
  d.pattern0
  d.shading0
  d.xObject0
  d.font0
  d.procSet0
  (just (DirectOrRef (GenPdfDict Dict)))

def ExtendResourceDict k dict = {
  if k == "ExtGState" then {
    dict.extGState0 is nothing;
    just (AddExtGState dict)
  }
  else if k == "ColorSpace" then {
    dict.colorSpace0 is nothing;
    just (AddColorSpace dict)
  }
  else if k == "Pattern" then {
    dict.pattern0 is nothing;
    just (AddPattern dict)
  }
  else if k == "Shading" then {
    dict.shading0 is nothing;
    just (AddShading dict)
  }
  else if k == "XObject" then {
    dict.xObject0 is nothing;
    just (AddXObject dict)
  }
  else if k == "Font" then {
    dict.font0 is nothing;
    just (AddFont dict)
  }
  else if k == "ProcSet" then {
    dict.procSet0 is nothing;
    just (AddProcSet dict)
  }
  else if k == "Properties" then {
    dict.properties0 is nothing;
    just (AddProperties dict)
  }
  else nothing
}

-- ResourceDict d: coerce d into a resource dictionary, using default values
def ResourceDict d = {
  extGState = defaultEmpty d.extGState0;
  colorSpace = defaultEmpty d.colorSpace0;
  pattern = defaultEmpty d.pattern0;
  shading = defaultEmpty d.shading0;
  xObject = defaultEmpty d.xObject0;
  font = defaultEmpty d.font0;
  procSet = defaultEmptyArr d.procSet0;
  properties = defaultEmpty d.properties0;
}

-- ResourceDictP: parse resource dictionaries. Currently only does
-- detailed parsing of font dictionaries.
def ResourceDictP = GenPdfDict1
  InitResourceDict
  ExtendResourceDict
  ResourceDict

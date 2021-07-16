import Stdlib
import Array
import Map

import PdfValue
import GenPdfValue
import FontDict

-- ResourceDict: resource dictionary, with default values in all fields

def resourceDictFields = [
  "ExtGState",
  "ColorSpace",
  "Pattern",
  "Shading",
  "XObject",
  "Font",
  "ProcSet",
  "Properties",
]

def ResourceDict0 = {
  extGState0 = nothing;
  colorSpace0 = nothing;
  pattern0 = nothing;
  shading0 = nothing;
  xObject0 = nothing;
  font0 = nothing;
  procSet0 = nothing;
  properties0 = nothing;
  others0 = empty;
}

def AddExtGState d : ResourceDict0 = {
  extGState0 = just {
    NameToken "ExtGState";
    d.extGState0 is nothing;
    DirectOrRef (GenPdfDict Dict); -- TODO: refine using Sec. 8.4.5, if needed
  };

  colorSpace0 = ^d.colorSpace0;
  pattern0 = ^d.pattern0;
  shading0 = ^d.shading0;
  xObject0 = ^d.xObject0;
  font0 = ^d.font0;
  procSet0 = ^d.procSet0;
  properties0 = ^d.properties0;
  others0 = empty;
}

def AddColorSpace d : ResourceDict0 = {
  extGState0 = ^d.extGState0;

  colorSpace0 = just {
    NameToken "ColorSpace";
    d.colorSpace0 is nothing;
    DirectOrRef Dict; -- TODO: refine using Sec 8.6, if needed
  };

  pattern0 = ^d.pattern0;
  shading0 = ^d.shading0;
  xObject0 = ^d.xObject0;
  font0 = ^d.font0;
  procSet0 = ^d.procSet0;
  properties0 = ^d.properties0;
  others0 = empty;
}

def AddPattern d : ResourceDict0 = {
  extGState0 = ^d.extGState0;
  colorSpace0 = ^d.colorSpace0;

  pattern0 = just {
    NameToken "Pattern";
    d.pattern0 is nothing;
    DirectOrRef Dict; -- TODO: refine using Sec. 8.7, if needed
  };

  shading0 = ^d.shading0;
  xObject0 = ^d.xObject0;
  font0 = ^d.font0;
  procSet0 = ^d.procSet0;
  properties0 = ^d.properties0;
  others0 = empty;
}

def AddShading d : ResourceDict0 = {
  extGState0 = ^d.extGState0;
  colorSpace0 = ^d.colorSpace0;
  pattern0 = ^d.pattern0;

  shading0 = just {
    NameToken "Shading";
    d.shading0 is nothing;
    DirectOrRef (GenPdfDict Dict); -- TODO: refine using Sec. 8.7.4.5, if needed
  };

  xObject0 = ^d.xObject0;
  font0 = ^d.font0;
  procSet0 = ^d.procSet0;
  properties0 = ^d.properties0;
  others0 = empty;
}

def AddXObject d : ResourceDict0 = {
  extGState0 = ^d.extGState0;
  colorSpace0 = ^d.colorSpace0;
  pattern0 = ^d.pattern0;
  shading0 = ^d.shading0;

  xObject0 = just {
    NameToken "XObject";
    d.xObject0 is nothing;
    DirectOrRef Dict;
  };

  font0 = ^d.font0;
  procSet0 = ^d.procSet0;
  properties0 = ^d.properties0;
  others0 = empty;
}

def AddFont d : ResourceDict0 = {
  extGState0 = ^d.extGState0;
  colorSpace0 = ^d.colorSpace0;
  pattern0 = ^d.pattern0;
  shading0 = ^d.shading0;
  xObject0 = ^d.xObject0;

  font0 = just {
    NameToken "Font";
    d.font0 is nothing;
    DirectOrRef (GenPdfDict FontDict);
  };

  procSet0 = ^d.procSet0;
  properties0 = ^d.properties0;
  others0 = empty;
}

def AddProcSet d : ResourceDict0 = {
  extGState0 = ^d.extGState0;
  colorSpace0 = ^d.colorSpace0;
  pattern0 = ^d.pattern0;
  shading0 = ^d.shading0;
  xObject0 = ^d.xObject0;
  font0 = ^d.font0;

  procSet0 = just {
    NameToken "ProcSet";
    d.procSet0 is nothing;
    DirectOrRef Array;
  };

  properties0 = ^d.properties0;
  others0 = empty;
}

def AddProperties d : ResourceDict0 = {
  extGState0 = ^d.extGState0;
  colorSpace0 = ^d.colorSpace0;
  pattern0 = ^d.pattern0;
  shading0 = ^d.shading0;
  xObject0 = ^d.xObject0;
  font0 = ^d.font0;
  procSet0 = ^d.procSet0;

  properties0 = just {
    NameToken "Properties";
    d.properties0 is nothing;
    DirectOrRef (GenPdfDict Dict); -- TODO: refine using Sec. 14.6.2, if needed
  };

  others0 = empty;
}

def AddOther d : ResourceDict0 = {
  extGState0 = ^d.extGState0;
  colorSpace0 = ^d.colorSpace0;
  pattern0 = ^d.pattern0;
  shading0 = ^d.shading0;
  xObject0 = ^d.xObject0;
  font0 = ^d.font0;
  procSet0 = ^d.procSet0;
  properties0 = ^d.properties0;

  others0 = { -- parse any unspecified fields
    @k = Token Name; 
    Guard (!(member k resourceDictFields));
    @v = Token Value;
    InsertFresh k v d.others0
  };
}

def ResourceDictRec dict = Default dict {
    @dict0 = Choose1 {
      AddExtGState dict;
      AddColorSpace dict;
      AddPattern dict;
      AddShading dict;
      AddXObject dict;
      AddFont dict;
      AddProcSet dict;
      AddProperties dict;
      AddOther dict;
    };
    ResourceDictRec dict0
  }

def CoerceResourceDict d = {
  extGState = defaultEmpty d.extGState0;
  colorSpace = defaultEmpty d.colorSpace0;
  pattern = defaultEmpty d.pattern0;
  shading = defaultEmpty d.shading0;
  xObject = defaultEmpty d.xObject0;
  font = defaultEmpty d.font0;
  procSet = defaultEmptyArr d.procSet0;
  properties = defaultEmpty d.properties0;
  others = d.others0;
}

-- ResourceDictP: parse resource dictionaries. Currently only does
-- detailed parsing of font dictionaries.
def ResourceDictP = Between "<<" ">>" {
  @initDict = ResourceDict0;
  @partialRes = ResourceDictRec initDict;
  CoerceResourceDict partialRes
}

import Stdlib

import PdfValue
import GenPdfValue
import FontDict

-- ResourceDict: resource dictionary, with default values in all fields

-- TODO: replace these with options, to ensure that each field is
-- defined once
def ResourceDict = {
  extGState = empty;
  colorSpace = empty;
  pattern = empty;
  shading = empty;
  xObject = empty;
  font = empty;
  procSet = ^[ ];
  properties = empty;
  others = empty;
}

def AddExtGState d : ResourceDict = {
  extGState = GenPdfDict Dict; -- TODO: refine using Sec. 8.4.5, if needed

  colorSpace = ^d.colorSpace;
  pattern = ^d.pattern;
  shading = ^d.shading;
  xObject = ^d.xObject;
  font = ^d.font;
  procSet = ^d.procSet;
  properties = ^d.properties;
  others = empty;
}

def AddColorSpace d : ResourceDict = {
  extGState = ^d.extGState;

  colorSpace = Dict; -- TODO: refine using Sec 8.6, if needed

  pattern = ^d.pattern;
  shading = ^d.shading;
  xObject = ^d.xObject;
  font = ^d.font;
  procSet = ^d.procSet;
  properties = ^d.properties;
  others = empty;
}

def AddPattern d : ResourceDict = {
  extGState = ^d.extGState;
  colorSpace = ^d.colorSpace;

  pattern = Dict; -- TODO: refine using Sec. 8.7, if needed

  shading = ^d.shading;
  xObject = ^d.xObject;
  font = ^d.font;
  procSet = ^d.procSet;
  properties = ^d.properties;
  others = empty;
}

def AddShading d : ResourceDict = {
  extGState = ^d.extGState;
  colorSpace = ^d.colorSpace;
  pattern = ^d.pattern;

  shading = GenPdfDict Dict; -- TODO: refine using Sec. 8.7.4.5, if needed

  xObject = ^d.xObject;
  font = ^d.font;
  procSet = ^d.procSet;
  properties = ^d.properties;
  others = empty;
}

def AddXObject d : ResourceDict = {
  extGState = ^d.extGState;
  colorSpace = ^d.colorSpace;
  pattern = ^d.pattern;
  shading = ^d.shading;
  xObject = ^d.xObject;
  font = ^d.font;
  procSet = ^d.procSet;
  properties = ^d.properties;
  others = empty;
}

def AddFont d : ResourceDict = {
  extGState = ^d.extGState;
  colorSpace = ^d.colorSpace;
  pattern = ^d.pattern;
  shading = ^d.shading;
  xObject = ^d.xObject;

  font = GenPdfDict FontDict;

  procSet = ^d.procSet;
  properties = ^d.properties;
  others = empty;
}

def AddProcSet d : ResourceDict = {
  extGState = ^d.extGState;
  colorSpace = ^d.colorSpace;
  pattern = ^d.pattern;
  shading = ^d.shading;
  xObject = ^d.xObject;
  font = ^d.font;

  procSet = Array;

  properties = ^d.properties;
  others = empty;
}

def AddProperties d : ResourceDict = {
  extGState = ^d.extGState;
  colorSpace = ^d.colorSpace;
  pattern = ^d.pattern;
  shading = ^d.shading;
  xObject = ^d.xObject;
  font = ^d.font;
  procSet = ^d.procSet;

  properties = GenPdfDict Dict; -- TODO: refine using Sec. 14.6.2, if needed

  others = empty;
}

def AddOther d : ResourceDict = {
  extGState = ^d.extGState;
  colorSpace = ^d.colorSpace;
  pattern = ^d.pattern;
  shading = ^d.shading;
  xObject = ^d.xObject;
  font = ^d.font;
  procSet = ^d.procSet;
  properties = ^d.properties;

  others = {
    @k = Token Name; -- parse any unspecified fields
    @v = Token Value;
    Insert k v d.others
  };
}

-- TODO: refactor NameToken's
def ResourceDictRec dict = {
    @dict0 = Choose1 {
      { NameToken "ExtGState";
        AddExtGState dict
      };
      { NameToken "ColorSpace";
        AddColorSpace dict
      };
      { NameToken "Pattern";
        AddPattern dict
      };
      { NameToken "Shading";
        AddShading dict
      };
      { NameToken "XObject";
        AddXObject dict
      };
      { NameToken "Font";
        AddFont dict
      };
      { NameToken "ProcSet";
        AddProcSet dict
      };
      { NameToken "Properties";
        AddProperties dict
      };
      AddOther dict
    };
    ResourceDictRec dict0
  } <|
  ^dict

-- ResourceDict.ddl: definition of parser for resource
-- dictionaries. Currently only extracts the font dictionary.
def ResourceDictP = {
  @initDict = ResourceDict;
  ResourceDictRec initDict
}
  

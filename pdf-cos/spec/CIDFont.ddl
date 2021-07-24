-- CID fonts:
import Stdlib
import GenPdfValue
import PdfValue
import PdfDecl

import FontDesc

-- TODO: Universe C may allow an extra value here
def CIDFontType = Choose1 {
  cidFontType0 = NameToken "CIDFontType0";
  cidFontType2 = NameToken "CIDFontType2";
}

-- DW2: specifies default metrics for veritical writing
def DW2 = {
  metric0 = Token Integer;
  metric1 = Token Integer;
}

def DefaultDW2 : DW2 = {
  metric0 = 880;
  metric1 = 1000;
}

def CIDToGIDMapRec (pos : uint 64) m = Default m
  (CIDToGIDMapRec
    (inc pos)
    { @hb = UInt8 as uint 16;
      @lb = UInt8 as uint 16;
      Insert pos ((hb * 256) + lb) m
    })

def CIDMap = CIDToGIDMapRec (0 : uint 64) empty

def CIDToGIDMap = Choose {
  strm = WithReffedStreamBody CIDMap;
  identity = DirectOrRef (NameToken "Identity");
}

def PartialCIDFont = {
  type = false;
  subtype = nothing : maybe CIDFontType;
  baseFont = nothing : maybe [ uint 8 ];
  cidSysInfo = nothing : maybe [ [ uint 8 ] -> Value ];
  fontDescriptor = nothing : maybe [ [ uint 8 ] -> Value ];
  dw = nothing : maybe (uint 64);
  w = nothing : maybe [ uint 64 ];
  dw2 = nothing : maybe DW2;
  w2 = nothing : maybe [ Value ];
  cidToGidMap = nothing : maybe CIDToGIDMap;
}

def MkPartialCIDFont (ty: bool) (sty: maybe CIDFontType) (bf: maybe string)
  sysInfo (fd: maybe FontDescriptor)
  (pdw: maybe (uint 64)) (pw: maybe [ uint 64 ])
  (pdw2: maybe DW2) (pw2: maybe [ PdfValue ])
  (pCIDToGID: maybe CIDToGIDMap) : PartialCIDFont = {

  type = ty;
  subtype = sty;
  baseFont = bf;
  cidSysInfo = sysInfo;
  fontDescriptor = fd;
  dw = pdw;
  w = pw;
  dw2 = pdw2;
  w2 = pw2;
  cidToGidMap = pCIDToGID;
}

def InitCIDFont = MkPartialCIDFont
  false
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing
  nothing

def CIDAddType f = MkPartialCIDFont
  (Holds (Guard ((DirectOrRef (Token Name)) == "Font")))
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddSubtype f = MkPartialCIDFont
  f.type
  (just CIDFontType)
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddBaseFont f = MkPartialCIDFont
  f.type
  f.subtype
  (just (DirectOrRef (Token Name)))
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddSysInfo f = MkPartialCIDFont
  f.type
  f.subtype
  f.baseFont
  (just (DirectOrRef Dict)) -- TODO: possibly strengthen
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddFontDesc f = MkPartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  (just (ParseAtRef (Token Ref) Dict))
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddDW f = MkPartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  (just UNatural)
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddW f = MkPartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  (just (GenArray UNatural))
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddDW2 f = MkPartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  (just DW2)
  f.w2
  f.cidToGidMap

def CIDAddW2 f = MkPartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  (just Array) -- TODO: possibly strengthen
  f.cidToGidMap

def CIDAddCIDToGIDMap f = MkPartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
  (just CIDToGIDMap)

def ExtendCIDFont k f =
  if k == "Type" then {
    f.type is false;
    just (CIDAddType f)
  }
  else if k == "Subtype" then {
    f.subtype is nothing;
    just (CIDAddSubtype f)
  }
  else if k == "BaseFont" then {
    f.baseFont is nothing;
    just (CIDAddBaseFont f)
  }
  else if k == "CIDSystemInfo" then {
    f.cidSysInfo is nothing;
    just (CIDAddSysInfo f)
  }
  else if k == "FontDescriptor" then {
    f.fontDescriptor is nothing;
    just (CIDAddFontDesc f)
  }
  else if k == "DW" then {
    f.dw is nothing;
    just (CIDAddDW f)
  }
  else if k == "W" then {
    f.w is nothing;
    just (CIDAddW f)
  }
  else if k == "DW2" then {
    f.dw2 is nothing;
    just (CIDAddDW2 f)
  }
  else if k == "W2" then {
    f.w2 is nothing;
    just (CIDAddW2 f)
  }
  else if k == "CIDToGIDMap" then {
    f.cidToGidMap is nothing;
    just (CIDAddCIDToGIDMap f)
  }
  else nothing

def CIDFont (f : PartialCIDFont) = {
  Guard f.type; -- required
  cidSubtype = f.subtype is just; -- required
  cidBaseFont = f.baseFont is just; -- TODO: may need more here
  cidSystemInfo = f.cidSysInfo is just; -- required
  cidFontDesc = f.fontDescriptor is just; -- required

  cidDW = f.dw;
  cidW = f.w;
  cidDW2 = case f.dw2 of {
    just d -> d
  ; nothing -> DefaultDW2
  };
  cidW2 = f.w2;
  cid2GIDMap = f.cidToGidMap;
}

def CIDFontP = GenPdfDict1
  InitCIDFont
  ExtendCIDFont
  CIDFont

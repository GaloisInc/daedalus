-- CID fonts:
import Stdlib
import GenPdfValue
import PdfValue
import PdfDecl

import FontDesc

-- DW2: specifies default metrics for veritical writing
def DW2 = {
  metric0 = Token Number;
  metric1 = Token Number;
}

def DefaultDW2 : DW2 = {
  metric0 = intNumber 880;
  metric1 = intNumber 1000;
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

def PartialCIDFont (ty: bool) (sty: maybe CIDFontType) (bf: maybe FontName)
  (sysInfo : maybe [ [ uint 8 ] -> Value ]) (fd: maybe Ref)
  (pdw: maybe Number) (pw: maybe [ PdfValue ])
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

def InitCIDFont = PartialCIDFont
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

def CIDAddType f = PartialCIDFont
  (Holds (DirectOrRef (NameToken "Font")))
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddSubtype f = PartialCIDFont
  f.type
  (just (Token (GenName CIDFontType)))
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddBaseFont f = PartialCIDFont
  f.type
  f.subtype
  (just (DirectOrRef (Token (GenName FontName))))
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddSysInfo f = PartialCIDFont
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

def CIDAddFontDesc f = PartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  (just (Token Ref))
  f.dw
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddDW f = PartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  (just (DirectOrRef (Token Number)))
  f.w
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddW f = PartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  (just (DirectOrRef (GenArray Value)))
  f.dw2
  f.w2
  f.cidToGidMap

def CIDAddDW2 f = PartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  (just (DirectOrRef (Between "[" "]" DW2)))
  f.w2
  f.cidToGidMap

def CIDAddW2 f = PartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  (just (DirectOrRef Array)) -- TODO: possibly strengthen
  f.cidToGidMap

def CIDAddCIDToGIDMap f = PartialCIDFont
  f.type
  f.subtype
  f.baseFont
  f.cidSysInfo
  f.fontDescriptor
  f.dw
  f.w
  f.dw2
  f.w2
-- DBG:
--  (just CIDToGIDMap)
  (When Value nothing)

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
  cidFontDesc = ParseAtRef (f.fontDescriptor is just)
    (FontDescP (cidTypeSym cidSubtype) cidBaseFont);

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

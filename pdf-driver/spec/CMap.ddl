import Stdlib
import Map

import GenPdfValue
import PdfValue

-- CodeRange: a range of code characters
def CodeRange = {
  lowEnd = HexString;
  highEnd = HexString;
  Guard (lowEnd <= highEnd) -- TODO: required in standard?
}

def subsumesRanges r0 r1 =
  r0.lowEnd <= r1.lowEnd &&
  r1.highEnd <= r0.highEnd

-- strictlyLower r0 r1: span of range r0 is before span of range r1
def strictlyLower r0 r1 = r0.highEnd < r1.lowEnd

def disjointRanges r0 r1 = 
  (strictlyLower r0 r1) ||
  (strictlyLower r1 r0)

-- minusRanges r0 r1: subtraction of range r1 from range r0
def MinusRanges r0 r1 : [ CodeRange ] = {
  @org = ^(if disjointRanges r0 r1 then just r0 else nothing);
  @leftDiff = {
    @ld = {
      lowEnd = r1.highEnd;
      highEnd = r0.highEnd;
    };
    condJust (!(subsumesRanges r1 r0) && r1.lowEnd < r0.lowEnd) ld
  };
  @rightDiff = {
    @rd = {
      lowEnd = r0.lowEnd;
      highEnd = r1.lowEnd;
    };
    condJust (!(subsumesRanges r1 r0) && r0.highEnd < r1.highEnd) rd;
  };
  ^(concat [
      (optionToList org),
      (optionToList leftDiff),
      (optionToList rightDiff) ] )
}

def MinusRangeArray (r0s : [ CodeRange ]) (r1 : CodeRange) : [ CodeRange ] =
  for (acc = r0s; r0 in r0s) {
    @ds = map (rngDiff in acc) MinusRanges rngDiff r1;
    ^(concat ds)
  }

-- DisjointUnion m0 m1: the union of range maps m0, m1 over disjoint
-- ranges
def DisjointUnion m0 m1 = for (acc1 = m0; rng1, v1 in m1) {
  -- assert that ranges are disjoint
  for (acc0 = {}; rng0, v0 in m0) Guard (disjointRanges rng1 rng0);
  -- add subject binding to map
  Insert rng1 v1 acc1
}

-- Overwrite m0 m1: union of range maps m0 and m1, preferring bindings
-- in m1
def Overwrite m0 m1 = for (acc = m1; rng0, v0 in m0) {
  @rngs0 = for (rngDiffs0 = [ rng0 ]; rng1, v1 in m1) 
    MinusRangeArray rngDiffs0 rng1;
  @x = MapTo rngs0 v0;
  UnionMaps acc x
}

-- A CMap definition:
def CMapDefn P = {
  $$ = P;
  KW "def";
}

-- CMapScope: a definition in a scope
def CMapScope P = {
  KW "begin";
  $$ = P;
  KW "end";
}

-- CMapDict: parse a dictionary
def CMapDict Key Val = CMapDefn {
  @size = UInt8;
  KW "dict";
  KW "dup"; -- ?
  CMapDefn (
    Many (size as uint 64) (CMapDefn {
      @k = GenName Key;
      @v = Val k;
    }))
}

def CIDSysInfoKey = Choose1 {
  registry = Match "Registry";
  ordering = Match "Ordering";
  supplement = Match "Supplement";
}

def CIDSysInfoVal (k : CIDSysInfoKey) = case k of
  registry -> {| registryVal = String |}
  ordering -> {| orderingVal = String |}
  supplement -> {| supplementVal = Number |}

-- CMapKey: keys in the CMap
def CMapKey = Choose1 {
  cidSysInfo = Match "CIDSystemInfo";
  cMapMatch = Match "CMapMatch";
  cMapVersion = Match "CMapVersion";
  cMapType = Match "CMapType";
  uidOffset = Match "UIDOffset";
  xuid = Match "XUID";
  wMode = Match "WMode";
}

-- CMapVal: values in the CMap
def CMapVal (k : CMapKey) = case k of
  cidSysInfo -> {| cidSysInfoVal = CMapDict CIDSysInfoKey CIDSysInfoVal |}
  cMapMatch -> {| cMapMatchVal = Name |}
  cMapVersion -> {| cMapVersionVal = Number |}
  cMapType -> {| cMapTypeVal = Number |}
  uidOffset -> {| uidOffsetVal = Number |}
  xuid -> {| xuidVal = GenArray Number |}
  wMode -> {| wModeVal = Number |}

def CodeSpaceMap nm Rng = {
  @size = UInt8;
  Guard (size <= 100);
  Many (size as uint 64) {
    codeRange = CodeRange;
    val = Rng;
  };

  -- TODO: check that codespaces do not overlap, but only for
  -- codespace ranges
}

def CodeRange = CodeSpaceMap "codespace" Unit

def CodeNumMap nm = CodeSpaceMap nm Number

def CodeRanges = {
  codeSpace = CodeRange;
  cid = CodeNumMap;
  notDef = CodeNumMap;
  bf = CodeNumMap;
}

-- TODO: define operations for extending fields of CodeRanges
def ExtendCodeSpace cr : CodeRanges = {
  codeSpace = ...;
  cid = ^cr.cid;
  notDef = ^cr.notDef;
  bf =  ^cr.bf;
}

-- CMapTail: the remainder of a CMap
def CMapTail ds = ...

-- parser for CMap files, as specified in Adobe Technical Note #5014,
-- Adobe CMap and CIDFont Files Specification. Definition is based
-- largely on examples and Sec. 7.3 CMap File Overview
def CMapStream = {
  -- header comments must appear first
  KW (Many Comment);

  -- CIDInit procset findresource appears immedately after the header
  -- information
  Token (NameStr "CIDInit");
  Token (NameStr "ProcSet");
  KW "findresource";

  CMapScope {
    @size = Number;
    KW "dict";
    CMapScope {
      -- the CMap dictionary:
      -- TODO: generalize the scoping op
      Between "begincmap" "endcmap" {
        cMapDict0 = Many (CMapDefn {
          k = GenName CMapKey;
          v = CMapVal k;
        });

        codeSpaceRange = CodeSpaceMap "codespace" Unit;
        -- TODO: is this required?

        -- TODO: parse dictionary entries and range operators, by
        -- defining recursive parser that maintains maps
        Many (Choose1 {
          cMapDict = {
            @k = GenName CMapKey;
            @v = CMapDefn (CMapVal k);
            -- TODO: extend accumulated map
          };
          cidRange = CodeNumMap "cid";
          notDefRange = CodeNumMap "notdef";
        });
      };

      KW "CMapName";
      cMapName = Many NameChar;
      Name;
      KW "defineresource";
      KW "pop";
      -- TODO: complete definition
    };
  };
} 

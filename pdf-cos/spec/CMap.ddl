import Stdlib
import Pair
import Sum
import Map

import GenPdfValue
import PdfValue

def HexStringNum3 : uint 64 = {
  @hbs = HexString;
  bytesNum hbs
}

-- CodeRange: a range of code characters
def CodeRange = {
  lowEnd = HexStringNum3;
  highEnd = HexStringNum3;
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
      lowEnd = inc r1.highEnd;
      highEnd = r0.highEnd;
    };
    condJust (!(subsumesRanges r1 r0) && r1.lowEnd < r0.lowEnd) ld
  };
  @rightDiff = {
    @rd = {
      lowEnd = r0.lowEnd;
      highEnd = dec r1.lowEnd;
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

def RangesCover rs0 r1 = {
  @diff = for (acc = [ r1 ]; r0 in rs0) MinusRangeArray acc r0;
  Guard (diff == [ ]);
}  

-- RangeArrayCovers rs0 rs1: check that rs0 covers rs1
def RangeArrayCovers rs0 rs1 = for (acc = {}; r1 in rs1) RangesCover rs0 r1

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

-- GenCMapScope P nm: a generalized scope
def GenCMapScope P nm = {
  KW "begin";
  Match nm;
  $$ = P;
  KW "end";
  Match nm;
}

-- CMapScope: a definition in a scope
def CMapScope P = GenCMapScope P ""

def UnsignedDigit = Match1 ('0' .. '9') - '0' as uint 64
def UnsignedNatural = numBase 10 (Many (1..) UnsignedDigit)

-- CMapDict: parse a dictionary
def CMapDict Key Val = CMapDefn {
  @size = UnsignedNatural;
  KW "dict";
  KW "dup"; -- TODO: when should this be present?
  CMapScope {
    @es = Many size (CMapDefn {
      fst = GenName Key;
      snd = Val fst;
    });
    ListToMap es
  }
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
  -- TODO: replace with PDF dict
  cMapMatch -> {| cMapMatchVal = Name |}
  cMapVersion -> {| cMapVersionVal = Number |}
  cMapType -> {| cMapTypeVal = Number |}
  uidOffset -> {| uidOffsetVal = Number |}
  xuid -> {| xuidVal = GenArray Number |}
  wMode -> {| wModeVal = Number |}

-- CodeSpaceMap nm Rng: a map from code spaces to Rng
def CodeSpaceMap nm Rng = {
  @size = UInt8;
  Guard (size <= 100);
  GenCMapScope {
      @es = Many (size as uint 64) (Pair CodeRange Rng);
      ListToMap es
    }
    nm
}

-- TODO: check that codespaces do not overlap, but only for
-- codespace ranges

def CodeRanges = CodeSpaceMap "codespace" Unit

def CodeNumMap nm = CodeSpaceMap nm Number

def CidRangeOp = CodeNumMap "cid"

def NotDefRangeOp = CodeNumMap "notdef"

def BfRangeOp = CodeNumMap "bf"
-- TODO: replace with map to range of codes

-- TODO: define bfchar range op

-- CodeRangeOps: code range operations
def CodeRangeOp = Choose1 {
  cid = CidRangeOp;
  notDef = NotDefRangeOp;
  bf = BfRangeOp;
}

def CMapDictEntry = CMapDefn {
  fst = GenName CMapKey;
  snd = CMapVal fst;
}

def PreRangeOp = Choose1 {
  useCMap = {
    Token Name;
    KW "usecmap";
  }
}

-- DictAndP P: parse dictionary entries and P
def DictAnd Code Ops = {
  @xs = Lists2 CMapDictEntry (Sum Code Ops);
  des = xs.fst;
  @codesAndOps = sepLists2 xs.snd;
  codes = codesAndOps.fst;
  ops = codesAndOps.snd;
}

def CollectRangeOp dom maybeOps = {
  $$ = for (acc = empty; m in optionsToList maybeOps) Overwrite acc m;
  RangeArrayCovers dom (mapDomain $$)
}

-- parser for CMap files, as specified in Adobe Technical Note #5014,
-- Adobe CMap and CIDFont Files Specification. Definition is based
-- largely on examples and Sec. 7.3 CMap File Overview
def CMapStream = {
  -- A CMap file must begin with the comment characters %!
  Optional (GenComment "!");
  -- more comments
  Many Comment;

  -- CIDInit procset findresource appears immedately after the header
  -- information
  Token (NameStr "CIDInit");
  Token (NameStr "ProcSet");
  KW "findresource";

  CMapScope {
    @size = UnsignedNatural;
    KW "dict";
    CMapScope {
      -- the CMap dictionary:
      GenCMapScope {
          -- TODO: refactor
          @items0 = DictAnd CodeRanges PreRangeOp; -- code ranges
          @items1 = DictAnd Void CodeRangeOp; -- code range operations
          Guard ((size as uint 64) ==
                 (length items0.des) +
                 (length items0.codes) +
                 (length items0.ops) +
                 (length items1.des) +
                 (length items1.codes) +
                 (length items1.ops));

          -- cmapDict: define the cmap dictionary
          cmapDict = ListToMap (append items0.des items1.des);

          -- codeRanges: the code ranges
          codeRanges = {
            @codeRangesM = UnionMapArray items0.codes;
            mapDomain codeRangesM
          };

          -- collect CIDS:
          @rangeOps = items1.codes : [ CodeRangeOp ];
          cids = CollectRangeOp codeRanges (map (op in rangeOps) (
            case op of
              cid x2 -> just x2
              _ -> nothing
            ));

          -- collect NotDefs:
          notDefs = CollectRangeOp codeRanges (map (op in rangeOps) (
            case op of
              notDef x2 -> just x2
              _ -> nothing
            ));

          -- collect BFs:
          bfs = CollectRangeOp codeRanges (map (op in rangeOps) (
            case op of
              bf x2 -> just x2
              _ -> nothing
            ));
        }
        "cmap";

      -- a CMapName directive
      KW "CMapName";
      cMapName = Many NameChar;
      Name;
      KW "defineresource";
      KW "pop";
    };
  };

  Token (GenComment "%EndResource");
  Token (GenComment "%EOF");
} 

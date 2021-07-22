import Stdlib
import Pair
import Sum
import Map
import Array
       
-- import PDF libraries:
import GenPdfValue
import PdfValue
import Unicode

-- numEntries rng: number of entires in the code range rng
def numEntries rng = inc rng.numExtras

-- highEnd rng: highest code in the code range rng
def highEnd rng = rng.lowEnd + rng.numExtras

-- CodeRange: a range of code characters
def CodeRange CharCode = {
  lowEnd = CharCode;

  @highEnd = CharCode;
  Guard (lowEnd <= highEnd); -- PDFA: required by standard?
  numExtras = (highEnd - lowEnd) as uint 32
}

-- SingletonRange: a code range containing exactly one entry
def SingletonRange CharCode : CodeRange = {
  lowEnd = CharCode;
  numExtras = ^0;
}

-- strictlyLower r0 r1: span of range r0 is before span of range r1
def strictlyLower r0 r1 = (highEnd r0) < r1.lowEnd

def disjointRanges r0 r1 = 
  (strictlyLower r0 r1) ||
  (strictlyLower r1 r0)

-- minusRanges r0 r1: subtraction of range r1 from range r0
def MinusRanges r0 r1 : [ CodeRange ] = {
  @leftDiff = {
    @ld = {
      lowEnd = max r0.lowEnd (inc (highEnd r1));
      numExtras = r0.numExtras - (lowEnd - r0.lowEnd);
    };
    condJust ((highEnd r1) < (highEnd r0)) ld
  };

  @rightDiff = {
    @rd = {
      lowEnd = r0.lowEnd;
      numExtras = r0.numExtras -
                  ((highEnd r0) - (min (highEnd r0) r1.lowEnd));
    };
    condJust (r0.lowEnd < r1.lowEnd) rd;
  };
  
  ^(append
      (optionToList leftDiff)
      (optionToList rightDiff))
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

-- GenCMapScope nm P: a generalized scope
def GenCMapScope nm P = {
  KW (append "begin" nm);
  $$ = P;
  KW (append "end" nm);
}

-- CMapScope: a definition in a scope
def CMapScope P = GenCMapScope "" P

def UnsignedDigit = Match1 ('0' .. '9') - '0' as uint 64
def UnsignedNatural = numBase 10 (Many (1..) UnsignedDigit)

-- CMapDict: parse a dictionary
def CMapDict Key Val = CMapDefn {
  @size = UnsignedNatural;
  KW "dict";
  KW "dup"; 
  CMapScope {
    @es = Many size (CMapDefn {
      fst = GenName Key;
      snd = Val fst;
    });
    ListToMap es
  }
}

-- keys in a CIDSystemInfo map
def CIDSysInfoKey = Choose1 {
  registry = Match "Registry";
  ordering = Match "Ordering";
  supplement = Match "Supplement";
}

-- values in a CIDSystemInfo map
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
  cMapName = Match "CMapName";
  uidOffset = Match "UIDOffset";
  xuid = Match "XUID";
  wMode = Match "WMode";
}

-- CMapVal: values in the CMap
def CMapVal (k : CMapKey) = case k of
  cidSysInfo -> {| cidSysInfoVal = PdfDict CIDSysInfoKey CIDSysInfoVal |}
  cMapMatch -> {| cMapMatchVal = Name |}
  cMapVersion -> {| cMapVersionVal = Number |}
  cMapType -> {| cMapTypeVal = Number |}
  cMapName -> {| cMapNameVal = Name |}
  uidOffset -> {| uidOffsetVal = Number |}
  xuid -> {| xuidVal = GenArray Number |}
  wMode -> {| wModeVal = Number |}

def SizedOp Domain Rng nm = {
  @size = UnsignedNatural;
  Guard (size <= 100); -- upper bound of 100 imposed by standard
  GenCMapScope nm {
    @es = DictEntries Domain Rng;
    Guard (length es == (size as uint 64));
    ListToMap es
    }
}

def CodeSpaceMap CharCode Rng nm = SizedOp
  (CodeRange CharCode) Rng
  (append nm "range")

-- UnicodeSeq: a sequence of unicode characters
def UnicodeSeq cc = {
  -- single unicode char:
  (LiftPToArray UTF8) |
  -- multiple unicode chars:
  (Between "[" "]" (Many ((numEntries cc) as uint 64) (Token UTF8)))
}

-- TODO: check that codespaces do not overlap, but only for
-- codespace ranges

def CodeRanges CharCode = CodeSpaceMap CharCode (Const Unit) "codespace" 

def CodeNumMap CharCode nm = CodeSpaceMap CharCode (Const Number) nm

-- BfCharOp: define unicode of a single character
def BfCharOp CharCode = SizedOp
  (SingletonRange CharCode) (Const (LiftPToArray UTF8))
  "bfchar"

def BfRangeOp CharCode = CodeSpaceMap CharCode UnicodeSeq "bf"

def CidRangeOp CharCode = CodeNumMap CharCode "cid"

def NotDefRangeOp CharCode = CodeNumMap CharCode "notdef"

-- CodeRangeOps: code range operations
def CodeRangeOp CharCode = Choose1 {
  cid = CidRangeOp CharCode;
  notDef = NotDefRangeOp CharCode;
  bf = (BfRangeOp CharCode) |
       (BfCharOp CharCode);
}

-- FIXME: BUG: doesn't tokenize right
-- def CMapDictEntry = CMapDefn (DictEntry CMapKey CMapVal)
def CMapDictEntry : Pair = CMapDefn {
  fst = Token (GenName CMapKey);
  snd = Token (CMapVal fst);
} 

def PreRangeOp = Choose1 {
  useCMap = {
    Token Name;
    KW "usecmap";
  }
}

-- DictAnd Code Ops: parse dictionary entries, code operations Code
-- and operations Ops
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

def CMapProper CharCode = {
  @items0 = DictAnd (CodeRanges CharCode) PreRangeOp; -- code ranges
  @items1 = Lists2 CMapDictEntry (CodeRangeOp CharCode);

  -- extract data from items1:
  -- TEST
  @alldefs = append items0.des items1.fst;
  @rangeOps = items1.snd : [ CodeRangeOp ];
  
  -- code range operations
  -- NOTE
  --  - so far I'm seeing size always equal to 12!?
  --  - disabling this guard for now.
  --  - FIXME: reinstate or change or _
  -- Guard ((size as uint 64) ==
  --        (length items0.des) +
  --        (length items0.codes) +
  --        (length items0.ops) +
  --        (length items1.des) +
  --        (length items1.codes) +
  --        (length items1.ops));

  -- cmapDict: define the cmap dictionary
  cmapDict = ListToMap alldefs;

  -- codeRanges: the code ranges
  codeRanges = {
    @codeRangesM = UnionMapArray items0.codes;
    mapDomain codeRangesM
  };

  -- collect CIDS:
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
        
-- ToUnicodeCMap: follows Sec. 9.10.3. Parser for CMap's that slighly
-- differ from the ones specified in Adobe Technical Note #5014.
def ToUnicodeCMap0 CharCode = {
  Many Comment;

  Token (NameStr "CIDInit");
  Token (NameStr "ProcSet");
  KW "findresource";
  CMapScope {
    @size = Token UnsignedNatural;
    KW "dict";
    CMapScope {
      -- the CMap dictionary:
      $$ = GenCMapScope "cmap" (CMapProper CharCode);

      -- a CMapName directive
      KW "CMapName";
      @cMapName = Token(Many NameChar);  -- FIXME: need to add back this name?
      Name;
      KW "defineresource";
      KW "pop";
    };
  };
} 

-- HexByte: a single byte, represented as a hex code
def HexByte = numBase 16 (Many 2 HexDigit) as? uint 32

-- fontType: different types of fonts. Inherited from context of CMap.
def FontType = Choose {
  simpleFont = ^{};
  cidFont = ^{};
}

-- FontCode: parse a font code as an integer:
def FontCode (fontTy : FontType) = Between "<" ">" (case fontTy of
  simpleFont -> HexByte
  cidFont -> numBase 256 (Many (1..) HexByte)
)

def ToUnicodeCMap fontTy = ToUnicodeCMap0 (FontCode fontTy)

-- Main: entry point, for testing
def Main = {
  @testFontTy = {| simpleFont = ^{} |};
  ToUnicodeCMap testFontTy
}


def ToUnicodeCMap_simpleFont = ToUnicodeCMap {| simpleFont = ^{} |}
def ToUnicodeCMap_cidFont    = ToUnicodeCMap {| cidFont = ^{} |}




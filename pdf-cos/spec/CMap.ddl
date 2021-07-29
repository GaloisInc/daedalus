import Stdlib
import Pair
import Sum
import Map
import Array
       
-- import PDF libraries:
import PdfDecl -- not directly needed/wanted
-- import PdfDeclStubs  -- FIXME: TESTINGONLY
import GenPdfValue
import PdfValue
import Unicode


---- CodeRange type ------------------------------------------------------------

-- numEntries rng: number of entries in the code range rng
def numEntries rng = inc rng.numExtras

-- highEnd rng: highest code in the code range rng
def highEnd rng = rng.lowEnd + rng.numExtras

-- CodeRange: a range of code characters
def CodeRange CharCode = {
  lowEnd = CharCode as int;

  @highEnd = CharCode as int;
  Guard (lowEnd <= highEnd); -- PDFA: required by standard?
  numExtras = (highEnd - lowEnd) as int;
}

-- SingletonRange: a code range containing exactly one entry
def SingletonRange CharCode : CodeRange = {
  lowEnd = CharCode as int;
  numExtras = ^(0 : int);
}


---- Operators on CodeRange type -----------------------------------------------

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
      (optionToArray leftDiff)
      (optionToArray rightDiff))
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
  MapUnion acc x
}

def CollectRangeOp dom ops = {
  $$ = for (acc = empty; m in ops) Overwrite acc m;
  RangeArrayCovers dom (MapDomain $$)
}



---- general CMap items --------------------------------------------------------

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
    @es = Many size (PairMapEntry (DepPair (GenName Key) Val));
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
  cidSysInfo -> {| cidSysInfoVal = GenPdfDict CIDSysInfoKey CIDSysInfoVal |}
  cMapMatch -> {| cMapMatchVal = Name |}
  cMapVersion -> {| cMapVersionVal = Number |}
  cMapType -> {| cMapTypeVal = Number |}
  cMapName -> {| cMapNameVal = Name |}
  uidOffset -> {| uidOffsetVal = Number |}
  xuid -> {| xuidVal = GenArray Number |}
  wMode -> {| wModeVal = Number |}


---- CodeSpace/CodeRange functions ---------------------------------------------

-- UnicodeSeq: a sequence of unicode characters
def UnicodeSeq cc =
  Choose {
    -- single unicode char:
    singleUnicode= ParseCharacterCodes;

    -- multiple unicode chars:
    multipleUnicode=
      { @entries = numEntries cc as? uint 64;
        Between "[" "]" (Many entries (Token ParseCharacterCodes))
      }
  }

def UnicodeSeq_single = {
  @cc = ParseCharacterCodes;
  ^ {| singleUnicode= cc |} : UnicodeSeq
  }

def ParseCharacterCodes : [uint 32] =
  Token (Between "<" ">" ParseHexUTF16_BE)
  -- for simple fonts just one unicode values
  -- for CID fonts could be multiple unicode values
                
-- TODO: check that codespaces do not overlap, but only for
-- codespace ranges

def CodeRanges CharCode = {
  @size = Token(UnsignedNatural);
  Guard (size <= 100); -- upper bound of 100 imposed by standard
  GenCMapScope "codespacerange" {
    @es = Many (1..) (DepPair (CodeRange CharCode) (Const Unit));
    Guard (length es == (size as uint 64));
    ListOfPairsToMap es
   }
}

def CidRangeOp CharCode = CodeNumMap CharCode "cid"

def NotDefRangeOp CharCode = CodeNumMap CharCode "notdef"

def CodeNumMap CharCode nm = CodeSpaceMap CharCode (Const Number) nm

def CodeSpaceMap CharCode Rng nm = SizedOp
                                     (CodeRange CharCode)
                                     Rng
                                     (append nm "range")

-- CodeRangeOp: code range operations
def CodeRangeOp CharCode = Choose1 {
  cid    = CidRangeOp CharCode;
  notDef = NotDefRangeOp CharCode;
  bfrange= SizedOp (CodeRange CharCode)
                   UnicodeSeq
                  "bfrange";
  bfchar = SizedOp (SingletonRange CharCode)
                   (Const UnicodeSeq_single)
                   "bfchar";
}

def SizedOp Domain Rng nm = {
  @size = Token(UnsignedNatural);
  Guard (size <= 100); -- upper bound of 100 imposed by standard
  GenCMapScope nm {
    @es = Many (DepPair Domain Rng);
    Guard (length es == (size as uint 64));
    ListOfPairsToMap es
    }
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

-- FIXME: currently dead:
-- DictAnd Code Ops: parse dictionary entries, code operations Code
-- and operations Ops
def DictAnd Code Ops = {
  @xs = Lists2 CMapDictEntry (Sum Code Ops);
  des = xs.fst;
  @codesAndOps = sepLists2 xs.snd;
  codes = codesAndOps.fst;
  ops = codesAndOps.snd;
}

-- parse bytes for "CMapProper" but leaave processing/validating to others
def CMapProper_Raw CharCode = {
  @items0 = Lists2 CMapDictEntry (CodeRanges CharCode);
            -- NOTE: this is only call of 'CodeRanges'
            -- FIXME: add back PreRangeOp;
            
  @items1 = Lists2 CMapDictEntry (CodeRangeOp CharCode);

-- NOTE
  --  - so far I'm seeing size always equal to 12!?
  --  - disabling this guard for now.
  --  - FIXME: reinstate or change or _
  -- Guard ((size as uint 64) ==  -- 'size' now in caller
  --        (length items0.des) +
  --        (length items0.codes) +
  --        (length items0.ops) +
  --        (length items1.des) +
  --        (length items1.codes) +
  --        (length items1.ops));

  -- extract/merge data:
  defs     = append items0.fst items1.fst;
  codes    = items0.snd;
  rangeOps = items1.snd;
}

def CMapProper CharCode = {
  @raw      = CMapProper_Raw CharCode;
  @defs     = raw.defs;
  @codes    = raw.codes;
  @rangeOps = raw.rangeOps;
  
  -- cmapDict: define the cmap dictionary
  cmapDict = ListOfPairsToMap defs;
  
  -- codeRanges: the code ranges
  codeRanges = {
    @codeRangesM = UnionMapArray codes;
    MapDomain codeRangesM
  };

  -- collect CIDS:
  cids = CollectRangeOp
           codeRanges
           (optionsToArray (map (op in rangeOps)
                                (case op of
                                   cid x2 -> just x2
                                   _      -> nothing
                                )));

  -- collect NotDefs:
  notDefs = CollectRangeOp
              codeRanges
              (optionsToArray (map (op in rangeOps)
                                   (case op of
                                      notDef x2 -> just x2
                                      _         -> nothing
                                   )));
                                   
  -- collect BFs:
  @ranges_ = optionsToArray (map (op in rangeOps)
                                 (case op of
                                    bfchar x2  -> just x2
                                    bfrange x2 -> just x2
                                    _          -> nothing
                                 ));
  bfs = CollectRangeOp codeRanges ranges_;
  bfsMap = CodeRangeMapToCharCodeMap bfs;
}

-- ToUnicodeCMap: follows Sec. 9.10.3. Parser for CMap's that slightly
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
      dict = GenCMapScope "cmap" (CMapProper CharCode);

      -- a CMapName directive
      KW "CMapName";
      cMapName = Token(Many NameChar);
      Name;
      KW "defineresource";
      KW "pop";
    };
  };
} 

-- The 1 byte / n byte formats for char codes in a cmap:

-- HexByte: a single byte, represented as a hex code
def HexByte = numBase 16 (Many 2 HexDigit) as int

def FontCode_SF  = Between "<" ">" HexByte

def FontCode_CID = Between "<" ">" (numBase (256 : int) (Many (1..) HexByte))

-- fontType: different types of fonts. Inherited from context of CMap.
def FontType = Choose {
  simpleFont = ^{};
  cidFont = ^{};
}

def SimpleFontType : FontType = {| simpleFont = {} |}

-- FontCode: parse a font code as an integer:
def FontCode (fontTy : FontType) =
  case fontTy of
    simpleFont -> FontCode_SF
    cidFont    -> FontCode_CID

def ToUnicodeCMap fontTy = ToUnicodeCMap0 (FontCode fontTy)

def CMapRef (ft : FontType) : ToUnicodeCMap0 = WithReffedStreamBody
  (ToUnicodeCMap SimpleFontType)


-- functions for using cmaps --------------------------------------------------

def MapLookupCodepoint (index : int) cmap : [uint 32] = {
  InCMapCodeRange index cmap;
  @ccmap = CodeRangeMapToCharCodeMap cmap.bfs;
  Lookup index ccmap;
}

def InCMapCodeRange (index : int) cmap : bool = {
  ^true
  -- FIXME: TODO
  -- Guard(charindex >= cmap.codeRanges 
}

def CodeRangeMapToCharCodeMap (crmap : [ CodeRange -> UnicodeSeq ] )  : [ int -> [uint 32] ] =
  for (ccmap = empty; k, unicode in crmap)
    for (ccmap2 = ccmap ; j in rangeUp 0 (k.numExtras + 1))
      { @key = k.lowEnd + j;
        @val = case unicode : UnicodeSeq of {
                  singleUnicode cs -> 
                     { ^ map (c in cs) (c + (j as! uint 32))
                     };
                     -- FIXME: if (length cs > 1 && k.numExtras > 0)
                     --        then the above map is going to be weird/wrong!
                  multipleUnicode css ->
                     { Guard (length css == (k.numExtras as! uint 64));
                         -- shouldn't fail, see @entries above
                       Index css (j as! uint 64);
                     };
                  };
        Insert key val ccmap2
      }

-- Special Entry Points --------------------------------------------------------
-- NOTE: these parsers called in dom/.../Main.hs: and can be invoked from the
-- pdf-dom command-line

def ToUnicodeCMap_simpleFont = ToUnicodeCMap {| simpleFont = ^{} |}
def ToUnicodeCMap_cidFont    = ToUnicodeCMap {| cidFont = ^{} |}






import Daedalus
import PdfDecl
import PdfValue


-- Maybe this should be an ENTRY also...
def PdfStart =
  block
    Match "%PDF-"

    First
      block Match "1."; @$['0' .. '7']
      @Match "2.0"

    SkipTillEOL
    case Optional BinaryMarker of
      just    -> true         -- a binary file is indicated
      nothing -> false

def BinaryMarker =
  block
    Match "%"
    Many (4..) $[ 128 .. ]
    SkipTillEOL

-- ENTRY
def PdfEnd =
  block
    Many $simpleWS; Match "startxref";        WhiteTillEOL
    Many $simpleWS; $$ = Natural as? uint 64; WhiteTillEOL
    Many $simpleWS; Match "%%EOF"

--------------------------------------------------------------------------------
-- xref section and trailer

-- ENTRY
def CrossRef =
  First
    oldXref = CrossRefAndTrailer
    newXref = XRefObj

def CrossRefAndTrailer =
  block
    xref    = CrossRefSection
    Many JustWhite   -- no comments, but arbitrary whitespace allowed here.
    KW "trailer"
    trailer = TrailerDict Dict


-- NOTE 7.5.4 of ISO_32000-2_2020:
--  PDF comments shall not be included in a cross-reference table
--  between the keywords xref and trailer.

def CrossRefSection =
  block
    ManyWS
    Match "xref"; WhiteTillEOL
    Many (1..) CrossRefSubSection

def CrossRefSubSection =
  block
    firstId = Natural; $space
    let num = Natural as? uint 64; WhiteTillEOL
    entries = Many num CrossRefEntry

def CrossRefEntry =
  block
    let num = NatN 10; $space
    let gen = NatN 5;  $space
    $$ = First
           inUse = UsedEntry num gen
           free  = FreeEntry num gen

    First
      { $cr;    $lf }
      { $space; $cr }
      { $space; $lf }
      $cr               -- the last 2 are not standar complaint
      $lf               -- but some tools allow them

def UsedEntry (num : int) (gen : int) =
  block
    Match1 'n'
    offset = num
    gen    = gen

def FreeEntry (num : int) (gen : int) =
  block
    Match1 'f'
    obj = num
    gen = gen

def NatN n = numBase 10 (Many n Digit)





--------------------------------------------------------------------------------
-- Cross-reference streams (section 7.5.8)
-- Introduced in PDF 1.5

def XRefObj =
  block
    let str = TopDecl.obj is stream;
    WithStream (str.body is ok) (XRefObjTable (XRefMeta str.header))

def XRefMeta header =
  block
    CheckType "XRef" header
    index    = XRefIndex header
    widths   = XRefFormat header
    header   = header

def XRefIndex header =
  block
    let size = LookupNat  "Size" header
    let arr  = Default [0,size] (LookupNats "Index" header)
    map (i in rangeUp 0 (length arr) 2)
      block
        firstId = Index arr i
        num     = Index arr (i+1) as? uint 64

def XRefFormat header =
  block
    let vs = (LookupResolve "W" header) is array;
    b1     = LookupInt vs 0 as? uint 64;
    b2     = LookupInt vs 1 as? uint 64;
    b3     = LookupInt vs 2 as? uint 64;
    let bigwidth = for (s = 0; x in vs) (s + NatValue x)
    width  = bigwidth as? uint 64;

def LookupInt arr i =
  if i < length arr then NatValue (Index arr i)
                    else 0




--------------------------------------------------------------------------------
-- Contents of object stream

def XRefObjTable (meta : XRefMeta) =
  block
    xref = map (idx in meta.index)
             block
               firstId = idx.firstId
               entries = Many idx.num (XRefObjEntry meta.widths)

    trailer = TrailerDict meta.header

-- Section 7.5.8.3

def XRefFieldWithDefault x n =
  case n of
    0 -> x
    _ -> BEBytes n

def XRefFieldRequired n =
  block
    Guard (n != 0)
    BEBytes n

def BEBytes n =
  block
    let bs = Many n UInt8;
    for (v = 0; b in bs) (v * 256 + (b as int))


def XRefObjEntry (w : XRefFormat) =
  Chunk w.width
    case XRefFieldWithDefault 1 w.b1 of
      0 -> {| free       = XRefFree w |}
      1 -> {| inUse      = XRefOffset w |}
      2 -> {| compressed = XRefCompressed w |}
      _ -> {| null |}

{- NOTE: Table 18 in Section 7.5.8.3 has some issues:

  * Type 0, Field 1 is given a default value.
    This contradicts Table 17, which specifies that if the type of an entry
    is missing it shall be 1

  * Type 1, Field 2 is given a default value.
    This contradicts Table 17, which specifies that the 2nd entry
    should never be 0 (i.e., can't have a default value.

  * Type 2, Field 3 does not have a default value.
    However, some "valid" PDF files seem to assume that the field
    may be ommitted and will have the value 0.

-}


def XRefFree (w : XRefFormat) =
  block
    obj = XRefFieldRequired w.b2
    gen = XRefFieldWithDefault 0 w.b3

def XRefOffset (w : XRefFormat) =
  block
    offset = XRefFieldRequired w.b2
    gen    = XRefFieldWithDefault 0 w.b3

def XRefCompressed (w : XRefFormat) =
  block
    container_obj = XRefFieldRequired w.b2   -- generation is implicitly 0
    obj_index     = XRefFieldWithDefault 0 w.b3



--------------------------------------------------------------------------------
-- Trailers

def TrailerDict (dict : [ [uint 8] -> Value] ) =
  block
    size    = LookupNatDirect "Size" dict

    root    = case lookup "Root" dict of
                just x -> just (x is ref)
                nothing -> nothing

    prev    = Optional (LookupNatDirect "Prev" dict)

    xrefstm = Optional (LookupNat "XRefStm" dict)

    encrypt = case lookup "Encrypt" dict of
                just d  -> just (TrailerDictEncrypt dict d)
                nothing -> nothing

    all     = dict


def TrailerDictEncrypt (trailer : Dict) (d : Value) =
  block
    d = d
    eref = d is ref
    let x = TrailerIds trailer
    id0 = x.id0
    id1 = x.id1

-- Sec 7.5.5 File Trailer, Table 15, ID may be optional
def TrailerIds (trailer : Dict) =
  case lookup "ID" trailer of
    nothing ->
      block
        id0 = ""
        id1 = ""

    just v ->
      block
        let arr = v is array
        (length arr == 2) is true
        id0 = Index arr 0 is string
        id1 = Index arr 1 is string




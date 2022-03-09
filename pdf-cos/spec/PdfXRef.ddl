import PdfDecl
import PdfValue
import Stdlib

def CrossRef = Choose {
  oldXref = CrossRefAndTrailer;
  newXref = XRefObj;
}

def PdfEnd =
  block
    Match "startxref"
    EOL
    $$ = Natural as? uint 64
    EOL
    Match "%%EOF"

--------------------------------------------------------------------------------
-- xref section and trailer
-- "Old style"? not really, not deprecated.
--   However, an alternative approach via "cross reference streams" was added in PDF 1.5.

def CrossRefAndTrailer = {
  xref    = CrossRefSection;
  Many JustWhite;   -- no comments, but arbitrary whitespace allowed here.
  KW "trailer";

  trailer = TrailerDict Dict;
}

-- TrailerEnd should follow the CrossRefAndTrailer.
-- This must be at the end of every [incremental] "body".
-- Unclear from spec whether comments are allowed (we'll start by not allowing)
-- We capture the offsets for the sake of reporting and determining 'cavities'

def TrailerEnd = {
  offset0 = Offset;
  Many AnyWS;
  offset1 = Offset;
  Match "startxref"; Many $simpleWS; EOL;
  offset2 = Offset;
  xrefStart = Natural as? uint 64; Many $simpleWS; EOL;
    -- spec doesn't constrain the integer, but ...
  offset3 = Offset;
  Match "%%EOF"; Many $simpleWS;
  offset4 = Offset;
}

-- NOTE 7.5.4 of ISO_32000-2_2020:
--  PDF comments shall not be included in a cross-reference table
--  between the keywords xref and trailer.

def CrossRefSection = {
  ManyWS;
  Match "xref"; Many $simpleWS; EOL;

    -- we enforce EOL above (rejecting some NCBUR files).  The spec:
    --   7.5.4
    --   "Each cross-reference section shall begin with a line containing the keyword xref. Following
    --   this line shall be one or more cross-reference subsections"

  @x  = CrossRefSubSection;
  @xs = Many CrossRefSubSection;
  ^ concat [[x],xs];  -- this greatly improves error messages when errors in 'x'
}

def CrossRefSubSection = {
  firstId = Natural; $space;
  @num    = Natural as? uint 64; Many $simpleWS; EOL;
  entries = Many num CrossRefEntry;
}

def CrossRefEntry = {
  @num = NatN 10; $space;
  @gen = NatN 5;  $space;
  $$   = Choose1 {
           inUse = UsedEntry num gen;
           free  = FreeEntry num gen;
        };

   { $simpleWS;  $cr <| $lf }
    -- standard compliant:

    -- | { $cr; $lf }

    -- Extending the above to allow *commonly allowed* (qpdf, mutool) exuberances:
    -- (which allows the CrossRefEntry to possibly only have 19 bytes):

    <| { $cr <| $lf ; Choose1{ $cr, $lf, ^0 }}
}

def UsedEntry (num : int) (gen : int) = {
  Match1 'n'; offset = ^num; gen = ^gen;
}

def FreeEntry (num : int) (gen : int) = {
  Match1 'f'; obj = ^num; gen = ^gen;
}





--------------------------------------------------------------------------------
-- Cross-reference streams (section 7.5.8)
--  "beginning with PDF 1.5"
--  Peter Wyatt: "not preferable"

def XRefObj = {
  @str  = TopDecl.obj is stream;
  WithStream (str.body is ok) (XRefObjTable (XRefMeta str.header));
}

def XRefMeta header = {
  CheckType "XRef" header;
  index    = XRefIndex header;
  widths   = XRefFormat header;
  header   = ^ header;
}

def XRefFormat header = {
  @kv    = LookupResolve "W" header;
  @vs    = kv is array;
  b1     = LookupInt vs 0 as? uint 64;
  b2     = LookupInt vs 1 as? uint 64;
  b3     = LookupInt vs 2 as? uint 64;
  @bigwidth = for (s = 0; x in vs) { s + NatValue x };
  width  = bigwidth as? uint 64;
}

def LookupInt arr i =
  if i < length arr then NatValue (Index arr i)
                    else 0


def XRefIndex header = {
  @size = LookupNat  "Size" header;
  @arr  = Default [0,size] (LookupNats "Index" header);
  map (i in rangeUp 0 (length arr) 2) {
    firstId = Index arr i;
    num     = Index arr (i+1) as? uint 64;
  }
}



--------------------------------------------------------------------------------
-- Contents of object stream

def XRefObjTable (meta : XRefMeta) = {
  xref = map (idx in meta.index) {
           firstId = ^ idx.firstId;
           entries = Many idx.num (XRefObjEntry meta.widths)
         };
  trailer = TrailerDict meta.header;
}

-- Section 7.5.8.3
def XRefObjEntry (w : XRefFormat) = Chunk w.width {
  @ftype = XRefFieldWithDefault 1 w.b1;
  Choose1 {
    free       = { Guard (ftype == 0); XRefFree w };
    inUse      = { Guard (ftype == 1); XRefOffset w };
    compressed = { Guard (ftype == 2); XRefCompressed w };
    null       = { Guard (ftype > 2); }
  }
}

def XRefFieldWithDefault x n = { Guard (n == 0); ^ x } <| BEBytes n
def XRefFieldRequired n      = { Guard (n != 0); BEBytes n }

def XRefFree (w : XRefFormat) = {
  obj = XRefFieldRequired w.b2;
  gen = XRefFieldWithDefault 0 w.b3;
}

{- NOTE: For offset spec has default of 0, but also says that this field
should always be present.  Seems like a bug in the spec, and having
a default offset doesn't really make sense. -}
def XRefOffset (w : XRefFormat) = {
  offset = XRefFieldRequired w.b2;
  gen    = XRefFieldWithDefault 0 w.b3;
}

def XRefCompressed (w : XRefFormat) = {
  container_obj = XRefFieldRequired w.b2;   -- generation is implicitly 0
  obj_index     = XRefFieldRequired w.b3;
}



--------------------------------------------------------------------------------
-- Trailers

def TrailerDict (dict : [ [uint 8] -> Value] ) =
  block
    size    = LookupNatDirect "Size" dict

    root    = case Optional (Lookup "Root" dict) of
                just x -> just (x is ref)
                nothing -> nothing

    prev    = Optional (LookupNatDirect "Prev" dict)

    xrefstm = Optional (LookupNat "XRefStm" dict)

    encrypt = case Optional (Lookup "Encrypt" dict) of
                just d  -> just (TrailerDictEncrypt dict d)
                nothing -> nothing

    all     = dict

def TrailerDictEncrypt (trailer : [ [uint 8] -> Value ]) (d : Value) =
  block
    d = d
    eref = d is ref
    let arr = (Lookup "ID" trailer) is array
    (length arr == 2) is true
    id0 = Index arr 0 is string
    id1 = Index arr 1 is string

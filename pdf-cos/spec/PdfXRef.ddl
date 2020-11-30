import PdfDecl
import PdfValue


def CrossRef = Choose {
  oldXref = CrossRefAndTrailer;
  newXref = XRefObj;
}

--------------------------------------------------------------------------------
-- "Old style" xref section and trailer

def CrossRefAndTrailer = {
  xref    = CrossRefSection;
  KW "trailer";
  @t = Dict;
  trailer = TrailerDict t;
}

def CrossRefSection = {
  KW "xref";
  Many (1..) CrossRefSubSection;
}

def CrossRefSubSection = {
  firstId = Token Natural;
  @num    = Token Natural;
  entries = Many num CrossRefEntry;
}

def CrossRefEntry = {
  @num = NatN 10; $simpleWS;
  @gen = NatN 5;  $simpleWS;
  $$   = Choose {
           inUse = UsedEntry num gen;
           free  = FreeEntry num gen;
        };
  { $simpleWS; $cr | $lf } | { $cr; $lf };
}

def UsedEntry (num : int) (gen : int) = {
  Match1 'n'; offset = ^num; gen = ^gen;
}

def FreeEntry (num : int) (gen : int) = {
  Match1 'f'; obj = ^num; gen = ^gen;
}





--------------------------------------------------------------------------------
-- "New style" xref object

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
  b1     = LookupInt vs 0;
  b2     = LookupInt vs 1;
  b3     = LookupInt vs 2;
  witdth = for (s = 0; x in vs) {
             @n = NatValue x;
             ^ s + n
           };
}

def LookupInt arr i = Default 0 {
  @n = Index arr i;
  commit;
  NatValue n;
}


def XRefIndex header = {
  @size = LookupNat  "Size" header;
  @arr  = Default [0,size] (LookupNats "Index" header);
  map (i in rangeUp 0 (length arr) 2) {
    firstId = Index arr i;
    num     = Index arr (i+1);
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
def XRefObjEntry (w : XRefFormat) = Chunk w.witdth {
  @ftype = XRefFieldWithDefault 1 w.b1;
  Choose {
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

def TrailerDict (dict : [ [uint 8] -> Value] ) = {
  size    = LookupNat "Size" dict;
  root    = Default nothing { -- allowed to be nothing for linearlized PDF
                @x = Lookup "Root" dict;
                commit;
                just (x is ref);
              };
  prev    = Optional (LookupNat "Prev" dict);
  encrypt = Optional { 
    d = Lookup "Encrypt" dict; 
    commit; 
    eref = d is ref; 
    @i = Lookup "ID" dict; 
    length (i is array) == 2;  
    id0 = Index (i is array) 0 is string; 
    id1 = Index (i is array) 1 is string; 
  }; 
  all = ^ dict;
}


import PdfValue

def TopDecl = {
  id   = Token Natural;
  gen  = Token Natural;
  KW "obj";
  @val = Value;
  obj  = TopDeclDef val;
  Match "endobj";
}

def TopDeclDef (val : Value) = Choose1 {
  stream = Stream val;
  value  = ^ val
}

def Stream (val : Value) = {
  header = val is dict;
  Match "stream";
  commit;
  SimpleEOL;
  body = StreamBody header;
  KW "endstream"
}

--------------------------------------------------------------------------------
-- Object Streams (pdf 1.4, S3.4.6)

-- Parser for the body of an object stream.  Note that we can't really
-- use a map here, as XRef streams index into the resulting array to
-- lookup refs (could also ignore that part of the xref entry and just
-- lookup the ref)

def ObjectStreamEntry (oid : Nat) = {
  oid = ^ oid;
  val = Value; -- FIXME: we should check this isn't a ref etc?  (c.f. pdf 1.7, pg 101)
}

def ObjStreamMeta first = {
  oid     = Token Natural;
  off     = (Token Natural as uint 64) + first;
}

def ObjectStream (n : uint 64) (first : uint 64) = {
  @meta = Many n (ObjStreamMeta first);
  map (entry in meta) {
    @here = Offset;
    Guard (here <= entry.off);
    SkipBytes (entry.off - here);
    ObjectStreamEntry entry.oid;
  };
}

def ObjectStreamNth (n : uint 64) (first : Nat) (idx : Nat) = {
  -- FIXME: only really need to parse up to idx
  @meta  = Many n (ObjStreamMeta first);
  @entry = Index meta idx;
  @here  = Offset;
  Guard (here <= entry.off);
  SkipBytes (entry.off - here);
  ObjectStreamEntry entry.oid;
}

def SkipBytes n = Chunk n {}

--------------------------------------------------------------------------------
-- Resolving of Refernece

-- Returns 'nothing' if there is no entry for this declaration.
-- For values this means we should return 'null'.
def ResolveRef (r : Ref) : maybe TopDecl

def CheckExpected (r : ref) (d : TopDecl) = {
  Guard (d.id  == r.obj && d.gen == r.gen);
  ^ d.obj;
}

def ResolveStream (v : Value) = {
  @r  = v is ref;
  CheckExpected r (ResolveRef r is just) is stream;
}

def ResolveValRef (r : Ref) : Value = {
  @mb = ResolveRef r;
    { mb is nothing; ^ nullValue }
  | CheckExpected r (mb is just) is value;
}

def ResolveObjectStream (v : Value) : [ ObjectStreamEntry ] = {
  @stm = ResolveStream v;
  CheckType "ObjStm" stm.header;
  @n       = LookupSize "N" stm.header;
  @first   = LookupSize "First" stm.header;
  WithStream (stm.body is ok) (ObjectStream n first);
}

def ResolveObjectStreamEntry
      (oid : Nat) (gen : Nat) (idx : uint 64) : TopDecl = {
  @stm = ResolveStream {| ref = { obj = oid; gen = gen } |};
  CheckType "ObjStm" stm.header;
  @n       = LookupSize "N"     stm.header;
  @first   = LookupSize "First" stm.header;
  @s       = stm.body is ok;
  @entry   = WithStream s (ObjectStreamNth n first idx);
  ^ { id = entry.oid; gen = 0; obj = {| value = entry.val |} };
}

def ResolveVal (v : Value) = Default v {
  @r = v is ref;
  commit;
  ResolveValRef r;
}

def LookupResolve k header = {
  @v = Lookup k header;
  ResolveVal v;
}


--------------------------------------------------------------------------------
-- Section 7.3.8.1


def StreamBody header = Token {
  @len   = StreamLen header;
  Chunk len {
    @body = GetStream;
    ApplyFilters header body;
  };
}

def StreamLen header = {
  @lenV = LookupResolve "Length" header;
  @lenI = lenV is number;
  NumberAsNat lenI as uint 64;
}

-- Section 7.3.8.2
def ApplyFilters header initialBody = {
  @decrypt = Decrypt initialBody; -- A no-op if crypto is disabled
  @filter_names  = LookOptArray "Filter" header;
  @filter_params = LookOptArray "DecodeParms" header;
  for (bytes = {| ok = decrypt |}; ix, name in filter_names) {
    @param  = Default nullValue (Index filter_params ix);
    @filter = Filter name param;
    Default bytes { @bs = bytes is ok; commit; ApplyFilter filter bs; };
  };
}


def Filter (name : Value) (param : Value) = {
  name  = name is name;
  param = FilterParam param;
}

def FilterParam (param : Value) =
    { param      is null; ^ nothing }
  | { @x = param is dict; ^ just x }

-- Stub for the Decrypt primitive 
def Decrypt (body : stream) 
            : stream 

def ApplyFilter (f : Filter) (body : stream) = Choose1 {

  ok = {
    Guard (f.name == "FlateDecode");
    @params = FlateDecodeParams f.param;
    -- For now, we only have these settings implemented.
    Guard (params.predictor == 1 || params.predictor == 12);
    Guard (params.colors == 1);
    Guard (params.bpc == 8);
    commit;
    FlateDecode params.predictor
                params.colors
                params.bpc
                params.columns
                body;
  };

  ok = {
    Guard (f.name == "LZWDecode"); 
    @params = LZWDecodeParams f.param;
    Guard (params.predictor == 1 || params.predictor == 12);
    Guard (params.colors == 1);
    Guard (params.bpc == 8);
    commit;
    LZWDecode params.predictor
              params.colors
              params.bpc
              params.columns
              params.earlychange
              body;
  }; 

  ok = {
    Guard (f.name == "ASCIIHexDecode");
    commit;
    ASCIIHexDecode body;
  }; 

  ok = {
    Guard (f.name == "ASCII85Decode");
    commit;
    ASCII85Decode body;
  }; 

  -- | ... others ...
  unsupported = { f.param is nothing; ^ f.name }
             <| ^ concat [ f.name, " (with params)" ]
}

-- XXX: some more checking (e.g., predictor 1 does not support the other ps)
def FlateDecodeParams (params : maybe [ [uint 8] -> Value ]) =
  { params is nothing;
    ^ fdDefaults;
  }
<|
  { @ps       = params is just;
    predictor = Default fdDefaults.predictor (LookupNat "Predictor" ps);
    colors    = Default fdDefaults.colors    (LookupNat "Colors" ps);
    bpc       = Default fdDefaults.bpc       (LookupNat "BitsPerComponent" ps);
    columns   = Default fdDefaults.columns   (LookupNat "Columns" ps);
  }

def fdDefaults = {
  predictor = 1 : int;
  colors    = 1 : int;
  bpc       = 8 : int;
  columns   = 1 : int;
}

def FlateDecode (predictor : int)
                (colors    : int)
                (bpc       : int)
                (columns   : int)
                (body      : stream)
              : stream

-- XXX: Lots of duplication between LZWDecode and FlateDecode 
def LZWDecodeParams (params : maybe [ [uint 8] -> Value ]) =
  { params is nothing;
    ^ lzwDefaults;
  }
  <|
  { @ps         = params is just;
    predictor   = Default lzwDefaults.predictor     (LookupNat "Predictor" ps);
    colors      = Default lzwDefaults.colors        (LookupNat "Colors" ps);
    bpc         = Default lzwDefaults.bpc           (LookupNat "BitsPerComponent" ps);
    columns     = Default lzwDefaults.columns       (LookupNat "Columns" ps);
    earlychange = Default lzwDefaults.earlychange   (LookupNat "EarlyChange" ps);
  }

def lzwDefaults = {
  predictor   = 1 : int; 
  colors      = 1 : int; 
  bpc         = 8 : int; 
  columns     = 1 : int; 
  earlychange = 1 : int; 
}

def LZWDecode (predictor   : int)
              (colors      : int)
              (bpc         : int)
              (columns     : int)
              (earlychange : int)
              (body        : stream)
              : stream


def ASCIIHexDecode (body : stream)
                   : stream

def ASCII85Decode (body : stream)
                  : stream


--------------------------------------------------------------------------------
def LookOptArray (key : [uint 8]) header =
  Default [] { @x = LookupResolve key header; OneOrArray x }

def OneOrArray (v : Value) = Default [v] (v is array)

def Default x P = P <| ^ x

def Chunk n P = {
  @cur  = GetStream;
  @this = Take n cur;
  @next = Drop n cur;
  SetStream this;
  $$ = P;
  SetStream next;
}


--------------------------------------------------------------------------------
-- Helpers


def CheckType x h = Guard ((LookupResolve "Type" h is name) == x)


def BEBytes n =
            { @bs = Many n UInt8;
              ^ for (v = 0; b in bs) (v * 256 + (b as int))
            }


def NatN n = { @ds = Many n Digit; ^ numBase 10 ds }

def LookupNat k m =
  { @vV = LookupResolve k m : Value;
    -- XXX: we should have a "strong" commit here.
    @v  = vV is number;
    NumberAsNat v; 
  }

def LookupSize k m = LookupNat k m as uint 64

def LookupNats k m = {
  @kV = LookupResolve k m : Value;
  @vs = kV is array;
  map (v in vs) {
    @v1 = ResolveVal v;
    @rV = v1 is number;
    NumberAsNat rV;
  }
}

def LookupRef k m =
  { @vV = Lookup k m : Value;
    vV is ref;
  }

def LookupName k m = {
  @vV = LookupResolve k m : Value;
  vV is name;
}

def WithStream s P = {
  @cur = GetStream;
  SetStream s;
  $$ = P;
  SetStream cur;
}

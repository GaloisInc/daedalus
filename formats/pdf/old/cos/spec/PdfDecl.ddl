import Stdlib
import PdfValue
import GenPdfValue
import JpegBasics
import Debug

def TopDecl = {
  ManyWS;          -- FIXME: would rather do in Haskell and provide warning when this occurs!
                   -- we cannot rely on the post token whitespace consumption because TopDecl is
                   -- called immediately upon "jumping" to a byte offset.
  id   = Token Natural;
  gen  = Token Natural;
  (((0xD5 : uint 8) .&. 0x0F) as! uint 4) == 5 is true;
  KW "obj";
  @val = Value;
  obj  = TopDeclDef val;
  Match "endobj";
}

def TopDeclDef (val : Value) = First
  stream = Stream val
  value  = ^ val


def ObjStream (s : Stream) = block
  let h = s.header
  Guard ("ObjStm" == LookupName "Type" h)

  let body = s.body is ok
  let n = LookupNat "N" h as? uint 64
  let first = LookupNat "First" h as? uint 64

  -- XXX: Support Extends
  index = Nested body (Many n ObjStmMeta)
  bytes = Drop first body

def ObjStmMeta = block
  oid = Token Natural
  off = Token Natural

def Stream (val : Value) =
  block
    header = val is dict

    Match "stream"
    SimpleEOL
    body = StreamBody header
    KW "endstream"

--------------------------------------------------------------------------------
-- Object Streams (pdf 1.4, S3.4.6)

-- Parser for the body of an object stream.  Note that we can't really
-- use a map here, as XRef streams index into the resulting array to
-- lookup refs (could also ignore that part of the xref entry and just
-- lookup the ref)

def ObjectStreamEntry (oid : int) = {
  oid = ^ oid;
  val = Value; -- FIXME: we should check this isn't a ref etc?  (c.f. pdf 1.7, pg 101)
}

def ObjStreamMeta first = {
  oid     = Token Natural;
  off     = (Token Natural + (first as int)) as? uint 64
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

def ObjectStreamNth (n : uint 64) (first : uint 64) (idx : uint 64) = {
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
-- Resolving references to streams

-- NOTE: warning: this grammar is actually never called, but defined here to declare the type associated
def ObjStart (s : stream) = First
  inInput = s
  inObjStream = s


-- WrapGetStream: local wrapper to GetStream, used for primitive
def WrapGetStream : ObjStart = {|
  inInput = GetStream
|}

-- TODO: ugly near-clone of ObjectStreamNth, refactor. Used to
-- implement InputAtRef primitive.
def ObjectStreamStrm (n : uint 64) (first : uint 64) (idx : uint 64) : stream = {
  -- FIXME: only really need to parse up to idx
  @meta  = Many n (ObjStreamMeta first);
  @entry = Index meta idx;
  @here  = Offset;
  Guard (here <= entry.off);
  SkipBytes (entry.off - here);
  GetStream
}

-- TODO: ugly near clone of ResolveObjectStreamEntry. Used to
-- implement InputAtRef primitive.
def ResolveObjectStreamPoint
      (oid : int) (gen : int) (idx : uint 64) : ObjStart = {
  @stm = ResolveStream {| ref = { obj = oid; gen = gen } |};
  CheckType "ObjStm" stm.header;
  @n       = LookupSize "N"     stm.header;
  @first   = LookupSize "First" stm.header;
  @s       = stm.body is ok;
  {| inObjStream = WithStream s (ObjectStreamStrm n first idx)
  |}
}

def ObjectStreamValues : [Value] = {
  Many Value
}

-- InputStream r: the input stream at reference r
def InputAtRef (r : Ref) : maybe ObjStart

-- ParseAtRef P r: parse the input at r, using P
def ParseAtRef r P = case (InputAtRef r) is just of {
  inInput s -> WithStream s (GenObj P)
; inObjStream s -> WithStream s P
}

-- DirectOrRef P: parse either the current input or parse a ref and
-- parse the input that it references.
def DirectOrRef P = case OrRef P of {
  direct x -> x
; pref r -> ParseAtRef r P  
}

def WithReffedStreamBody P = WithStream
  ((ResolveStreamRef (Token Ref)).body is ok)
  P

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Resolving of Refernece

-- Returns 'nothing' if there is no entry for this declaration.
-- For values this means we should return 'null'.
def ResolveRef (r : Ref) : maybe TopDecl


def ResolveDeclRef (r : Ref) : TopDeclDef =
  case ResolveRef r of
    nothing -> {| value = nullValue |}
    just d  -> CheckExpected r d

def ResolveStreamRef (r : Ref) = ResolveDeclRef r is stream
def ResolveValRef    (r : Ref) = ResolveDeclRef r is value

def ResolveStream (v : Value) = ResolveStreamRef (v is ref)
def ResolveVal (v : Value) =
  case v of
    ref r -> ResolveValRef r
    _     -> v

def CheckExpected (r : Ref) (d : TopDecl) : TopDeclDef =
  block
    GuardMsg ((d.id  == r.obj && d.gen == r.gen))
      "objid and gen don't match between xref table and the object definition";
    d.obj
--------------------------------------------------------------------------------


def ResolveObjectStream (v : Value) : [ ObjectStreamEntry ] = {
  @stm = ResolveStream v;
  CheckType "ObjStm" stm.header;
  @n       = LookupSize "N" stm.header;
  @first   = LookupSize "First" stm.header;
  WithStream (stm.body is ok) (ObjectStream n first);
}

def ResolveObjectStreamEntry
      (oid : int) (gen : int) (idx : uint 64) : TopDecl = {
  @stm = ResolveStream {| ref = { obj = oid; gen = gen } |};
  CheckType "ObjStm" stm.header;
  @n       = LookupSize "N"     stm.header;
  @first   = LookupSize "First" stm.header;
  @s       = stm.body is ok;
  @entry   = WithStream s (ObjectStreamNth n first idx);
  ^ { id = entry.oid; gen = 0; obj = {| value = entry.val |} };
}


def LookupResolve k header = {
  @v = Lookup k header;
  ResolveVal v;
}


--------------------------------------------------------------------------------
-- Section 7.3.8.1


def StreamBody header = Token
  (Chunk (StreamLen header) (ApplyFilters header GetStream))

def StreamLen header =
  block
    let lenV = LookupResolve "Length" header
    let lenI = lenV is number
    NumberAsNat lenI as? uint 64

-- Section 7.3.8.2
def ApplyFilters header initialBody : ApplyFilter =
  block
    let decrypt = Decrypt initialBody -- A no-op if crypto is disabled
    let filter_names  = LookOptArray "Filter" header
    let filter_params = LookOptArray "DecodeParms" header
    for (bytes = {| ok = decrypt |}; ix, name in filter_names)
      block
        let param  = Default nullValue (Index filter_params ix)
        let filter = Filter name param
        case bytes of
          ok bs -> TryApplyFilter filter bs
          _     -> bytes


def Filter (name : Value) (param : Value) =
  block
    name  = name is name
    param = FilterParam param

def FilterParam (param : Value) =
  case param of
    null   -> nothing
    dict x -> just x


-- Stub for the Decrypt primitive
def Decrypt (body : stream) : stream

def TryApplyFilter (f : Filter) (body : stream) =

  if f.name == "FlateDecode"
    then block
      let params = FlateDecodeParams f.param
      ApplyFilter f ( (params.predictor == 1 || params.predictor == 12) &&
                      (params.colors == 1) &&
                      (params.bpc == 8)) body
                    (FlateDecode params.predictor
                                 params.colors
                                 params.bpc
                                 params.columns
                                 body)

  else if f.name == "LZWDecode"
    then block
      let params = LZWDecodeParams f.param
      ApplyFilter f ( (params.predictor == 1 || params.predictor == 12) &&
                      (params.colors == 1) &&
                      (params.bpc == 8) ) body
                    ( LZWDecode params.predictor
                                params.colors
                                params.bpc
                                params.columns
                                params.earlychange
                                body )

  else if f.name == "ASCIIHexDecode"
    then ApplyFilter f true body (ASCIIHexDecode body)

  else if f.name == "ASCII85Decode"
    then ApplyFilter f true body (ASCII85Decode body)

  else if f.name == "DCTDecode"
    then
      ApplyFilter f true body
        block
          -- Jpeg disabled
          -- WithStream body SomeJpeg  -- Just validate
          body


  else ApplyFilter f false body (Fail "Unsupported filter")

def ApplyFilter (f : Filter) guard (body: stream) (P : stream) : ApplyFilter = block
  -- Trace "ApplyFilter"
  if guard
    then {| ok = P |} <| {| undecoded = body |}
    else {| unsupported =
              case f.param of
                nothing -> f.name
                _       -> concat [ f.name, " (with params)" ]
         |}

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
    @v  = vV is number;
    NumberAsNat v; 
  }

-- like LookupNat, but indirect reference disallowed
def LookupNatDirect k m =
  { @vV = Lookup k m : Value;
    @v  = vV is number;
    NumberAsNat v; 
  }

def LookupSize k m = LookupNat k m as? uint 64

def LookupNats k m = {
  @kV = LookupResolve k m : Value;
  @vs = kV is array;
  map (v in vs) {
    @v1 = ResolveVal v;
    @rV = v1 is number;
    NumberAsNat rV;
  }
}

def LookupRef k m = (Lookup k m : Value) is ref

def LookupName k m = {
  @vV = LookupResolve k m : Value;
  vV is name;
}

def Nested s P = block
  let now = GetStream
  SetStream s
  $$ = P
  SetStream now

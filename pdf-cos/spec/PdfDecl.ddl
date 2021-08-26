import Stdlib
import PdfValue
import GenPdfValue
import JpegBasics


-- N.B. regarding PDF Terminology:
--  - Stream Object : the top level stream that encodes ALL byte data
--  - Object Stream : (see below), the Stream Object that encodes a sequence of objects
--  - Content Stream : the Stream Object containing graphical instructions
-- 
-- and Daedalus built-in names
--  - stream - the built in type returned by GetStream


def TopDecl = {
  id   = Token Natural;
  gen  = Token Natural;
  KW "obj";
  @val = Value;
  obj  = TopDeclDef val;
  Match "endobj";
}

def TopDeclDef (val : Value) = Choose1 {
  stream = StreamObject val;
  value  = ^ val
}

def StreamObject (val : Value) = {
  header = val is dict;
  Match "stream";
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

def ObjectStream (n : uint 64) (first : uint 64) = {
  @meta = Many n (ObjStreamMeta first);
  map (entry in meta) {
    @here = Offset;
    Guard (here <= entry.off);
    SkipBytes (entry.off - here);
    ObjectStreamEntry entry.oid;
  };
}

def ObjectStreamEntry (oid : int) = {
  oid = ^ oid;
  val = Value; -- FIXME: we should check this isn't a ref etc?  (c.f. pdf 1.7, pg 101)
}

def ObjStreamMeta first = {
  oid     = Token Natural;
  off     = (Token Natural + (first as int)) as? uint 64
}

def SkipBytes n = Chunk n {}

--------------------------------------------------------------------------------
-- Our two "resolving" primitives (declarations only):

-- Returns 'nothing' if there is no entry for the Ref (in the xref table)
-- - see Resolve_Ref_ToValue : where we turn nothing into 'null'

def ResolveRef (r : Ref) : maybe TopDecl

-- InputAtRef r: the input stream at reference r
def InputAtRef (r : Ref) : maybe ObjStart

{- copied from pdf-cos/Setup.hs as an aid:

      [ primName "PdfDecl" "ResolveRef" AGrammar |->
        aps "D.resolveImpl" [ "PdfDecl.pTopDecl"
                            , "PdfDecl.pResolveObjectStreamEntry"
                            , fld "obj" "r"
                            , fld "gen" "r"
                            ]
      , primName "PdfDecl" "InputAtRef" AGrammar |-> -- get a stream:
        aps "D.resolveImpl" [ "PdfDecl.pWrapGetStream"
                            , "PdfDecl.pResolveObjectStreamPoint"
                            , fld "obj" "r"
                            , fld "gen" "r"
                            ]
-}

--------------------------------------------------------------------------------
-- Parsers unused otherwise but which are used to implement primitives

-- - Parsers directly called:

-- WrapGetStream: local wrapper to GetStream
def WrapGetStream : ObjStart = {|
  inInput = GetStream
|}

-- ResolveObjectStreamEntry:
--  - passed to Haskell code, used to parse the ObjectStream (finding the Object)
--  - there could be multiple recursive calls to our primitives here:
--    - this calls Resolve_ValueRef_ToStream, that calls ResolveRef
--    - CheckType and LookupSize also can [indirectly] call ResolveRef
--
def ResolveObjectStreamEntry
      (oid : int) (gen : int) (idx : uint 64) : TopDecl = {
  @stm = Resolve_ValueRef_ToStream {| ref = { obj = oid; gen = gen } |};
  CheckType "ObjStm" stm.header;
  @n       = LookupSize "N"     stm.header;
  @first   = LookupSize "First" stm.header;
  @s       = stm.body is ok;
  @entry   = WithStream s (ObjectStreamNth n first idx);
  ^ { id = entry.oid; gen = 0; obj = {| value = entry.val |} };
}


-- TODO: ugly near clone of ResolveObjectStreamEntry.
def ResolveObjectStreamPoint
      (oid : int) (gen : int) (idx : uint 64) : ObjStart = {
  @stm = Resolve_ValueRef_ToStream {| ref = { obj = oid; gen = gen } |};
  CheckType "ObjStm" stm.header;
  @n       = LookupSize "N"     stm.header;
  @first   = LookupSize "First" stm.header;
  @s       = stm.body is ok;
  {| inObjStream = WithStream s (ObjectStreamStrm n first idx)
  |}
}

-- FIXME: dead code: use or delete
def ResolveObjectStream (v : Value) : [ ObjectStreamEntry ] = {
  @stm = Resolve_ValueRef_ToStream v;
  CheckType "ObjStm" stm.header;
  @n       = LookupSize "N" stm.header;
  @first   = LookupSize "First" stm.header;
  WithStream (stm.body is ok) (ObjectStream n first);
}


-- Indirectly called parsers:

def ObjectStreamNth (n : uint 64) (first : uint 64) (idx : uint 64) : ObjectStreamEntry = {
  -- FIXME: only really need to parse up to idx
  @meta  = Many n (ObjStreamMeta first);
  @entry = Index meta idx;
  @here  = Offset;
  Guard (here <= entry.off);
  SkipBytes (entry.off - here);
  ObjectStreamEntry entry.oid;
}

-- TODO: ugly near-clone of ObjectStreamNth, refactor.
def ObjectStreamStrm (n : uint 64) (first : uint 64) (idx : uint 64) : stream = {
  -- FIXME: only really need to parse up to idx
  @meta  = Many n (ObjStreamMeta first);
  @entry = Index meta idx;
  @here  = Offset;
  Guard (here <= entry.off);
  SkipBytes (entry.off - here);
  GetStream
}



--------------------------------------------------------------------------------
-- Resolving references to streams

-- ObjStart: never used as parser, but the sum datatype is used
def ObjStart (s : stream) = Choose {
  inInput = s
; inObjStream = s
}

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
  ((Resolve_Ref_ToStream (Token Ref)).body is ok)
  P

--------------------------------------------------------------------------------

-- extract object from TopDecl, checking that the object id of
--  the reference and the object definition are the same.
def ValidateMatchingObjectIDs (r : Ref) (d : TopDecl) : TopDeclDef = {
  Guard (d.id  == r.obj && d.gen == r.gen);
  ^ d.obj;
}

def Resolve_Ref_ToStream (r : Ref) = 
  ValidateMatchingObjectIDs r (ResolveRef r is just) is stream
  -- didn't you just create?

-- deprecated: really mean to fail when not `is ref`?
def Resolve_ValueRef_ToStream (v : Value) = {
  @r  = v is ref;
  Resolve_Ref_ToStream r
}

def Resolve_Ref_ToValue (r : Ref) : Value = {
  @mb = ResolveRef r;
    { mb is nothing; ^ nullValue }
  | ValidateMatchingObjectIDs r (mb is just) is value;
}

def ResolveVal (v : Value) =
  case v of
    ref r -> Resolve_Ref_ToValue r
    _     -> v


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
  NumberAsNat lenI as? uint 64;
}

-- Section 7.3.8.2
def ApplyFilters header initialBody = {
  @decrypt = Decrypt initialBody; -- A no-op if crypto is disabled
  @filter_names  = LookOptArray "Filter" header;
  @filter_params = LookOptArray "DecodeParms" header;
  for (bytes = {| ok = decrypt |}; ix, name in filter_names) {
    @param  = Default nullValue (Index filter_params ix);
    @filter = Filter name param;
    case bytes of
      ok bs -> TryApplyFilter filter bs
      _     -> bytes
  };
}


def Filter (name : Value) (param : Value) = {
  name  = name is name;
  param = FilterParam param;
}

def FilterParam (param : Value) =
    { param      is null; ^ nothing }
  | { @x = param is dict; ^ just x }

def TryApplyFilter (f : Filter) (body : stream) =

  if f.name == "FlateDecode"
    then block
      let params = FlateDecodeParams f.param
      ApplyFilter f ( (params.predictor == 1 || params.predictor == 12) &&
                      (params.colors == 1) &&
                      (params.bpc == 8))
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
                      (params.bpc == 8) )
                    ( LZWDecode params.predictor
                                params.colors
                                params.bpc
                                params.columns
                                params.earlychange
                                body )

  else if f.name == "ASCIIHexDecode"
    then ApplyFilter f true (ASCIIHexDecode body)

  else if f.name == "ASCII85Decode"
    then ApplyFilter f true (ASCII85Decode body)

  else if f.name == "DCTDecode"
    then ApplyFilter f true
          block
            WithStream body SomeJpeg  -- Just validate
            body

  else ApplyFilter f false (Fail "Unsupported filter")


def ApplyFilter (f : Filter) guard (P : stream) : ApplyFilter =
  if guard
    then {| ok = P |}
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

--------------------------------------------------------------------------------
-- Our five decoding primitives (declarations only):
-- note that 'stream' is a built-in type (GetStream returns)

def Decrypt (body : stream) 
            : stream 

def FlateDecode (predictor : int)
                (colors    : int)
                (bpc       : int)
                (columns   : int)
                (body      : stream)
              : stream

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

-- FIXME: LookupNatArray, sig.
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


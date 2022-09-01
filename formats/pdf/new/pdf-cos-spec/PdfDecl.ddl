import Daedalus
import PdfValue

-- ENTRY
def TopDecl =
  block
    ManyWS
    id   = Token Natural
    gen  = Token Natural

    KW "obj"
    obj = TopDeclDef
    Match "endobj"

def TopDeclDef =
  block
    let val = Value
    First
      stream = Stream val
      value  = val

def Stream (val : Value) =
  block
    header = val is dict

    Match "stream"
    SimpleEOL
    body = StreamBody header
    KW "endstream"


--------------------------------------------------------------------------------
-- Resolving Referneces

-- Returns 'nothing' if there is no entry for this declaration.
-- For values this means we should return 'null'.
def ResolveRef (r : Ref) : maybe TopDecl


def ResolveDeclRef (r : Ref) : TopDeclDef =
  case ResolveRef r of
    nothing -> {| value = nullValue |}
    just d  -> CheckExpected r d

def CheckExpected (r : Ref) (d : TopDecl) : TopDeclDef =
  block
    GuardMsg (d.id  == r.obj && d.gen == r.gen)
      "objid and gen don't match between xref table and the object definition"
    d.obj


def ResolveStreamRef (r : Ref) = ResolveDeclRef r is stream
def ResolveValRef    (r : Ref) = ResolveDeclRef r is value

def ResolveStream (v : Value)  = ResolveStreamRef (v is ref)
def ResolveVal (v : Value) =
  case v of
    ref r -> ResolveValRef r
    _     -> v

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Object Streams (pdf 1.4, S3.4.6)

-- ENTRY
def ObjStream (s : Stream) =
  block
    let h = s.header
    CheckType "ObjStm" h

    let body  = s.body is ok
    let n     = LookupSize "N" h
    let first = LookupSize "First" h

    index     = WithStream body (Many n ObjStreamMeta)
    bytes     = Drop first body

def ObjStreamMeta =
  block
    oid = Token Natural
    off = Token Natural as? uint 64


-- ENTRY
def ObjStreamEntry (o : ObjStream) (i : uint 64) : TopDecl =
  block
    let info  = Index o.index i
    id        = info.oid
    gen       = 0
    let info  = Index o.index i
    obj       = {| value = WithStream (Drop info.off o.bytes) Value |}


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
    let decrypt =
          First
            block
              Guard (Lookup "Type" header is name == "XRef")
              initialBody
            Decrypt initialBody
    let filter_names  = LookupOptArray "Filter" header
    let filter_params = LookupOptArray "DecodeParms" header
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



--------------------------------------------------------------------------------
-- Various helper function for looking stutff up in dictinaries

def LookupResolve k header = ResolveVal (Lookup k header)

def LookupNat k m = NumberAsNat (LookupResolve k m is number)

-- like LookupNat, but indirect reference disallowed
def LookupNatDirect k (m : Dict) = NumberAsNat (Lookup k m is number)

def LookupSize k m = LookupNat k m as? uint 64

def LookupNats k m =
  block
    let vs = LookupResolve k m is array
    map (v in vs)
        (NumberAsNat (ResolveVal v is number))

def LookupRef k m = (Lookup k m : Value) is ref

def LookupName k m = LookupResolve k m is name

def CheckType x h = Guard (LookupName "Type" h == x)

def LookupOptArray key (header : Dict) =
  Default [] (OneOrArray (LookupResolve key header))

def OneOrArray (v : Value) = Default [v] (v is array)



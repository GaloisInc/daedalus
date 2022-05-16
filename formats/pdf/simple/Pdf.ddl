import PdfValue

def Main = Only {
  version = SkipTo { Match "%PDF-"; $$ = Number; };
  chunks  = Many PdfChunk;
  Many AnyWS;
}

def PdfChunk = {
  objects = Many ObjDecl;
  xref    = CrossRefSection;
  trailer = Trailer;
  Match "startxref"; EOL;
  declaredStartXref = { $$ = Natural; EOL };
  KW "%%EOF";
}

def ObjDecl = {
  id   = Token Natural;
  gen  = Token Natural;
  KW "obj";
  @val = PdfValue;
  obj  = TopDeclDef val;
  KW "endobj";
}

def TopDeclDef (val : Value) = First
  stream = Stream val
  value  = ^ val

def Stream (val : Value) = {
  header = val is dict;
  Match "stream";
  SimpleEOL;
  @start = GetStream;
  @len   = CountTo (KW "endstream") 0;
  body   = Take len start;
}


def Trailer = {
  KW "trailer";
  PdfDict
}

def CrossRefSection = {
  offset = Offset;
  KW "xref";
  entries = Many (1..) CrossRefSubSection;
}

def CrossRefSubSection = {
  firstId = Token Natural;
  @num    = Token Natural as? uint 64;
  entries = Many num CrossRefEntry;
}

def CrossRefEntry = {
  @num = NatN 10; $simpleWS;
  @gen = NatN 5;  $simpleWS;
  $$   = First
           inUse = UsedEntry num gen
           free  = FreeEntry num gen
  ;
  { $simpleWS; $cr <| $lf } <| { $cr; $lf };
}

def UsedEntry (num : uint 64) (gen : uint 64) = {
  $['n']; offset = num; gen = gen;
}

def FreeEntry (num : uint 64) (gen : uint 64) = {
  $['f']; obj = num; gen = gen;
}

def NatN n = { @ds = Many n Digit; numBase 10 ds as? uint 64 }
--------------------------------------------------------------------------------
-- Utilities

def CountTo P count = { P; count } <| { UInt8; CountTo P (count+1) }
def SkipTo P        = case { P; true } <| false of {
                        true  -> {};
                        false -> { UInt8; SkipTo P };
                      }


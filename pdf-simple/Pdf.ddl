import PdfValue

def Main = {
  version = SkipTo { Match "%PDF-"; $$ = Number; };
  chunks  = Many PdfChunk;
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
  @val = Value;
  obj  = TopDeclDef val;
  KW "endobj";
}

def TopDeclDef (val : Value) = Choose1 {
  stream = Stream val;
  value  = ^ val
}

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
  Dict;
}

def CrossRefSection = {
  offset = Offset;
  KW "xref";
  entries = Many (1..) CrossRefSubSection;
}

def CrossRefSubSection = {
  firstId = Token Natural;
  @num    = Token Natural;
  entries = Many num CrossRefEntry;
}

def CrossRefEntry = {
  @num = NatN 10; $simpleWS;
  @gen = NatN 5;  $simpleWS;
  $$   = Choose1 {
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

def NatN n = { @ds = Many n Digit; ^ numBase 10 ds }
--------------------------------------------------------------------------------
-- Utilities

def CountTo P count = { P; count } <| { UInt8; CountTo P (count+1) }
def SkipTo P        = P <| { UInt8; SkipTo P }


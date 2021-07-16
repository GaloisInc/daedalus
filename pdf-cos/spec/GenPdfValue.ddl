-- generalized PDF values
import Stdlib
import Pair
import Map

import PdfValue

-- GenArray P: PDF array of P's
def GenArray P = Between "[" "]" (Many (Token P)) 

-- GenName P: a name built from P
def GenName P = {
  Match "/";
  P
}

def GenObj P = {
  Token Natural;
  Token Natural;
  KW "obj";
  $$ = P;
  Match "endobj";
}

-- NameStr s: name with string s
def NameStr s = GenName (Match s)

def Token1 P x = Token (P x)

def NameToken s = Token (GenName s)

def DictEntry Key Val = DepPair (Token (GenName Key)) (Token1 Val)

-- experimental: rank-2 parsing
def DictEntries Key Val = Many (DictEntry Key Val)

def DictMap Key Val = {
  @es = DictEntries Key Val;
  ListToMap es
}

-- PdfDict: a PDF dictionary
def PdfDict Key Val = Between "<<" ">>" (DictMap Key Val)

def GenPdfDict Val = Between "<<" ">>" (DictMap Name (Const Val))

-- InputStream r: the input stream at reference r
def InputStream r : Stream = Void
-- TODO: implement as a new primitive

-- ParseAtRef P r: parse the input at r, using P
def ParseAtRef r P = {
  @s = InputStream r;
  WithStream s (GenObj P)
}

-- DirectOrRef P: parse either the current input or parse a ref and
-- parse the input that it references.
def DirectOrRef P = P <| {
  @r = Ref;
  ParseAtRef r P
}

def CheckNoLocalDefn (x : maybe Pair) = case x of {
  just rs -> Guard (!rs.fst);
  nothing -> ^{};
}

def Bestow (x : maybe Pair) = case x of {
  just x -> just x.snd;
  nothing -> nothing;
}

def Inherit x : maybe Pair = case x of {
  just y -> just {
    fst = false;
    snd = y;
  };
  nothing -> nothing;
}

def LocalDefn P : maybe Pair = just {
  fst = true;
  snd = P;
}

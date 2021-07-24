-- generalized PDF values
import Stdlib
import Pair
import Map

import PdfValue

def Integer : int = {
  @sign = Sign;
  @n = Natural;
  case sign of {
    pos -> n;
    neg -> -1 * n;
  }
}

def UNatural = Natural as! uint 64

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

def NameBase = Many NameChar

-- NameStr s: name with string s
def NameStr s = GenName (Match s)

def NameToken s = Token (NameStr s)

def DictEntry Key Val = DepPair (Token (GenName Key)) Val

-- experimental: rank-2 parsing
def DictEntries Key Val = Many (DictEntry Key Val)

def DictMap Key Val = ListToMap (DictEntries Key Val)

def GenPdfDict Key Val = Between "<<" ">>" (DictMap Key Val)

-- PdfDict: a PDF dictionary
def PdfDict Val = GenPdfDict NameBase (Const Val)

def DictAdderRec Adder d = Default d (DictAdderRec Adder
  { @k = Token Name;
    case Adder k d of {
      just d2 -> ^d2
    ; nothing -> When Value d
    }
  })

-- TODO: maintain a set of all keys defined

-- TODO: refactor to use a GenMany combinator
 
def GenPdfDict1 init Adder Final = {
  @x = Between "<<" ">>" (DictAdderRec Adder init);
  Final x
}

-- OrRef P: parse a P, or a Ref
def OrRef P = Choose1 {
  direct = P;
  pref = Token Ref;
}

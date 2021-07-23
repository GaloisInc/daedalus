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

-- NameStr s: name with string s
def NameStr s = GenName (Match s)

def Token1 P x = Token (P x)

def NameToken s = Token (NameStr s)

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

def DictAdderRec Adder d = Default d {
  @d0 = {
    @k = Token Name;
    @d1 = Adder k d;
    case d1 of {
      just d2 -> ^d2
    ; nothing -> When Value d
    }
  };
  DictAdderRec Adder d0
}


def GenPdfDict1 init Adder Final = {
  @x = Between "<<" ">>" (DictAdderRec Adder init);
  Final x
}
 
-- OrRef P: parse a P, or a Ref
def OrRef P = Choose1 {
  direct = P;
  pref = Token Ref;
}

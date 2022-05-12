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

-- FIXME!
--  - very closely duplicated code in the PdfDecl.TopDecl parser
--  - some evidence that this, as well as the above parser are *both* being called on the same input!
def GenObj P = {
  @header = 
  { ManyWS;        -- FIXME: would like to warn user (a cavity!) when this consumes input!
  Token Natural;
  Token Natural;
  ^{} } <|
  (@(Match ""));
  KW "obj";
  $$ = P;
  KW "endobj";
}

def NameBase = Many NameChar

-- NameStr s: name with string s
def NameStr s = GenName (Match s)

def NameToken s = Token (NameStr s)

def DictEntry Key Val = PairMapEntry (DepPair (Token (GenName Key)) Val)

-- experimental: rank-2 parsing
def DictEntries Key Val = Many (DictEntry Key Val)

def DictMap Key Val = ListToMap (DictEntries Key Val)

def GenPdfDict Key Val = Between "<<" ">>" (DictMap Key Val)

-- PdfDict: a PDF dictionary
def PdfDict Val = GenPdfDict NameBase (Const Val)

-- OPT:
def DictAdderRec0 Adder d = Default d (DictAdderRec Adder
  { @k = Token Name;
    case Adder k d of {
      just d2 -> ^d2
    ; nothing -> When Value d
    }
  })

def DictAdderRec Adder d = {
  -- try to get the next entry
  @mayNext = Optional {
    @k = Token Name;
    case Adder k d of {
      just d2 -> d2
    ; nothing -> When Value d
    }
  };
  case mayNext of {
    just dNext -> DictAdderRec Adder dNext
  ; nothing -> d
  }
}

-- TODO: maintain a set of all keys defined

-- TODO: refactor to use a GenMany combinator
 
def GenPdfDict1 init Adder Final = {
  @x = Between "<<" ">>" (DictAdderRec Adder init);
  Final x
}

-- OrRef P: parse a P, or a Ref
def OrRef P = First
  direct = P
  pref = Token Ref


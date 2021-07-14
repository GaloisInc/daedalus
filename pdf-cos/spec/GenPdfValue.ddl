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

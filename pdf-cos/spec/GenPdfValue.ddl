-- generalized PDF values
import Stdlib
import Pair
import Map

-- GenArray P: PDF array of P's
def GenArray P = Between "[" "]" P

-- GenName P: a name built from P
def GenName P = {
  Match "/";
  P
}

-- NameStr s: name with string s
def NameStr s = GenName (Match s)

-- experimental: rank-2 parsing
def DictEntries Key Val = Many (DepPair Key Val)

def DictMap Key Val = {
  @es = DictEntries Key Val;
  ListToMap es
}

-- PdfDict: a PDF dictionary
def PdfDict Key Val = Between "<<" ">>" (DictMap Key Val)

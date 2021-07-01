-- generalized PDF values
import Stdlib

-- GenArray P: PDF array of P's
def GenArray P = Between "[" "]" P

-- GenName P: a name built from P
def GenName P = {
  Match "/";
  P
}

-- NameStr s: name with string s
def NameStr s = GenName (Match s)

def InsertNext Key Val m = {
  @p = DepPair Key Val;
  Insert p.fst p.snd m
}

def DictEntries Key Val = {
  @es = Many (DepPair Key Val);
  for (acc = empty; e in es) Insert e.fst e.snd acc
}

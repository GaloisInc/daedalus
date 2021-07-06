-- Pair.ddl: a library for pairs
def Pair P0 P1 = {
  fst = P0;
  snd = P1;
}

def DepPair P0 P1 = {
  depFst = P0;
  depSnd = P1 depFst;
}

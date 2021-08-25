-- Pair.ddl: a library for pairs
def Pair P0 P1 = {
  fst = P0;
  snd = P1;
}

-- DepPair: dependent pair
-- TODO: experimental support
def DepPair P0 P1 : Pair = {
  fst = P0;
  snd = P1 fst;
}

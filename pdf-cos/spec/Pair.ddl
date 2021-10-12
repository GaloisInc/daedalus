-- Pair.ddl: a library for pairs
def pair x y = {
  fst = x;
  snd = y;
}

def Pair P0 P1 : pair = pair P0 P1

-- DepPair: dependent pair
-- TODO: experimental support
def DepPair P0 P1 : pair = {
  fst = P0;
  snd = P1 fst;
}

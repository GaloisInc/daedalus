{- Tests
   SelUnion
   SelJust
-}

def Word1 = { y = Match1 'y'; x = Match1 'x' }

def Word = Choose { a = Word1; b = Word1; }

def Main = {
  @w1 = Word;
  @w2 = Word;
  w3 = w1 is a;
  w4 = w2 is b;
  @w5 = ^ just w3;
  w6 = w5 is just;
}

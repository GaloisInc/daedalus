{- More involved test for biased choice. Tested the index/level. -}

def A = Choose1
    { a = Match "a"
    ; b = Match "a"
    }

def B = { x = A
    ; y = Choose1
      { c = Match "a"
      ; d = Match "a"
      };
    }

def C = Choose1
    { z = B
    ; t = Match "a"
    }

def Main = { p = C; q = Match "b"; END}

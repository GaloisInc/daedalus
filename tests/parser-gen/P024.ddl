{- More involved test for biased choice. Tested the index/level. -}

def A = Choose1
    { a = "a"
    ; b = "a"
    }

def B = { x = A
    ; y = Choose1
      { c = "a"
      ; d = "a"
      };
    }

def C = Choose1
    { z = B
    ; t = "a"
    }

def Main = { p = C; q = "b"; END}

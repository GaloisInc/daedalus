{- More involved test for biased choice. Tested the index/level. -}

def A =
  First
    a = Match "a"
    b = Match "a"

def B =
  block
    x = A
    y = First
          c = Match "a"
          d = Match "a"

def C =
  First
    z = B
    t = Match "a"

def Main =
  block
    p = C
    q = Match "b"
    END

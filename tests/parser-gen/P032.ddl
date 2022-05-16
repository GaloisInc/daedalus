{- Test behavior of commit -}


-- (b <| a <| ab)
-- ((b commit c) <| b) | (bc | bd)

def A = { @x = $['a'] ; ^ "a" }
def B = { @x = $['b'] ; ^ "b" }
def C = { @x = $['c'] ; ^ "c" }
def D = { @x = $['d'] ; ^ "d" }

def AB = { @x = $['a'] ; @y = $['b'] ; ^ "ab" }

def Grp1 = (B <| (A <| AB))

def Grp2 =
  First
    { B ; commit ; C }
    B

def Grp3 =
  Choose
  { { B ; C }
  ; { B ; D }
  }

def Main =
  { Grp1
  ; Choose
    { Grp2
    ; Grp3
    }
  ; END
  }

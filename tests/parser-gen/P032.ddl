{- Test behavior of commit -}


-- (b <| a <| ab)
-- ((b commit c) <| b) | (bc | bd)

def A = { @x = Match1 'a' ; ^ "a" }
def B = { @x = Match1 'b' ; ^ "b" }
def C = { @x = Match1 'c' ; ^ "c" }
def D = { @x = Match1 'd' ; ^ "d" }

def AB = { @x = Match1 'a' ; @y = Match1 'b' ; ^ "ab" }

def Grp1 = (B <| (A <| AB))

def Grp2 =
  Choose1
  { { B ; commit ; C }
  ; B
  }
  
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

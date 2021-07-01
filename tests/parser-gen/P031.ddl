{- Check the Map grammar operator -}

def A =
    { x = Many (Match1 'a')
    ; y = map (z in x)
          { @t = Match1 'b'
          ; ^ 'c'
          }
    ; z = map (x in y)
          { @t = Match1 'd'
          ; ^ x
          }
    }

def Main = {res = A; END}

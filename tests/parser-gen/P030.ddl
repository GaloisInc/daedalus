{- Check the Map grammar operator -}

def A =
    { x = Many (Match1 'a')
    ; y = map (z in x)
          { @t = Match1 'b'
          ; ^ 'c'
          }
    ; ^ y
    }

def Main = {res = A; END}

{- Check the Map grammar operator -}

def A =
    { x = Many $['a']
    ; y = map (z in x)
          { @t = $['b']
          ; ^ 'c'
          }
    }

def Main = {res = A; END}

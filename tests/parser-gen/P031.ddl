{- Check the Map grammar operator -}

def A =
    { x = Many $['a']
    ; y = map (z in x)
          { @t = $['b']
          ; ^ 'c'
          }
    ; z = map (x in y)
          { @t = $['d']
          ; ^ x
          }
    }

def Main = {res = A; END}

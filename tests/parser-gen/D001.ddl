-- simplest test

def Main =
  { $$ = Choose
           { x = $['a']
           ; y = $['b']
           }
  ; END
  }

-- test overlap of intervals

def Main =
  { $$ = Choose
    { x = { $['0'..'7'] ; $['a']; }
    ; y = { $['3'..'9'] ; $['b']; }
    }
  ; END
  }

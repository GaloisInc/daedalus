-- test overlap of intervals

def Main =
  { $$ = Choose
    { x = { $['0'..'7'] ; $['a'..'y']; }
    ; y = { $['3'..'9'] ; $['x'..'z']; }
    }
  ; END
  }

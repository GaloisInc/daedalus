-- test overlap of intervals

def Main =
  { $$ = Choose
    { x = { Match1 ('0'..'7') ; Match1 ('a'); }
    ; y = { Match1 ('3'..'9') ; Match1 ('b'); }
    }
  ; END
  }
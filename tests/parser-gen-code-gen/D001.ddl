def Main =
  { $$ = Choose
      { x = Match1 'a'
      ; y = Match1 'b'
      }
  ; END
  }

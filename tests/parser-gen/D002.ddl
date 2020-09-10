-- catching simple determinization and ambiguous report

def Main =
  { @a = Choose
    { x = Match "ab"
    ; y = Match "ac"
    }
  ; @b = Choose
    { x = Match "ab"
    ; y = Match "ab"
    }
  ; END
  }
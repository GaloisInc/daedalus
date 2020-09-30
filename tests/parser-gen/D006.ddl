-- simple example testing the * of LL(*)

def A = { Many (Match "a") }

def Main =
  { @a = Choose
    { x = { A ; Match "c" }
    ; y = { A ; Match "d" }
    }
  ; END
  }

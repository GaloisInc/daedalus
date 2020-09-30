-- testing the * of LL(*) on an example involving character intervals.

def A =
  { Many (Match1 ('a' .. 'm'))
  }

def B = { Many (Match1 ('f' .. 't')) }

def Main =
  { @a = Choose
    { x = { A ; Match "c" }
    ; y = { B ; Match "d" }
    }
  ; END
  }
-- testing the * of LL(*) on an example involving character intervals.

def A =
  { Many $['a' .. 'm']
  }

def B =
  { Many $['f' .. 't']
  }

def Main =
  { @a = Choose
    { x = { A ; Match "c" }
    ; y = { B ; Match "d" }
    }
  ; END
  }

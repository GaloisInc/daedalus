-- test grammar Call

def A = { $['a']; $['b'] }

def B = { $['b']; $['c'] }

def Main =
  { Choose
    { x = { A ; $['c'] }            -- abc
    ; y = { $['a']; B; $['d'] } -- abcd
    }
  ; END
  }

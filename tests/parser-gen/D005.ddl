-- test grammar Call

def A = { Match1 'a'; Match1 'b'}

def B = { Match1 'b'; Match1 'c'}

def Main =
  { Choose
    { x = { A ; Match1 'c' }            -- abc
    ; y = { Match1 'a'; B; Match1 'd' } -- abcd
    }
  ; END
  }

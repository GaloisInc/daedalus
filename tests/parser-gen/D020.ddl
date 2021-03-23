-- test class value defined in environment


def A x =
  @Match1 x

def Main =
  { A 97 -- 'a'
  ; A 98 -- 'b'
  ; END
  }

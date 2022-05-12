def A = Choose
    { x = Match "ab"
    ; y = Match "a"
    }

def B = First
  z = A
  t = Match "a"


def Main = { p = B; q = Match "bb"; END}
  <| { p = B; q = Match "bb"; END}

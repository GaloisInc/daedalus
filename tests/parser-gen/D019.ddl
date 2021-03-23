-- test Fail instruction


def A =
  @Match "b"
  -- <| {}
  <| Fail "Its not b"

def Main =
  { Match "a"
  ; A
  }

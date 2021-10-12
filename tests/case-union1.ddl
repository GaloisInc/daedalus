

def P =
  First
    c1 = Match "a"
    c2 = Match "b"

def Main =
  block
    p   = P
    tag = case p of
            c1 _, c2 _ -> ^ "either"

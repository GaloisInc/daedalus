
def P =
  Choose
    c1 = Match "a"
    c2 = Match "b"

def Main =
  block
    p   = P
    tag = case p of
            _ -> ^ "default"

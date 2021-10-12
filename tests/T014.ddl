
def TwoBytes =
  block
    x = UInt8
    y = UInt8

def Main =
  block
    first  = TwoBytes
    second = TwoBytes
    eq     = ^ (first == second)

import common

def Main =
  block
    TokenM "LENGTH"
    len = Token Natural
    Many len UInt8

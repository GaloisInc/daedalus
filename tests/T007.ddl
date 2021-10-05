

def Sign =
  First
    pos = @Match1 '+'
    neg = @Match1 '-'
    pos = Accept

def SignWithCase =
  First
    case UInt8 of
      '+' -> Accept
      '-' -> Accept
    Accept


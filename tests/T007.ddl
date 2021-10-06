

def Sign =
  First
    pos = @$['+']
    neg = @$['-']
    pos = Accept

def SignWithCase =
  First
    case UInt8 of
      '+' -> Accept
      '-' -> Accept
    Accept


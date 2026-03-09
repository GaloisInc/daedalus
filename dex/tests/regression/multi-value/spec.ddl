def Main =
  block
    a = UInt8
    b = XorY
    c = UInt8
    d = XorY
    e = UInt8
    f = Many (e as uint 64) UInt8

def XorY =
  First
    X = @$['x']
    Y = @$['y']
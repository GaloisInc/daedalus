
def P =
  block
    x = UInt8
    y = UInt8

def Q =
  block
    let s = P
    $$    = ^s.x

def R =
  First
    a = UInt8
    b = Accept

def S =
  block
    let u = R
    $$ = u is a


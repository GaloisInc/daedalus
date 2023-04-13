def Token P =
  block
    $$ = P
    Many (1..) WS

def TokenM str = Token (Match str)

def WS = $[0 | 9 | 12 | 32 | '\n' | '\r' ]

def Natural =
  block
    let ds = Many (1..) Digit
    ^ for (val = 0; d in ds) (addDigit val d)

def Digit =
  block
    let d = $['0' .. '9']
    ^ d - '0'

def addDigit val d =
  10 * val + (d as uint 64)

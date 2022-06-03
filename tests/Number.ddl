
def Natural = for (val = 0; d in Many (1..) Digit) (addDigit val d)

def Frac n w =
  block
    let ds = block $['.']; Many (n ..) Digit
    ^ for ( val = w; d in ds) { num = addDigit val.num d, exp = val.exp - 1 }


def addDigit val d  = 10 * val + d

def Digit =
  block
    let d = $['0' .. '9']
    (d - '0') as int

def HexDigit  =
  First
    Digit
    10 + ($['a' .. 'f'] - 'a') as int
    10 + ($['A' .. 'F'] - 'A') as int


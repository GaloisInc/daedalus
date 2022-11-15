-- BEGIN PPM_MAIN
def Main =
  block
    $$ = PPM
-- END PPM_MAIN

-- BEGIN PPM_TOKEN
def Token P =
  block
    $$ = P
    Many (1..) WS
-- END PPM_TOKEN

-- BEGIN PPM_PPM
def PPM =
  block
    Match "P"
    let version = Token Natural
    version == 3 is true
    width  = Token Natural
    height = Token Natural
    maxVal = Token Natural
    data   = Many height (Many width RGB)
-- END PPM_PPM

-- BEGIN PPM_RGB
def RGB =
  block
    red   = Token Natural
    green = Token Natural
    blue  = Token Natural
-- END PPM_RGB

-- BEGIN PPM_WS
def WS = $[0 | 9 | 12 | 32 | '\n' | '\r']
-- END PPM_WS

-- BEGIN PPM_NATURAL
def Natural =
  block
    let ds = Many (1..) Digit
    ^ for (val = 0; d in ds) (addDigit val d)
-- END PPM_NATURAL

-- BEGIN PPM_DIGIT
def Digit =
  block
    let d = $['0' .. '9']
    ^ d - '0'
-- END PPM_DIGIT

def addDigit val d =
  10 * val + (d as uint 64)

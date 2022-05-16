-- This is to test a subfragment of plain-ppm.ddl
-- in particular the fixed lower bound on Many


def Token P = {
  $$ = P;
  Many (1..) WS;
}

def RGB = {
  red   = Token Natural;
  green = Token Natural;
  blue  = Token Natural;
}



-- def WS = $[0 | 9 | 12 | 32 | '\n' | '\r']
def WS = $[0]


def Natural = {
  @ds = Many (1..) Digit;
  ^ for (val = 0; d in ds) (addDigit val d);
}

def Digit = {
  @d = $['0' .. '9'];
  ^ d - '0';
}


def addDigit val d =
  10 * val + (d as int)

def Main =
  { RGB
  ; END
  }

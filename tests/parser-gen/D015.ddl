-- testing `Many (1..)` on a fragment of ppm


def Main = {
  $$ = PPM;
  END;
}

def Token P = {
  $$ = P;
  Many (1..) WS;
}

def PPM = {
  Match "P";
  @version = Token Natural;
  version == 3 is true;
  width  = Token Natural;
  height = Token Natural;
  maxVal = Token Natural3;
}


def WS = $[0 | 9 | 12 | 32 | '\n' | '\r']


def Natural = {
  @ds = Many (1..) Digit;
  ^ for (val = 0; d in ds) (addDigit val d);
}

def Natural3 = {
   @ds = Many (3..) Digit;
   ^ for (val = 0; d in ds) (addDigit val d);
}

def Digit = {
  @d = $['0' .. '9'];
  ^ d - '0';
}


def addDigit val d =
  10 * val + (d as int)

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
  width  = Token Natural as uint 64;
  height = Token Natural as uint 64;
  maxVal = Token Natural as uint 64;
  data   = Many height (Many width RGB);
}

def RGB = {
  red   = Token Natural;
  green = Token Natural;
  blue  = Token Natural;
}



def WS = Match1 (0 | 9 | 12 | 32 | '\n' | '\r')


def Natural = {
  @ds = Many (1..) Digit;
  ^ for (val = 0; d in ds) (addDigit val d);
}

def Digit = {
  @d = Match1 ('0' .. '9');
  ^ d - '0';
}


def addDigit val d =
  10 * val + (d as int)



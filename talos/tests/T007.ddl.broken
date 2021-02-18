
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

def Main = { @v = Natural;
             100 < v is true;
             END }





def Main = { v = Natural;
             100 < v;
           }

def Natural = {
  @ds = Many (1..) Digit;
  ^ for (val = 0; d in ds) (addDigit val d);
}

def Digit = {
  @d = '0' .. '9';
  ^ d - '0' as int;
}

def addDigit val d = val * 10 + d




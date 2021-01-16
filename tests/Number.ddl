
def Natural = {
  @ds = Many (1..) Digit;
  ^ for (val = 0; d in ds) (addDigit val d)
}

def Frac n w = {
  @ds = { Match "."; Many (n ..) Digit };
  ^ for ( val = w; d in ds)
    { num = addDigit val.num d; exp = val.exp - 1 }
}


def addDigit val d  = 10 * val + d

def Digit     = { @d = Match1 ('0' .. '9'); ^ (d - '0') as int }
def HexDigit  =
    Digit
  | { @d = Match1 ('a' .. 'f'); ^ 10 + ((d - 'a') as int) }
  | { @d = Match1 ('A' .. 'F'); ^ 10 + ((d - 'A') as int) }


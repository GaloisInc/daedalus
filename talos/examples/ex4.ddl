
def Main = Many (1..) Digit

def Digit = {
  @d = '0' .. '9';
  ^ (d - '0' as int);
}



def Main = {
  "P3"; $ws ;
  width  = ReasonableSize;
  height = ReasonableSize;
  maxVal = MaxVal;
  data   = Many height (Many width (RGB maxVal));
}

def ReasonableSize = {
    $$ = Token Natural;
    10 < $$; $$ < 30;
}    

def RGB maxVal = {
  red   = Token (BoundedNat maxVal);
  green = Token (BoundedNat maxVal);
  blue  = Token (BoundedNat maxVal);
}

def MaxVal = {
    @v = Token Natural;
    0 < v; v < 65536;
    ^ v
}

def BoundedNat bound = {
  @v = Token Natural;
  v < bound;
  ^ v;
}

def Token P = {
  $$ = P;
  Many (1..3) $ws;
}

def $ws = ' ' | '\n' | '\r'

def Natural = {
  @ds = Many (1..) Digit;
  ^ for (val = 0; d in ds) (addDigit val d);
}

def Digit = {
  @d = '0' .. '9';
  ^ (d - '0' as int);
}

def addDigit val d = val * 10 + d

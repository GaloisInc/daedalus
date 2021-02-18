def Main = {
  $$ = PPM;
  END;
}

def Token P = {
  $$ = P;
  Many (1..3) $ws; -- FIXME
}

def MaxVal = {
    @v = Token Natural;
    0 < v; v < 65536;
    ^ v
}

-- def ReasonableSize = {
--   @v = Token Natural;
--   0 < v; v < 1024;
--   ^ v;
-- }

def VersionCheck = {
  @v = Token Natural;
  v == 3;
  ^ v;
}

def PPM = {
  "P";
  @version = VersionCheck;
  width  = Token Natural;
  height = Token Natural;
  maxVal = MaxVal;
  data   = Many height (Many width (RGB maxVal));
}

def BoundedNat bound = {
  @v = Token Natural;
  v < bound;
  ^ v;
}

def RGB maxVal = {
  red   = Token (BoundedNat maxVal);
  green = Token (BoundedNat maxVal);
  blue  = Token (BoundedNat maxVal);
}

-- def $ws = 0 | 9 | 12 | 32 | '\n' | '\r'
def $ws = 9 | 12 | 32 | '\n' | '\r'

def Natural = {
  @ds = Many (1..) Digit;
  ^ for (val = 0; d in ds) (addDigit val d);
}

def Digit = {
  @d = '0' .. '9';
  ^ d - '0';
}

def addDigit val d =
  10 * val + (d as int)

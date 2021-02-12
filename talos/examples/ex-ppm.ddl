

-- Simplified PPM 

def Main = {
  "P3"; $ws ;
  -- Bounded for the sake of demonstration  
  width  = BoundedNat 20 50;
  height = BoundedNat 20 50;
  maxVal = BoundedNat 1 65536;
  data   = Many height (Many width (RGB maxVal));
}

def RGB maxVal = {
  red   = BoundedNat 0 maxVal;
  green = BoundedNat 0 maxVal;
  blue  = BoundedNat 0 maxVal;
}

-- Parse a number in [low..high)
def BoundedNat low high = {$$ = Token Natural; low <= $$; $$ < high}

def Token P = { $$ = P; $ws }

def $ws = ' ' | '\n'

def Digit = {
  @d = '0' .. '9';
  ^ (d - '0' as int);
}

def Natural = {
  @ds = Many (1..) Digit;
  ^ for (val = 0; d in ds) (addDigit val d);
}

def addDigit val d = val * 10 + d

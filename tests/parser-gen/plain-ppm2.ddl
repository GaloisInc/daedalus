def Main = block
  $$ = PPM;
  END;

def $ws = 0 | 9 | 12 | 32 | '\n' | '\r'
def WS = Match1 $ws

def Token P = block
  $$ = P
  Many (1..) WS
  LookaheadNotWS

def PPM = block
  Match "P"
  @version = Token Natural
  version == 3 is true
  width  = Token Natural
  height = Token Natural
  maxVal = Token Natural
  data   = Many height (Many width RGB)

def RGB = block
  red   = Token Natural
  green = Token Natural
  blue  = Token Natural

def Natural = block
  @ds = Many (1..) Digit
  ^ for (val = 0; d in ds) (addDigit val d)

def Digit = block
  @d = Match1 ('0' .. '9')
  ^ d - '0'

def addDigit val d = 10 * val + (d as uint 64)

def LookaheadNotWS = block
  @curr = GetStream
  ( @Match1 (! $ws) | END )
  SetStream curr

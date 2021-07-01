def Unit = ^{}

def FoldMany P acc = Choose1 {
  { @acc0 = P acc;
    FoldMany P acc0
  };
  ^ acc
}

def numBase base ds       = for (val = 0; d in ds) (val * base + d)
def Only P                = { $$ = P; END }
def When P x              = { P; ^ x }
def Guard p               = p is true

def append x y = concat [ x, y ]

def cons x xs = append [ x ] xs

def snoc x xs = append xs [ x ]

def condJust p x = if p then just x else nothing

def optionToList x = case x of
  just y -> [ y ]
  nothing -> [ ]

-- types of bounded sequences of bytes
def Bytes1 = UInt8

def Bytes2 = {
  second = UInt8;
  rest1 = Bytes1;
}

def Bytes3 = {
  third = UInt8;
  rest2 = Bytes2;
}

def Bytes4 = {
  fourth = UInt8;
  rest3 = Bytes3;
}

-- functions for serializing bounded structures into arrays
def bndBytes1 bs1 = [ bs1 ]

def bndBytes2 bs2 = cons bs2.second (bndBytes1 bs2.rest1)

def bndBytes3 bs3 = cons bs3.third (bndBytes2 bs3.rest2)

def bndBytes4 bs4 = cons bs4.fourth (bndBytes3 bs4.rest3)

def Default x P = P <| ^ x

def Pair P0 P1 = {
  fst = P0;
  snd = P1;
}

def DepPair P0 P1 = {
  depFst = P0;
  depSnd = P1 fst;
}

def GenMany P acc0 =
  { @acc1 = P acc0;
    GenMany P acc1
  } <|
  ^acc0


def WithStream s P = {
  @cur = GetStream;
  SetStream s;
  $$ = P;
  SetStream cur;
}

--------------------------------------------------------------------------------
-- White Space (Section 7.2)

def $lf                   = 10
def $cr                   = 13
def $space                = 32
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } | $lf
def EOL                   = SimpleEOL <| $cr
def Comment               = { Match "%"; Many (Match1 (! ($lf | $cr))); EOL }
def JustWhite             = $simpleWS | EOL

def AnyWS                 = $simpleWS | Comment | EOL

--------------------------------------------------------------------------------
-- Helpers

def Token P               = { $$ = P; Many AnyWS }
def KW x                  = @ (Token (Match x))
def Between open close P  = { KW open; $$ = P; KW close }

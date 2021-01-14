
def Main = {
  version = Number;
  entries = Token (Match1 'a');
}

def $lf                   = 10
def $cr                   = 13
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } | $lf
def EOL                   = SimpleEOL <| $cr
def Comment               = { Match "%"; Many (Match1 (! ($lf | $cr))); EOL }
def AnyWS                 = $simpleWS | Comment | EOL

def Token P               = { $$ = P; Many AnyWS }
def numBase base ds       = for (val = 0; d in ds) (val * base + d)
def When P x              = { P; ^ x }

def Number = Token {
  @sign = Sign;
  @n    = UnsignedNumber;
    When (sign is pos) n
  | When (sign is neg) { num = 0 - n.num; exp = n.exp }
}

def Sign = Choose {
  pos = @Optional (Match "+");
  neg = @Match "-"
}

def UnsignedNumber =
    UnsignedLeadDigits
  | Frac 1 { num = 0, exp = 0 }

def UnsignedLeadDigits = {
  @n   = Natural;
  @val = ^ { num = n; exp = 0 : int };
  Frac 0 val  <| (^ val)
  -- biased choice here is important
  -- otherwise 1.2 can be processed as "1.2" or "1" followed by ".2"
}

def Natural = numBase 10 (Many (1..) Digit)

def Frac n (w : Number) : Number = {
  Match ".";
  @ds = Many (n ..) Digit;
  ^ for ( val = w; d in ds)
          { num = 10 * val.num + d; exp = val.exp - 1 }
}

def Digit     = Match1 ('0' .. '9') - '0' as int
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- White Space

def $lf               = 10
def $cr               = 13
def $simpleWS         = 0 | 9 | 12 | 32

def SimpleEOL         = { $cr; $lf } | $lf
def EOL               = SimpleEOL <| $cr
def Comment           = { Match "%"; Many $[! ($lf | $cr)]; EOL }

def AnyWS             = $simpleWS | Comment | EOL
--------------------------------------------------------------------------------
def Token P               = { $$ = P; Many AnyWS }
def KW P                  = @(Token P)
def Between open close P  = { KW (Match open); $$ = P; KW (Match close) }
def numBase base ds       = for (val = 0; d in ds) (val * base + d)
def Only P                = { $$ = P; END }

--------------------------------------------------

def Number = Token { @sign = Sign;
                 @n    = UnsignedNumber;
                   { sign is pos; ^ n }
                 | { sign is neg; ^ { num = 0 - n.num; exp = n.exp } }
               }


def Sign = Choose { pos = @(Match "+" | Match ""); neg = @Match "-" }


def UnsignedNumber =
  { @n   = Natural;
                   @val = ^ { num = n; exp = 0 : int };
                   Frac 0 val  <| (^ val)
                   -- biased choice here is important
                   -- otherwise 1.2 cane be processed as [1.2] or [1 .2]
                 }
                 |
                 Frac 1 { num = 0; exp = 0 }

def Natural = { @ds = Many (1..) Digit; ^ numBase 10 ds }

def Frac n (w : Number) : Number =
  { @ds = { Match "."; Many (n ..) Digit };
    ^ for ( val = w; d in ds)
          { num = 10 * val.num + d; exp = val.exp - 1 }
  }

def Digit     = { @d = $['0' .. '9']; ^ d - '0' as int }

def Main = { x = Many Number; END}


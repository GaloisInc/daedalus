def $lf                   = 10
def $cr                   = 13
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } | $lf
def EOL                   = SimpleEOL <| $cr
def Comment               = { Match "%"; Many $[!($lf | $cr)]; EOL }
def AnyWS                 = $simpleWS | Comment | EOL

--------------------------------------------------------------------------------
def Token P               = { $$ = P; Many (1..) AnyWS }
def KW P                  = @(Token P)
def Between open close P  = { KW (Match open); $$ = P; KW (Match close) }


def Main = { x = Many (Token (Many $['a'..'z'])); END }

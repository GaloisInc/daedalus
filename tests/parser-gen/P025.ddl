def $lf                   = 10
def $cr                   = 13
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } | $lf
def EOL                   = SimpleEOL <| $cr
def Comment               = { "%"; Many (!($lf | $cr)); EOL }
def AnyWS                 = $simpleWS | Comment | EOL

--------------------------------------------------------------------------------
def Token P               = { $$ = P; Many (1..) AnyWS }
def KW P                  = @(Token P)
def Between open close P  = { KW open; $$ = P; KW close }


def Main = { x = Many (Token (Many ('a'..'z'))); END }

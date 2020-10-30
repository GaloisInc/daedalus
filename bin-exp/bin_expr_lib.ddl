-- library for whitespace:
def $lf                   = 10
def $cr                   = 13
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } | $lf
def EOL                   = SimpleEOL <| $cr
def AnyWS = $simpleWS | EOL

def Token P               = { $$ = P; Many AnyWS }

def Some P = { hd = P ; tl = Many P }

-- Min: min function:
def Min a b =
  { a < b ; ^ a} |
  ^ b


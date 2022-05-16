-- testing LL(k) on lexicalistic rules

def Main = { AnyWS ; END }

def $lf                   = 10
def $cr                   = 13
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } <| $lf
def EOL                   = SimpleEOL <| $cr
def Comment               = { Match "%"; Many $[! ($lf | $cr)]; EOL }
def AnyWS                 = $simpleWS <| Comment <| EOL



def Sexp =
  First
    sexp =
      block
        KW $['(']
        $$ = Many (1..) Sexp
        KW $[')']
    symbol = Atom

def WS      = @$[0 | 9 | 12 | 32 | '\n' | '\r']
def Token P = block $$ = P; Many WS
def KW P    = @(Token P)
def Atom    = Token (Many (1..) $['a' .. 'z'])



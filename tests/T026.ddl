-- Map test

def WS = @(0 | 9 | 12 | 32 | '\n' | '\r' )

def Token P = { $$ = P; Many (1..) WS }
def KW P    = @(Token P)
def Between open close P = { KW open; $$ = P; KW close }

def Symbol = Token (Many (1..) ('a' .. 'z'))

def MkDict m = (^ m)
         | { @k = Symbol; @v = Symbol; @m2 = Insert k v m; MkDict m2 }

def Main = Between "<<" ">>" (MkDict empty)


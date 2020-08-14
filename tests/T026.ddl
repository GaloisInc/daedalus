-- Map test

def WS = @Match1 (0 | 9 | 12 | 32 | '\n' | '\r' )

def Token P = { $$ = P; Many (1..) WS }
def KW P    = @(Token P)
def Between open close P = { KW (Match open); $$ = P; KW (Match close) }

def Symbol = Token (Many (1..) (Match1 ('a' .. 'z')))

def MkDict m = (^ m)
         | { @k = Symbol; @v = Symbol; @m2 = Insert k v m; MkDict m2 }

def Main = Between "<<" ">>" (MkDict empty)


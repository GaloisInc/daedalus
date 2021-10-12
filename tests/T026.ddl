-- Map test

def WS = @Match1 (0 | 9 | 12 | 32 | '\n' | '\r' )

def Token P =
  block
    $$ = P
    Many (1..) WS

def KW P = @(Token P)
def Between open close P =
  block
    KW (Match open)
    $$ = P
    KW (Match close)

def Symbol = Token (Many (1..) $['a' .. 'z'])

def MkDict m =
  First
    MkDict (Insert Symbol Symbol m)
    m

def Main = Between "<<" ">>" (MkDict empty)


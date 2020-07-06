
def P = { x = UInt8; y = UInt8 }

def Q = { @s = P; $$ = ^s.x }

def R = Choose { a = UInt8; b = {} }

def S = { @u = R; $$ = u is a }


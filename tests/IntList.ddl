
import Number

def WS = @$[0 | 9 | 12 | 32 | '\n' | '\r']

def Token P =
  block
    $$ = P
    Many (1..) WS

def IntList =
  First
    cons = block hd = Token Natural; tail = IntList
    nil  = Accept


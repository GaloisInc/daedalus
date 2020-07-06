
import Number

def WS = @(0 | 9 | 12 | 32 | '\n' | '\r' )

def Token P              = { $$ = P; @Many (1..) WS }

def IntList =
   Choose { cons = { hd = Token Natural; tail = IntList };
            nil  = {};
          }


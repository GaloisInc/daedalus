

def lf = '\10'
def cr = '\13'

def WS = @(0 | 9 | 12 | 32 | '\n' | '\r')

def Token P              = { $$ = P; @Many WS }
def KW P                 = @(Token P)

def Sexp =
   Choose {
      sexp = { KW "("
            ; $$ = ManySexp
            ; KW ")"
            };

      symbol = Token (Many (1..) ('a' .. 'z'));
    }

def ManySexp = Many Sexp

def Main = { @Many WS; $$ = Sexp }

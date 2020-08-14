

def lf = '\10'
def cr = '\13'

def WS = @(Match1 (0 | 9 | 12 | 32 | '\n' | '\r'))

def Token P              = { $$ = P; @Many WS }
def KW P                 = @(Token P)

def Sexp =
   Choose {
      sexp = { KW (Match "(")
            ; $$ = ManySexp
            ; KW (Match ")")
            };

      symbol = Token (Many (1..) (Match1 ('a' .. 'z')));
    }

def ManySexp = Many Sexp

def Main = { @Many WS; $$ = Sexp }

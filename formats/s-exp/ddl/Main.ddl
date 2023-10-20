import SExp

def Main =
  block
    Many WS
    $$ = count Sexp
    END

def count (se : Sexp) : uint 64 =
  case se of
    symbol _ -> 1
    sexp xs -> for (s = 0; x in xs) (s + count x)

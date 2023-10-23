grammar sexpr;

sexp
    : IDENT                # Symbol
    | '(' sexp+ ')'        # Node
    ;

WS      : [\f\t\n\r ]+ -> skip;
IDENT   : [a-z]+ ;



{
module Grammar(SExp(..), sexp) where

import Data.ByteString(ByteString)
import Lexer
}

%tokentype { Token }
%monad { Maybe } { >>= } { pure }

%token
  '('     { OpenParen }
  ')'     { CloseParen }
  IDENT   { Ident $$ }

%name sexp sexp

%%

sexp :: { SExp }
  : IDENT           { Symbol $1 }
  | '(' sexps ')'   { Node (reverse $2) }

sexps :: { [SExp] }
  : sexp            { [$1] }
  | sexps sexp      { $2 : $1 }

{
type Parser a = [Token]

data SExp  = Symbol ByteString | Node [SExp]
happyError _ = Nothing
}

{
module Parser where

import Control.Monad
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import AlexTools
import Lexer
}

%tokentype { Lexeme Token }

%token
  IDENT         { $$@Lexeme { lexemeToken = TokIdent } }
  OBJ           { $$@Lexeme { lexemeToken = TokObject } }
  'def'         { Lexeme { lexemeToken = TokKWDef, lexemeRange = $$ } }
  'case'        { Lexeme { lexemeToken = TokKWCase, lexemeRange = $$ } }
  'of'          { Lexeme { lexemeToken = TokKWOf, lexemeRange = $$ } }
  '='           { Lexeme { lexemeToken = TokEqual, lexemeRange = $$ } }
  '->'          { Lexeme { lexemeToken = TokRightArrow, lexemeRange = $$ } }
  '('           { Lexeme { lexemeToken = TokParenOpen,  lexemeRange = $$ } } 
  ')'           { Lexeme { lexemeToken = TokParenClose, lexemeRange = $$ } }
  '.'           { Lexeme { lexemeToken = TokDot, lexemeRange = $$ } }
  ','           { Lexeme { lexemeToken = TokComma, lexemeRange = $$ } }
  ':'           { Lexeme { lexemeToken = TokColon, lexemeRange = $$ } }
  'v{'          { Lexeme { lexemeToken = TokLayoutStart, lexemeRange = $$ } }
  'v;'          { Lexeme { lexemeToken = TokLayoutSep, lexemeRange = $$ } }
  'v}'          { Lexeme { lexemeToken = TokLayoutEnd, lexemeRange = $$ } }

%monad { Parser }
%lexer { nextToken } { Lexeme { lexemeToken = TokEOF } }

%name expr expr

%%

decl_block ::               { () }
  : 'v{' decls 'v}'         { $2 }
  | 'v{' 'v}'               { () }

decls ::                    { () }
  : decl                    { () }
  | decls 'v;' decl         { () }

decl ::                     { () }
  : 'def' IDENT decl_sig decl_def { () }

decl_sig ::                 { () }
  : '(' IDENT ':' type ')' '->' obj_type { () }

type ::                     { () }
  : IDENT                   { () }

obj_type ::                 { () }
  : OBJ                     { () }

decl_def ::                 { () }
  :                         { () }




expr ::                     { () }
  : IDENT '(' params ')'    { () }

ddl_expr ::                 { () }
  : IDENT                   { () }
  | ddl_expr '.' IDENT      { () }

params                   :: { () }
  : ddl_expr                { () }
  | params ',' ddl_expr     { () }

{    

data ParseError = ParseError
  deriving Show

newtype Parser a = Parser (RW -> (Either ParseError a, RW))

data RW = RW { lastToken :: Maybe (Lexeme Token), nextTokens :: [Lexeme Token] }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser \rw -> (Right a, rw)
  (<*>)  = ap

instance Monad Parser where
  Parser m >>= k = Parser \rw ->
    case m rw of
      (Left err, rw') -> (Left err, rw')
      (Right a, rw') ->
        let Parser m1 = k a
        in m1 rw'

nextToken :: (Lexeme Token -> Parser a) -> Parser a
nextToken k = Parser \rw ->
  case nextTokens rw of
    [] -> error "Missing TokEOF"
    t : ts ->
      let Parser m = k t
      in m RW { lastToken = Just t, nextTokens = ts }

happyError :: Parser a
happyError = Parser \rw -> (Left ParseError, rw)

parserAt :: SourcePos -> Text -> Parser a -> (Either ParseError a, RW)
parserAt loc txt (Parser m) = m rw
  where
  rw = RW { lastToken = Nothing, nextTokens = lexerAt loc txt }

parseFromFile :: FilePath -> Parser a -> IO (Either ParseError a, RW)
parseFromFile file p =
  do txt <- Text.readFile file
     let loc = startPos (Text.pack file)
     pure (parserAt loc txt p)
}


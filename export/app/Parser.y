{
module Parser (
  parseFromFile,
  moduleParser,
  ParseError(..)
) where

import Control.Monad
import Control.Applicative((<|>))
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.IO qualified as Text
import Data.Map(Map)
import Data.Map qualified as Map
import AlexTools
import Daedalus.PP
import Daedalus.Core qualified as Core
import Lexer
import AST
import Quote
import Name qualified as Name
}

%tokentype { Lexeme Token }

%token
  IDENT         { $$@Lexeme { lexemeToken = TokIdent } }
  NUMBER        { $$@Lexeme { lexemeToken = TokNumber } }
  OBJ           { $$@Lexeme { lexemeToken = TokObject } }

  'def'         { Lexeme { lexemeToken = TokKW_def, lexemeRange = $$ } }
  'case'        { Lexeme { lexemeToken = TokKW_case, lexemeRange = $$ } }
  'of'          { Lexeme { lexemeToken = TokKW_of, lexemeRange = $$ } }
  
  '('           { Lexeme { lexemeToken = TokParenOpen,  lexemeRange = $$ } } 
  ')'           { Lexeme { lexemeToken = TokParenClose, lexemeRange = $$ } }
  '['           { Lexeme { lexemeToken = TokBracketOpen,  lexemeRange = $$ } } 
  ']'           { Lexeme { lexemeToken = TokBracketClose, lexemeRange = $$ } }
  '{'           { Lexeme { lexemeToken = TokBraceOpen,  lexemeRange = $$ } } 
  '}'           { Lexeme { lexemeToken = TokBraceClose, lexemeRange = $$ } }

  '='           { Lexeme { lexemeToken = TokEqual, lexemeRange = $$ } }
  '->'          { Lexeme { lexemeToken = TokRightArrow, lexemeRange = $$ } }
  '.'           { Lexeme { lexemeToken = TokDot, lexemeRange = $$ } }
  ','           { Lexeme { lexemeToken = TokComma, lexemeRange = $$ } }
  '::'          { Lexeme { lexemeToken = TokColonColon, lexemeRange = $$ } }
  ':'           { Lexeme { lexemeToken = TokColon, lexemeRange = $$ } }
  '$'           { Lexeme { lexemeToken = TokDollar, lexemeRange = $$ } }

  'v{'          { Lexeme { lexemeToken = TokLayoutStart, lexemeRange = $$ } }
  'v;'          { Lexeme { lexemeToken = TokLayoutSep, lexemeRange = $$ } }
  'v}'          { Lexeme { lexemeToken = TokLayoutEnd, lexemeRange = $$ } }

%monad { Parser }
%lexer { nextToken } { Lexeme { lexemeToken = TokEOF } }

%name moduleParser module

%%

module ::                   { Module }
  : 'v{' sepBy('v;',decl) 'v}'         { Module $2 }

decl ::                     { Decl }
  : 'def' ename '(' ename ':' type ')' '->' 'v{' obj_type 'v}' decl_def { Decl $2 $4 $6 $10 $12 }

decl_def ::                                 { DeclDef }
  : '->' obj_expr                           { DeclDef $2 }

ename ::                                    { Name.Name }
  : IDENT                                   { Name.fromText (lexemeText $1) }

qname ::                                    { (Maybe (Lexeme Token), Lexeme Token) }
  : IDENT                                   { (Nothing, $1) }
  | IDENT '::' IDENT                        { (Just $1, $3) }
  
type ::                                     { Core.Type }
  : atype                                   { $1 }
  | qname listOf1(type_arg)                 {% resolveType $1 $2 }

type_arg ::                                 { Either Core.SizeType Core.Type }
  : NUMBER                                  { Left (Core.TSize (read (Text.unpack (lexemeText $1)))) }
  | atype                                   { Right $1 }

atype                                    :: { Core.Type }
  : '(' type  ')'                           { $2 }
  | '[' arr_or_map ']'                      { $2 }
  | '{' '}'                                 { Core.TUnit }
  | qname                                   {% resolveType $1 [] }


arr_or_map                               :: { Core.Type }
  : type                                    { Core.TArray $1 }
  | type ':' type                           { Core.TMap $1 $3 }
    -- NOTE: This syntax differs from Daedalus, which uses K -> V, but -> is quite special for us.

obj_type ::                                 { Q ExportType }
  : listOf1(OBJ)                            { Q (map objWord $1) } -- XXX: Spaces, etc

obj_expr ::                                 { Q ExportExpr }
  : 'v{' listOf(obj_word) 'v}'              { Q $2 }

obj_word ::                                 { QuoteWord ExportExpr }
  : OBJ                                     { objWord $1 }
  | '$' ename                               { Meta (ExportExpr ExportDefault (DDLVar $2)) }
  | '$' '(' expr ')'                        { Meta $3 }

expr ::                                     { ExportExpr }
  : ename '(' ddl_expr ')'                  { ExportExpr (ExportWith $1) $3 }
  | ddl_expr                                { ExportExpr ExportDefault $1 }

ddl_expr ::                                 { DDLExpr }
  : ename                                   { DDLVar $1 }
  | ename '.' ename                         { DDLSelect $1 $3 }


--------------------------------------------------------------------------------
sepBy1(s,p)                              :: { [p] }
  : p revSep(s,p)                           { $1 : reverse $2 }

sepBy(s,p)                               :: { [p] }
  : sepBy1(s,p)                             { $1 }
  | {- empty -}                             { [] }

revSep(s,p)                              :: { [p] }
  : revSep(s,p) s p                         { $3 : $1 }
  | {- empty -}                             { [] }

listOf1(p)                                :: { [p] }
  : p listOf(p)                              { $1 : $2 }

listOf(p)                                 :: { [p] }
  : revListOf(p)                             { reverse $1 }

revListOf(p)                              :: { [p] }
  : revListOf(p) p                           { $2 : $1 }
  | {- empty -}                              { [] }



{    
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

objWord :: Lexeme Token -> QuoteWord a
objWord = Object . LazyText.fromStrict . lexemeText


--------------------------------------------------------------------------------
-- Parse Errors
--------------------------------------------------------------------------------

data ParseError =
    ParseError SourcePos
  | UndefinedType SourceRange
  | AmbiguousType SourceRange [Text]
  | MalformedType SourceRange
  deriving Show

instance PP ParseError where
  pp err =
    case err of
      ParseError loc -> ppErr loc "Parse error"
      UndefinedType rng -> ppErr (sourceFrom rng) "Undefined type"
      AmbiguousType r ms ->
        ppErr (sourceFrom r) "Ambiguous type, defined in module:" $$
          nest 2 (vcat (map (text . Text.unpack) ms))
      MalformedType rng -> ppErr (sourceFrom rng) "Malformed type"
    where
    ppErr l msg = text (prettySourcePosLong l) <.> ":" <+> msg




--------------------------------------------------------------------------------
-- Parser Monad
--------------------------------------------------------------------------------

newtype Parser a = Parser (ParserState -> (Either ParseError a, ParserState))

-- | Maps module name, to type definitions in it.
type TypeDefs = Map Text (Map Text Core.TDecl)

data ParserState = ParserState {
  parserStartPos  :: SourcePos,
  lastToken       :: Maybe (Lexeme Token),
  nextTokens      :: [Lexeme Token],
  typeDefsByMod   :: TypeDefs,
  typeDefsByName  :: Map Text [Core.TDecl]
}

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
      in m rw { lastToken = Just t, nextTokens = ts }

happyError :: Parser a
happyError = Parser \rw ->
  let rng = fmap (sourceFrom . lexemeRange)
      loc = fromMaybe (parserStartPos rw)
              (rng (lastToken rw) <|> rng (listToMaybe (nextTokens rw)))
  in 
  (Left (ParseError loc), rw)

parserAt :: SourcePos -> TypeDefs -> Text -> Parser a -> Either ParseError a
parserAt loc tyDefs txt (Parser m) = fst (m rw)
  where
  rw =
    ParserState {
      parserStartPos = loc,
      lastToken = Nothing,
      nextTokens = lexerAt loc txt,
      typeDefsByMod = tyDefs,
      typeDefsByName =
        Map.fromListWith (++)
          [ (Core.tnameText (Core.tName d), [d])
          | ds <- Map.elems tyDefs,
            d  <- Map.elems ds
          ]
    }

parseFromFile :: FilePath -> TypeDefs -> Parser a -> IO (Either ParseError a)
parseFromFile file tyDefs p =
  do txt <- Text.readFile file
     let loc = startPos (Text.pack file)
     pure (parserAt loc tyDefs txt p)




--------------------------------------------------------------------------------
-- Resolving Types
--------------------------------------------------------------------------------

-- | Map a name to the corresponding Core type.
resolveType ::
  (Maybe (Lexeme Token), Lexeme Token) ->
  [Either Core.SizeType Core.Type] -> Parser Core.Type
resolveType (q,l) args =
  case q of
    Nothing -> resolveUnqualType l args
    Just m  -> resolveQualType m l args

resolveUnqualType ::
  Lexeme Token -> [Either Core.SizeType Core.Type] -> Parser Core.Type
resolveUnqualType l ts =
  case lexemeText l of
    "bool"    -> noArg Core.TBool
    "float"   -> noArg Core.TFloat
    "double"  -> noArg Core.TDouble
    "stream"  -> noArg Core.TStream
    "int"     -> noArg Core.TInteger
    "uint"    -> numArg Core.TUInt
    "sint"    -> numArg Core.TSInt
    "maybe"   -> valArg Core.TMaybe
    "builder" -> valArg Core.TBuilder
    nm        -> getDecl nm >>= \def -> appType (lexemeRange l) def ts
  where
  bad = Parser \rw -> (Left (MalformedType (lexemeRange l)), rw)
  noArg t =
    case ts of
      [] -> pure t
      _  -> bad
  numArg t =
    case ts of
      [Left n] -> pure (t n)
      _        -> bad
  valArg t =
    case ts of
      [Right n] -> pure (t n)
      _         -> bad
  getDecl nm =
    Parser \rw ->
      let ans = 
            case Map.findWithDefault [] nm (typeDefsByName rw) of
              [def] -> Right def
              []    -> Left (UndefinedType (lexemeRange l))
              ds    -> Left (AmbiguousType (lexemeRange l)
                                [ Core.mNameText (Core.tnameMod (Core.tName d))
                                | d <- ds ])
      in (ans,rw)

resolveQualType ::
  Lexeme Token -> Lexeme Token -> [Either Core.SizeType Core.Type] ->
  Parser Core.Type
resolveQualType q l args = getDecl >>= \def -> appType rng def args
  where
  rng = lexemeRange q <-> lexemeRange l
  getDecl =
    Parser \rw ->
      let mb = Map.lookup (lexemeText l) =<<
                                Map.lookup (lexemeText q) (typeDefsByMod rw)
          ans =
            case mb of
              Nothing -> Left (UndefinedType rng)
              Just a  -> Right a
      in (ans, rw)
        

-- | Make a type application, using the given type definition and arguments.
-- Note that in the Core type declarations, we have rearranged the arguments
-- to separate numeric and value type parameter, so we don't know what the
-- exact original order was, so we accept any permutation that still matches
-- the requirements.  For example, for a type with a numeric parameter `n`
-- and value parameter `a`, we'd accept either `T n a` or `T a n` as it is
-- not ambiguous how to match the arguments.
appType ::
 SourceRange -> Core.TDecl -> [Either Core.SizeType Core.Type] ->
 Parser Core.Type
appType loc decl args0 =
  go (Core.tTParamKNumber decl) (Core.tTParamKValue decl) args0
  where
  go numPs valPs args =
    case (args, numPs, valPs) of
      ([], [], []) ->
        pure (Core.TUser Core.UserType {
          Core.utName = Core.tName decl,
          Core.utNumArgs = [ s | Left s <- args0 ],
          Core.utTyArgs  = [ s | Right s <- args0 ]
        })
      (Left _ : moreArgs, _ : moreNumPs, _) -> go moreNumPs valPs moreArgs
      (Right _ : moreArgs, _, _ : moreValPs) -> go numPs moreValPs moreArgs
      _ -> Parser \rw -> (Left (MalformedType loc), rw)

}
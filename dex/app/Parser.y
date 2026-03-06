{
module Parser (
  parseFromFile,
  moduleParser,
  ParseError(..)
) where

import Control.Monad
import Control.Applicative((<|>))
import Data.Either(partitionEithers)
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void(Void)
import AlexTools
import Daedalus.PP
import Lexer
import Type
import AST
import Quote
import Name
import Monad
}

%tokentype { Lexeme Token }

%token
  IDENT         { $$@Lexeme { lexemeToken = TokIdent } }
  NUMBER        { $$@Lexeme { lexemeToken = TokNumber } }
  OBJ           { $$@Lexeme { lexemeToken = TokObject } }

  LAYOUT_SEP    { Lexeme { lexemeToken = TokLayoutSep, lexemeRange = $$ } }

  'case'        { Lexeme { lexemeToken = TokKW_case, lexemeRange = $$ } }
  'def'         { Lexeme { lexemeToken = TokKW_def, lexemeRange = $$ } }
  'default'     { Lexeme { lexemeToken = TokKW_default, lexemeRange = $$ } }
  'extern'      { Lexeme { lexemeToken = TokKW_extern, lexemeRange = $$ } }
  'for'         { Lexeme { lexemeToken = TokKW_for, lexemeRange = $$ } }
  'import'      { Lexeme { lexemeToken = TokKW_import, lexemeRange = $$ } }
  'in'          { Lexeme { lexemeToken = TokKW_in, lexemeRange = $$ } }
  'init'        { Lexeme { lexemeToken = TokKW_init, lexemeRange = $$ } }
  'of'          { Lexeme { lexemeToken = TokKW_of, lexemeRange = $$ } }
  'return'      { Lexeme { lexemeToken = TokKW_return, lexemeRange = $$ } }
  'type'        { Lexeme { lexemeToken = TokKW_type, lexemeRange = $$ } }
  'using'       { Lexeme { lexemeToken = TokKW_using, lexemeRange = $$ } }

  '('           { Lexeme { lexemeToken = TokParenOpen,  lexemeRange = $$ } } 
  ')'           { Lexeme { lexemeToken = TokParenClose, lexemeRange = $$ } }
  '['           { Lexeme { lexemeToken = TokBracketOpen,  lexemeRange = $$ } } 
  ']'           { Lexeme { lexemeToken = TokBracketClose, lexemeRange = $$ } }
  '{'           { Lexeme { lexemeToken = TokBraceOpen,  lexemeRange = $$ } } 
  '}'           { Lexeme { lexemeToken = TokBraceClose, lexemeRange = $$ } }
  '<'           { Lexeme { lexemeToken = TokLt,  lexemeRange = $$ } } 
  '>'           { Lexeme { lexemeToken = TokGt, lexemeRange = $$ } }

  '='           { Lexeme { lexemeToken = TokEqual, lexemeRange = $$ } }
  '->'          { Lexeme { lexemeToken = TokRightArrow, lexemeRange = $$ } }
  '=>'          { Lexeme { lexemeToken = TokFatRightArrow, lexemeRange = $$ } }
  '.'           { Lexeme { lexemeToken = TokDot, lexemeRange = $$ } }
  ','           { Lexeme { lexemeToken = TokComma, lexemeRange = $$ } }
  '::'          { Lexeme { lexemeToken = TokColonColon, lexemeRange = $$ } }
  ':'           { Lexeme { lexemeToken = TokColon, lexemeRange = $$ } }
  '$'           { Lexeme { lexemeToken = TokDollar, lexemeRange = $$ } }

%monad { Parser }
%lexer { nextToken } { Lexeme { lexemeToken = TokEOF } }
%error.expected

%name moduleParser module

%%
module ::                                   { Name -> Module PName PName }
  : listOf(top_decl)                        { mkModule $1 }

top_decl ::                                 { TopDecl }
  : import_decl                             { TopImport $1 }
  | using_decl                              { TopUsing $1 }
  | extern_decl                             { TopExtern $1 }
  | type_alias_decl                         { TopTypeAlias $1 }
  | export_decl                             { TopDecl $1 }

import_decl ::                              { Roots }
  : 'import' roots                          { $2 }

using_decl ::                               { Loc Name }
  : 'using' ename                           { $2 }

roots ::                                    { Roots }
  : ename                                   { defaultRoot $1 }
  | ename '(' sepBy1(',', ename) ')'        { Roots { rootModule = $1,
                                                      rootNames = $3 } }

extern_decl ::                              { ForeignBlock }
  : 'extern' opt_extern_def
    foreign_block(extern_splice)            { ForeignBlock $3 $2 }

opt_extern_def ::                           { Bool }
  : 'def'                                   { True }
  | {- empty -}                             { False }

type_alias_decl ::                          { [ForeignTypeDecl] }
  : 'type' type_head              
    foreign_block(ename)                    { [ForeignTypeDecl (fst $2) (snd $2) (Just $3)] }
  | 'extern' 'type'
    sepBy1(',', type_head)                  { [ ForeignTypeDecl x y Nothing | (x,y) <- $3 ] }

type_head ::                                { (Loc Name, [Loc Name]) }
  : ename type_alias_params                 { ($1, $2) }

type_alias_params ::                        { [Loc Name] }
  : '<' sepBy1(',',ename) '>'               { $2 }
  | {- empty -}                             { [] }


export_decl ::                              { Decl PName PName }
  : default 'def' ename opt_special_params params
    ':' foreign_type decl_def               { mkDecl $1 $3 $4 $5 $7 $8 }

default ::                                  { Bool }
  : 'default'                               { True }
  | {-empty -}                              { False }

opt_special_params ::                       { ([Loc Name],[(Loc Name,BasicExporterType PName PName)]) }
  : '<' sepBy1(',',special_param) '>'       {% checkSpecialParams $2 }
  | {- empty -}                             { ([],[]) }

special_param ::                            { Either (Loc Name) (Loc Name,BasicExporterType PName PName) }
  : ename ':' fun_type                      { Right ($1,$3) }
  | ename                                   { Left $1 }

params ::                                   { [(Loc Name, Type PName)] }
  : '(' sepBy1(',', param) ')'              { $2 }

param ::                                    { (Loc Name, Type PName) }
  : ename ':' ddl_type                      { ($1,$3) }

fun_type ::                                 { BasicExporterType PName PName }
  : fun_args '=>' foreign_type              { $1 :-> $3 }

fun_args ::                                 { [Type PName] }
  : ddl_type                                { [$1] }
  | '(' sepBy1(',', ddl_type) ')'           { $2 }

decl_def ::                                 { DeclDef PName PName }
  : foreign_code                            { DeclDef $1 }
  | '=' 'extern'                            { DeclExtern }
  | '=' 'case' ename 'of'
    sepBy(LAYOUT_SEP,case_alt)              { DeclCase $3 $5 }
  | '=' loop                                { DeclLoop $2 }

  
loop ::                                     { Loop PName PName }
  : 'init' foreign_block(ename)
    'for' sepBy1(',',ename) 'in' ename
     foreign_code
    'return' foreign_block(extern_splice)   { Loop $2 ($4,$6,$7) $9 }


case_alt ::                                 { (Pat PName, ForeignCode PName PName) }
  : pat foreign_code                        { ($1, $2) }

foreign_code ::                             { ForeignCode PName PName }
  : foreign_block(expr_splice)              { Splice (fmap SpliceCode $1) }
  | '=' expr                                { Direct $2 }

pat ::                                      { Pat PName }
  : ename                                   { PCon $1 Nothing }
  | ename ename                             { PCon $1 (Just ($2,Nothing)) }

ename ::                                    { Loc Name }
  : IDENT                                   { Loc {
                                                locThing = nameFromText (lexemeText $1),
                                                locRange = lexemeRange $1 }
                                            }

qname ::                                    { Loc PName }
  : ename                                   { Unqual `fmap` $1 }
  | ename '::' ename                        { Loc { locRange = locRange $1 <-> locRange $3,
                                                    locThing = Qual (QName { qModule = locThing $1, qName = locThing $3 }) } }
  
ddl_type ::                                 { Type PName }
  : atype                                   { $1 }
  | qname listOf1(type_arg)                 { uncurry (Type $1) (partitionEithers $2) }

type_arg ::                                 { Either (Type PName) Integer }
  : NUMBER                                  { Right (read (Text.unpack (lexemeText $1))) }
  | atype                                   { Left $1 }

atype                                    :: { Type PName }
  : '(' ddl_type  ')'                       { $2 }
  | '[' arr_or_map ']'                      { $2 ($1 <-> $3) }
  | '{' '}'                                 { mkType ($1 <-> $2) "{}" [] }
  | qname                                   { Type $1 [] [] }

arr_or_map                               :: { SourceRange -> Type PName }
  : ddl_type                                { \r -> mkType r "[]" [$1] }
  | ddl_type ':' ddl_type                   { \r -> mkType r "[:]" [$1,$3] }
    -- NOTE: This syntax differs from Daedalus, which uses K -> V, but -> is quite special for us.



foreign_type ::                             { Type PName }
  : qname                                   { Type $1 [] [] }
  | qname '<' sepBy1(',', foreign_type) '>' { Type $1 $3 [] }

expr ::                                     { ExportExpr PName PName }
  : exporter '(' sepBy1(',', ddl_expr) ')'  { ExportExpr (Just $1) $3 Nothing }
  | ddl_expr                                { ExportExpr Nothing [$1] Nothing }

exporter ::                                 { Exporter PName PName }
  : qname opt_exp_args                      { ExportTop $1 [] [] $2 Nothing }

opt_exp_args ::                             { [Exporter PName PName] }
  : '<' sepBy(',',exporter) '>'             { $2 }
  | {- empty -}                             { [] }

ddl_expr ::                                 { DDLExpr }
  : ename                                   { DDLVar $1 }
  | ddl_expr '.' ename                      { DDLSel $1 (StructSelector :. $3) }

expr_splice ::                              { ExportExpr PName PName }
  : ename                                   { ExportExpr Nothing [DDLVar $1] Nothing }
  | '(' expr ')'                            { $2 }

extern_splice ::                            { Void }
  : {- empty -}                             {% externSplice }

foreign_block(s) ::                         { Q s }
  : '->' foreign_words(s)                   { Q (reverse $2) }

foreign_words(s) ::                         { [QuoteWord s] }
  : foreign_words(s) foreign_word(s)        { $2 : $1 }
  | {- empty -}                             { [] }

foreign_word(s) ::                          { QuoteWord s }
  : OBJ                                     { Object (lexemeText $1) }
  | '$' s                                   { Meta $2 } 

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

defaultRoot :: Loc Name -> Roots
defaultRoot m = Roots {
  rootModule = m,
  rootNames  = [ m { locThing = nameFromText "Main" } ]
}

data TopDecl =
    TopImport Roots
  | TopUsing (Loc Name)
  | TopExtern ForeignBlock
  | TopTypeAlias [ForeignTypeDecl]
  | TopDecl (Decl PName PName)


mkModule :: [TopDecl] -> Name -> Module PName PName
mkModule topds nm = Module {
  moduleName = nm,
  moduleRoots = [ r | TopImport r <- topds ],
  moduleUsing = [ u | TopUsing u <- topds ],
  moduleForeign = [ e | TopExtern e <- topds ],
  moduleForeignTypes = [ t | TopTypeAlias ts <- topds, t <- ts ],
  moduleDecls = [ d | TopDecl d <- topds ]
}

mkDecl ::
  Bool ->
  Loc Name ->
  ([Loc Name], [(Loc Name, BasicExporterType PName PName)]) ->
  [(Loc Name, Type PName)] ->
  Type PName ->
  DeclDef PName PName ->
  Decl PName PName
mkDecl d n (tps,fs) params r def = Decl {
  declDefault = d, 
  declName = Unqual <$> n,
  declDDLTParams = tps,   -- both DDL and foreign
  declForeignTParams = [], -- Filled in later
  declFunParams = fs,
  declArg = map fst params,
  declType =  map snd params :-> r,
  declDef = def
}

mkType :: SourceRange -> Text -> [Type PName] -> Type PName
mkType rng nm as = Type tc as []
  where
  tc = Loc { locThing = Unqual (nameFromText nm), locRange = rng }

checkSpecialParams ::
  [Either (Loc Name) (Loc Name, BasicExporterType PName PName)] ->
  Parser ([Loc Name], [(Loc Name, BasicExporterType PName PName)])
checkSpecialParams = checkTPs []
  where
  checkTPs done xs =
    case xs of
      [] -> pure (reverse done, [])
      Left x : more -> checkTPs (x : done) more
      _ -> checkFuns done [] xs
  checkFuns tps funs xs =
    case xs of
      [] -> pure (reverse tps, reverse funs)
      Left x : _ -> reportError (TyParamAfterFunction x)
      Right f : more -> checkFuns tps (f : funs) more




--------------------------------------------------------------------------------
-- Parse Errors
--------------------------------------------------------------------------------

data ParseError =
    LexicalError SourceRange Text
  | ParseError SourcePos
  | ExternSplice SourceRange
  | TyParamAfterFunction (Loc Name)

instance PP ParseError where
  pp err =
    case err of

      LexicalError rng msg -> ppErr (sourceFrom rng) (text (Text.unpack msg))
      
      ParseError loc -> ppErr loc "Parse error"

      ExternSplice x ->
        ppErr (sourceFrom x) "Splices are not allowed in this context"

      TyParamAfterFunction x ->
        ppErr (sourceFrom (locRange x))
          ("Type parameter" <+> quot (locThing x) <+>
              "should be before function parameters.")
    where
    ppErr l msg = text (prettySourcePosLong l) <.> ":" <+> msg
    quot x = "`" <.> pp x <.> "`"

 
--------------------------------------------------------------------------------
-- Parser Monad
--------------------------------------------------------------------------------

type Parser = M () ParserState ParseError

data ParserState = ParserState {
  parserStartPos    :: SourcePos,
  lastToken         :: Maybe (Lexeme Token),
  nextTokens        :: [Lexeme Token]
}

nextToken :: (Lexeme Token -> Parser a) -> Parser a
nextToken k =
  do
    rw <- getState
    case nextTokens rw of
      [] -> error "Missing TokEOF"
      t : ts ->
        case lexemeToken t of
          TokError msg ->
            reportError (LexicalError (lexemeRange t) msg)
          _ ->
            do
              setState rw { lastToken = Just t, nextTokens = ts }
              k t

happyError :: [String] -> Parser a
happyError next = 
  do
    rw <- getState
    let rng = fmap (sourceFrom . lexemeRange)
    let loc = fromMaybe (parserStartPos rw)
                (rng (lastToken rw) <|> rng (listToMaybe (nextTokens rw)))
    reportError (ParseError loc)

externSplice :: Parser a
externSplice =
  do
    rw <- getState
    case lastToken rw of
      Just x  -> reportError (ExternSplice (lexemeRange x))
      Nothing -> error "[BUG] externSplice"

parserAt :: SourcePos -> Text -> Parser a -> Either ParseError a
parserAt loc txt m = fst <$> runMonad m () rw
  where
  rw =
    ParserState {
      parserStartPos = loc,
      lastToken = Nothing,
      nextTokens = lexerAt loc txt
    }

parseFromFile ::
  FilePath -> Parser a -> IO (Either ParseError a)
parseFromFile file p =
  do
    txt <- Text.readFile file
    let loc = startPos (Text.pack file)
    pure (parserAt loc txt p)

}
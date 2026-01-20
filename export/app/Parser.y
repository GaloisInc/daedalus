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
import Data.Set qualified as Set
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Void(Void)
import AlexTools
import Daedalus.PP
import Daedalus.Core qualified as Core
import Lexer
import AST
import Quote
import Name qualified as Name
import Daedalus.Driver
}

%tokentype { Lexeme Token }

%token
  IDENT         { $$@Lexeme { lexemeToken = TokIdent } }
  NUMBER        { $$@Lexeme { lexemeToken = TokNumber } }
  OBJ           { $$@Lexeme { lexemeToken = TokObject } }

  'case'        { Lexeme { lexemeToken = TokKW_case, lexemeRange = $$ } }
  'def'         { Lexeme { lexemeToken = TokKW_def, lexemeRange = $$ } }
  'import'      { Lexeme { lexemeToken = TokKW_import, lexemeRange = $$ } }
  'of'          { Lexeme { lexemeToken = TokKW_of, lexemeRange = $$ } }
  'extern'      { Lexeme { lexemeToken = TokKW_extern, lexemeRange = $$ } }
  
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
%error.expected

%name moduleParser module

%%

module ::                                   { Module }
  : 'v{' early_decls late_decls 'v}'        { mkModule $2 $3 }

early_decls ::                              { [EarlyDecl] }
  : listOf1(early_decl)                     {% setTypeEnv $1 }

early_decl ::                               { EarlyDecl }
  : 'import' entries 'v;'                   { TopImport $2 }
  | foreign_decl 'v;'                       { EarlyForeign $1 }

late_decls ::                               { [LateDecl] }
  : decl listOf(late_decl)                  { TopDef $1 : $2 }

late_decl ::                                { LateDecl }
  : 'v;' decl                               { TopDef $2 }
  | 'v;' foreign_decl                       { LateForeign $2 }

foreign_decl ::                             { Q Void }
  : 'extern' '->' 'v{' listOf(OBJ) 'v}'     { Q (map objWord $4) }


entries ::                                  { Entries }
  : ename                                   { defaultEntry $1 }
  | ename '(' sepBy1(',', ename) ')'        { Entries { entryModule = $1, entryNames = $3 } }

decl ::                     { Decl }
  : 'def' ename '(' ename ':' type ')' '->' obj_type decl_def { Decl $2 $4 $6 $9 $10 }

decl_def ::                                 { DeclDef }
  : '->' obj_expr                           { DeclDef $2 }
  | '=' 'case' ename 'of' 'v{' sepBy('v;',case_alt) 'v}' { DeclCase $3 $6 }

case_alt ::                                 { (Pat, Q ExportExpr) }
  : pat '->' obj_expr                       { ($1, $3) }

pat ::                                      { Pat }
  : ename                                   { PCon $1 Nothing }
  | ename ename                             { PCon $1 (Just $2) }

ename ::                                    { LName }
  : IDENT                                   { LName { nameName = Name.fromText (lexemeText $1), nameRange = lexemeRange $1 }  }

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
  : 'v{' listOf(OBJ) 'v}'                   { Q (map objWord $2) } -- XXX: Spaces, etc

obj_expr ::                                 { Q ExportExpr }
  : 'v{' listOf(obj_word) 'v}'              { Q $2 }

obj_word ::                                 { QuoteWord ExportExpr }
  : OBJ                                     { objWord $1 }
  | '$' ename                               { Meta (ExportExpr ExportDefault (DDLVar $2)) }
  | '$' '(' expr ')'                        { Meta $3 }

expr ::                                     { ExportExpr }
  : ename ddl_expr                          { ExportExpr (ExportWith $1) $2 }
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
objWord = Object . lexemeText

defaultEntry :: LName -> Entries
defaultEntry m = Entries {
  entryModule = m,
  entryNames  = [ m { nameName = Name.fromText "Main" } ]
}

data EarlyDecl = EarlyForeign (Q Void) | TopImport Entries
data LateDecl  = LateForeign (Q Void) | TopDef Decl

mkModule :: [EarlyDecl] -> [LateDecl] -> Module
mkModule es ds =
  Module {
    moduleEntries = [ e | TopImport e <- es ],
    moduleForeign = [ x | EarlyForeign x <- es ] ++ [ x | LateForeign x <- ds ],
    moduleDecls   = [ d | TopDef d <- ds ]
  }


--------------------------------------------------------------------------------
-- Parse Errors
--------------------------------------------------------------------------------

data ParseError =
    LexicalError SourceRange Text
  | ParseError SourcePos
  | UndefinedType SourceRange
  | AmbiguousType SourceRange [Text]
  | MalformedType SourceRange
  deriving Show

instance PP ParseError where
  pp err =
    case err of
      LexicalError rng msg -> ppErr (sourceFrom rng) (text (Text.unpack msg))
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

newtype Parser a = Parser (ParserState -> Daedalus (Either ParseError (a, ParserState)))

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
  pure a = Parser \rw -> pure (Right (a, rw))
  (<*>)  = ap

instance Monad Parser where
  Parser m >>= k = Parser \rw ->
    do
      res1 <- m rw
      case res1 of
        Left err -> pure (Left err)
        Right (a, rw') ->
          do
            let Parser m1 = k a
            m1 rw'

nextToken :: (Lexeme Token -> Parser a) -> Parser a
nextToken k = Parser \rw ->
  case nextTokens rw of
    [] -> error "Missing TokEOF"
    t : ts ->
      case lexemeToken t of
        TokError msg ->
          pure (Left (LexicalError (lexemeRange t) msg))
        _ ->
          do
            let Parser m = k t
            -- ddlPrint (lexemeToken t)
            m rw { lastToken = Just t, nextTokens = ts }

happyError :: [String] -> Parser a
happyError next = Parser \rw ->
  do
    let rng = fmap (sourceFrom . lexemeRange)
    let loc = fromMaybe (parserStartPos rw)
                (rng (lastToken rw) <|> rng (listToMaybe (nextTokens rw)))
    pure (Left (ParseError loc))

parserAt :: SourcePos -> Text -> Parser a -> Daedalus (Either ParseError a)
parserAt loc txt (Parser m) = fmap fst <$> m rw
  where
  rw =
    ParserState {
      parserStartPos = loc,
      lastToken = Nothing,
      nextTokens = lexerAt loc txt,
      typeDefsByMod = error "[BUG] `parserAt`: typeDefsByMod",
      typeDefsByName = error "[BUG] `parserAt`: typeDefsByNames"
    }

parseFromFile :: FilePath -> Parser a -> Daedalus (Either ParseError a)
parseFromFile file p =
  do txt <- ddlIO (Text.readFile file)
     let loc = startPos (Text.pack file)
     parserAt loc txt p



--------------------------------------------------------------------------------
-- Load Daedalus
--------------------------------------------------------------------------------

setTypeEnv :: [EarlyDecl] -> Parser [EarlyDecl]
setTypeEnv ents = Parser \rw ->
  do
    coreM <- loadDaedalus [ i | TopImport i <- ents ]
    let tyDefs = getTypeDecls coreM
    pure (Right 
           (  ents,
              rw {
                typeDefsByMod = tyDefs,
                typeDefsByName =
                  Map.fromListWith (++)
                    [ (Core.tnameText (Core.tName d), [d])
                    | ds <- Map.elems tyDefs,
                      d  <- Map.elems ds
                    ]
              }))


loadDaedalus :: [Entries] -> Daedalus Core.Module
loadDaedalus ents =
  do
    let toText = Name.toText . nameName
    let ms = Set.toList (Set.fromList (map (toText . entryModule) ents))
    mapM_ ddlLoadModule ms
    let entries =
          [ (toText (entryModule e), toText x) | e <- ents, x <- entryNames e ]
    let specMod = "DaedalusMain"
    passSpecialize specMod entries
    passCore specMod
    ddlGetAST specMod astCore

-- | Get the type declarations, indexed by original module name.
getTypeDecls :: Core.Module -> Map Text (Map Text Core.TDecl)
getTypeDecls m =
  Map.fromListWith Map.union [
    (mo, Map.singleton (Core.tnameText nm) d) |
    r <- Core.mTypes m,
    d <- Core.recToList r,
    let nm = Core.tName d
        mo = Core.mNameText (Core.tnameMod nm)
  ]

  

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
  bad = Parser \_ -> pure (Left (MalformedType (lexemeRange l)))
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
      pure
        case Map.findWithDefault [] nm (typeDefsByName rw) of
          [def] -> Right (def, rw)
          []    -> Left (UndefinedType (lexemeRange l))
          ds    -> Left (AmbiguousType (lexemeRange l)
                            [ Core.mNameText (Core.tnameMod (Core.tName d))
                            | d <- ds ])

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
      in
        pure
          case mb of
            Nothing -> Left (UndefinedType rng)
            Just a  -> Right (a,rw)
        

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
      _ -> Parser \_ -> pure (Left (MalformedType loc))

}
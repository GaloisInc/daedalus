{
module Parser (
  parseFromFile,
  moduleParser,
  ParseError(..)
) where

import Control.Monad
import Control.Applicative((<|>))
import Data.Maybe(listToMaybe, fromMaybe, mapMaybe)
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.IO qualified as Text
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Void(Void)
import Data.Foldable(toList)
import AlexTools
import Daedalus.PP
import Daedalus.Core qualified as Core
import Lexer
import Type
import AST
import Quote
import Name(Name,LName(..))
import Name qualified as Name
import Daedalus.Driver (Daedalus)
import Daedalus.Driver qualified as Daedalus

}

%tokentype { Lexeme Token }

%token
  IDENT         { $$@Lexeme { lexemeToken = TokIdent } }
  NUMBER        { $$@Lexeme { lexemeToken = TokNumber } }
  OBJ           { $$@Lexeme { lexemeToken = TokObject } }

  LAYOUT_SEP    { Lexeme { lexemeToken = TokLayoutSep, lexemeRange = $$ } }

  'case'        { Lexeme { lexemeToken = TokKW_case, lexemeRange = $$ } }
  'def'         { Lexeme { lexemeToken = TokKW_def, lexemeRange = $$ } }
  'daedalus'    { Lexeme { lexemeToken = TokKW_daedalus, lexemeRange = $$ } }
  'default'     { Lexeme { lexemeToken = TokKW_default, lexemeRange = $$ } }
  'extern'      { Lexeme { lexemeToken = TokKW_extern, lexemeRange = $$ } }
  'for'         { Lexeme { lexemeToken = TokKW_for, lexemeRange = $$ } }
  'import'      { Lexeme { lexemeToken = TokKW_import, lexemeRange = $$ } }
  'in'          { Lexeme { lexemeToken = TokKW_in, lexemeRange = $$ } }
  'init'        { Lexeme { lexemeToken = TokKW_init, lexemeRange = $$ } }
  'of'          { Lexeme { lexemeToken = TokKW_of, lexemeRange = $$ } }
  'return'      { Lexeme { lexemeToken = TokKW_return, lexemeRange = $$ } }
  'type'        { Lexeme { lexemeToken = TokKW_type, lexemeRange = $$ } }

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
module ::                                   { Module }
  : early_decls late_decls                  { mkModule $1 $2 }

early_decls ::                              { [EarlyDecl] }
  : listOf1(early_decl)                     {% setTypeEnv $1 }

early_decl ::                               { EarlyDecl }
  : import_decl                             { TopOther $1 }
  | extern_decl                             { TopExtern $1 }
  | type_alias_decl                         { TopTypeAlias $1 }

late_decls ::                               { [LateDecl] }
  : export_decl listOf(late_decl)           { TopOther $1 : $2 }

late_decl ::                                { LateDecl }
  : export_decl                             { TopOther $1 }
  | extern_decl                             { TopExtern $1 }
  | type_alias_decl                         { TopTypeAlias $1 }


import_decl ::                              { Roots }
  : 'daedalus' roots                        { $2 }

roots ::                                    { Roots }
  : ename                                   { defaultRoot $1 }
  | ename '(' sepBy1(',', ename) ')'        { Roots { rootModule = $1, rootNames = $3 } }


extern_decl ::                              { Q Void }
  : 'extern' foreign_block(extern_splice)   { $2 }

type_alias_decl ::                          { ForeignTypeDecl }
  : 'type' ename type_alias_params              
    foreign_block(ename)                    {% addForeignTypeDecl $2 $3 $4 }

type_alias_params ::                        { [LName] }
  : '<' sepBy1(',',ename) '>'               { $2 }
  | {- empty -}                             { [] }


export_decl ::                              { Decl }
  : default 'def' ename opt_tparams listOf1(param)
    ':' foreign_type decl_def               {% mkDecl $1 $3 $5 $7 $8 }

default ::                                  { Bool }
  : 'default'                               { True }
  | {-empty -}                              { False }

opt_tparams ::                              { () }
  : '<' sepBy1(',',ename) '>'               {% setTParams $2 }
  | {- empty -}                             {% setTParams [] }

param ::                                    { Param }
  : '(' ename ':' fun_type ')'              { FunParam $2 $4 }
  | '(' ename ':' type     ')'              { ValParam $2 $4 }

fun_type ::                                 { BasicExporterType }
  : type '=>' foreign_type                  { $1 :-> $3 }

decl_def ::                                 { DeclDef }
  : foreign_code                            { DeclDef $1 }
  | '=' 'extern'                            { DeclExtern }
  | '=' 'case' ename 'of'
    sepBy(LAYOUT_SEP,case_alt)              { DeclCase $3 $5 }
  | '=' loop                                { DeclLoop $2 }

  
loop ::                                     { Loop }
  : 'init' foreign_block(ename)
    'for' sepBy1(',',ename) 'in' ename
     foreign_code
    'return' foreign_block(extern_splice)   { Loop $2 ($4,$6,$7) $9 }


case_alt ::                                 { (Pat, ForeignCode) }
  : pat foreign_code                        { ($1, $2) }

foreign_code ::                             { ForeignCode }
  : foreign_block(expr_splice)              { Splice $1 }
  | '=' expr                                { Direct $2 }

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



foreign_type ::                             { Type }
  : ename                                   {% resolveForeign $1 [] }
  | ename '<' sepBy1(',', foreign_type) '>' {% resolveForeign $1 $3 }

expr ::                                     { ExportExpr }
  : exporter ddl_expr                       { ExportExpr (Just $1) $2 Nothing }
  | ddl_expr                                { ExportExpr Nothing $1 Nothing }

exporter ::                                 { Exporter }
  : aexporter                               { $1 }
  | exporter aexporter                      { ExportApp $1 $2 }

aexporter ::                                { Exporter }
  : ename                                   { ExportTop $1 [] [] }
  | '(' exporter ')'                        { $2 }

ddl_expr ::                                 { DDLExpr }
  : ename                                   { DDLExpr $1 [] }
  | ename '.' ename                         { DDLExpr $1 [StructSelector :. $3] }

expr_splice ::                              { ExportExpr }
  : ename                                   { ExportExpr Nothing (DDLExpr $1 []) Nothing }
  | '(' expr ')'                            { $2 }

extern_splice ::                            { Void }
  : {- empty -}                             {% externSplice }

foreign_block(s) ::                         { Q s }
  : '->' foreign_words(s)                   { Q (reverse $2) }

foreign_words(s) ::                         { [QuoteWord s] }
  : foreign_words(s) foreign_word(s)        { $2 : $1 }
  | {- empty -}                             { [] }

foreign_word(s) ::                          { QuoteWord s }
  : OBJ                                     { objWord $1 }
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

objWord :: Lexeme Token -> QuoteWord a
objWord = Object . lexemeText

defaultRoot :: LName -> Roots
defaultRoot m = Roots {
  rootModule = m,
  rootNames  = [ m { nameName = Name.fromText "Main" } ]
}

data TopDecl a = TopExtern (Q Void) | TopTypeAlias ForeignTypeDecl | TopOther a
type EarlyDecl = TopDecl Roots
type LateDecl  = TopDecl Decl

data Param     = FunParam LName BasicExporterType | ValParam LName Core.Type

mkModule :: [EarlyDecl] -> [LateDecl] -> Module
mkModule es ds =
  Module {
    moduleRoots         = [ e | TopOther e <- es ],
    moduleForeign       = [ x | TopExtern x <- es ] ++
                          [ x | TopExtern x <- ds ],
    moduleForeignTypes  = [ x | TopTypeAlias x <- es ] ++
                          [ x | TopTypeAlias x <- ds ],
    moduleDecls         = [ d | TopOther d <- ds ]
  }

mkDecl ::
  Bool {- ^ Default? -} ->
  LName {- ^ Definition name -} ->
  [Param] {- ^ Parameters -} ->
  Type {- ^ Target type to export to -} ->
  DeclDef {- ^ Exporter definition -} ->
  Parser Decl
mkDecl isDefault f params res def =
  Parser \rw ->
    pure
    case unusedTParams rw of
      [] ->
        case checkFunParams [] params of
          Left err -> Left err
          Right (fs,x,t) ->
            Right
              ( Decl {
                  declDefault = isDefault,
                  declName = f,
                  declDDLTParams = ddlTParams rw,
                  declForeignTParams = foreignTParams rw,
                  declFunParams = fs,
                  declArg = x,
                  declArgType = t,
                  declResType = res,
                  declDef = def
                }, rw
              )
      x : _ -> Left (UnusedTParam x)
  where
  checkFunParams fs ps =
    case ps of
      [] -> Left (MissingValueParameter f)
      FunParam f t : more -> checkFunParams ((f,t) : fs) more
      [ValParam x t] -> Right (reverse fs, x, t)
      ValParam x _ : ValParam y _ : _ -> Left (MultipleValueParameters x y)
      ValParam x _ : FunParam f _ : _ -> Left (FunctionAfterValue x f)
        
-- | Check if the given name corresponds to a type parameter,
-- and if so, remove it from the unused list and return it.
isUnusedTParam :: LName -> Parser (Maybe LName)
isUnusedTParam x = Parser \rw ->
  pure $
  case break (\p -> nameName p == nameName x) (unusedTParams rw) of
    (as, b : bs) -> Right (Just b, rw { unusedTParams = as ++ bs })
    _ -> Right (Nothing, rw)



--------------------------------------------------------------------------------
-- Parse Errors
--------------------------------------------------------------------------------

data ParseError =
    LexicalError SourceRange Text
  | ParseError SourcePos
  | UndefinedType SourceRange Text
  | AmbiguousType SourceRange [Text]
  | MalformedType SourceRange
  | MultipleDefs Name SourceRange SourceRange
  | UnusedTParam LName
  | UndefinedForeignTypeParameter LName
  | UndefinedForeignType LName
  | ExternSplice SourceRange
  | MissingValueParameter LName
  | MultipleValueParameters LName LName
  | FunctionAfterValue LName LName


instance PP ParseError where
  pp err =
    case err of
      LexicalError rng msg -> ppErr (sourceFrom rng) (text (Text.unpack msg))
      ParseError loc -> ppErr loc "Parse error"
      UndefinedType rng txt -> ppErr (sourceFrom rng) ("Undefined type" <+> quot txt)
      AmbiguousType r ms ->
        ppErr (sourceFrom r) "Ambiguous type, defined in module:" $$
          nest 2 (vcat (map (text . Text.unpack) ms))
      MalformedType rng -> ppErr (sourceFrom rng) "Malformed type"
      MultipleDefs x r1 r2 ->
        ppErr (sourceFrom r2) ("Multiple definitions for" <+> quot x <.> ":") $$
          nest 2 ("Also defined here:" <+> text (prettySourcePos (sourceFrom r1)))
      UndefinedForeignTypeParameter x ->
        ppErr (sourceFrom (nameRange x)) ("Undefined type parameter:" <+> quot (nameName x))
      UndefinedForeignType x ->
        ppErr (sourceFrom (nameRange x)) ("Undefined foreign type" <+> quot (nameName x))
      UnusedTParam x ->
        ppErr (sourceFrom (nameRange x)) ("Unused type parameter" <+> quot (nameName x))
      ExternSplice x -> ppErr (sourceFrom x) "Splices are not allowed in this context"
      MissingValueParameter f ->
        ppErr (sourceFrom (nameRange f))
          ("The definitions of" <+> quot (nameName f) <+> "does not have a value to export.")
      MultipleValueParameters x y ->
        ppErr (sourceFrom (nameRange y))
          ("Multiple export values:" $$
            nest 2 ("Other export value:" <+> text (prettySourcePos (sourceFrom (nameRange x)))))
      FunctionAfterValue x f ->
        ppErr (sourceFrom (nameRange f))
          "Function parameters may not appear after the export value."
    where
    ppErr l msg = text (prettySourcePosLong l) <.> ":" <+> msg
    quot x = "`" <.> pp x <.> "`"




--------------------------------------------------------------------------------
-- Parser Monad
--------------------------------------------------------------------------------

newtype Parser a = Parser (ParserState -> Daedalus (Either ParseError (a, ParserState)))

-- | Maps module name, to type definitions in it.
type TypeDefs = Map Text (Map Text Core.TDecl)

data ParserState = ParserState {
  parserStartPos    :: SourcePos,
  lastToken         :: Maybe (Lexeme Token),
  nextTokens        :: [Lexeme Token],
  typeDefsByMod     :: TypeDefs,
  typeDefsByName    :: Map Text [Core.TDecl],
  foreignTypeDecls  :: Map Name ForeignTypeDecl,
  unusedTParams     :: [ LName ],
  ddlTParams        :: [ (LName, Core.TParam) ],
  foreignTParams    :: [ LName ]
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
            -- Daedalus.ddlPutStrLn (show (lexemeToken t) ++ ": " ++ show (lexemeText t) )
            m rw { lastToken = Just t, nextTokens = ts }

happyError :: [String] -> Parser a
happyError next = Parser \rw ->
  do
    let rng = fmap (sourceFrom . lexemeRange)
    let loc = fromMaybe (parserStartPos rw)
                (rng (lastToken rw) <|> rng (listToMaybe (nextTokens rw)))
    pure (Left (ParseError loc))

externSplice :: Parser a
externSplice = Parser \rw ->
  case lastToken rw of
    Just x -> pure (Left (ExternSplice (lexemeRange x)))
    Nothing -> error "[BUG] externSplice"

parserAt :: SourcePos -> Text -> Parser a -> Daedalus (Either ParseError (a,Map Core.TName Core.TDecl))
parserAt loc txt (Parser m) =
  do
    res <- m rw
    let getVal (a,s) = (a, Map.fromList [ (Core.tName d, d) | ds <- Map.elems (typeDefsByName s), d <- ds ])
    pure (getVal <$> res)
  where
  rw =
    ParserState {
      parserStartPos = loc,
      lastToken = Nothing,
      nextTokens = lexerAt loc txt,
      typeDefsByMod = error "[BUG] `parserAt`: typeDefsByMod",
      typeDefsByName = error "[BUG] `parserAt`: typeDefsByNames",
      foreignTypeDecls = mempty,
      unusedTParams = mempty,
      ddlTParams = mempty,
      foreignTParams = mempty
    }

parseFromFile ::
  FilePath -> Parser a -> Daedalus (Either ParseError (a, Map Core.TName Core.TDecl))
parseFromFile file p =
  do txt <- Daedalus.ddlIO (Text.readFile file)
     let loc = startPos (Text.pack file)
     parserAt loc txt p



--------------------------------------------------------------------------------
-- Load Daedalus
--------------------------------------------------------------------------------

setTypeEnv :: [EarlyDecl] -> Parser [EarlyDecl]
setTypeEnv ents = Parser \rw ->
  do
    coreM <- loadDaedalus [ i | TopOther i <- ents ]
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


loadDaedalus :: [Roots] -> Daedalus Core.Module
loadDaedalus ents =
  do
    let toText = Name.toText . nameName
    let ms = Set.toList (Set.fromList (map (toText . rootModule) ents))
    mapM_ Daedalus.ddlLoadModule ms
    let entries =
          [ (toText (rootModule e), toText x) | e <- ents, x <- rootNames e ]
    let specMod = "DaedalusMain"
    Daedalus.passSpecialize specMod entries
    Daedalus.passCore specMod
    Daedalus.ddlGetAST specMod Daedalus.astCore

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
-- Resolving Foreign Types
--------------------------------------------------------------------------------
addForeignTypeDecl :: LName -> [LName] -> Q LName -> Parser ForeignTypeDecl
addForeignTypeDecl f xs def = Parser \rw ->
  let fs = foreignTypeDecls rw
      nm = nameName f
      thisRng = nameRange f
  in
  pure
  case Map.lookup nm fs of
    Just def -> Left (MultipleDefs nm (nameRange (ftName def)) thisRng)
    Nothing  ->
      let ps = Set.fromList (map nameName xs)
          check x = if nameName x `Set.member` ps
                      then Nothing
                      else Just (UndefinedForeignTypeParameter x)
      in
        case mapMaybe check (toList def) of
          err : _ -> Left err
          [] ->
            let
              d = ForeignTypeDecl {
                    ftName = f,
                    ftParams = xs,
                    ftDef = def
                  }
            in Right (d, rw { foreignTypeDecls = Map.insert nm d fs })



isForeignTParam :: LName -> Parser (Maybe LName)
isForeignTParam x =
  do
    mb <- isUnusedTParam x
    case mb of
      Nothing ->
        Parser \rw ->
          pure $ Right
          case break (\p -> nameName p == nameName x) (foreignTParams rw) of
            (_, b : _) -> (Just b, rw)
            _ -> (Nothing, rw)
      Just yes ->
        Parser \rw ->
          pure (Right (Just x, rw { foreignTParams = yes : foreignTParams rw }))



resolveForeign :: LName -> [Type] -> Parser Type
resolveForeign x fs =
  do
    mb <- isForeignTParam x
    case mb of
      Just yes ->
        case fs of
          [] -> pure (TVar yes)
          _  -> Parser \_ -> pure (Left (MalformedType (nameRange x)))
      Nothing ->
        Parser \rw ->
          pure
          case Map.lookup (nameName x) (foreignTypeDecls rw) of
            Nothing -> Left (UndefinedForeignType x)
            Just def ->
              let have = length fs
                  need = length (ftParams def)
              in if have == need then Right (Type x fs, rw)
                                 else Left (MalformedType (nameRange x))


--------------------------------------------------------------------------------
-- Resolving Daedalus Types
--------------------------------------------------------------------------------


isDDLTParam :: LName -> Parser (Maybe Core.TParam)
isDDLTParam x =
  do
    mb <- isUnusedTParam x
    case mb of
      Nothing ->
        Parser \rw ->
          pure $ Right
          case [ t | (a,t) <- ddlTParams rw, nameName x == nameName a ] of
            [] -> (Nothing, rw)
            a : _ -> (Just a, rw)
      Just p ->
        Parser \rw ->
          pure $ Right
            let t = Core.TP (length (ddlTParams rw))
            in (Just t, rw { ddlTParams = (p,t) : ddlTParams rw })

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
    nm        ->
      do
        mb <- isDDLTParam LName { nameName = Name.fromText (lexemeText l), nameRange = lexemeRange l }
        case mb of
          Nothing -> getDecl nm >>= \def -> appType (lexemeRange l) def ts
          Just yes -> pure (Core.TParam yes)
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
          []    -> Left (UndefinedType (lexemeRange l) (lexemeText l))
          ds    -> Left (AmbiguousType (lexemeRange l)
                            [ Core.mNameText (Core.tnameMod (Core.tName d))
                            | d <- ds ])

setTParams :: [LName] -> Parser ()
setTParams xs = Parser \rw ->
  pure
    case Map.minView bad of
      Just (err,_) -> Left err
      Nothing ->
        Right ((), rw { unusedTParams = xs, ddlTParams = [], foreignTParams = [] })
  where
  bad = Map.mapMaybeWithKey hasRep
      $ Map.fromListWith (++) [ (nameName x, [nameRange x]) | x <- xs ]
  hasRep x xs =
    case xs of
      a : b : _ -> Just (MultipleDefs x a b)
      _ -> Nothing

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
            Nothing -> Left (UndefinedType rng (lexemeText q <> "." <> lexemeText l))
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
{-# Language OverloadedStrings, ImportQualifiedPost #-}
module Daedalus.VM.Backend.Rust.Lang (
  module Daedalus.VM.Backend.Rust.Lang,
  module Language.Rust.Syntax,
  module Language.Rust.Data.Ident,
  module Language.Rust.Pretty
) where

import Data.ByteString(ByteString)
import Data.ByteString qualified as BS
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Maybe(maybeToList,mapMaybe)
import Data.Char(isAlpha,toLower,isUpper,toUpper)
import Numeric(showHex)
import Data.List(intersperse,groupBy)
import Language.Rust.Syntax
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Pretty
import Daedalus.Panic

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

simplePath :: Ident -> Path ()
simplePath n = simplePath' [n]

simplePath' :: [Ident] -> Path ()
simplePath' ns = Path False [ PathSegment n Nothing () | n <- ns ] ()

pathWithGen :: [Ident] -> [GenericArg ()] -> Path ()
pathWithGen ns0 gs = Path False (go ns0) ()
  where
    go ns =
      case ns of
        [] -> panic "pathWithTypes" ["empty names"]
        [lst] -> [PathSegment lst pathParams ()]
        n:t -> PathSegment n Nothing () : go t
    pathParams =
      case gs of
        [] -> Nothing
        _  -> Just (AngleBracketed gs [] ())


pathWithTypes :: [Ident] -> [Ty ()] -> Path ()
pathWithTypes ns0 tys = pathWithGen ns0 (map TypeArg tys)

typeQualifiedExpr :: Ty () -> Path () -> Expr ()
typeQualifiedExpr ty n = PathExpr [] (Just (QSelf ty 0)) n ()


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

pathType :: Path () -> Ty ()
pathType path = PathTy Nothing path ()

simpleType :: Ident -> Ty ()
simpleType = pathType . simplePath

tBool :: Ty ()
tBool = simpleType "bool"

tIndexed :: String -> Integer -> Ty ()
tIndexed pref i = simpleType (mkIdent (pref ++ show i))

tU :: Integer -> Ty ()
tU = tIndexed "u"

tUsize :: Ty ()
tUsize = simpleType "usize"

tI :: Integer -> Ty ()
tI = tIndexed "i"

tF :: Integer -> Ty ()
tF = tIndexed "f"

tTuple :: [Ty ()] -> Ty ()
tTuple xs = TupTy xs ()

tVec :: Ty () -> Ty ()
tVec elT = pathType (pathWithTypes ["Vec"] [elT])

tOption :: Ty () -> Ty ()
tOption elT = pathType (pathWithTypes ["Option"] [elT])

tRef :: Maybe (Lifetime ()) -> Ty () -> Ty ()
tRef l ty = Rptr l Immutable ty ()

noGenerics :: Generics ()
noGenerics = mkGenerics [] noWhereClause

noWhereClause :: WhereClause ()
noWhereClause = WhereClause [] ()

mkGenerics ::
  [GenericParam ()] ->
  WhereClause () ->
  Generics ()
mkGenerics ps wc = Generics ps wc ()

tyParam :: Ident -> GenericParam ()
tyParam a = TypeParam [] a [] Nothing ()

constGeneric :: Expr () -> GenericArg ()
constGeneric = ConstArg

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

identPat' :: Mutability -> Ident -> Pat ()
identPat' mut ident = IdentP bindingMode ident Nothing ()
  where bindingMode = ByValue mut

identPat :: Ident -> Pat ()
identPat = identPat' Immutable

litPat :: Lit () -> Pat ()
litPat lit = LitP (litExpr lit) ()

conPat :: Path () -> [Pat ()] -> Pat ()
conPat c ps
  | null ps   = PathP Nothing c ()
  | otherwise = TupleStructP c ps ()

tuplePat :: [Pat ()] -> Pat ()
tuplePat ps = TupleP ps ()

nonePat :: Pat ()
nonePat = conPat (simplePath "None") []

somePat :: Pat () -> Pat ()
somePat p = conPat (simplePath "Some") [p]

wildPat :: Pat ()
wildPat = WildP ()

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

block :: [Stmt ()] -> Block ()
block xs = Block xs Normal ()

localLet ::
  [Ident] {- ^ Disable these warnings -} ->
  Ident {- ^ Name -} ->
  Maybe (Ty ()) ->
  Expr () ->
  Stmt ()
localLet allow rname ty def =
  Local (identPat rname) ty (Just def) attrs ()
  where
    attrs = map disableWarning allow


localLetMut ::
  [Ident] {- ^ Disable these warnings -} ->
  Ident {- ^ Name -} ->
  Maybe (Ty ()) ->
  Expr () ->
  Stmt ()
localLetMut allow rname ty def =
  Local (identPat' Mutable rname) ty (Just def) attrs ()
  where
    attrs = map disableWarning allow

expr_ :: Expr () -> Stmt ()
expr_ e = Semi e ()

expr :: Expr () -> Stmt ()
expr e = NoSemi e ()

itemStmt :: Item () -> Stmt ()
itemStmt i = ItemStmt i ()

assign :: Expr () -> Expr () -> Stmt ()
assign lhs rhs = expr_ (Assign [] lhs rhs ())

continue :: Stmt ()
continue = expr (Continue [] Nothing ())

ret_ :: Stmt ()
ret_ = expr_ (Ret [] Nothing ())

ret :: Expr () -> Stmt ()
ret x = expr (Ret [] (Just x) ())

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

intLit' :: Suffix -> Integer -> Lit ()
intLit' s i = Int Dec i s ()

intLit :: Integer -> Lit ()
intLit = intLit' Unsuffixed

boolLit :: Bool -> Lit ()
boolLit b = Bool b Unsuffixed ()

floatLit :: Double -> Lit ()
floatLit f = Float f Unsuffixed ()

strLit :: String -> Lit ()
strLit x = Str x Cooked Unsuffixed ()

-- | Make a cooked byte literal 
bytesLit :: ByteString -> Lit ()
bytesLit x = ByteStr (BS.unpack x) Cooked Unsuffixed ()

litExpr :: Lit () -> Expr ()
litExpr l = Lit [] l ()

pathExpr :: Path () -> Expr ()
pathExpr p = PathExpr [] Nothing p ()

identExpr :: Ident -> Expr ()
identExpr = pathExpr . simplePath

blockExpr' :: Block () -> Expr ()
blockExpr' b = BlockExpr [] b Nothing ()

blockExpr :: [Stmt ()] -> Expr ()
blockExpr = blockExpr' . block


tupleExpr :: [Expr ()] -> Expr ()
tupleExpr es = TupExpr [] es ()

arrExpr :: [Expr ()] -> Expr ()
arrExpr es = Vec [] es ()

loopExpr :: Block () -> Expr ()
loopExpr b = Loop [] b Nothing ()

matchExpr :: Expr () -> [Arm ()] -> Expr ()
matchExpr scrut arms = Match [] scrut arms ()

-- | An arm of a @match@ expression.
matchArm :: Pat () -> Expr () -> Arm ()
matchArm pat body =
  Arm
    []
    pat -- No or-patterns; just a single pattern per arm
    Nothing -- No guard expression
    body
    ()

call :: Expr () -> [Expr ()] -> Expr ()
call fn args = Call [] fn args ()

callCon :: Path () -> [Expr ()] -> Expr ()
callCon p es =
  case es of
    [] -> pathExpr p
    _  -> call (pathExpr p) es

callMethod :: Expr () -> Ident -> [Expr ()] -> Expr ()
callMethod obj meth args = MethodCall [] obj (PathSegment meth Nothing ()) args ()

fieldAccess :: Expr () -> Ident -> Expr ()
fieldAccess e l = FieldAccess [] e l ()

struct :: Path () -> [(Ident,Expr ())] -> Expr ()
struct c fs = Struct [] c (map toField fs) Nothing ()
  where toField (l,e) = Field l (Just e) [] ()

index :: Expr () -> Expr () -> Expr ()
index v i = Index [] v i ()

cast :: Expr () -> Ty () -> Expr ()
cast e t = Cast [] e t ()

uni :: UnOp -> Expr () -> Expr ()
uni op e = Unary [] op e ()

bin :: BinOp -> Expr () -> Expr () -> Expr ()
bin op e1 e2 = Binary [] op e1 e2 ()

addrOf :: Expr () -> Expr ()
addrOf e = AddrOf [] Immutable e ()


callMacro :: Path () -> [Expr ()] -> Expr ()
callMacro m es = callMacro' m args
  where
  args = Stream (intersperse tokComma (map exprToken es))

callMacro' :: Path () -> TokenStream -> Expr ()
callMacro' m args = MacExpr [] (mac m args) ()

mac :: Path () -> TokenStream -> Mac ()
mac m args = Mac m args ()

exprToken :: Expr () -> TokenStream
exprToken = treeToken . Interpolated
          . NtExpr . fmap (const dummySpan)

tyToken :: Ty () -> TokenStream
tyToken = treeToken . Interpolated
          . NtTy . fmap (const dummySpan)

treeToken :: Token -> TokenStream
treeToken = Tree . Token dummySpan

tokComma :: TokenStream
tokComma = Tree (Token dummySpan Comma)

-- | Create a token from an identifier
identToken :: Ident -> TokenStream
identToken i = treeToken (IdentTok i)

-- | Create a token from a string literal
strToken :: String -> TokenStream
strToken s = treeToken (LiteralTok (StrTok s) Nothing)

-- | Create a token stream from a list with commas between elements
commaList :: [TokenStream] -> TokenStream
commaList ts = Stream (intersperse tokComma ts)

-- | Wrap tokens in parentheses: (...)
parenTokens :: [TokenStream] -> TokenStream
parenTokens contents =
  Stream
    [ Tree (Delimited dummySpan Paren (Stream contents))
    ]

-- | Wrap a token stream in angle brackets: <...>
angleTokens :: TokenStream -> TokenStream
angleTokens (Stream contents) =
  Stream
    [ Tree (Token dummySpan Less)
    , Stream contents
    , Tree (Token dummySpan Greater)
    ]
angleTokens tree@(Tree _) = angleTokens (Stream [tree])



--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

use' :: [Attribute ()] -> UseTree () -> Item ()
use' as x = Use as InheritedV x ()

use :: UseTree () -> Item ()
use = use' []

useOne :: Path () -> Maybe Ident -> UseTree ()
useOne p mbAs = UseTreeSimple p mbAs ()

useGlob :: Path () -> UseTree ()
useGlob p = UseTreeGlob p ()

useSelect :: Path () -> [UseTree ()] -> UseTree ()
useSelect p ts = UseTreeNested p ts ()

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

docAttribute :: Text -> Attribute ()
docAttribute doc = SugaredDoc Outer True (Text.unpack doc) ()

disableWarning :: Ident -> Attribute ()
disableWarning i = Attribute Inner (simplePath "allow") toks ()
  where
  toks = Tree
       $ Delimited dummySpan Paren
       $ Tree (Token dummySpan (IdentTok i))

deriveAttribute :: [Ident] -> Attribute ()
deriveAttribute is = Attribute Inner (simplePath "derive") toks ()
  where
  toks = Tree
       $ Delimited dummySpan Paren
       $ Stream
       $ intersperse tokComma [ Tree (Token dummySpan (IdentTok i)) | i <- is ]

macDecl :: Mac () -> Item ()
macDecl m = MacItem [] m ()

mkFnItem ::
  Maybe Text        {- ^ Documentation -} ->
  [Ident]           {- ^ Disable these warnings -} ->
  [Attribute ()]    {- ^ Extra attributes -} ->
  Visibility ()     {- ^ Is this visible -} ->
  Ident             {- ^ Name -} ->
  Generics () -> [(Ident, Ty ())] -> Ty () ->
  Block () ->  Item ()
mkFnItem mbDoc allow extraAttrs vis nm generics params returnTy body =
  Fn attrs vis nm decl fnHdr generics body ()
  where
    attrs           = docAttrs ++ noWarnAttrs ++ extraAttrs
    noWarnAttrs     = map disableWarning allow
    docAttrs        = maybeToList (docAttribute <$> mbDoc)
    fnHdr           = FnHeader Normal NotAsync NotConst Rust ()
    mkArg (n, t)    = Arg [] (Just (identPat n)) t ()
    decl            = FnDecl (mkArg <$> params) (Just returnTy) False ()

mkEnum :: [Ident] -> Visibility () -> Ident -> Generics () -> [(Ident,[Ty ()])] -> Item ()
mkEnum der vis nm gs cons = Enum derA vis nm (map mkCon cons) gs ()
  where
  mkCon (i,ts)  = Variant i [] (if null ts then UnitD () else TupleD (map anon ts) ()) Nothing ()
  anon t        = StructField Nothing InheritedV t [] ()
  derA          = if null der then [] else [deriveAttribute der]

mkStruct :: [Ident] -> Visibility () -> Ident -> Generics () -> [(Ident,Ty ())] -> Item ()
mkStruct der vis nm gs flds = StructItem derA vis nm (StructD fs ()) gs ()
  where
  fs = [ StructField (Just x) vis t [] () | (x,t) <- flds ]
  derA          = if null der then [] else [deriveAttribute der]

mkTySyn :: Visibility () -> Ident -> Generics () -> Ty () -> Item ()
mkTySyn vis nm gen def = TyAlias [] vis nm def gen ()
--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

-- | Use snake_case
snakeCase :: String -> String
snakeCase str = lower False str ""
  where
  -- We use ShowS (i.e., a difference list of Strings) in the functions below to
  -- amortize the cost of appending to the end of the buffer in the `upper`
  -- function. This also avoids needing to call `reverse` in various places.

  lower ::
    Bool
      {- 'True' if the previous character was alphabetic. -} ->
    String
      {- The string to convert to snake_case. -} ->
    ShowS
  lower precededByAlpha xs =
    case xs of
      x : more
        | isUpper x -> upper x id precededByAlpha more
        | otherwise -> showChar x . lower (isAlpha x) more
      [] -> id

      

  -- Lower an uppercase character to a form suitable for snake_case.
  lowerToSnakeCase :: Char -> ShowS
  lowerToSnakeCase upperChar
    -- If the uppercase letter has a distinct lowercase counterpart, then use
    -- the lowercase version.
    | upperChar /= lowerChar
    = showChar lowerChar
    -- If the uppercase letter does *not* have a distinct lowercase counterpart
    -- (e.g., 𝑂), then the only way we can convert it to snake_case is by
    -- escaping it.
    | otherwise
    = escUnicodeChar upperChar
    where
      lowerChar = toLower upperChar

  upper ::
    Char
      {- The most recently encountered character. -} ->
    ShowS
      {- The buffer of characters encountered so far. -} ->
    Bool
      {- 'True' if the buffer is preceded by an alphabetic character, and
         therefore must be separated with an underscore in snake_case. -} ->
    String
      {- The rest of the string to convert to snake_case. -} ->
    ShowS
  upper b buf precededByAlpha xs =
    case xs of
      x : more
        | isUpper x -> upper x (buf . bLower) precededByAlpha more
        -- If character is not alphabetic (i.e., not a letter), then skip it and
        -- process the remainder of the string. This case ensures that we don't
        -- add any additional underscores into names like:
        --
        -- - `A_B` (lest we end up with `a__b`, see #115)
        -- - `ABC123` (lest we end up with `ab_c123`, see #143)
        | not (isAlpha x) -> upper x (buf . bLower) precededByAlpha more
      [] -> leadingUnderscore . buf . bLower
      _ | bufEmpty -> leadingUnderscore . bLower . lower (isAlpha b) xs
        | otherwise -> leadingUnderscore . buf . showString "_" . bLower . lower (isAlpha b) xs
    where
      bLower :: ShowS
      bLower
        | isAlpha b = lowerToSnakeCase b
        | otherwise = showChar b

      -- Only add a leading underscore if the buffer is preceded by an alphabetic
      -- character.
      leadingUnderscore :: ShowS
      leadingUnderscore
        | precededByAlpha = showString "_"
        | otherwise       = id

      bufEmpty :: Bool
      bufEmpty = null (buf "")

upperCamelCase :: String -> String
upperCamelCase = concat . mapMaybe check . groupBy isUnder2
  where
  isUnder x = x == '_'
  isUnder2 x y = isUnder x == isUnder y

  check g =
    case g of
      '_' : _ -> Nothing
      c : cs  -> Just (toUpper c : map toLower cs)
      [] -> panic "check" ["groupBy returned []"]


-- | Name mangling for exotic Unicode characters. For example, the character
-- 𝑂 is mangled to u1d442, where 1d442 is the hexadecimal value of 𝑂's
-- codepoint.
--
-- We need to escape uppercase Unicode characters that do not have distinct
-- lowercase counterparts (e.g., 𝑂).
escUnicodeChar :: Char -> ShowS
escUnicodeChar c = showChar 'u' . showHex (fromEnum c)

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

dummySpan :: Span
dummySpan = Span NoPosition NoPosition

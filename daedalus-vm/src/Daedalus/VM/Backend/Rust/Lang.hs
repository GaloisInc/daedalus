{-# Language OverloadedStrings, ImportQualifiedPost #-}
module Daedalus.VM.Backend.Rust.Lang (
  module Daedalus.VM.Backend.Rust.Lang,
  module Language.Rust.Syntax,
  module Language.Rust.Data.Ident,
  module Language.Rust.Pretty
) where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Maybe(maybeToList)
import Data.Char(isAlpha,toLower,isUpper)
import Numeric(showHex)
import Data.List(intersperse)
import Data.List.NonEmpty qualified as NonEmpty
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

pathWithTypes :: [Ident] -> [Ty ()] -> Path ()
pathWithTypes ns0 tys = Path False (go ns0) ()
  where
    go ns =
      case ns of
        [] -> panic "pathWithTypes" ["empty names"]
        [lst] -> [PathSegment lst (Just pathParams) ()]
        n:t -> PathSegment n Nothing () : go t
    pathParams = AngleBracketed [] tys [] ()


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
noGenerics = mkGenerics [] [] (WhereClause [] ())

mkGenerics ::
  [LifetimeDef ()] ->
  [TyParam ()] ->
  WhereClause () ->
  Generics ()
mkGenerics lts tps wc = Generics lts tps wc ()

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
conPat c ps = TupleStructP c ps Nothing ()

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

litExpr :: Lit () -> Expr ()
litExpr l = Lit [] l ()

pathExpr :: Path () -> Expr ()
pathExpr p = PathExpr [] Nothing p ()

identExpr :: Ident -> Expr ()
identExpr = pathExpr . simplePath

blockExpr' :: Block () -> Expr ()
blockExpr' b = BlockExpr [] b ()

blockExpr :: [Stmt ()] -> Expr ()
blockExpr = blockExpr' . block


tupleExpr :: [Expr ()] -> Expr ()
tupleExpr es = TupExpr [] es ()

loopExpr :: Block () -> Expr ()
loopExpr b = Loop [] b Nothing ()

matchExpr :: Expr () -> [Arm ()] -> Expr ()
matchExpr scrut arms = Match [] scrut arms ()

-- | An arm of a @match@ expression.
matchArm :: Pat () -> Expr () -> Arm ()
matchArm pat body =
  Arm
    []
    (pat NonEmpty.:| []) -- No or-patterns; just a single pattern per arm
    Nothing -- No guard expression
    body
    ()

call :: Expr () -> [Expr ()] -> Expr ()
call fn args = Call [] fn args ()

callMethod :: Expr () -> Ident -> [Expr ()] -> Expr ()
callMethod obj meth args = MethodCall [] obj meth Nothing args ()

callMacro :: Path () -> [Expr ()] -> Expr ()
callMacro m es = callMacro' m args
  where
  args = Stream (intersperse tokComma (map exprToken es))

callMacro' :: Path () -> TokenStream -> Expr ()
callMacro' m args = MacExpr [] (Mac m args ()) ()

exprToken :: Expr () -> TokenStream
exprToken = treeToken . Interpolated
          . NtExpr . fmap (const dummySpan)

treeToken :: Token -> TokenStream
treeToken = Tree . Token dummySpan

tokComma :: TokenStream
tokComma = Tree (Token dummySpan Comma)


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


mkFnItem ::
  Maybe Text        {- ^ Documentation -} ->
  [Ident]           {- ^ Disable these warnings -} ->
  [Attribute ()]    {- ^ Extra attributes -} ->
  Ident             {- ^ Name -} ->
  Generics () -> [(Ident, Ty ())] -> Ty () ->
  Block () ->  Item ()
mkFnItem mbDoc allow extraAttrs nm generics params returnTy body =
  Fn attrs vis nm decl unsafety constness abi generics body ()
  where
    attrs           = docAttrs ++ noWarnAttrs ++ extraAttrs
    noWarnAttrs     = map disableWarning allow
    docAttrs        = maybeToList (docAttribute <$> mbDoc)
    vis             = PublicV
    unsafety        = Normal
    constness       = NotConst
    abi             = Rust
    mkArg (n, t)    = Arg (Just (identPat n)) t ()
    decl            = FnDecl (mkArg <$> params) (Just returnTy) False ()

mkEnum :: Ident -> Generics () -> [(Ident,[Ty ()])] -> Item ()
mkEnum nm gs cons = Enum [] InheritedV nm (map mkCon cons) gs ()
  where
  mkCon (i,ts)  = Variant i [] (TupleD (map anon ts) ()) Nothing ()
  anon t        = StructField Nothing InheritedV t [] ()

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

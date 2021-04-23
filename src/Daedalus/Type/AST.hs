{-# Language BlockArguments, OverloadedStrings, DeriveTraversable #-}
{-# Language GADTs, DataKinds, ExistentialQuantification #-}
{-# Language KindSignatures #-}
{-# Language NamedFieldPuns #-}
{-# Language ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-} -- For deriving ord and eqs
{-# Language StandaloneDeriving #-}
{-# Language RecordWildCards #-}

module Daedalus.Type.AST
  ( module Daedalus.Type.AST
  , module LocalAST
  , Rec(..), recToList
  , SourceRange
  ) where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List(intersperse)
import qualified Data.Kind as HS
import Data.Text(Text)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.Parameterized.Classes -- OrdF

import Daedalus.SourceRange
import Daedalus.Rec
import Daedalus.AST as LocalAST
        ( Name(..), ManyBounds(..), BinOp(..), TriOp(..)
        , Ident, ScopedIdent(..), primName
        , Commit(..)
        , UniOp(..), Label, Ctx(..)
        , ModuleName
        , isLocalName
        , nameScopeAsLocal, Context(..), TypeF(..)
        , Located(..), ScopedIdent(..), Value, Grammar, Class, Literal(..))

import Daedalus.PP

type HS = HS.Type


data Kind       = KValue | KGrammar | KClass | KNumber
                  deriving (Eq,Show)


data RuleType   = [Type] :-> Type
                  deriving Show

data Poly a     = Poly [TVar] [Constraint] a
                  deriving Show

data Constraint = Numeric Type
                | HasStruct Type Label Type
                | TyDef TyDef TCTyName Type [(Label,Located Type)]
                  -- ^ The TCTyName is the name to use
                  -- in case we decide to solve this by using an anonymous type.
                | HasUnion  Type Label Type
                | Coerce Lossy Type Type
                | Literal Integer Type
                | CAdd Type Type Type
                | Traversable Type
                | Mappable Type Type          -- incol, outcol
                | ColElType Type Type         -- col, elT
                | ColKeyType Type Type        -- col, key
                | IsNamed Type
                  deriving Show

data TyDef = StructDef | UnionDef
  deriving Show

data Lossy = Lossy | NotLossy
  deriving (Eq,Show)


data Type       = Type (TypeF Type)
                | TCon TCTyName [Type]
                | TVar !TVar
                  deriving (Eq,Show)

data TVar       = TV { tvarId    :: !Int
                     , tvarKind  :: !Kind
                     , tvarRange :: !SourceRange
                       -- XXX: add description?
                     } deriving Show


data TCName (k :: Ctx)   = TCName { tcName    :: !Name
                                  , tcType    :: !Type
                                  , tcNameCtx :: !(Context k)
                                  } deriving Show

newtype TC a k = TC (TCAnnot a (TCF a k))
                   deriving Show

data TCAnnot a e = TCAnnot
  { tcAnnot     :: a
  , tcAnnotExpr :: e
  } deriving (Show, Functor, Foldable, Traversable)


-- Whether or not to produce a semantic value.
data WithSem = NoSem | YesSem deriving (Eq,Show)

data TCF :: HS -> Ctx -> HS where
   TCPure       :: TC a Value -> TCF a Grammar
   TCDo         :: Maybe (TCName Value) ->
                   TC a Grammar -> TC a Grammar -> TCF a Grammar

   -- This is just a tag for error reporting.
   TCLabel      :: Text -> TC a Grammar -> TCF a Grammar

   TCGetByte    :: WithSem -> TCF a Grammar
   TCMatch      :: WithSem -> TC a Class -> TCF a Grammar
   TCGuard      :: TC a Value -> TCF a Grammar
   TCMatchBytes :: WithSem -> TC a Value -> TCF a Grammar

   TCChoice     :: Commit -> [TC a Grammar] -> Type -> TCF a Grammar
   TCOptional   :: Commit -> TC a Grammar -> TCF a Grammar
   TCMany       :: WithSem ->
                    Commit -> ManyBounds (TC a Value) -> TC a Grammar -> TCF a Grammar
   TCEnd        :: TCF a Grammar
   TCOffset     :: TCF a Grammar

   TCCurrentStream  :: TCF a Grammar
   TCSetStream      :: TC a Value -> TCF a Grammar
   TCStreamLen      :: WithSem -> TC a Value -> TC a Value -> TCF a Grammar
   TCStreamOff      :: WithSem -> TC a Value -> TC a Value -> TCF a Grammar

   -- Maps
   TCMapEmpty   :: Type -> TCF a Value
   TCMapLookup  :: WithSem -> TC a Value -> TC a Value -> TCF a Grammar
   TCMapInsert  :: WithSem -> TC a Value -> TC a Value -> TC a Value -> TCF a Grammar

   -- Array operations
   TCArrayLength :: TC a Value -> TCF a Value
   TCArrayIndex  :: WithSem -> TC a Value -> TC a Value -> TCF a Grammar -- Partial

   -- coercion
   TCCoerceCheck  :: WithSem -> Type -> Type -> TC a Value -> TCF a Grammar
   TCCoerce       :: Type -> Type -> TC a Value -> TCF a Value

   -- Value constructors

   -- We only really need the Type for LNumber
   TCLiteral    :: Literal -> Type -> TCF a Value
   
   TCNothing    :: Type -> TCF a Value
   TCJust       :: TC a Value -> TCF a Value

   TCUnit       :: TCF a Value
   TCStruct     :: [ (Label,TC a Value) ] -> Type -> TCF a Value
   -- The type is the type of the result,
   -- which should be a named struct type, possibly with some parameters.

   TCArray      :: [TC a Value] -> Type -> TCF a Value
    -- Type of elements (for empty arr.)

   TCIn         :: Label -> TC a Value -> Type -> TCF a Value
    -- The type is the type of the final result,
    -- which should be a named union type, possibly with some parameters.

   TCTriOp :: TriOp -> TC a Value -> TC a Value -> TC a Value -> Type -> TCF a Value

   TCBinOp :: BinOp -> TC a Value -> TC a Value -> Type -> TCF a Value
    -- the type is the type of the result

   TCUniOp :: UniOp -> TC a Value -> TCF a Value

   -- a set of bytes (aka a byte predicate)
   TCSetAny           :: TCF a Class
   TCSetSingle        :: TC a Value -> TCF a Class
   TCSetComplement    :: TC a Class -> TCF a Class
   TCSetUnion         :: [TC a Class] -> TCF a Class
   TCSetOneOf         :: ByteString -> TCF a Class -- like union of singletons
   TCSetDiff          :: TC a Class -> TC a Class -> TCF a Class
   TCSetRange         :: TC a Value -> TC a Value -> TCF a Class

   -- destructors
   TCFor :: Loop a k -> TCF a k

   TCSelStruct :: TC a Value -> Label -> Type -> TCF a Value
   TCIf        :: TC a Value -> TC a k -> TC a k -> TCF a k

   TCVar  :: TCName k -> TCF a k
   -- Local variable/parameter

   TCCall :: TCName k -> [Type] -> [Arg a] -> TCF a k
   {- The name may be either top-level or local.  The type stored in
      the name is the type of the result, *not* the whole function.
   -}

   TCErrorMode :: Commit -> TC a Grammar -> TCF a Grammar
   TCFail :: Maybe (TC a Value) -> Type -> TCF a Grammar
      -- Custom error message: message (byte array)

   TCCase :: TC a Value           {- thing we examine -} ->
             NonEmpty (TCAlt a k) {- brances; non-empty -} ->
             Maybe (TC a k)       {- default -} ->
             TCF a k


deriving instance Show a => Show (TCF a k)

-- | A branch in a case.  Succeeds if *any* of the patterns match.
-- All alternatives must bind the same variables (with the same types)
data TCAlt a k = TCAlt { tcAltPatterns :: [TCPat]
                       , tcAltBody     :: (TC a k)
                       }
  deriving Show

-- | Deconstruct a value
data TCPat = TCConPat Type Label TCPat
           | TCNumPat Type Integer
           | TCBoolPat Bool
           | TCJustPat TCPat
           | TCNothingPat Type
           | TCVarPat (TCName Value)
           | TCWildPat Type
             deriving Show



data LoopFlav a = Fold (TCName Value) (TC a Value)
                | LoopMap
  deriving Show

data Loop a k = Loop
  { loopFlav    :: LoopFlav a
  , loopKName   :: Maybe (TCName Value) -- Key name, optional
  , loopElName  :: TCName Value
  , loopCol     :: TC a Value
  , loopBody    :: TC a k
  , loopType    :: !Type
  } deriving Show


data Arg a  = ValArg (TC a Value)
            | ClassArg (TC a Class)
            | GrammarArg (TC a Grammar)
             deriving Show

data Param = ValParam (TCName Value)
           | ClassParam (TCName Class)
           | GrammarParam (TCName Grammar)
             deriving Show

data TCTyDecl   = TCTyDecl
                   { tctyName   :: !TCTyName
                   , tctyParams :: ![TVar]
                   , tctyDef    :: !TCTyDef
                   } deriving Show


data TCTyName   = TCTyAnon !Name !Int
                | TCTy !Name
                  deriving (Eq,Ord,Show)

data TCTyDef    = TCTyStruct [(Label,Type)]
                | TCTyUnion  [(Label,Type)]
                  deriving Show


data TCDecl a   = forall k.
                  TCDecl { tcDeclName     :: !Name
                         , tcDeclTyParams :: ![TVar]
                         , tcDeclCtrs     :: ![Constraint]
                         , tcDeclParams   :: ![Param]
                         , tcDeclDef      :: !(TCDeclDef a k)
                         , tcDeclCtxt     :: !(Context k)
                         , tcDeclAnnot    :: !(a)
                         }

deriving instance Show a => Show (TCDecl a)

data TCDeclDef a k = ExternDecl Type | Defined (TC a k)
  deriving Show


-- | A module consists of a collection of types and a collection of decls.
data TCModule a = TCModule { tcModuleName    :: ModuleName
                           , tcModuleImports :: [ Located ModuleName ]
                           , tcModuleTypes   :: [ Rec TCTyDecl ]
                           , tcModuleDecls   :: [ Rec (TCDecl a) ]
                           } deriving Show

--------------------------------------------------------------------------------
-- Pretty

instance PP Kind where
  pp k =
    case k of
      KValue   -> "a semantic value"
      KGrammar -> "a grammar"
      KClass   -> "a byte preicate"
      KNumber  -> "a size"

instance PP RuleType where
  ppPrec n (as :-> b) = wrapIf (n > 0)
                      $ hsep
                      $ intersperse "->"
                      $ map (ppPrec 1)
                      $ as ++ [b]


instance PP TCTyName where
  ppPrec _ ty =
    case ty of
      TCTy x       -> pp x
      TCTyAnon i n -> pp i <.> pp n



instance PP Type where
  ppPrec n ty =
    case ty of
      Type t -> ppPrec n t
      TVar x -> pp x
      TCon c ts ->
        case ts of
          [] -> pp c
          _  -> wrapIf (n > 1) (pp c <+> hsep (map (ppPrec 2) ts))

instance PP (TCName k) where
  ppPrec n = ppPrec n . tcName

-- instance PP (TCName k) where
--   ppPrec n t = parens (pp (tcName t) <+> "::" <+> pp (tcType t))


instance PP TVar where
  pp x = "?a" <.> int (tvarId x)

instance PP (TCDecl a) where
  pp d@TCDecl { tcDeclName, tcDeclDef } = pp tcDeclName <+>
         hsep (map pp (tcDeclTyParams d)) <+>
         hsep (map (ppPrec 2) (tcDeclCtrs d)) <+>
         hsep (map pp (tcDeclParams d)) <+>
         ":" <+> pp (typeOf tcDeclDef) <+> this $$ nest 2 next

    where (this,next) = case tcDeclDef of
                          Defined e     -> ("=", pp e)
                          ExternDecl {} -> (empty,empty)


instance PP (Arg a) where
  ppPrec n arg =
    case arg of
      ValArg e -> ppPrec n e
      ClassArg e -> ppPrec n e
      GrammarArg e -> ppPrec n e

instance PP Param where
  ppPrec _ arg =
    case arg of
      ValParam e -> ppBinder e
      ClassParam e -> ppBinder e
      GrammarParam e -> ppBinder e

instance PP (TC a k) where
  ppPrec n = ppPrec n . texprValue

instance PP (Loop a k) where
  ppPrec n lp = wrapIf (n > 0) $
    kw <+> parens (st <+>
                   ppK <+> ppBinder (loopElName lp) <+> "in" <+> pp (loopCol lp)
      ) $$ nest 2 (ppPrec 1 (loopBody lp))
    where
    ppK = case loopKName lp of
            Nothing -> empty
            Just k  -> ppBinder k <.> comma
    (kw,st) = case loopFlav lp of
                Fold x e -> ("for", ppBinder x <+> "=" <+> pp e <.> semi)
                LoopMap  -> ("map", empty)


instance PP (TCF a k) where
  ppPrec n texpr =
    case texpr of
      TCPure e      -> wrapIf (n > 0) ("pure" <+> ppPrec 1 e)
      TCDo {}       -> "do" <+> ppStmt texpr

      TCLabel l p    -> "{-" <+> pp l <+> "-}" <+> ppPrec n p

      TCGetByte s    -> annotKW' s "GetByte"
      TCMatch s b    -> wrapIf (n > 0) (annotKW' s "Match" <+> ppPrec 1 b)
      TCGuard e      -> wrapIf (n > 0) ("Guard" <+> ppPrec 1 e)

      TCMatchBytes s b -> wrapIf (n > 0)
                                 (annotKW' s "MatchBytes" <+> ppPrec 1 b)

      TCEnd          -> "END"
      TCOffset       -> "Offset"

      TCMapEmpty _   -> "empty"
      TCMapInsert s k v m -> wrapIf (n > 0) (annotKW' s "Insert"
                                <+> ppPrec 1 k <+> ppPrec 1 v <+> ppPrec 1 m)
      TCMapLookup s k m ->
          wrapIf (n > 0) (annotKW' s "Lookup" <+> ppPrec 1 k <+> ppPrec 1 m)

      TCArrayLength e ->
          wrapIf (n > 0) ("Length" <+> ppPrec 1 e)

      TCArrayIndex s v ix ->
          wrapIf (n > 0) (annotKW' s "Index" <+> ppPrec 1 v <+> ppPrec 1 ix)

      TCChoice c es _ -> "Choose" <+> pp c $$
                               nest 2 (block "{" "|" "}" (map pp es))
      TCOptional c e -> wrapIf (n > 0) $ annotKW YesSem c "Optional" <+> ppPrec 1 e

      TCMany s c bnds e -> wrapIf (n > 0)
                         $ annotKW s c "Many" <.> pp bnds <+> pp e

      TCVar x -> pp x


      -- Eliminators
      TCFor lp -> ppPrec n lp

      TCIf be te fe -> wrapIf (n > 0) $
        "if" <+> ppPrec 1 be <+> "then" <+> pp te <+> "else" <+> pp fe

      -- Values
      TCIn l e _    -> braces (pp l <.> colon <+> ppPrec 1 e)
      
      TCLiteral l _ -> pp l
      TCNothing _   -> "nothing"
      TCJust e      -> wrapIf (n > 0) ("just" <+> ppPrec 1 e)
      TCStruct xs _ -> braces (vcat (punctuate comma (map ppF xs)))
        where ppF (x,e) = pp x <+> "=" <+> pp e
      TCUnit        -> "{}"
      TCArray xs _  -> brackets (vcat (punctuate comma (map pp xs)))

      TCCall f [] []  -> pp f
      TCCall f ts xs -> wrapIf (n > 0) (pp f <+>
                                        hsep (map (ppPrec 2) ts) <+>
                                        hsep (map (ppPrec 1) xs))

      TCTriOp op e1 e2 e3 _ ->
        wrapIf (n > 0) (pp op <+> ppPrec 1 e1 <+> ppPrec 1 e2 <+> ppPrec 1 e3)

      TCBinOp op@ArrayStream e1 e2 _ ->
          wrapIf (n > 0) (pp op <+> ppPrec 1 e1 <+> ppPrec 1 e2)

      TCBinOp op e1 e2 _ ->
          wrapIf (n > 0) (ppPrec 1 e1 <+> pp op <+> ppPrec 1 e2)

      TCUniOp op e -> wrapIf (n > 0) (pp op <+> ppPrec 1 e)
      TCSelStruct x l _ -> wrapIf (n > 0) (ppPrec 1 x <.> "." <.> pp l)

      -- Sets
      TCSetAny -> "UInt8"
      TCSetSingle e -> braces (pp e)
      TCSetComplement e -> wrapIf (n > 0) ("!" <> ppPrec 1 e)
      TCSetRange e1 e2 -> wrapIf (n > 0) (pp e1 <+> ".." <+> pp e2)
      TCSetUnion [] -> "{}"
      TCSetUnion [e] -> ppPrec n e
      TCSetUnion es -> wrapIf (n > 0)
                     $ fsep (intersperse "|" (map (ppPrec 1) es))
      TCSetOneOf bs -> text (show (BS8.unpack bs))
      TCSetDiff e1 e2 -> wrapIf (n > 0) (ppPrec 1 e1 <+> "-" <+> ppPrec 1 e2)

      -- Coercions
      TCCoerceCheck s _ t2 e ->
        wrapQuietIf s (n > 0) (pp e <+> "AS" <+> pp t2)

      TCCoerce _ t2 e      -> wrapIf (n > 0) (pp e <+> "as" <+> pp t2)

      -- Streams
      TCCurrentStream    -> "CurrentStream"
      TCSetStream s      -> wrapIf (n > 0) ("SetStream" <+> ppPrec 1 s)

      TCStreamLen sem e s  ->
        wrapIf (n > 0)
        (annotKW' sem "StreamLen" <+> ppPrec 1 e <+> ppPrec 1 s)

      TCStreamOff sem e s  ->
        wrapIf (n > 0)
        (annotKW' sem "StreamOff" <+> ppPrec 1 e <+> ppPrec 1 s)

      TCErrorMode c p -> wrapIf (n > 0) (hang kw 2 (ppPrec 1 p))
        where kw = case c of
                     Commit    -> "Commit"
                     Backtrack -> "Try"

      TCFail mbMsg _ ->
        case mbMsg of
          Nothing  -> "Fail"
          Just msg -> wrapIf (n > 0) ("Fail" <+> ppPrec 1 msg)

      TCCase e pats mdef ->
        wrapIf (n > 0) (
        "case" <+> pp e <+> "is" $$
          nest 2 (block "{" ";" "}" (addDefault (map pp (NE.toList pats)))))
        where
        addDefault xs = case mdef of
                          Nothing -> xs
                          Just d  -> xs ++ ["_" <+> "->" <+> pp d]



instance PP a => PP (Poly a) where
  ppPrec n (Poly xs cs a) =
    case (xs,cs) of
      ([],[]) -> ppPrec n a
      _       -> let qual = case xs of
                              [] -> empty
                              _  -> braces (commaSep (map pp xs))
                     ctrs = case cs of
                              [] -> empty
                              _  -> parens (commaSep (map pp cs)) <+> "=>"
                 in wrapIf (n > 0) (qual <+> ctrs <+> pp a)



instance PP (TCModule a) where
  ppPrec _ m =
    vcat' [ "module" <+> pp (tcModuleName m)
          , "--- Imports:" $$
           vcat (map (\n -> "import" <+>
               pp (thingValue n)) (tcModuleImports m))

         , "--- Type defs:" $$
           vcat' (map pp (tcModuleTypes m))

         , "--- Rules:"
         , vcat' (map ppTCRuleRes (tcModuleDecls m))
         ]

instance PP TCTyDecl where
  ppPrec _ d =
    "type" <+> pp (tctyName d)
           <+> hsep (map pp (tctyParams d)) <+> "=" <+> pp (tctyDef d)

instance PP TCTyDef where
  ppPrec _ d =
    case d of
      TCTyStruct fs -> block "{" ";" "}" (map ppF fs)
      TCTyUnion  fs -> "Choose" <+> block "{" ";" "}" (map ppF fs)

    where
    ppF (x,t) = pp x <.> ":" <+> pp t


instance PP (TCAlt a k) where
  ppPrec _ (TCAlt ps e) = lhs <+> "->" <+> pp e
    where lhs = sep $ punctuate comma $ map pp ps

instance PP TCPat where
  ppPrec n pat =
    case pat of
      TCConPat _ l p  -> "{|" <+> pp l <+> "=" <+> pp p <+> "|}"
      TCNumPat _ i    -> pp i
      TCBoolPat b     -> if b then "true" else "false"
      TCJustPat p     -> wrapIf (n > 0) ("just" <+> ppPrec 1 p)
      TCNothingPat _  -> "nothing"
      TCVarPat x      -> pp x
      TCWildPat _     -> "_"

ppTCRuleRes :: Rec (TCDecl a) -> Doc
ppTCRuleRes sc =
  case sc of
    NonRec d -> ppRule d
    MutRec ds -> "rec value" $$ nest 2 (vcat' (map ppRule ds))
  where
  ppRule r = pp r {-vcat [ pp (tcDeclName r) <+> ":" <+> pp t
                  , pp r
                  ]-}

ppTyDef :: Rec (TVar,Type) -> Doc
ppTyDef sc =
  case sc of
    NonRec d -> ppDef d
    MutRec xs -> "rec type" $$ nest 2 (vcat' (map ppDef xs))
  where
  ppDef (x,t) = pp x <+> "=" <+> pp t




-- This is a hack, it assumes that Commit doesn't add anything
annotKW' :: WithSem -> Doc -> Doc
annotKW' s = annotKW s Commit

annotKW :: WithSem -> Commit -> Doc -> Doc
annotKW s cmt kw = pref <.> kw <.> suff
  where pref = case s of
                 NoSem  -> "@"
                 YesSem -> empty
        suff = case cmt of
                 Commit -> empty
                 Backtrack -> "?"

wrapQuietIf :: WithSem -> Bool -> Doc -> Doc
wrapQuietIf s p d = pref <.> wrapIf (p || w) d
  where (w,pref) = case s of
                     NoSem -> (True, "@")
                     YesSem -> (False,empty)

ppStmt :: TCF a k -> Doc
ppStmt texpr =
  case texpr of
    TCDo mb (texprValue -> e1) (texprValue -> e2) ->
      case mb of
        Nothing -> pp e1 $$ ppStmt e2
        Just x  -> (ppBinder x <+> "<-" <+> pp e1) $$ ppStmt e2

    _ -> pp texpr


ppBinder :: TCName k -> Doc
ppBinder x = parens (pp (tcName x) <+> ":" <+> pp (tcType x))

instance PP TyDef where
  pp d = case d of
           StructDef -> "struct"
           UnionDef  -> "union"

instance PP Constraint where
  ppPrec n c =
    case c of
      Numeric x -> wrapIf (n > 0) ("Numeric" <+> ppPrec 2 x)
      HasStruct x l t -> wrapIf (n > 0) ("HasStruct" <+> pp x <+> pp l <+> pp t)
      TyDef ty _ t fs -> wrapIf (n > 0)
          (pp ty <+> ppPrec 2 t <+> block "{" ";" "}" (map ppF fs))
        where ppF (f,ft) = pp f <.> ":" <+> pp ft
      HasUnion  x l t -> wrapIf (n > 0) ("HasUnion" <+> pp x <+> pp l <+> pp t)
      Coerce s t1 t2 ->
          wrapIf (n > 0) ("Coerce" <+> pp s <+> ppPrec 2 t1 <+> ppPrec 2 t2)
      Literal i t ->
        wrapIf (n > 0) ("Literal" <+> pp i <+> ppPrec 2 t)
      CAdd t1 t2 t3 ->
        wrapIf (n > 0)
               (ppPrec 2 t1 <+> "+" <+> ppPrec 2 t2 <+> "=" <+> ppPrec 2 t3)

      Traversable t -> wrapIf (n > 0)
                       ("Traversable" <+> ppPrec 2 t)

      Mappable t s -> wrapIf (n > 0)
                        ("Mappable" <+> ppPrec 2 t <+> ppPrec 2 s)

      ColElType s t -> wrapIf (n > 0)
                        ("HasElement" <+> ppPrec 2 s <+> ppPrec 2 t)

      ColKeyType s t -> wrapIf (n > 0)
                        ("HasKey" <+> ppPrec 2 s <+> ppPrec 2 t)

      IsNamed t ->  wrapIf (n > 0) ("Named" <+> ppPrec 2 t)

instance PP Lossy where
  pp l = case l of
           Lossy -> "trunc"
           NotLossy -> "safe"



--------------------------------------------------------------------------------
-- Range


instance HasRange a => HasRange (TC a k) where
  range (TC x) = range (tcAnnot x)

instance HasRange TVar where
  range = tvarRange

instance HasRange (TCName k) where
  range = range . tcName

instance HasRange a => HasRange (Arg a) where
  range arg =
    case arg of
      ValArg e -> range e
      ClassArg e -> range e
      GrammarArg e -> range e


--------------------------------------------------------------------------------


tNum :: Integer -> Type
tNum n = Type (TNum n)

tUInt :: Type -> Type
tUInt n = Type (TUInt n)

tSInt :: Type -> Type
tSInt n = Type (TSInt n)

tByte :: Type
tByte = tUInt (tNum 8)

tGrammar :: Type -> Type
tGrammar t = Type (TGrammar t)

tFun :: Type -> Type -> Type
tFun s t = Type (TFun s t)

tFunMany :: [Type] -> Type -> Type
tFunMany args res = foldr tFun res args

tMaybe :: Type -> Type
tMaybe t = Type (TMaybe t)

tArray :: Type -> Type
tArray t = Type (TArray t)

tUnit :: Type
tUnit = Type TUnit

tInteger :: Type
tInteger = Type TInteger

-- | This type is a bit like size_t in `C`, and is used for
-- sizes of things, indexing, shifting operators, etc.
tSize :: Type
tSize = tUInt (tNum 64)

tBool :: Type
tBool = Type TBool

tByteClass :: Type
tByteClass = Type TByteClass

tStream :: Type
tStream = Type TStream

tMap :: Type -> Type -> Type
tMap kt vt = Type (TMap kt vt)

tCon :: TCTyName -> [Type] -> Type
tCon c ts = TCon c ts

instance Eq TVar where
  x == y = tvarId x == tvarId y

instance Ord TVar where
  compare x y = compare (tvarId x) (tvarId y)



annotExpr :: a -> TCF a k -> TC a k
annotExpr a e = TC TCAnnot { tcAnnot = a, tcAnnotExpr = e }

exprAt :: HasRange r => r -> TCF SourceRange k -> TC SourceRange k
exprAt r = annotExpr (range r)

texprValue :: TC a k -> TCF a k
texprValue (TC x) = tcAnnotExpr x

texprAnnot :: TC a k ->  a
texprAnnot (TC x) = tcAnnot x



--------------------------------------------------------------------------------
kindOf :: Type -> Kind
kindOf ty =
  case ty of
    TVar x      -> tvarKind x
    TCon {}     -> KValue
    Type t ->
      case t of
        TGrammar {} -> KGrammar
        TFun _ k    -> kindOf k
        TStream     -> KValue
        TByteClass  -> KClass
        TNum {}     -> KNumber
        TUInt _     -> KValue
        TSInt _     -> KValue
        TInteger    -> KValue
        TBool       -> KValue
        TUnit       -> KValue
        TArray {}   -> KValue
        TMaybe {}   -> KValue
        TMap {}     -> KValue

class TypeOf t where
  typeOf :: t -> Type

instance TypeOf Type where
  typeOf t = t

instance TypeOf (TCDecl a) where
  typeOf TCDecl {..} = typeOf tcDeclDef

instance TypeOf (TCDeclDef a k) where
  typeOf d = case d of
               ExternDecl t -> t
               Defined e    -> typeOf e

instance TypeOf t => TypeOf (TCAnnot a t) where
  typeOf = typeOf . tcAnnotExpr

instance TypeOf (TCName k) where
  typeOf = tcType

instance TypeOf (Arg a) where
  typeOf arg =
    case arg of
      ValArg e -> typeOf e
      ClassArg e -> typeOf e
      GrammarArg e -> typeOf e

instance TypeOf Param where
  typeOf arg =
    case arg of
      ValParam e -> typeOf e
      ClassParam e -> typeOf e
      GrammarParam e -> typeOf e


instance TypeOf (TC a k) where
  typeOf = typeOf . texprValue

mbTy :: WithSem -> Type -> Type
mbTy s r = case s of
               NoSem  -> tUnit
               YesSem -> r


instance TypeOf (Loop a k) where
  typeOf lp = loopType lp

instance TypeOf (TCF a k) where
  typeOf expr =
    case expr of
      TCPure e        -> tGrammar (typeOf e)
      TCDo _ _ e      -> typeOf e

      TCLabel _ e     -> typeOf e

      TCGetByte s     -> tGrammar (mbTy s tByte)

      TCGuard _       -> tGrammar tUnit
      TCMatch s _     -> tGrammar (mbTy s tByte)
      TCMatchBytes s _-> tGrammar (mbTy s (tArray tByte))
      TCEnd           -> tGrammar tUnit
      TCOffset        -> tGrammar tSize

      TCMapEmpty t    -> t
      TCMapInsert s _ _ m -> tGrammar (mbTy s (typeOf m))
      TCMapLookup s _ m   -> let Type (TMap _ vt) = typeOf m
                             in tGrammar (mbTy s vt)

      TCArrayLength _    -> tSize
      TCArrayIndex s e _ -> let Type (TArray t) = typeOf e
                            in tGrammar (mbTy s t)

      TCChoice _ _ t  -> tGrammar t

      TCOptional _ e  -> let Type (TGrammar a) = typeOf e
                         in tGrammar (tMaybe a)

      TCMany s _ _ e    -> let Type (TGrammar a) = typeOf e
                           in tGrammar (mbTy s (tArray a))

      TCFor lp -> typeOf lp

      TCIf _ e _      -> typeOf e

      TCLiteral _ t   -> t
      TCUnit          -> tUnit
      TCNothing t     -> t
      TCJust e        -> tMaybe (typeOf e)
      TCStruct _ t    -> t
      TCArray _ t     -> tArray t
      TCIn _ _ t      -> t
      TCVar x         -> tcType x

      TCCall f _ _    -> tcType f

      TCTriOp _ _ _ _ t -> t
      TCBinOp _ _ _ t -> t

      TCUniOp op e ->
        case op of
          Not    -> tBool
          Neg    -> typeOf e
          Concat -> let Type (TArray (Type (TArray t))) = typeOf e
                    in tArray t
          BitwiseComplement -> typeOf e


      TCSelStruct _ _ t  -> t

      TCSetAny          -> tByteClass
      TCSetSingle _     -> tByteClass
      TCSetOneOf _      -> tByteClass
      TCSetRange _ _    -> tByteClass
      TCSetComplement _ -> tByteClass
      TCSetUnion _      -> tByteClass
      TCSetDiff _ _     -> tByteClass

      TCCoerceCheck s _ t2 _ -> tGrammar (mbTy s t2)
      TCCoerce _ t2 _      -> t2

      TCCurrentStream     -> tGrammar tStream
      TCSetStream _       -> tGrammar tUnit
      TCStreamLen _ _ _   -> tGrammar tStream
      TCStreamOff _ _ _   -> tGrammar tStream

      TCErrorMode _ p     -> typeOf p
      TCFail _ t          -> tGrammar t
      TCCase _ ps _       -> typeOf (NE.head ps)

instance TypeOf (TCAlt a k) where
  typeOf (TCAlt _ e) = typeOf e


declTypeOf :: TCDecl a -> Poly RuleType
declTypeOf d@TCDecl { tcDeclDef } =
    Poly (tcDeclTyParams d) (tcDeclCtrs d)
             $ map typeOf (tcDeclParams d) :-> typeOf tcDeclDef


-- | The type of thing we match
instance TypeOf TCPat where
  typeOf pat =
    case pat of
      TCConPat t _ _ -> t
      TCNumPat t _ -> t
      TCBoolPat _ -> tBool
      TCJustPat p -> tMaybe (typeOf p)
      TCNothingPat t -> tMaybe t
      TCVarPat x -> typeOf x
      TCWildPat t -> t

patBinds :: TCPat -> [TCName Value]
patBinds pat =
  case pat of
    TCConPat _ _ p  -> patBinds p
    TCNumPat {}     -> []
    TCBoolPat {}    -> []
    TCJustPat p     -> patBinds p
    TCNothingPat {} -> []
    TCVarPat x      -> [x]
    TCWildPat {}    -> []

patBindsSet :: TCPat -> Set (TCName Value)
patBindsSet = Set.fromList . patBinds

altBinds :: TCAlt a k -> [TCName Value]
altBinds (TCAlt ps _) = patBinds (head ps)



$(return [])

--------------------------------------------------------------------------------
-- OrdF, TestEquality, etc.

instance TestEquality TCName where
  testEquality t1 t2 | tcName t1 == tcName t2
    = testEquality (tcNameCtx t1) (tcNameCtx t2)
  testEquality _ _ = Nothing

instance Eq (TCName k) where
  k == k' =  isJust (testEquality k k')

instance EqF TCName where
  eqF k k' = k == k'

-- NOTE: these ignore types, so names with different types will cause issues.
instance OrdF TCName where
  compareF tn1 tn2 =
    joinOrderingF (fromOrdering (compare (tcName tn1) (tcName tn2)))
                  (compareF (tcNameCtx tn1) (tcNameCtx tn2))

instance Ord (TCName k) where
  compare x y = toOrdering (compareF x y)

    
    

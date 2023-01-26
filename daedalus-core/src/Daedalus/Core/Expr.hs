{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}

module Daedalus.Core.Expr where

import           Control.Applicative   (Const (..))
import           Control.DeepSeq       (NFData)
import           Data.ByteString       (ByteString)
import           Data.Functor.Identity (Identity (..))
import           Data.Maybe            (maybeToList)
import           GHC.Generics          (Generic)

import           Daedalus.PP
import           Daedalus.Panic        (panic)

import           Daedalus.Core.Basics


data Expr =
    Var Name
  | PureLet Name Expr Expr
  | Struct UserType [ (Label, Expr) ]
    -- The order of these need NOT match the declaration

  | ECase (Case Expr)
  | ELoop (LoopMorphism Expr)

  | Ap0 Op0
  | Ap1 Op1 Expr
  | Ap2 Op2 Expr Expr
  | Ap3 Op3 Expr Expr Expr
  | ApN OpN [Expr]
  deriving (Generic,NFData,Eq)

data Op0 =
    Unit
  | IntL Integer Type
  | FloatL Double Type
  | BoolL Bool
  | ByteArrayL ByteString
  | NewBuilder Type
  | MapEmpty Type Type
  | ENothing Type
  deriving (Eq,Ord,Generic,NFData)

data Op1 =
    CoerceTo Type
  | IsEmptyStream
  | Head
  | StreamOffset
  | StreamLen
  | BytesOfStream
  | OneOf ByteString
  | Neg
  | BitNot
  | Not
  | ArrayLen
  | Concat
  | FinishBuilder
  | NewIterator
  | IteratorDone
  | IteratorKey
  | IteratorVal
  | IteratorNext
  | EJust
  | FromJust
  | SelStruct Type Label
  | InUnion UserType Label
  | FromUnion Type Label
  | WordToFloat
  | WordToDouble
  | IsNaN
  | IsInfinite
  | IsDenormalized
  | IsNegativeZero
  deriving (Eq, Generic,NFData)

data Op2 =
    IsPrefix
  | Drop
  | Take

  | Eq
  | NotEq
  | Leq
  | Lt

  | Add
  | Sub
  | Mul
  | Div
  | Mod

  | BitAnd
  | BitOr
  | BitXor
  | Cat
  | LCat
  | LShift
  | RShift

  | ArrayIndex
  | Emit
  | EmitArray
  | EmitBuilder
  | MapLookup
  | MapMember

  | ArrayStream
  deriving (Eq, Generic,NFData)

data Op3 =
    RangeUp
  | RangeDown
  | MapInsert
  deriving (Eq, Generic,NFData)

data OpN =
    ArrayL Type
  | CallF FName
  deriving (Eq, Generic,NFData)

-- folds and maps
data LoopMorphism e =
  FoldMorphism Name Expr LoopCollection e  -- for (s = e; ... in ...) ...
  | MapMorphism LoopCollection e           -- map (... in ...) ...
  deriving (Functor, Generic, NFData, Eq)

morphismBody :: LoopMorphism a -> a
morphismBody (FoldMorphism _ _ _ a) = a
morphismBody (MapMorphism      _ a) = a

morphismE :: Applicative f => (Expr -> f Expr) -> (a -> f a) ->
             LoopMorphism a -> f (LoopMorphism a)
morphismE ef af lm = case lm of
  FoldMorphism s e lc b -> 
    FoldMorphism s <$> ef e <*> goLC lc <*> af b
  MapMorphism lc b -> MapMorphism <$> goLC lc <*> af b
  where
    goLC lc = LoopCollection (lcKName lc) (lcElName lc) <$> ef (lcCol lc)

-- Used for maps and folds in both Expr and Grammar,
-- c.f. Daedalus.Type.AST.LoopCollection
data LoopCollection = LoopCollection
  { lcKName   :: Maybe Name
  , lcElName  :: Name
  , lcCol     :: Expr
  } deriving (Generic, NFData, Eq)

loopCollectionBinders :: LoopCollection -> [Name]
loopCollectionBinders lc = lcElName lc : maybeToList (lcKName lc)

--------------------------------------------------------------------------------
-- Traversals

childrenE ::
    Applicative f => (Expr -> f Expr) -> Expr -> f Expr
childrenE f expr =
  case expr of
    Var {} -> pure expr
    PureLet n e1 e2 -> PureLet n <$> f e1 <*> f e2
    Struct ut flds  -> Struct ut <$> traverse (\(fld,e) -> (,) fld <$> f e) flds
    ECase cs        -> ECase <$> traverse f cs
    ELoop lm        -> ELoop <$> morphismE f f lm
    Ap0 {} -> pure expr
    Ap1 op1 e  -> Ap1 op1 <$> f e
    Ap2 op2 e1 e2 -> Ap2 op2 <$> f e1 <*> f e2
    Ap3 op3 e1 e2 e3 -> Ap3 op3 <$> f e1 <*> f e2 <*> f e3
    ApN opN es -> ApN opN <$> traverse f es

mapChildrenE :: (Expr -> Expr) -> Expr -> Expr
mapChildrenE f e = e1
  where Identity e1 = childrenE (Identity . f) e

foldMapChildrenE :: Monoid m => (Expr -> m) -> Expr -> m
foldMapChildrenE f e = m
  where Const m = childrenE (Const . f) e

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

class CoreSyn t where
  coreLet  :: Name  -> Expr -> t -> t
  coreCase :: Name  -> [(Pattern,t)] -> t
  coreCall :: FName -> [Expr] -> t

coreIf :: CoreSyn t => Name -> t -> t -> t
coreIf x ifTrue ifFalse =
  coreCase x [ (PBool True, ifTrue), (PBool False, ifFalse) ]



instance CoreSyn Expr where
  coreLet       = PureLet
  coreCase x as = ECase (Case x as)
  coreCall      = callF

--------------------------------------------------------------------------------
unit    = Ap0 Unit
callF f = ApN (CallF f)


--------------------------------------------------------------------------------
-- Maybe

just      = Ap1 EJust
eFromJust = Ap1 FromJust
nothing t = Ap0 (ENothing t)


--------------------------------------------------------------------------------
-- Datatypes

selStruct t l     = Ap1 (SelStruct t l)
inUnion t l       = Ap1 (InUnion t l)
fromUnion t l     = Ap1 (FromUnion t l)



--------------------------------------------------------------------------------
-- Coercions

coerceTo t      = Ap1 (CoerceTo t)

--------------------------------------------------------------------------------
-- Input

isEmptyStream = Ap1 IsEmptyStream
isPrefix      = Ap2 IsPrefix
eHead         = Ap1 Head
eDrop         = Ap2 Drop
eTake         = Ap2 Take
streamOffset  = Ap1 StreamOffset
streamLen     = Ap1 StreamLen
arrayStream   = Ap2 ArrayStream
bytesOfStream = Ap1 BytesOfStream

--------------------------------------------------------------------------------
-- Relations

eq            = Ap2 Eq
notEq         = Ap2 NotEq
leq           = Ap2 Leq
lt            = Ap2 Lt
oneOf b       = Ap1 (OneOf b)


--------------------------------------------------------------------------------
-- Numbers

intL n t      = Ap0 (IntL n t)
floatL n t    = Ap0 (FloatL n t)
byteL b       = Ap0 (IntL (toInteger b) (TUInt (TSize 8)))
neg           = Ap1 Neg
add           = Ap2 Add
sub           = Ap2 Sub
mul           = Ap2 Mul
eDiv          = Ap2 Div
eMod          = Ap2 Mod


--------------------------------------------------------------------------------
-- Bitvectors

bitNot        = Ap1 BitNot
bitAnd        = Ap2 BitAnd
bitOr         = Ap2 BitOr
bitXor        = Ap2 BitXor
cat           = Ap2 Cat
lCat          = Ap2 LCat
lShift        = Ap2 LShift
rShift        = Ap2 RShift


--------------------------------------------------------------------------------
-- Boolean
boolL b      = Ap0 (BoolL b)
eNot         = Ap1 Not

--------------------------------------------------------------------------------
-- Arrays

arrayL t      = ApN (ArrayL t)
byteArrayL b  = Ap0 (ByteArrayL b)
arrayLen      = Ap1 ArrayLen
arrayIndex    = Ap2 ArrayIndex
eConcat       = Ap1 Concat
rangeUp       = Ap3 RangeUp
rangeDown     = Ap3 RangeDown


--------------------------------------------------------------------------------
-- Builder (list?)
finishBuilder = Ap1 FinishBuilder
emit          = Ap2 Emit
emitArray     = Ap2 EmitArray
emitBuilder   = Ap2 EmitBuilder
newBuilder t  = Ap0 (NewBuilder t)



--------------------------------------------------------------------------------
-- Iterators

newIterator   = Ap1 NewIterator
iteratorDone  = Ap1 IteratorDone
iteratorKey   = Ap1 IteratorKey
iteratorVal   = Ap1 IteratorVal
iteratorNext  = Ap1 IteratorNext


--------------------------------------------------------------------------------
-- Map

mapEmpty tk tv  = Ap0 (MapEmpty tk tv)
mapLookup       = Ap2 MapLookup
mapMember       = Ap2 MapMember
mapInsert       = Ap3 MapInsert


--------------------------------------------------------------------------------
-- Floating Point

wordToFloat     = Ap1 WordToFloat
wordToDouble    = Ap1 WordToDouble
eIsNaN          = Ap1 IsNaN
eIsInfinite     = Ap1 IsInfinite
eIsDenormalized = Ap1 IsDenormalized
eIsNegativeZero = Ap1 IsNegativeZero


--------------------------------------------------------------------------------

instance PP Expr where
  ppPrec n expr =
    case expr of
      Var x -> pp x
      PureLet x e1 e2 ->
        wrapIf (n > 0)
          $ "let" <+> pp x <+> "=" <+> pp e1 <+> "in"
          $$ pp e2

      Struct t fs -> ppPrec 1 t <+> braces (commaSep (map ppF fs))
        where ppF (l,e) = pp l <+> "=" <+> pp e

      ECase c -> pp c
      ELoop l -> pp l

      Ap0 op   -> ppPrec n op

      Ap1 op e -> wrapIf (n > 0) (pp op <+> ppPrec 1 e)

      Ap2 op e1 e2 -> wrapIf (n > 0)
        case ppOp2 op of
          (how,d) ->
            case how of
              PPPref   -> d <+> ppPrec 1 e1 <+> ppPrec 1 e2
              PPInf    -> ppPrec 1 e1 <+> d <+> ppPrec 1 e2
              PPCustom -> panic "PP Ap2" [show d]

      Ap3 op e1 e2 e3 -> wrapIf (n > 0) $
        case ppOp3 op of
          (PPPref,d) -> d <+> ppPrec 1 e1 <+> ppPrec 1 e2 <+> ppPrec 1 e3
          (_,d) -> panic "PP Ap3" [show d]

      ApN op es ->
        case op of
          ArrayL t ->
            case es of
              [] -> ppTApp n "[]" [t]
              _  -> brackets (commaSep (map pp es))
          CallF f -> pp f <.> parens (commaSep (map pp es))


ppTApp :: Int -> Doc -> [Type] -> Doc
ppTApp n x ts = wrapIf (n > 0) (x <+> hsep [ "@" <.> ppPrec 1 t | t <- ts ])


instance PP Op0 where
  ppPrec n op =
    case op of
      Unit            -> "()"
      IntL i t        -> ppTApp n (pp i) [t]
      BoolL b         -> pp b
      FloatL f _      -> pp f
      ByteArrayL b    -> pp b
      NewBuilder t    -> ppTApp n "nil" [t]
      MapEmpty t1 t2  -> ppTApp n "mEmpty"   [t1,t2]
      ENothing t      -> ppTApp n "nothing" [t]

instance PP Op1 where
  pp op =
    case op of
      CoerceTo t      -> ppTApp 0 "cast" [t]
      IsEmptyStream   -> "iNull"
      Head            -> "iHead"
      StreamOffset    -> "iOffset"
      StreamLen       -> "iLen"
      BytesOfStream   -> "bytesOfStream"
      OneOf xs        -> "oneOf" <+> pp xs
      Neg             -> "neg"
      BitNot          -> "complement"
      Not             -> "not"

      ArrayLen        -> "aLen"
      Concat          -> "concat"

      FinishBuilder   -> "listToArray"

      NewIterator     -> "itNew"
      IteratorDone    -> "itNull"
      IteratorKey     -> "itKey"
      IteratorVal     -> "itVal"
      IteratorNext    -> "itNext"

      EJust           -> "just"
      FromJust        -> "fromJust"

      SelStruct _ l   -> "get" <+> pp l
      InUnion t l     -> ppTApp 0 ("tag" <+> pp l) [TUser t]
      FromUnion _ l   -> "fromTag" <+> pp l

      WordToFloat     -> "wordToFloat"
      WordToDouble    -> "wordToDouble"
      IsNaN           -> "isNaN"
      IsInfinite      -> "isInfinite"
      IsDenormalized  -> "isDenormalized"
      IsNegativeZero  -> "isNegativeZero"



data PPHow = PPPref | PPInf | PPCustom

ppAsOp :: (PPHow,Doc) -> Doc
ppAsOp (how,d) =
  case how of
    PPInf -> parens d
    _     -> d


ppOp2 :: Op2 -> (PPHow, Doc)
ppOp2 op =
    let inf x  = (PPInf,x)
        pref x = (PPPref,x)
    in
    case op of

      IsPrefix    -> pref "iPrefix"
      Drop        -> pref "iDrop"
      Take        -> pref "iTake"

      Eq          -> inf "=="
      NotEq       -> inf "/"
      Leq         -> inf "<="
      Lt          -> inf "<"

      Add         -> inf "+"
      Sub         -> inf "-"
      Mul         -> inf "*"
      Div         -> inf "/"
      Mod         -> inf "%"

      BitAnd      -> inf "&"
      BitOr       -> inf "|"
      BitXor      -> inf "^"
      Cat         -> inf "#"
      LCat        -> inf "<#"
      LShift      -> inf "<<"
      RShift      -> inf ">>"

      ArrayIndex  -> pref "aGet"
      Emit        -> pref "emit"
      EmitArray   -> pref "emitArray"
      EmitBuilder -> pref "emitBuilder"
      MapLookup   -> pref "mGet"
      MapMember   -> pref "mMember"

      ArrayStream -> pref "arrayStream"


instance PP Op2 where
  pp = ppAsOp . ppOp2

ppOp3 :: Op3 -> (PPHow, Doc)
ppOp3 op3 =
  case op3 of
    RangeUp   -> (PPPref, "rangeUp")
    RangeDown -> (PPPref, "rangeDown")
    MapInsert -> (PPPref, "mInsert")


instance PP Op3 where
  pp = ppAsOp . ppOp3

instance PP OpN where
  pp op =
    case op of
      ArrayL _ -> parens ("arrayLit")
      CallF f  -> parens ("call" <+> pp f)

instance PP e => PP (LoopMorphism e) where
  ppPrec n lp = wrapIf (n > 0) $
    kw <+> parens hdr $$ nest 2 (ppPrec 1 body)
    where
    (kw, hdr, body) =
      case lp of
        FoldMorphism x e c b -> ("for", pp x <+> "=" <+> pp e <.> semi <+> pp c, b)
        MapMorphism c b   -> ("map", pp c, b)
      
instance PP LoopCollection where
  ppPrec _ lp = ppK <+> pp (lcElName lp) <+> "in" <+> pp (lcCol lp)
    where
    ppK = case lcKName lp of
            Nothing -> empty
            Just k  -> pp k <.> comma



{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveTraversable #-}
module Daedalus.Core.Expr where

import Data.ByteString(ByteString)

import Daedalus.Panic(panic)
import Daedalus.PP

import Daedalus.Core.Basics

data Expr =
    Var Name
  | PureLet Name Expr Expr
  | Struct UserType [ (Label, Expr) ]
  | ECase (Case Expr)

  | Ap0 Op0
  | Ap1 Op1 Expr
  | Ap2 Op2 Expr Expr
  | Ap3 Op3 Expr Expr Expr
  | ApN OpN [Expr]

data Op0 =
    Unit
  | IntL Integer Type
  | BoolL Bool
  | ByteArrayL ByteString
  | NewBuilder Type
  | MapEmpty Type Type
  | ENothing Type
    deriving (Eq,Ord)

data Op1 =
    CoerceTo Type
  | CoerceMaybeTo Type
  | IsEmptyStream
  | Head
  | StreamOffset
  | StreamLen
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
  | ConsBuilder
  | MapLookup
  | MapMember

  | ArrayStream

data Op3 =
    RangeUp
  | RangeDown
  | MapInsert

data OpN =
    ArrayL Type
  | CallF FName


data Case k = Case Expr [(Pattern,k)]
  deriving (Functor,Foldable,Traversable)

eCase :: Expr -> [(Pattern,Expr)] -> Expr
eCase e ps = ECase (Case e ps)

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
coerceMaybeTo t = Ap1 (CoerceMaybeTo t)


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
eOr x y      = eIf x (boolL True) y
eAnd x y     = eIf x y (boolL False)
eIf e e1 e2  = eCase e [ (PBool True, e1), (PBool False, e2) ]


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
consBuilder   = Ap2 ConsBuilder
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

instance PP Expr where
  ppPrec n expr =
    case expr of
      Var x -> pp x
      PureLet x e1 e2 ->
        wrapIf (n > 0)
          $ "let" <+> pp x <+> "=" <+> pp e1 <+> "int"
          $$ pp e2

      Struct t fs -> ppPrec 1 t <+> braces (commaSep (map ppF fs))
        where ppF (l,e) = pp l <+> "=" <+> pp e

      ECase c -> pp c

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
      ByteArrayL b    -> pp b
      NewBuilder t    -> ppTApp n "nil" [t]
      MapEmpty t1 t2  -> ppTApp n "mEmpty"   [t1,t2]
      ENothing t      -> ppTApp n "nothing" [t]

instance PP Op1 where
  pp op =
    case op of
      CoerceTo t      -> ppTApp 0 "cast" [t]
      CoerceMaybeTo t -> ppTApp 0 "castMaybe" [t]
      IsEmptyStream   -> "iNull"
      Head            -> "iHead"
      StreamOffset    -> "iOffset"
      StreamLen       -> "iLen"
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
      ConsBuilder -> pref "cons"
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
instance PP a => PP (Case a) where
  pp (Case e as) = "case" <+> pp e <+> "of" $$ nest 2 (vcat (map alt as))
    where
    alt (p,g) = pp p <+> "->" $$ nest 2 (pp g)



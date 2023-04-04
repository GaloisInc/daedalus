module Daedalus.Core.Type where

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Daedalus.BDD as BDD
import Daedalus.PP(showPP)
import Daedalus.Panic(panic)

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.Grammar
import Daedalus.Core.Decl


bdUniverse :: Map TName TDecl -> Type -> BDD.Pat
bdUniverse env ty =
  case ty of

    TUInt w ->
      case w of
        TSize n -> BDD.pWild (fromIntegral n)
        _       -> bad

    TSInt w ->
      case w of
        TSize n -> BDD.pWild (fromIntegral n)
        _       -> bad

    TFloat          -> BDD.pWild 32
    TDouble         -> BDD.pWild 64
    TUnit           -> BDD.pWild 0
    TUser ut        -> case Map.lookup (utName ut) env of
                         Just decl | TBitdata p _ <- tDef decl -> p
                         _ -> bad

    TStream         -> bad
    TInteger        -> bad
    TBool           -> bad
    TArray {}       -> bad
    TMaybe {}       -> bad
    TMap {}         -> bad
    TBuilder {}     -> bad
    TIterator {}    -> bad
    TParam {}       -> bad
  where
  bad = panic "bdUniverse" [ "Not a bitdata type", showPP ty ]



class TypeOf t where
  typeOf :: t -> Type

instance TypeOf Name where
  typeOf = nameType

instance TypeOf FName where
  typeOf = fnameType


sizeType :: Type
sizeType = TUInt (TSize 64)

instance TypeOf Expr where
  typeOf expr =
    case expr of
      Var x -> typeOf x
      PureLet _ _ e -> typeOf e
      Struct ut _ -> TUser ut
      ECase c -> typeOf c
      ELoop lm -> typeOf lm
      
      Ap0 op ->
        case op of
          Unit         -> TUnit
          IntL _ t     -> t
          FloatL _ t   -> t
          BoolL _      -> TBool
          ByteArrayL _ -> TArray (TUInt (TSize 8))
          NewBuilder t -> TBuilder t
          MapEmpty k v -> TMap k v
          ENothing t   -> TMaybe t

      Ap1 op e ->
        case op of
          CoerceTo t      -> t
          IsEmptyStream   -> TBool
          Head            -> TUInt (TSize 8)
          StreamOffset    -> sizeType
          BytesOfStream   -> TArray (TUInt (TSize 8))
          OneOf _         -> TBool
          Neg             -> typeOf e
          BitNot          -> typeOf e
          Not             -> TBool
          ArrayLen        -> sizeType

          WordToFloat     -> TFloat
          WordToDouble    -> TDouble
          IsNaN           -> TBool
          IsInfinite      -> TBool
          IsDenormalized  -> TBool
          IsNegativeZero  -> TBool

          Concat ->
            case typeOf e of
              TArray a -> a
              _ -> bad "Concat"

          FinishBuilder ->
            case typeOf e of
              TBuilder t -> TArray t
              _ -> bad "FinishBuilder"

          NewIterator   -> TIterator (typeOf e)
          IteratorDone  -> TBool

          IteratorKey ->
            case typeOf e of
              TIterator t ->
                case t of
                  TArray _ -> sizeType
                  TMap k _ -> k
                  _        -> bad "IteratorKey/1"
              _ -> bad "IteratorKey/2"

          IteratorVal ->
            case typeOf e of
              TIterator t ->
                case t of
                  TArray x -> x
                  TMap _ v -> v
                  _        -> bad "IteratorVal/1"
              _ -> bad "IteratorVal/2"

          IteratorNext  -> typeOf e
          EJust         -> TMaybe (typeOf e)
          FromJust      -> case typeOf e of
                             TMaybe t -> t
                             _ -> bad "FromJust"
          SelStruct t _ -> t
          InUnion ut _  -> TUser ut
          FromUnion t _ -> t


      Ap2 op e1 e2 ->
        case op of
          IsPrefix  -> TBool
          Drop      -> TStream
          DropMaybe -> TMaybe TStream
          Take      -> TStream
          Eq        -> TBool
          NotEq     -> TBool
          Leq       -> TBool
          Lt        -> TBool

          Add -> typeOf e1
          Sub -> typeOf e1
          Mul -> typeOf e1
          Div -> typeOf e1
          Mod -> typeOf e1

          BitAnd -> typeOf e1
          BitOr  -> typeOf e1
          BitXor -> typeOf e1
          Cat ->
            case (typeOf e1, typeOf e2) of
              (TUInt (TSize x), TUInt (TSize y)) -> TUInt (TSize (x+y))
              _ -> bad "Cat"

          LCat   -> typeOf e1
          LShift -> typeOf e1
          RShift -> typeOf e1

          ArrayIndex ->
            case typeOf e1 of
              TArray t -> t
              _        -> bad "ArrayIndex"

          Emit        -> typeOf e1
          EmitBuilder -> typeOf e1
          EmitArray   -> typeOf e1

          ArrayStream -> TStream

          MapLookup ->
            case typeOf e1 of
              TMap _ t -> TMaybe t
              _        -> bad "MapLookup"

          MapMember -> TBool


      Ap3 op e1 _ _ ->
        case op of
          RangeUp   -> TArray (typeOf e1)
          RangeDown -> TArray (typeOf e1)
          MapInsert -> typeOf e1


      ApN op _ ->
        case op of
          ArrayL t -> TArray t
          CallF f  -> typeOf f

    where
    bad x = panic "typeOf @Expr" [ x ++ ": invalid type" ]



instance TypeOf Grammar where
  typeOf gram =
    case gram of
      Pure e          -> typeOf e
      GetStream       -> TStream
      SetStream _     -> TUnit
      Match s m       -> case s of
                           SemNo  -> TUnit
                           SemYes -> typeOf m
      Fail _ t _      -> t
      Do_ _ g         -> typeOf g
      Do  _ _ g       -> typeOf g
      Let _ _ g       -> typeOf g
      OrBiased g _    -> typeOf g
      OrUnbiased g _  -> typeOf g
      Call f _        -> typeOf f
      Annot _ g       -> typeOf g
      GCase c         -> typeOf c
      Loop lc -> case lc of
        ManyLoop SemNo _b _l _m_h _g -> TUnit
        ManyLoop _s _b _l _m_h g -> TArray (typeOf g)
        RepeatLoop _b _n _e g   -> typeOf g
        MorphismLoop lm  -> typeOf lm
        
instance TypeOf Match where
  typeOf mat =
    case mat of
      MatchEnd      -> TUnit
      MatchBytes {} -> TArray (TUInt (TSize 8))
      MatchByte {}  -> TUInt (TSize 8)

instance TypeOf a => TypeOf (Case a) where
  typeOf (Case _ as) = typeOf (snd (head as))

instance TypeOf a => TypeOf (LoopMorphism a) where
  typeOf lm =
    case lm of
      FoldMorphism _s _e _lc b -> typeOf b
      MapMorphism lc b         ->
        case typeOf (lcCol lc) of
          TArray _       -> TArray (typeOf b)
          TMap k _       -> TMap k (typeOf b)
          _ -> panic "typeOf @(LoopMorphism a)" [ "invalid type" ]
      

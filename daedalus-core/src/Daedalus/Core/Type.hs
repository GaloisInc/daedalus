module Daedalus.Core.Type where

import Daedalus.Panic(panic)

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.Grammar

class TypeOf t where
  typeOf :: t -> Type


instance TypeOf Name where
  typeOf = nameType

instance TypeOf FName where
  typeOf = fnameType


instance TypeOf Expr where
  typeOf expr =
    case expr of
      Var x -> typeOf x
      PureLet _ _ e -> typeOf e
      Struct ut _ -> TUser ut

      Ap0 op ->
        case op of
          Unit         -> TUnit
          IntL _ t     -> t
          BoolL _      -> TBool
          ByteArrayL _ -> TArray (TUInt (TSize 8))
          NewBuilder t -> TBuilder t
          MapEmpty k v -> TMap k v
          ENothing t   -> TMaybe t

      Ap1 op e ->
        case op of
          CoerceTo t      -> t
          CoerceMaybeTo t -> TMaybe t
          IsEmptyStream   -> TBool
          Head            -> TUInt (TSize 8)
          StreamOffset    -> TInteger
          StreamLen       -> TInteger
          OneOf _         -> TBool
          Neg             -> typeOf e
          BitNot          -> typeOf e
          Not             -> TBool
          ArrayLen        -> TInteger

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
                  TArray _ -> TInteger
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
          IsJust        -> TBool
          FromJust      -> case typeOf e of
                             TMaybe t -> t
                             _ -> bad "FromJust"
          SelStruct t _ -> t
          InUnion ut _  -> TUser ut
          HasTag _      -> TBool
          FromUnion t _ -> t


      Ap2 op e1 e2 ->
        case op of
          IsPrefix  -> TBool
          Drop      -> TStream
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

          Or -> TBool
          And -> TBool
          ArrayIndex ->
            case typeOf e1 of
              TArray t -> t
              _        -> bad "ArrayIndex"

          ConsBuilder -> typeOf e2
          ArrayStream -> TStream

          MapLookup ->
            case typeOf e1 of
              TMap _ t -> TMaybe t
              _        -> bad "MapLookup"

          MapMember -> TBool


      Ap3 op e1 e2 _ ->
        case op of
          PureIf    -> typeOf e2
          RangeUp   -> typeOf e1
          RangeDown -> typeOf e1
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
      Fail _ t _      -> t
      Do_ _ g         -> typeOf g
      Do  _ _ g       -> typeOf g
      Let _ _ g       -> typeOf g
      OrBiased g _    -> typeOf g
      OrUnbiased g _  -> typeOf g
      Call f _        -> typeOf f
      Annot _ g       -> typeOf g
      If _ g _        -> typeOf g



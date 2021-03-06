{-# Language BangPatterns #-}
{-# Language BlockArguments #-}
{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
module Daedalus.Core.Semantics.Expr where

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.Maybe(isJust)

import Daedalus.Range(integerToInt)
import Daedalus.Panic(panic)

import Daedalus.Value
import RTS.Input (inputBytes)

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.Type(typeOf)

import Daedalus.Core.Semantics.Env


eval :: Expr -> Env -> Value
eval expr env =
  case expr of

    Var x -> lookupVar x env

    PureLet x e1 e2 ->
      eval e2 $! defLocal x (eval e1 env) env

    Struct _ fs -> vStruct [ seq v (f,v) | (f,e) <- fs, let v = eval e env ]

    ECase c -> evalCase eval err c env
      where err = panic "eval" [ "Pattern match failure in semantic value" ]

    Ap0 op          -> partial (evalOp0 op)
    Ap1 op e        -> evalOp1 op e env
    Ap2 op e1 e2    -> evalOp2 op e1 e2 env
    Ap3 op e1 e2 e3 -> evalOp3 op e1 e2 e3 env
    ApN op es       -> evalOpN op es env

partial :: Partial Value -> Value
partial mbV =
  case mbV of
    Right a  -> a
    Left err -> error err


evalCase :: (a -> Env -> b) -> b -> Case a -> Env -> b
evalCase cont nope (Case e alts) env =
  let v = eval e env
  in case [ k | (p,k) <- alts, matches p v ] of
       g : _ -> cont g env
       []    -> nope

matches :: Pattern -> Value -> Bool
matches pat v =
  case pat of
    PBool b  -> VBool b == v
    PNothing -> case valueToMaybe v of
                  Nothing -> True
                  _       -> False
    PJust    -> case valueToMaybe v of
                  Just {} -> True
                  _       -> False
    PNum n -> valueToIntegral v == n

    PCon l -> fst (valueToUnion v) == l
    PAny   -> True





evalArgs :: [Expr] -> Env -> [Value]
evalArgs xs env =
  case xs of
    e : more ->
      let a = eval e env
          as = evalArgs more env
      in a `seq` as `seq` (a : as)
    []       -> []


--------------------------------------------------------------------------------

evalOp0 :: Op0 -> Partial Value
evalOp0 op =
  case op of
    Unit -> pure vUnit

    IntL i t ->
      case t of
        TInteger        -> pure (VInteger i)
        TUInt (TSize n) -> case integerToInt n of
                             Just w  -> pure (vUInt w i)
                             Nothing -> panic "evalOp0" [ "Vector size too big"
                                                        , show n ]
        TSInt (TSize n) -> case integerToInt n of
                             Just w  -> vSInt w i
                             Nothing -> panic "evalOp0" [ "Vector size too big"
                                                        , show n ]

        _ -> panic "evalOp0" [ "Numeric type" ]

    BoolL b         -> pure (VBool b)
    ByteArrayL bs   -> pure (vByteString bs)
    NewBuilder _    -> pure vBuilder
    MapEmpty {}     -> pure (VMap Map.empty)
    ENothing _      -> pure (VMaybe Nothing)


--------------------------------------------------------------------------------

evalType :: Type -> TValue
evalType ty =
  case ty of
    TStream       -> TVOther
    TUInt n       -> TVUInt (sizeType n)
    TSInt n       -> TVSInt (sizeType n)
    TInteger      -> TVInteger
    TBool         -> TVOther
    TUnit         -> TVOther
    TArray {}     -> TVArray
    TMaybe {}     -> TVOther
    TMap {}       -> TVMap
    TBuilder {}   -> TVOther
    TIterator {}  -> TVOther
    TUser {}      -> TVOther
    TParam {}     -> panic "evalType" [ "Unexpected type parameter" ]
  where
  sizeType x = case x of
                 TSize y -> case integerToInt y of
                              Just v  -> v
                              Nothing -> panic "evalType" [ "Size type too big"]
                 TSizeParam {} ->
                   panic "evalType" [ "Unexpected numeric type parameter" ]

evalOp1 :: Op1 -> Expr -> Env -> Value
evalOp1 op e env =
  let v = eval e env
  in case op of
      CoerceTo t    -> fst (vCoerceTo (evalType t) v)

      IsEmptyStream -> vStreamIsEmpty v
      Head          -> partial (vStreamHead v)
      StreamOffset  -> vStreamOffset v
      StreamLen     -> vStreamLength v
      OneOf bs      -> VBool $ isJust $ BS.elemIndex (valueToByte v) bs
      Neg           -> partial (vNeg v)
      BitNot        -> vComplement v
      Not           -> vNot v
      ArrayLen      -> vArrayLength v
      Concat        -> vArrayConcat v
      FinishBuilder -> vFinishBuilder v
      NewIterator ->
        case evalType (typeOf e) of
          TVArray -> vIteratorFromArray v
          TVMap   -> vIteratorFromMap v
          _       -> panic "newIterator" [ "Not a map or array", show v ]

      IteratorDone -> vIteratorDone v
      IteratorKey  -> partial (vIteratorKey v)
      IteratorVal  -> partial (vIteratorValue v)
      IteratorNext -> partial (vIteratorNext v)

      EJust -> VMaybe (Just v)
      FromJust ->
        case valueToMaybe v of
          Just r  -> r
          Nothing -> panic "evalOp1" [ "Not Just" ]

      SelStruct _ l -> vStructLookup v l

      InUnion _ l   -> VUnionElem l v
      FromUnion _ _ -> snd (valueToUnion v)

--------------------------------------------------------------------------------

evalOp2 :: Op2 -> Expr -> Expr -> Env -> Value
evalOp2 op e1 e2 env =
  let v1 = eval e1 env
      v2 = eval e2 env
  in case op of
       IsPrefix ->
         VBool $ valueToByteString v1 `BS.isPrefixOf`
                                                inputBytes (valueToStream v2)

       Drop -> partial (vStreamDrop v1 v2)
       Take -> partial (vStreamTake v1 v2)

       Eq       -> vEq  v1 v2
       NotEq    -> vNeq v1 v2
       Leq      -> vLeq v1 v2
       Lt       -> vLt  v1 v2

       Add      -> partial (vAdd v1 v2)
       Sub      -> partial (vSub v1 v2)
       Mul      -> partial (vMul v1 v2)
       Div      -> partial (vDiv v1 v2)
       Mod      -> partial (vMod v1 v2)

       BitAnd   -> vBitAnd v1 v2
       BitOr    -> vBitOr  v1 v2
       BitXor   -> vBitXor v1 v2
       Cat      -> vCat    v1 v2
       LCat     -> partial (vLCat   v1 v2)
       LShift   -> partial (vShiftL v1 v2)
       RShift   -> partial (vShiftR v1 v2)

       -- array is 1st
       ArrayIndex -> partial (vArrayIndex v1 v2)

       -- builder is 2nd
       ConsBuilder -> vConsBuilder v1 v2

       -- map is 1st
       MapLookup ->
         case vMapLookup v2 v1 of
           Right a -> VMaybe (Just a)
           Left _  -> VMaybe Nothing

       -- map is 1st
       MapMember -> vMapMember v2 v1

       ArrayStream -> vStreamFromArray v1 v2


--------------------------------------------------------------------------------

evalOp3 :: Op3 -> Expr -> Expr -> Expr -> Env -> Value
evalOp3 op e1 e2 e3 env =
  let v1 = eval e1 env
      v2 = eval e2 env
      v3 = eval e3 env
  in case op of
       RangeUp   -> partial (vRangeUp v1 v2 v3)
       RangeDown -> partial (vRangeDown v1 v2 v3)
       MapInsert -> partial (vMapInsert v2 v3 v1)


--------------------------------------------------------------------------------

evalOpN :: OpN -> [Expr] -> Env -> Value
evalOpN op es env =
  let vs = evalArgs es env
  in case op of
       ArrayL _ -> vArray vs
       CallF f  -> case vs of
                     [] -> lookupConst f env
                     _  -> lookupFun f env vs


{-# Language BangPatterns #-}
{-# Language BlockArguments #-}
{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
module Daedalus.Core.Semantics.Expr where

import GHC.Float(double2Float)

import Data.Bits(shiftR, shiftL, (.|.))
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.Maybe(isJust)
import Data.List(foldl')
import qualified Data.Text as Text

import Daedalus.Range(integerToInt)
import Daedalus.Panic(panic)
import qualified Daedalus.BDD as BDD

import Daedalus.PP(showPP)
import Daedalus.Value
import RTS.Input (inputBytes)

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import qualified Daedalus.Core.Decl as Decl
import Daedalus.Core.Type(typeOf,bdUniverse)

import Daedalus.Core.Semantics.Env


eval :: Expr -> Env -> Value
eval expr env =
  case expr of

    Var x -> lookupVar x env

    PureLet x e1 e2 ->
      eval e2 $! defLocal x (eval e1 env) env

    Struct t fs ->
      let fvs = [ seq v (f,v) | (f,e) <- fs, let v = eval e env ]
      in
      case evalType env (TUser t) of
        TVBDStruct bd -> VBDStruct bd (bdStruct bd fvs)
        _             -> vStruct fvs

    ECase c -> evalCase eval err c env
      where err = panic "eval" [ "Pattern match failure in semantic value" ]

    Ap0 op          -> partial (evalOp0 op)
    Ap1 op e        -> evalOp1 env op (typeOf e) (eval e env)
    Ap2 op e1 e2    -> evalOp2 op (eval e1 env) (eval e2 env)
    Ap3 op e1 e2 e3 -> evalOp3 op (eval e1 env) (eval e2 env) (eval e3 env)
    ApN op es       -> evalOpN op (evalArgs es env) env

partial :: Partial a -> a
partial mbV =
  case mbV of
    Right a  -> a
    Left err -> error err


evalCase :: (a -> Env -> b) -> b -> Case a -> Env -> b
evalCase cont nope (Case x alts) env =
  let v = lookupVar x env
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

    PCon l -> case v of
                VUnionElem l1 _ -> l1 == l
                VBDUnion bd i   -> bduMatches bd l i
                _ -> panic "matches" [ "Not a union" ]
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
        TFloat  -> pure (vFloat  (fromIntegral i))
        TDouble -> pure (vDouble (fromIntegral i))

        _ -> panic "evalOp0" [ "Numeric type" ]

    BoolL b         -> pure (VBool b)
    FloatL f t      -> case t of
                         TFloat  -> pure (vFloat (double2Float f))
                         TDouble -> pure (vDouble f)
                         _       -> panic "evalOp0" [ "Floating type" ]

    ByteArrayL bs   -> pure (vByteString bs)
    NewBuilder _    -> pure vBuilder
    MapEmpty {}     -> pure (VMap Map.empty)
    ENothing _      -> pure (VMaybe Nothing)


--------------------------------------------------------------------------------

evalType :: Env -> Type -> TValue
evalType env ty =
  case ty of
    TStream       -> TVOther
    TUInt n       -> TVUInt (sizeType n)
    TSInt n       -> TVSInt (sizeType n)
    TInteger      -> TVInteger
    TFloat        -> TVFloat
    TDouble       -> TVDouble
    TBool         -> TVOther
    TUnit         -> TVOther
    TArray {}     -> TVArray
    TMaybe {}     -> TVOther
    TMap {}       -> TVMap
    TBuilder {}   -> TVOther
    TIterator {}  -> TVOther
    TUser UserType { utName = nm } ->
      case Decl.tDef (lookupType nm env) of
        Decl.TBitdata univ def ->
          case def of
            Decl.BDStruct fs -> tBDStruct nm univ fs
            Decl.BDUnion fs  -> tBDUnion nm univ fs

        _ -> TVOther
    TParam {}     -> panic "evalType" [ "Unexpected type parameter" ]
  where
  sizeType x = case x of
                 TSize y -> case integerToInt y of
                              Just v  -> v
                              Nothing -> panic "evalType" [ "Size type too big"]
                 TSizeParam {} ->
                   panic "evalType" [ "Unexpected numeric type parameter" ]

  tBDStruct nm univ fs =
    let mp = Map.fromList
                [ (l, \i -> vFromBits (evalType env ft)
                                      (i `shiftR` Decl.bdOffset f))
                | f <- fs, Decl.BDData l ft <- [Decl.bdFieldType f] ]
        outField fvs w f =
          shiftL w (Decl.bdWidth f) .|.
          case Decl.bdFieldType f of
            Decl.BDWild  -> 0
            Decl.BDTag n -> n
            Decl.BDData l _ ->
              vToBits
              case lookup l fvs of
                Just fv -> fv
                Nothing -> panic "outField" ["Missing field value", showPP l ]
    in
    TVBDStruct
      BDStruct
        { bdName     = tnameText nm
        , bdWidth    = BDD.width univ
        , bdGetField = (mp Map.!)
        , bdStruct   = \fvs -> foldl' (outField fvs) 0 fs
        , bdValid    = BDD.willMatch univ
        , bdFields   = [ l | f <- fs, Decl.BDData l _ <- [ Decl.bdFieldType f ]]
        }

  tBDUnion nm univ fs =
    let mp = Map.fromList
              [ (l, (evalType env t,bdUniverse (tEnv env) t)) | (l,t) <- fs ]
        name = tnameText nm
    in
    TVBDUnion BDUnion
      { bduName  = name
      , bduWidth = BDD.width univ
      , bduValid = BDD.willMatch univ
      , bduGet   = \l -> case Map.lookup l mp of
                           Just (t,_) -> vFromBits t
                           Nothing ->
                              panic ("bduGet@" ++ Text.unpack name)
                                        ["Unknown constructor: " ++ showPP l]
      , bduMatches = \l -> BDD.willMatch
                         case Map.lookup l mp of
                           Just (_,p) -> p
                           Nothing -> panic ("bduMatches@" ++ Text.unpack name)
                                        ["Unknown constructor: " ++ showPP l]
      , bduCases = map fst fs
      }





evalOp1 :: Env -> Op1 -> Type -> Value -> Value
evalOp1 env op ty v = case op of
  CoerceTo t    -> partial (fst (vCoerceTo (evalType env t) v))

  WordToFloat     -> vWordToFloat v
  WordToDouble    -> vWordToDouble v
  IsNaN           -> vIsNaN v
  IsInfinite      -> vIsInfinite v
  IsDenormalized  -> vIsDenormalized v
  IsNegativeZero  -> vIsNegativeZero v

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
    case evalType env ty of
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

  InUnion t l   ->
    case evalType env (TUser t) of
      TVBDUnion bd -> VBDUnion bd (vToBits v)
      _            -> VUnionElem l v
  FromUnion _ l ->
    case v of
      VBDUnion bd i   -> bduGet bd l i
      VUnionElem _ v' -> v'
      _               -> panic "evalOp1" [ "FromUnion not union" ]

--------------------------------------------------------------------------------

evalOp2 :: Op2 -> Value -> Value -> Value
evalOp2 op v1 v2 = case op of
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

evalOp3 :: Op3 -> Value -> Value -> Value -> Value
evalOp3 op v1 v2 v3 = case op of
  RangeUp   -> partial (vRangeUp v1 v2 v3)
  RangeDown -> partial (vRangeDown v1 v2 v3)
  MapInsert -> vMapInsert v2 v3 v1
  
--------------------------------------------------------------------------------

evalOpN :: OpN -> [Value] -> Env -> Value
evalOpN op vs env =
  case op of
    ArrayL _ -> vArray vs
    CallF f  -> case vs of
                  [] -> lookupConst f env
                  _  -> lookupFun f env vs


{-# LANGUAGE ScopedTypeVariables  #-}
{-# Language GADTs #-}

module Daedalus.ParserGen.LL.ClassInterval
  ( IntervalEndpoint(..)
  , ClassInterval(..)
  , showGraphvizClassInterval
  , insertItvInOrderedList
  , matchClassInterval
  , classToInterval
  )

where

-- import Debug.Trace

import Data.Word


import qualified Daedalus.Interp as Interp
import Daedalus.Type.AST

import Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (evalNoFunCall, isSimpleVExpr)

import Daedalus.ParserGen.LL.Result as R

data IntervalEndpoint =
    PlusInfinity
  | MinusInfinity
  | CValue Word8
  deriving(Eq)

instance Ord IntervalEndpoint where
  (<=) MinusInfinity _ = True
  (<=) (CValue _) MinusInfinity = False
  (<=) (CValue x) (CValue y) = x <= y
  (<=) (CValue _) PlusInfinity = True
  (<=) PlusInfinity PlusInfinity = True
  (<=) PlusInfinity _ = False

instance Show IntervalEndpoint where
  show PlusInfinity = "+inf"
  show MinusInfinity = "-inf"
  show (CValue i) = show (toEnum (fromIntegral i) :: Char)


showGraphvizIntervalPoint :: IntervalEndpoint -> String
showGraphvizIntervalPoint a =
  case a of
    CValue i ->
      let x = toInteger i in
      if (48 <= x && x <= 57) || (65 <= x && x <= 90) || (97 <= x && x <= 122)
      then (toEnum (fromIntegral i) :: Char) : ""
      else "0x" ++ show (fromIntegral i :: Integer)
    _ -> show a

incrItv :: IntervalEndpoint -> IntervalEndpoint
incrItv i =
  case i of
    PlusInfinity -> error "cannot increment plus infinity"
    MinusInfinity -> error "cannot increment minis infinity"
    CValue n -> CValue (n+1)

decrItv :: IntervalEndpoint -> IntervalEndpoint
decrItv i =
  case i of
    PlusInfinity -> error "cannot decrement plus infinity"
    MinusInfinity -> error "cannot decrement minis infinity"
    CValue n -> CValue (n-1)


data ClassInterval =
    ClassBtw IntervalEndpoint IntervalEndpoint

instance Show ClassInterval where
  show (ClassBtw i j) = if i == j then "[" ++ show i ++ "]" else "[" ++ show i ++ "," ++ show j ++ "]"




showGraphvizClassInterval :: ClassInterval -> String
showGraphvizClassInterval c =
  case c of
    ClassBtw i j ->
      if i == j
      then showGraphvizIntervalPoint i
      else showGraphvizIntervalPoint i ++ "-" ++ showGraphvizIntervalPoint j


data Who = A1 | A2 | A12
  deriving(Eq)

combineInterval :: ClassInterval -> ClassInterval -> [(ClassInterval, Who)]
combineInterval itv1 itv2 =
  case (itv1, itv2) of
    (ClassBtw i1 j1, ClassBtw i2 j2) ->
      case (compare i2 i1, compare i2 j1) of
        (LT, LT) -> -- i2 < i1 <= j1
          case (compare j2 i1, compare j2 j1) of
            (LT, LT) -> [(itv2, A2), (itv1, A1)]
            (LT, _ ) -> error "impossible"
            (EQ, LT) -> [ (ClassBtw i2 (decrItv j2), A2), (ClassBtw j2 j2, A12), (ClassBtw (incrItv j2) j1, A1) ]
            (EQ, EQ) -> [ (ClassBtw i2 (decrItv j2), A2), (ClassBtw j2 j2, A12)]
            (EQ, GT) -> error "impossible"
            (GT, LT) -> [ (ClassBtw i2 (decrItv i1), A2), (ClassBtw i1 j2, A12), (ClassBtw (incrItv j2) j1, A1) ]
            (GT, EQ) -> [ (ClassBtw i2 (decrItv i1), A2), (ClassBtw i1 j2, A12)]
            (GT, GT) -> [ (ClassBtw i2 (decrItv i1), A2), (ClassBtw i1 j1, A12), (ClassBtw (incrItv j1) j2, A2) ]
        (LT, EQ) -> error "impossible"
        (LT, GT) -> error "impossible"
        (EQ, EQ) -> -- i1 == j1 == i2
          case (compare j2 i1, compare j2 j1) of
            (LT, _) -> error "impossible"
            (EQ, EQ) -> -- i2 == j2 == *
              [(ClassBtw i1 i1, A12)]
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> error "impossible"
            (GT, GT) -> [(ClassBtw i1 i1, A12), (ClassBtw (incrItv i1) j2, A2)]
        (EQ, LT) -> -- i1 == i2 and i1 < j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _) -> error "impossible"
            (EQ, LT) -> [(ClassBtw i1 i1, A12), (ClassBtw (incrItv i1) j1, A1)]
            (EQ, _) -> error "impossible"
            (GT, LT) -> [(ClassBtw i1 j2, A12), (ClassBtw (incrItv j2) j1, A1)]
            (GT, EQ) -> [(ClassBtw i1 j1, A12)]
            (GT, GT) -> [(ClassBtw i1 j1, A12), (ClassBtw (incrItv j1) j2, A2)]
        (EQ, GT) -> error "impossible"
        (GT, LT) -> -- i1 < i2 < j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> [(ClassBtw i1 (decrItv i2), A1), (ClassBtw i2 j2, A12), (ClassBtw (incrItv j2) j1, A1)]
            (GT, EQ) -> [(ClassBtw i1 (decrItv i2), A1), (ClassBtw i2 j2, A12)]
            (GT, GT) -> [(ClassBtw i1 (decrItv i2), A1), (ClassBtw i2 j1, A12), (ClassBtw (incrItv j1) j2, A2)]
        (GT, EQ) -> -- i1 < i2 == j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> [(ClassBtw i1 (decrItv i2), A1), (ClassBtw i2 i2, A12)]
            (GT, GT) -> [(ClassBtw i1 (decrItv i2), A1), (ClassBtw i2 i2, A12), (ClassBtw (incrItv i2) j2, A2)]
        (GT, GT) -> -- i1 <= j1 < i2
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> error "impossible"
            (GT, GT) -> [(itv1, A1), (itv2, A2)]


insertItvInOrderedList :: forall a. (ClassInterval, a) -> [(ClassInterval, a)] -> (a -> a -> a) -> [(ClassInterval,a)]
insertItvInOrderedList (itv, a) lstItv merge =
  step [] (itv,a) lstItv
  where
    step :: [(ClassInterval, a)] -> (ClassInterval, a) -> [(ClassInterval, a)] -> [(ClassInterval, a)]
    step acc (itv1, a1) lst =
      case lst of
        [] -> reverse acc ++ [(itv1, a1)]
        (itv2, a2) : rest ->
          let comb = combineInterval itv1 itv2 in
            insertComb a1 a2 acc (reverse comb) rest

    insertComb :: a -> a -> [(ClassInterval, a)] -> [(ClassInterval, Who)] -> [(ClassInterval, a)] -> [(ClassInterval, a)]
    insertComb a1 a2 acc revComb rest =
      case revComb of
        [] -> error "nothing"
        (itv3, w) : xs ->
          case w of
            A1 ->
              let newAcc = applyAdd a1 a2 xs ++ acc
              in step newAcc (itv3, a1) rest
            _ -> reverse acc ++ reverse (applyAdd a1 a2 revComb) ++ rest


    applyAdd :: a -> a -> [(ClassInterval, Who)] -> [(ClassInterval, a)]
    applyAdd a1 a2 lst = map (expandAdd a1 a2) lst

    expandAdd :: a -> a -> (ClassInterval, Who) -> (ClassInterval, a)
    expandAdd a1 a2 (i,w) =
      case w of
        A1 -> (i, a1)
        A2 -> (i, a2)
        A12 -> (i, merge a1 a2)


matchClassInterval :: ClassInterval -> Word8 -> Bool
matchClassInterval itv c =
  case itv of
    ClassBtw (CValue a) (CValue b) -> a <= c && c <= b
    ClassBtw {} -> undefined


classToInterval :: PAST.NCExpr -> Result ClassInterval
classToInterval e =
  case texprValue e of
    TCSetAny -> Result $ ClassBtw MinusInfinity PlusInfinity
    TCSetSingle e1 ->
      if not (isSimpleVExpr e1)
      then Abort AbortClassIsDynamic
      else
        let v = evalNoFunCall e1 [] [] in
        case v of
          Interp.VUInt 8 x ->
            let vx = fromIntegral x
            in Result $ ClassBtw (CValue vx) (CValue vx)
          _                -> Abort (AbortClassNotHandledYet "SetSingle")
    TCSetRange e1 e2 ->
      if isSimpleVExpr e1 && isSimpleVExpr e2
      then
        let v1 = evalNoFunCall e1 [] []
            v2 = evalNoFunCall e2 [] []
        in case (v1, v2) of
             (Interp.VUInt 8 x, Interp.VUInt 8 y) ->
               let x1 = fromIntegral x
                   y1 = fromIntegral y
               in if x1 <= y1
                  then Result $ ClassBtw (CValue x1) (CValue y1)
                  else error ("SetRange values not ordered:" ++
                              show (toEnum (fromIntegral x1) :: Char) ++ " " ++
                              show (toEnum (fromIntegral y1) :: Char))
             _ -> Abort (AbortClassNotHandledYet "SetRange")
      else Abort AbortClassIsDynamic
    _ -> Abort (AbortClassNotHandledYet "other class case")

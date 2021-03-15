{-# LANGUAGE ScopedTypeVariables  #-}
{-# Language GADTs #-}

module Daedalus.ParserGen.LL.ClassInterval
  ( IntervalEndpoint(..)
  , ClassInterval(..)
  , showGraphvizClassInterval
  , insertByteConditionInOrderedList
  , unionClassIntervalList
  , matchClassInterval
  , classToInterval
  , ByteCondition(..)
  , showGraphvizByteCondition
  , matchByteCondition
  )

where

-- import Debug.Trace

import Numeric (showHex)
import Data.ByteString (unpack)
import Data.Word
import Data.List (intersperse)

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
      else "x" ++ showHex (fromIntegral i :: Integer) ""
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

-- combine interval and return an ordered list
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

newtype ByteCondition =
  ByteCondition { byteCondition :: [ClassInterval] }

instance Show ByteCondition where
  show b =
    concat $ intersperse
    "-"
    (map (\ c -> show c) (byteCondition b))



showGraphvizByteCondition :: ByteCondition -> String
showGraphvizByteCondition b =
  concat $ intersperse
  " and "
  (map (\ c -> showGraphvizClassInterval c)
   (byteCondition b) )


insertItvInOrderedList ::
  forall a.
  (ClassInterval, a) -> [(ClassInterval, a)] -> (a -> a -> a) -> [(ClassInterval,a)]
insertItvInOrderedList (itv, a) lstItv merge =
  go [] (itv,a) lstItv
  where
    go ::
      [(ClassInterval, a)] -> (ClassInterval, a) -> [(ClassInterval, a)] ->
      [(ClassInterval, a)]
    go acc (itv1, a1) lst =
      case lst of
        [] -> reverse acc ++ [(itv1, a1)]
        (itv2, a2) : rest ->
          let comb = combineInterval itv1 itv2 in
          case updateAcc acc a1 a2 (reverse comb) of
            (newAcc, Nothing) ->
              reverse newAcc ++ rest
            (newAcc, Just (itv3, a3)) ->
              go newAcc (itv3, a3) rest

    -- Update the accumulator and decided whether there is more to do.
    -- This function is constant time with revComb being of max length 3
    updateAcc ::
      [(ClassInterval, a)] -> a -> a -> [(ClassInterval, Who)] ->
      ([(ClassInterval, a)], Maybe (ClassInterval, a))
    updateAcc acc a1 a2 revComb =
      case revComb of
        [] -> error "nothing"
        (itv3, w) : xs ->
          case w of
            A1 ->
              -- if the last element is `A1` then the accumulator is
              -- just updated with the other elements and the last
              -- element is just returned
              let newAcc = applyAdd a1 a2 xs ++ acc
              in (newAcc, Just (itv3, a1))
            _ ->
              -- if the last element is different from `A1` then the
              -- accumulator is updated with all the intervals and
              -- nothing is left to insert
              let newAcc = applyAdd a1 a2 revComb ++ acc
              in (newAcc, Nothing)


    applyAdd :: a -> a -> [(ClassInterval, Who)] -> [(ClassInterval, a)]
    applyAdd a1 a2 lst = map (expandAdd a1 a2) lst

    expandAdd :: a -> a -> (ClassInterval, Who) -> (ClassInterval, a)
    expandAdd a1 a2 (i,w) =
      case w of
        A1 -> (i, a1)
        A2 -> (i, a2)
        A12 -> (i, merge a1 a2)


insertByteConditionInOrderedList ::
  forall a.
  (ByteCondition, a) -> [(ClassInterval, a)] -> (a -> a -> a) -> [(ClassInterval, a)]
insertByteConditionInOrderedList (bc, a) lstItv merge =
  let ByteCondition lst = bc in
  foldr (\ itv b -> insertItvInOrderedList (itv, a) b merge) lstItv lst


unionClassIntervalList ::
  [(ClassInterval, a)] -> [(ClassInterval, a)] -> (a -> a -> a) ->
  [(ClassInterval, a)]
unionClassIntervalList lst1 lst2 merge =
  foldr
  (\ (itv, s) acc -> insertItvInOrderedList (itv, s) acc merge)
  lst2
  lst1


matchClassInterval :: ClassInterval -> Word8 -> Bool
matchClassInterval itv c =
  case itv of
    ClassBtw (CValue a) (CValue b) -> a <= c && c <= b
    ClassBtw {} -> undefined


classToInterval :: PAST.NCExpr -> Result ByteCondition
classToInterval e =
  case texprValue e of
    TCSetAny -> Result $ ByteCondition [ ClassBtw (CValue 0) (CValue 255) ]
    TCSetSingle e1 ->
      if not (isSimpleVExpr e1)
      then Abort AbortClassIsDynamic
      else
        let v = evalNoFunCall e1 [] [] in
        case v of
          Interp.VUInt 8 x ->
            let vx = fromIntegral x
            in Result $ ByteCondition [ ClassBtw (CValue vx) (CValue vx) ]
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
                  then Result $ ByteCondition [ ClassBtw (CValue x1) (CValue y1) ]
                  else error ("SetRange values not ordered:" ++
                              show (toEnum (fromIntegral x1) :: Char) ++ " " ++
                              show (toEnum (fromIntegral y1) :: Char))
             _ -> Abort (AbortClassNotHandledYet "SetRange")
      else Abort AbortClassIsDynamic
    TCSetUnion lst ->
      let mLItv = go lst [] in
      case mLItv of
        Result lItv ->
          Result (ByteCondition (map (\ (a,()) -> a) lItv))
        Abort (AbortClassNotHandledYet _) -> coerceAbort mLItv
        Abort AbortClassIsDynamic -> coerceAbort mLItv
        _ -> error "case not possible"

      where
        go l acc =
          case l of
            [] -> Result acc
            e1 : rest ->
              let mbc = classToInterval e1 in
              let
                mNewAcc =
                  case mbc of
                    Result bc ->
                      Result $ insertByteConditionInOrderedList (bc, ()) acc (\ () () -> ())
                    Abort (AbortClassNotHandledYet _) -> coerceAbort mbc
                    Abort AbortClassIsDynamic -> coerceAbort mbc
                    _ -> error "case not possible"
              in
              case mNewAcc of
                Result newAcc -> go rest newAcc
                Abort (AbortClassNotHandledYet _) -> coerceAbort mNewAcc
                Abort AbortClassIsDynamic -> coerceAbort mNewAcc
                _ -> error "case not possible"
    TCSetOneOf lst ->
      let lItv = go (unpack lst) [] in
      Result (ByteCondition (map (\ (a,()) -> a) lItv))
      where
        go l acc =
          case l of
            [] -> acc
            b : rest ->
              let
                newAcc =
                  insertItvInOrderedList (ClassBtw (CValue b) (CValue b), ()) acc (\ () () -> ())
              in
                go rest newAcc

    TCSetComplement e1 ->
      let
        mbc = classToInterval e1
        go i lst acc =
          case lst of
            [] ->
              if i <= 255
              then Result $ ByteCondition (reverse (ClassBtw (CValue i) (CValue 255) : acc))
              else Result $ ByteCondition (reverse acc)
            ClassBtw (CValue j) (CValue k) : rest ->
              let j1 = j-1 in
              if i <= j1
              then
                go (k+1) rest (ClassBtw (CValue i) (CValue (j1)) : acc)
              else
                go (k+1) rest acc
      in
      case mbc of
        Result bc ->
          go 0 (byteCondition bc) []
        Abort (AbortClassNotHandledYet _) -> coerceAbort mbc
        Abort AbortClassIsDynamic -> coerceAbort mbc
        _ -> error "Should not be another"
    TCSetDiff _ _ -> error "Not implemented yet: SetDiff LL"
    _ ->
      -- trace (show (texprValue e)) $
      Abort (AbortClassNotHandledYet "other class case")


matchByteCondition :: ByteCondition -> Word8 -> Bool
matchByteCondition b byte  =
  go (byteCondition b)

  where
    go l =
      case l of
        [] -> False
        itv : itvs ->
          if matchClassInterval itv byte
          then True
          else go itvs

{-# LANGUAGE ScopedTypeVariables  #-}
{-# Language GADTs #-}

module Daedalus.ParserGen.LL.ClassInterval
  ( IntervalEndpoint(..)
  , ClassInterval(..)
  , showGraphvizClassInterval
  , insertItvInOrderedList
  , unionClassIntervalList
  , matchClassInterval
  , ByteCondition(..)
  , insertByteConditionInOrderedList
  , showGraphvizByteCondition
  , matchByteCondition
  )

where

-- import Debug.Trace

import Numeric (showHex)
import Data.Word
import Data.List (intersperse)



data IntervalEndpoint =
  CValue Word8
  deriving(Eq)

instance Ord IntervalEndpoint where
  (<=) (CValue x) (CValue y) = x <= y

instance Show IntervalEndpoint where
  show (CValue i) = show (toEnum (fromIntegral i) :: Char)


showGraphvizIntervalPoint :: IntervalEndpoint -> String
showGraphvizIntervalPoint a =
  case a of
    CValue i ->
      let x = toInteger i in
      if (48 <= x && x <= 57) || (65 <= x && x <= 90) || (97 <= x && x <= 122)
      then (toEnum (fromIntegral i) :: Char) : ""
      else "x" ++ showHex (fromIntegral i :: Integer) ""

incrItv :: IntervalEndpoint -> IntervalEndpoint
incrItv i =
  case i of
    CValue n -> CValue (n+1)

decrItv :: IntervalEndpoint -> IntervalEndpoint
decrItv i =
  case i of
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


insertByteConditionInOrderedList ::
  forall a.
  (ByteCondition, a) -> [(ClassInterval, a)] -> (a -> a -> a) -> [(ClassInterval, a)]
insertByteConditionInOrderedList (bc, a) lstItv merge =
  let ByteCondition lst = bc in
  foldr (\ itv b -> insertItvInOrderedList (itv, a) b merge) lstItv lst



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

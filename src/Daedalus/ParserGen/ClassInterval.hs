module Daedalus.ParserGen.ClassInterval where


data IntervalEndpoint =
    PlusInfinity
  | MinusInfinity
  | CValue Integer
  deriving(Eq)

instance Ord(IntervalEndpoint) where
  (<=) MinusInfinity _ = True
  (<=) (CValue _) MinusInfinity = False
  (<=) (CValue x) (CValue y) = x <= y
  (<=) (CValue _) PlusInfinity = True
  (<=) PlusInfinity PlusInfinity = True
  (<=) PlusInfinity _ = False

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


combineInterval :: (ClassInterval, a) -> (ClassInterval, a) -> (a -> a -> a) -> [(ClassInterval, a)]
combineInterval (itv1, a1) (itv2, a2) add =
  case (itv1, itv2) of
    (ClassBtw i1 j1, ClassBtw i2 j2) ->
      case (compare i2 i1, compare i2 j1) of
        (LT, LT) -> -- i2 < i1 <= j1
          case (compare j2 i1, compare j2 j1) of
            (LT, LT) -> [(itv2, a2), (itv1, a1)]
            (LT, _ ) -> error "impossible"
            (EQ, LT) -> [ (ClassBtw i2 (decrItv j2), a2),(ClassBtw j2 j2, add a1 a2),(ClassBtw (incrItv j2) j1, a1) ]
            (EQ, EQ) -> [ (ClassBtw i2 (decrItv j2), a2),(ClassBtw j2 j2, add a1 a2)]
            (EQ, GT) -> error "impossible"
            (GT, LT) -> [ (ClassBtw i2 (decrItv i1), a2), (ClassBtw i1 j2, add a1 a2), (ClassBtw (incrItv j2) j1, a1) ]
            (GT, EQ) -> [ (ClassBtw i2 (decrItv i1), a2), (ClassBtw i1 j2, add a1 a2)]
            (GT, GT) -> [ (ClassBtw i2 (decrItv i1), a2), (ClassBtw i1 j1, add a1 a2), (ClassBtw (incrItv j1) j2, a2) ]
        (LT, EQ) -> error "impossible"
        (LT, GT) -> error "impossible"
        (EQ, EQ) -> -- i1 == j1 == i2
          case (compare j2 i1, compare j2 j1) of
            (LT, _) -> error "impossible"
            (EQ, EQ) -> -- i2 == j2 == *
              [(ClassBtw i1 i1, add a1 a2)]
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> error "impossible"
            (GT, GT) -> [(ClassBtw i1 i1, add a1 a2), (ClassBtw (incrItv i1) j2, a2)]
        (EQ, LT) -> -- i1 == i2 and i1 < j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _) -> error "impossible"
            (EQ, LT) -> [(ClassBtw i1 i1, add a1 a2), (ClassBtw (incrItv i1) j1, a1)]
            (EQ, _) -> error "impossible"
            (GT, LT) -> [(ClassBtw i1 j2, add a1 a2), (ClassBtw (incrItv j2) j1, a1)]
            (GT, EQ) -> [(ClassBtw i1 j1, add a1 a2)]
            (GT, GT) -> [(ClassBtw i1 j1, add a1 a2), (ClassBtw (incrItv j1) j2, a2)]
        (EQ, GT) -> error "impossible"
        (GT, LT) -> -- i1 < i2 < j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 j2, add a1 a2), (ClassBtw (incrItv j2) j1, a1)]
            (GT, EQ) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 j2, add a1 a2)]
            (GT, GT) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 j1, add a1 a2), (ClassBtw (incrItv j1) j2, a2)]
        (GT, EQ) -> -- i1 < i2 == j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 i2, add a1 a2)]
            (GT, GT) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 i2, add a1 a2), (ClassBtw (incrItv i2) j2, a2)]
        (GT, GT) -> -- i1 <= j1 < i2
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> error "impossible"
            (GT, GT) -> [(itv1, a1), (itv2, a2)]

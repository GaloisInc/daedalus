module RTS.ByteRanges
  ( Ranges
  , noRanges
  , insertRange
  , constainRanges
  , foldRanges
  , rangesToList
  ) where

data Ranges = NoRanges
            | Ranges {-# UNPACK #-} !RangesNonEmpty

data Sub = Whole
         | Split {-# UNPACK #-} !RangesNonEmpty
                 {-# UNPACK #-} !RangesNonEmpty

data RangesNonEmpty = RangesNonEmpty
  { size         :: {-# UNPACK #-} !Int

  , lowerBound   :: {-# UNPACK #-} !Int
  , upperBound   :: {-# UNPACK #-} !Int

  , subDivisions :: !Sub
  }


whole :: Int -> Int -> RangesNonEmpty
whole low high =
  RangesNonEmpty {
    size = 1,
    lowerBound = low,
    upperBound = high,
    subDivisions = Whole
  }

disjoint :: RangesNonEmpty -> RangesNonEmpty -> RangesNonEmpty
disjoint lowR highR =
  RangesNonEmpty {
    size         = size lowR + size highR,
    lowerBound   = lowerBound lowR,
    upperBound   = upperBound highR,
    subDivisions = Split lowR highR
  }



touching :: RangesNonEmpty -> RangesNonEmpty -> RangesNonEmpty
touching lowR highR =
  RangesNonEmpty {
    size       = size lowR + size highR - 1,
    lowerBound = lowerBound lowR,
    upperBound = upperBound highR,
    subDivisions =
      case (subDivisions lowR, subDivisions highR) of
        (Whole,Whole)      -> Whole
        (Whole, Split l h) -> Split (touching lowR l) h
        (Split l h, Whole) -> Split l (touching h highR)
        (Split ll lh, Split hl hh) ->
          let mid = touching lh hl
          in if size ll < size hh
                then Split (disjoint ll mid) hh
                else Split ll (disjoint mid hh)
  }




addNE :: Int -> Int -> RangesNonEmpty -> RangesNonEmpty
addNE l h r
  | h + 1 < lowerBound r  = disjoint (whole l h) r
  | h + 1 == lowerBound r = touching (whole l h) r
  | upperBound r + 1 < l  = disjoint r (whole l h)
  | upperBound r + 1 == l = touching r (whole l h)
  | otherwise =  -- overlap
    case subDivisions r of
      Whole -> whole (min l (lowerBound r))
                     (max h (upperBound r))

      Split low high ->
        case compare (h+1) (lowerBound high) of
          LT -> disjoint (addNE l h low) high
          EQ -> touching (addNE l h low) high
          GT -> -- h is in high or beyond
            case compare (upperBound low + 1) l of
              LT -> disjoint low (addNE l h high)
              EQ -> touching low (addNE l h high)
              GT -> -- l is in low or beyond
                touching (addNE l (lowerBound high - 1) low)
                         (addNE (lowerBound high) h high)

insertRange :: Int -> Int -> Ranges -> Ranges
insertRange l h r
  | h < l = r
  | otherwise =
    case r of
      NoRanges  -> Ranges (whole l h)
      Ranges ne -> Ranges (addNE l h ne)


foldRanges :: (Int -> Int -> a -> a) -> Ranges -> a -> a
foldRanges f r =
  case r of
    NoRanges  -> id
    Ranges ne -> foldRangesNE f ne

foldRangesNE ::
  (Int -> Int -> a -> a) -> RangesNonEmpty -> a -> a
foldRangesNE f r =
  case subDivisions r of
    Whole     -> f (lowerBound r) (upperBound r)
    Split l h -> foldRangesNE f l . foldRangesNE f h


noRanges :: Ranges
noRanges = NoRanges

constainRanges :: Int -> Ranges -> Bool
constainRanges x r =
  case r of
    NoRanges -> False
    Ranges ne -> constainRangeNE x ne

constainRangeNE :: Int -> RangesNonEmpty -> Bool
constainRangeNE x r =
  lowerBound r <= x && x <= upperBound r &&
                            containsSub x (subDivisions r)

containsSub :: Int -> Sub -> Bool
containsSub x s =
  case s of
    Whole -> True
    Split l r
      | x <= upperBound l -> constainRangeNE x l
      | lowerBound r <= x -> constainRangeNE x r
      | otherwise         -> False


rangesToList :: Ranges -> [ByteRange]
rangesToList r = foldRanges cons r []
  where cons x y xs = if x == y then B x : xs else I x y : xs

data ByteRange = B Int | I Int Int

instance Show ByteRange where
  show b = case b of
             B n -> show n
             I x y -> show x ++ "--" ++ show y


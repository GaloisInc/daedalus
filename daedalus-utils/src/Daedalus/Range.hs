module Daedalus.Range where

-- | Closed interval for an n-bit unsigned number
uintRange :: Int -> (Integer,Integer)
uintRange w = (0, 2^w - 1)

-- | Closed interval for an n-bit signed number
sintRange :: Int -> (Integer,Integer)
sintRange w = (- (2^w'), 2^w' - 1)
  where w' = w - 1

-- | Closed interval for a Haskell 'Int'
intRange :: (Integer,Integer)
intRange = (toInteger (minBound :: Int), toInteger (maxBound :: Int))

-- | Check if the integer belongs to the given closed interval
inRange :: (Integer,Integer) -> Integer -> Bool
inRange (l,u) x = l <= x && x <= u

integerToInt :: Integer -> Maybe Int
integerToInt i = if inRange intRange i then Just (fromIntegral i) else Nothing


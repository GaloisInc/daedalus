{-# Language OverloadedStrings #-}
module RTS.InputTrace (InputTrace) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Short(ShortByteString)
import qualified Data.ByteString.Short as BSS
import Text.PrettyPrint
import RTS.JSON

import RTS.Input
import RTS.ParseError

{- | A collection of annotated byte sequences.
This maps each input name to the bytes in the input, and a collection
of ranges on those bytes.

Assumes that inputs will have non-overlapping names.
-}
data InputTrace = IT (Map ShortByteString Ranges)
  deriving Show

data Ranges = Ranges
  { rRanges :: ![Range]         -- ^ Ranges
  , rBytes  :: !ByteString      -- ^ Bytes of input
  } deriving Show

instance IsITrace InputTrace where
  emptyITrace = const emptyInputTrace
  addITrace   = addInputTrace
  unionITrace = unionInputTrace
  ppITrace    = ppInputTrace

instance HasInputs InputTrace where
  getInputs (IT mp) = rBytes <$> mp

instance ToJSON InputTrace where
  toJSON (IT mp) = jsArray (map one (Map.toList mp))
    where one (n,rs) = jsObject [ ("input",  toJSON n)
                                , ("ranges", jsArray (map toJSON (rRanges rs)))
                                ]

ppInputTrace :: InputTrace -> Doc
ppInputTrace (IT mp) = vcat [ one x | x <- Map.toList mp ]
  where
  one (n,rs) = text (BS8.unpack (BSS.fromShort n)) $$
                nest 2 (hsep (map ppRange (rRanges rs)))

emptyInputTrace :: InputTrace
emptyInputTrace = IT Map.empty

unionInputTrace :: InputTrace -> InputTrace -> InputTrace
unionInputTrace (IT x) (IT y) = IT (Map.unionWith merge x y)
  where merge a b = a { rRanges = mergeRanges (rRanges a) (rRanges b) }

singletonInputTrace :: Input -> InputTrace
singletonInputTrace i = IT (Map.singleton (inputName i) rs)
  where r  = inputOffset i
        rs = Ranges { rBytes  = inputTopBytes i
                    , rRanges = [R r r]
                    }

addInputTrace :: Input -> InputTrace -> InputTrace
addInputTrace = unionInputTrace . singletonInputTrace

--------------------------------------------------------------------------------

data Range = R !Int !Int
  deriving Show

instance ToJSON Range where
  toJSON (R x y) = toJSON (x,y)

ppRange :: Range -> Doc
ppRange (R x y) = hcat [ int x, "--", int y ]

mergeRanges :: [Range] -> [Range] -> [Range]
mergeRanges a b = fixUp (mergeRanges1 a b)

mergeRanges1 :: [Range] -> [Range] -> [Range]
mergeRanges1 xs ys =
  case (xs,ys) of
    ([],_) -> ys
    (_,[]) -> xs
    (x@(R a _) : moreA, y@(R b _) : moreB)
      | a <= b    -> x : mergeRanges1 moreA ys
      | otherwise -> y : mergeRanges1 xs moreB

fixUp :: [Range] -> [Range]
fixUp xs =
  case xs of
    R a b : rest@(R c d : more)
      | b + 1 < c -> R a b : fixUp rest
      | otherwise -> fixUp (R a (max b d) : more)
    _ -> xs




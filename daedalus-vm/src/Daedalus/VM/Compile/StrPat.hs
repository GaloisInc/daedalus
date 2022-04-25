module Daedalus.VM.Compile.StrPat where

import Data.Word(Word8)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.Map(Map)
import qualified Data.Map as Map

import Daedalus.Panic(panic)

strDecisionTree :: [(ByteString,a)] -> Map Int (StrTree a)
strDecisionTree xs =
  Map.fromListWith mergeTree [ (BS.length b, caseToTree x) | x@(b,_) <- xs ]

data StrTree a =
    StrDone a
  | StrCase (Map Word8 (StrTree a))
    deriving Show

mergeTree :: StrTree a -> StrTree a -> StrTree a
mergeTree t1 t2 =
  case (t1,t2) of
    (StrDone _,   StrDone y)   -> StrDone y -- duplicate cases, prefer older one
    (StrCase mp1, StrCase mp2) -> StrCase (Map.unionWith mergeTree mp1 mp2)
    _                          -> panic "mergeTree" [ "Length mismatch" ]

caseToTree :: (ByteString,a) -> StrTree a
caseToTree (k,v) =
  case BS.uncons k of
    Nothing     -> StrDone v
    Just (c,cs) -> StrCase (Map.singleton c (caseToTree (cs,v)))



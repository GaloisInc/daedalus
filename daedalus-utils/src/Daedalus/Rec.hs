{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
module Daedalus.Rec where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Graph(SCC(..))
import Data.Graph.SCC(stronglyConnComp)
import Daedalus.PP

data Rec a = NonRec a | MutRec [a]
  deriving (Show, Functor, Traversable, Foldable)

sccToRec :: SCC a -> Rec a
sccToRec (AcyclicSCC x)  = NonRec x
sccToRec (CyclicSCC  xs) = MutRec xs

recToList :: Rec a -> [a]
recToList (NonRec d)  = [d]
recToList (MutRec ds) = ds

forgetRecs :: [Rec a] -> [a]
forgetRecs = concatMap recToList

instance PP a => PP (Rec a) where
  ppPrec n x =
    case x of
      NonRec d   -> ppPrec n d
      MutRec ds  -> "rec" $$ nest 2 (vcat' (map pp ds))

topoOrder :: Ord b => (a -> (b,Set b)) -> [a] -> [Rec a]
topoOrder conc = map sccToRec . stronglyConnComp . map node
  where node a = case conc a of
                   (x,xs) -> (a,x,Set.toList xs)


{-# Language BlockArguments #-}
module Daedalus.Core.Effect(annotateNoFail, canFailFun, canFail) where

import Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.Core

canFailFun :: Fun Grammar -> Bool
canFailFun f =
  case fDef f of
    External -> True
    Def e    -> canFail e

canFail :: Grammar -> Bool
canFail gram =
  case gram of
    Pure {}           -> False
    GetStream {}      -> False
    SetStream {}      -> False
    Fail {}           -> True
    Call {}           -> True

    Annot a g         -> case a of
                           NoFail -> False
                           _      -> canFail g

    Do_ g1 g2         -> canFail g1 || canFail g2
    Do _ g1 g2        -> canFail g1 || canFail g2
    Let _ _ g         -> canFail g
    OrBiased _ g2     -> canFail g2
    OrUnbiased g1 g2  -> canFail g1 && canFail g2
    If _ g1 g2        -> canFail g1 || canFail g2

-- | Cache analysis results in annotation nodes
annotate :: (FName -> [Expr] -> Grammar) -> Grammar -> Grammar
annotate annCall = annot
  where
  annot gram =
    addAnn
    case gram of
      Pure {}           -> gram
      GetStream {}      -> gram
      SetStream {}      -> gram
      Fail {}           -> gram

      Call f es         -> annCall f es

      Annot a g         -> Annot a (annot g)

      Do_ g1 g2         -> Do_ (annot g1) (annot g2)
      Do x g1 g2        -> Do x (annot g1) (annot g2)
      Let x e g         -> Let x e (annot g)
      OrBiased g1 g2    -> OrBiased (annot g1) (annot g2)
      OrUnbiased g1 g2  -> OrUnbiased (annot g1) (annot g2)
      If e g1 g2        -> If e (annot g1) (annot g2)

  annNoFail g = case g of

                  -- these are already known to not fail, so no need for annot
                  Annot NoFail _ -> g
                  Pure {}        -> g
                  GetStream {}   -> g
                  SetStream {}   -> g

                  _              -> Annot NoFail g

  addAnn g1 = if canFail g1 then g1 else annNoFail g1


analyseF :: Set FName ->  Fun Grammar -> Fun Grammar
analyseF knownNoFail fun =
  case fDef fun of
    Def g     -> fun { fDef = Def (annotate annF g) }
    External  -> fun

  where
  annF f es = if f `Set.member` knownNoFail
                then Annot NoFail (Call f es)
                else Call f es

-- | Annotate function names if they are know to always never fail
annotateNoFail :: [Fun Grammar] -> [Fun Grammar]
annotateNoFail fs0 = go (Set.empty,fs0)
  where
  save f (k,ch) = if fName f `Set.member` k || canFailFun f
                    then (k,ch)
                    else (Set.insert (fName f) k, True)

  go (k,fs) = let fs1 = map (analyseF k) fs
                  (k1,ch) = foldr save (k,False) fs1
              in if ch then go (k1,fs1) else fs1




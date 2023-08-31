{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Path set analysis

module Talos.SymExec.Path where

import           Control.DeepSeq       (NFData)
import           Data.ByteString       (ByteString)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Map              (Map)
import           GHC.Generics          (Generic)

import           Daedalus.PP
import           Daedalus.Panic

import           Talos.Analysis.Merge  (Merge (..))
import           Talos.Analysis.Slice  (FInstId, assertionsFID)

--------------------------------------------------------------------------------
-- Representation of paths/pathsets

data SelectedPathF ch ca fn a = 
    SelectedHole 
  | SelectedBytes ProvenanceTag a
  --  | Fail ErrorSource Type (Maybe Expr)
  | SelectedDo (SelectedPathF ch ca fn a) (SelectedPathF ch ca fn a)
  | SelectedChoice (ch (SelectedPathF ch ca fn a))
  | SelectedCall (fn (SelectedPathF ch ca fn a))
  | SelectedCase (ca (SelectedPathF ch ca fn a))
  deriving (Functor, Foldable, Traversable, Generic)

data PathIndex a  = PathIndex { pathIndex :: Int, pathIndexPath :: a }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)

instance PP a => PP (PathIndex a) where
    pp (PathIndex i a) = pp i <> ":" <> pp a

data CallInstantiation a = CallInstantiation { instantiationId :: FInstId, instantiationVal :: a }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)

instance PP a => PP (CallInstantiation a) where
    pp (CallInstantiation fid sp) = parens (pp fid) <+> ppPrec 1 sp

assertionsCI :: CallInstantiation SelectedPath
assertionsCI = CallInstantiation assertionsFID SelectedHole

type SelectedPath = SelectedPathF PathIndex Identity CallInstantiation ByteString

deriving instance NFData SelectedPath

-- -- isXs, mainly because we don't always have equality over nodes
-- isUnconstrained, isDontCare, isPathNode :: SelectedPath -> Bool
-- isUnconstrained Unconstrained = True
-- isUnconstrained _             = False

-- isDontCare (DontCare {}) = True
-- isDontCare _             = False

-- isPathNode (PathNode {}) = True
-- isPathNode _             = False

splitPath :: SelectedPath -> (SelectedPath, SelectedPath)
splitPath cp =
  case cp of
    SelectedDo l r -> (l, r)
    SelectedHole   -> (SelectedHole, SelectedHole)
    _ -> panic "splitPath: saw a non-{Do,Hole}" []

-- --------------------------------------------------------------------------------
-- -- smart constructors

-- -- smart constructor for dontCares.
-- dontCare :: Int -> SelectedPath -> SelectedPath
-- dontCare 0 ps = ps
-- dontCare  n (DontCare m ps)   = DontCare (n + m) ps
-- dontCare _n Unconstrained = Unconstrained
-- dontCare n  ps = DontCare n ps

-- pathNode :: SelectedNode -> SelectedPath ->  SelectedPath
-- pathNode (SelectedDo Unconstrained) rhs = dontCare 1 rhs
-- pathNode n rhs = PathNode n rhs


--------------------------------------------------------------------------------
-- Provenances

type ProvenanceTag = Int
type ProvenanceMap = Map Int ProvenanceTag

randomProvenance :: ProvenanceTag
randomProvenance = 0

synthVProvenance :: ProvenanceTag -- XXX: this is a placeholder. Need to work out what to do 
synthVProvenance = 1

firstSolverProvenance :: ProvenanceTag
firstSolverProvenance = 2

--------------------------------------------------------------------------------
-- Synthesis result nodes

-- Question: do we care about a path inside a node for which we have a value?
--
-- def Zoo = {
--     a = UInt8;
--     b = UInt8;
--     c = { b < 10; ^ 10 } | { ^ 1 }
--     c < a;
-- }
--
-- Choosing a will fix a path for c, which will refine when choosing
-- b.  Thus, we _do care_.  This also means that we can pick a value
-- without picking all bytes that result in that value.

-- Interestingly, reversing the order of a and b changes this example
-- significantly (from a synthesis POV). 
--
-- def Zoo' = {
--     a = UInt8;
--     b = UInt8;
--     c = { a < 10; ^ 10 } | { ^ 1 }
--     c < b;
-- }
--
-- FIXME: we need to ensure that selecting a path doesn't make a
-- future selection infeasible, as in 

-- def Zoo = {
--     b = UInt8;
--     a = UInt8;
--     c = { b < 10; ^ 10 } | { ^ 1 }
--     c < 10;
-- }
--
-- where selecting the left choice (when picking b) results in an
-- infeasible path.
--
-- Solutions:
-- - merge paths when we are assigning in a choice
--   + Simplest
--   + Results in variables being related which don't really need to be
-- - Order path selection
--   + need to figure out deps and remember choices (not too tricky)
--   + might need to merge paths on cycles.
--   + might have to merge paths before we hit the variable

--------------------------------------------------------------------------------
-- Instances

-- FIXME: too general probably
instance Merge (SelectedPathF PathIndex Identity CallInstantiation a) where
  merge psL psR =
    case (psL, psR) of
      (SelectedHole, _) -> psR
      (_, SelectedHole) -> psL
      (SelectedChoice (PathIndex n1 sp1), SelectedChoice (PathIndex n2 sp2))
        | n1 /= n2  -> panic "BUG: Incompatible paths selected in merge" [show n1, show n2]
        | otherwise -> SelectedChoice (PathIndex n1 (merge sp1 sp2))
      (SelectedCase (Identity sp1), SelectedCase (Identity sp2))
        -> SelectedCase (Identity (merge sp1 sp2))
      (SelectedCall (CallInstantiation cl1 sp1), SelectedCall (CallInstantiation cl2 sp2))
        | cl1 /= cl2 -> panic "BUG: Incompatible function classes"  [] -- [showPP cl1, showPP cl2]
        | otherwise  -> SelectedCall (CallInstantiation cl1 (merge sp1 sp2))
      (SelectedDo l1 r1, SelectedDo l2 r2) -> SelectedDo (merge l1 l2) (merge r1 r2)
      _ -> panic "BUG: merging non-mergeable nodes" []

instance ( Functor ch, PP (ch Doc)
         , Functor ca, PP (ca Doc)
         , Functor fn, PP (fn Doc)
         , PP a) => PP ( SelectedPathF ch ca fn a ) where
  ppPrec n p = 
    case p of
      SelectedHole       -> "□"
      SelectedBytes _ bs -> pp bs
      SelectedDo {}      -> "do" <+> ppStmts' p
      SelectedChoice ch ->
          wrapIf (n > 0) $ "choice" <+> pp (pp <$> ch)
      SelectedCase   cs -> wrapIf (n > 0) $ "case" <+> ppPrec 1 (pp <$> cs)
      SelectedCall   fn -> wrapIf (n > 0) $ "call" <> ppPrec 1 (pp <$> fn)

ppStmts' :: ( Functor ch, PP (ch Doc)
            , Functor ca, PP (ca Doc)
            , Functor fn, PP (fn Doc)
            , PP a) => SelectedPathF ch ca fn a -> Doc
ppStmts' p =
  case p of
    SelectedDo g1 g2 -> pp g1 $$ ppStmts' g2
    _                -> pp p

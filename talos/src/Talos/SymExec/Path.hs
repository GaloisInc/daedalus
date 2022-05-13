{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- Path set analysis

module Talos.SymExec.Path where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Map (Map)

import Control.DeepSeq -- for benchmarking etc.

import Daedalus.PP
import Daedalus.Panic

import Talos.Analysis.Slice (FInstId)
import Talos.Analysis.Merge (Merge(..))

--------------------------------------------------------------------------------
-- Representation of paths/pathsets

data SelectedPath = 
    SelectedHole 
  | SelectedBytes ProvenanceTag ByteString
  --  | Fail ErrorSource Type (Maybe Expr)
  | SelectedDo SelectedPath SelectedPath
  | SelectedChoice Int SelectedPath
  | SelectedCall FInstId SelectedPath
  | SelectedCase Int SelectedPath
  deriving (Generic, NFData)

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
instance Merge SelectedPath where
  merge psL psR =
    case (psL, psR) of
      (SelectedHole, _) -> psR
      (_, SelectedHole) -> psL
      (SelectedChoice n1 sp1, SelectedChoice n2 sp2)
        | n1 /= n2  -> panic "BUG: Incompatible paths selected in merge" [show n1, show n2]
        | otherwise -> SelectedChoice n1 (merge sp1 sp2)
      (SelectedCase n1 sp1, SelectedCase n2 sp2)
        | n1 /= n2  -> panic "BUG: Incompatible cases selected in merge" [show n1, show n2]
        | otherwise -> SelectedCase n1 (merge sp1 sp2)
      (SelectedCall cl1 sp1, SelectedCall cl2 sp2)
        | cl1 /= cl2 -> panic "BUG: Incompatible function classes"  [] -- [showPP cl1, showPP cl2]
        | otherwise  -> SelectedCall cl1 (merge sp1 sp2)
      (SelectedDo l1 r1, SelectedDo l2 r2) -> SelectedDo (merge l1 l2) (merge r1 r2)
      _ -> panic "BUG: merging non-mergeable nodes" []

instance PP SelectedPath where
  ppPrec n p =
    case p of
      SelectedHole       -> "[]"
      SelectedBytes _ bs -> pp bs
      SelectedDo {}      -> "do" <+> ppStmts' p
      SelectedChoice n'  sp  -> wrapIf (n > 0) $ "choice" <+> pp n' <+> ppPrec 1 sp
      SelectedCase   n'  sp  -> wrapIf (n > 0) $ "case" <+> pp n' <+> ppPrec 1 sp
      SelectedCall   fid sp  -> wrapIf (n > 0) $ ("call" <> parens (pp fid)) <+> ppPrec 1 sp

ppStmts' :: SelectedPath -> Doc
ppStmts' p =
  case p of
    SelectedDo g1 g2 -> pp g1 $$ ppStmts' g2
    _                -> pp p

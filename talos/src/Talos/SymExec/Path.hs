{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

-- Path set analysis

module Talos.SymExec.Path where

import Data.ByteString (ByteString)
import Data.Map (Map)

import Daedalus.PP
import Daedalus.Panic

import Talos.Analysis.Slice (SummaryClass, Merge(..))

--------------------------------------------------------------------------------
-- Representation of paths/pathsets

-- The analysis will be over something like
--
-- type AnalysisState = [ (EntangledVars, PathSet) ]
--
-- although we might want a union-find structure to help with
-- entangling variables

-- A collection of possible path, or a path, depending on n.

-- FIXME: this could be normalised, s.t. DontCare is never followed by
-- Unconstrained, or DontCare.  We could express this in the type
-- system, but it might make it a pain to use, e.g.
--
-- data PathSet a k n where
--   Unconstrained :: PathSet a U n
--   DontCare :: Int -> PathSet a N n -> PathSet a U n
--   PathNode :: forall k. n   -> PathSet a k n -> PathSet a N n
--
-- We could also merge the PathNode following a DontCare into the
-- DontCare, but that duplicates where a node can appear.  For now we
-- just program defensively and assume a non-normalised term at the
-- cost of somewhat increased complexity and maybe some performance
-- overhead.
data SelectedPath =
  Unconstrained -- Base case, all paths are feasible
  | DontCare Int SelectedPath 
  -- ^ We don't care about the nodes, so they can take any value.

  -- A node we do care about.  The first argument is Just (v, tc) if
  -- the variable is entangled on this path, the tc being the lhs of
  -- the bind that assigns the variable.
  | PathNode SelectedNode SelectedPath

data SelectedNode =
  SelectedChoice Int SelectedPath
  -- ^ We chose an alternative.

  | SelectedCase Int SelectedPath
  -- ^ We have selected an alternative of a case.  The index is also
  -- determined by the value (here to check). The default (if any) is
  -- considered the last element in the list.

  | SelectedCall SummaryClass SelectedPath
  -- ^ We have a function call.
  
  | SelectedMatch ProvenanceTag ByteString 
  -- ^ The term has been fully processed and does not contain anything
  -- of interest to other paths.  We still need to execute the term on the bytes.

  | SelectedDo SelectedPath -- ^ Wraps a nested Do node

-- isXs, mainly because we don't always have equality over nodes
isUnconstrained, isDontCare, isPathNode :: SelectedPath -> Bool
isUnconstrained Unconstrained = True
isUnconstrained _             = False

isDontCare (DontCare {}) = True
isDontCare _             = False

isPathNode (PathNode {}) = True
isPathNode _             = False
        
splitPath :: SelectedPath -> (Maybe SelectedNode, SelectedPath)
splitPath cp =
  case cp of
    Unconstrained             -> (Nothing, Unconstrained)
    -- Shouldn't happen
    DontCare 0 cp' -> splitPath cp'
    DontCare n cp' -> (Nothing, dontCare (n - 1) cp')
    PathNode n cp' -> (Just n, cp')

--------------------------------------------------------------------------------
-- smart constructors

-- smart constructor for dontCares.
dontCare :: Int -> SelectedPath -> SelectedPath
dontCare 0 ps = ps
dontCare  n (DontCare m ps)   = DontCare (n + m) ps
dontCare _n Unconstrained = Unconstrained
dontCare n  ps = DontCare n ps

pathNode :: SelectedNode -> SelectedPath ->  SelectedPath
pathNode (SelectedDo Unconstrained) rhs = dontCare 1 rhs
pathNode n rhs = PathNode n rhs


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
      (Unconstrained, _) -> psR   
      (DontCare 0 rest, _)   -> merge rest psR
      (DontCare n rest, DontCare m rest') ->
        let count = min m n
        in dontCare count (merge (dontCare (n - count) rest) (dontCare (m - count) rest'))
      (DontCare n rest, PathNode pn ps') -> PathNode pn (merge (dontCare (n - 1) rest) ps')
      (PathNode n1 ps1, PathNode n2 ps2) ->
        PathNode (merge n1 n2) (merge ps1 ps2)
      _ -> merge psR psL

instance Merge SelectedNode where
  --  We may merge nested nodes and calls, although choices and cases should occur on only 1 path.
  merge (SelectedChoice n1 sp1) (SelectedChoice n2 sp2)
    | n1 /= n2  = panic "BUG: Incompatible paths selected in merge" [show n1, show n2]
    | otherwise = SelectedChoice n1 (merge sp1 sp2)
  merge (SelectedCase n1 sp1) (SelectedCase n2 sp2)
    | n1 /= n2  = panic "BUG: Incompatible cases selected in merge" [show n1, show n2]
    | otherwise = SelectedCase n1 (merge sp1 sp2)
  merge (SelectedCall cl1 sp1) (SelectedCall cl2 sp2)
    | cl1 /= cl2 = panic "BUG: Incompatible function classes" [showPP cl1, showPP cl2]
    | otherwise = SelectedCall cl1 (merge sp1 sp2)
  merge (SelectedDo ps1) (SelectedDo ps2) = SelectedDo (merge ps1 ps2)  
  merge _n1 _n2 = panic "BUG: merging non-mergeable nodes" []

instance PP SelectedPath where
  ppPrec n ps =
    case ps of
      Unconstrained -> "[..]*;"
      -- These probably shouldn't have Unconstrained after them, so we
      -- will not try to simplify the output
      DontCare 1 ps' -> wrapIf (n > 0) $ "[..]; " <> pp ps'
      DontCare n' ps' -> wrapIf (n > 0) $ "[..]" <> pp n' <> "; " <> pp ps'
      PathNode n' Unconstrained -> ppPrec n n'
      PathNode n' ps' -> wrapIf (n > 0) $  pp n' <> "; " <> pp ps'

instance PP SelectedNode where
  ppPrec n sn =
    case sn of
      SelectedChoice n' sp  -> wrapIf (n > 0) $ "choice" <+> pp n' <+> ppPrec 1 sp
      SelectedCase   n' sp  -> wrapIf (n > 0) $ "case" <+> pp n' <+> ppPrec 1 sp
      SelectedCall   _  sp  -> wrapIf (n > 0) $ "call" <+> ppPrec 1 sp
      SelectedMatch _pr bs -> pp bs  -- XXX: print provenance 
      SelectedDo ps        -> "do" $$ pp ps

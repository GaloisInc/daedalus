{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

-- Path set analysis

module Talos.Analysis.Slice where


import Control.Applicative ((<|>)) 
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Panic
import Daedalus.Core

import Talos.Analysis.EntangledVars

--------------------------------------------------------------------------------
-- Representation of paths/pathsets
-- smart constructor for dontCares.

sDontCare :: Int -> Slice -> Slice
sDontCare 0 sl = sl
sDontCare  n (SDontCare m ps)   = SDontCare (n + m) ps
sDontCare _n SUnconstrained = SUnconstrained
sDontCare n  ps = SDontCare n ps

--------------------------------------------------------------------------------
-- Nodes for path sets

data Assertion = GuardAssertion Expr

-- This is the class of summary for a function; 'Assertions' summaries
-- contain information internal to the function, while
-- 'FunctionResult' also includes information about the return value.
-- These are used when the result of a function is non used and when
-- it is, resp.
--
-- In practice 'FunctionResult' will be a superset of 'Assertions'
--
-- We could compute both (simultaneously?) but for the most part only
-- 'Assertions' will be required.
data SummaryClass = Assertions | FunctionResult -- FIXME: add fields
  deriving (Ord, Eq, Show)

-- Just to tell us not to e.g. merge
newtype Wrapped a = Wrapped a

-- We represent a Call by a set of the entangled args.  If the
-- args aren't futher entangled by the calling context, then for
-- each argument fps we get a single Call node, where the Set is a
-- singleton containing the representative var as returned by
-- 'explodeDomain'.  The second argument tells us how to instantiate
-- the params used by the call.
--
-- We require a set so we can merge where the callers entangle params.
--
-- The first argument is whether this call is an assigned, in which
-- case the entangled vars must contain ResultVar.  Note that we the
-- first argument can also be ResultVar, but that is the result of
-- the current function, not this call (e.g. def Foo = { ...; Bar })
--
-- Note that the set of entangled vars here is a bit different to
-- that which appears in a Domain, if only in intent --- these are
-- entangled by their context, while in a domain they are entangled
-- by use.  Also, the range of the Map is _not_ merged, it is just
-- carried around to avoid looking up the summary again.

data CallNode =
  CallNode { callClass        :: SummaryClass           
           , callAllArgs      :: Map Name Expr
           -- ^ A shared map (across all domain elements) of the args to the call
           , callName         :: FName
           -- ^ The called function
           
           -- FIXME: we probably get into trouble if we have the same var in different classes here.
           , callPaths        :: Map EntangledVar (Wrapped (EntangledVars, Slice))
           -- ^ All entangled params for the call, the range (wrapped)
           -- are in the _callee_'s namespace, so we don't merge etc.
           }
  
mergeCallNode :: CallNode -> CallNode -> CallNode
mergeCallNode cn@(CallNode { callClass = cl1, callPaths = paths1 })
                 (CallNode { callClass = cl2, callPaths = paths2 })
  | cl1 /= cl2 = panic "Saw different function classes" []
  | otherwise = cn { callPaths = Map.union paths1 paths2 -- don't have to use unionWith here
                   }

eqvCallNode :: CallNode -> CallNode -> Bool
eqvCallNode (CallNode { callClass = cl1, callPaths = paths1 })
            (CallNode { callClass = cl2, callPaths = paths2 }) =
  cl1 == cl2 && Map.keys paths1 == Map.keys paths2


type CaseNode = Case Slice

mergeCaseNode :: CaseNode -> CaseNode -> CaseNode 
mergeCaseNode (Case e alts1) (Case _e alts2) =
  Case e (zipWith goAlt alts1 alts2)
  where
    goAlt (p, a1) (_p, a2) = (p, mergeSlice a1 a2)

eqvCaseNode :: CaseNode -> CaseNode -> Bool
eqvCaseNode (Case _e alts1) (Case _e' alts2) =
  all (uncurry $ on sliceEqv snd) (zip alts1 alts2)

-- These are the supported leaf nodes -- we could just reuse TC but it
-- is nice to be explicit here.
data SliceLeaf =
  SPure Expr
  | SMatch Match
  | SAssertion Assertion -- FIXME: this is inferred from e.g. case x of True -> ...
  | SChoice [Slice] -- we represent all choices as a n-ary node, not a tree of binary nodes
  | SCall CallNode
  | SCase CaseNode
  
mergeSliceLeaf :: SliceLeaf -> SliceLeaf -> SliceLeaf
mergeSliceLeaf l r =
  case (l, r) of
    (SPure {}, SPure {})           -> l
    (SMatch {}, SMatch {})         -> l
    (SAssertion {}, SAssertion {}) -> l
    (SChoice cs1, SChoice cs2)     -> SChoice (zipWith mergeSlice cs1 cs2)
    (SCall lc, SCall rc)           -> SCall (mergeCallNode lc rc)
    (SCase lc, SCase rc)           -> SCase (mergeCaseNode lc rc)
    _                              -> panic "Mismatched terms in mergeSliceLeaf" [showPP l, showPP r]

sliceLeafEqv :: SliceLeaf -> SliceLeaf -> Bool
sliceLeafEqv l r =
  case (l, r) of
    (SPure {}, SPure {})           -> True
    (SMatch {}, SMatch {})         -> True
    (SAssertion {}, SAssertion {}) -> True    
    (SChoice ls, SChoice rs)       -> all (uncurry sliceEqv) (zip ls rs)
    (SCall lc, SCall rc)           -> eqvCallNode lc rc
    (SCase lc, SCase rc)           -> eqvCaseNode lc rc
    _                              -> panic "Mismatched terms in eqvSliceLeaf" [showPP l, showPP r]
  
-- We assume the core has been simplified (no nested do etc.)
data Slice =
  -- Sequencing
  SDontCare Int Slice
  | SDo (Maybe Name) SliceLeaf Slice -- We merge Do and Do_
  -- Terminals
  | SUnconstrained
  | SLeaf SliceLeaf

-- This assumes the slices come from the same program, i.e., we don't
-- simple slices should be identical.
mergeSlice :: Slice -> Slice -> Slice
mergeSlice l r = 
  case (l, r) of
    (_, SUnconstrained)            -> l
    (SUnconstrained, _)            -> r

    (SDontCare 0 rest, _)   -> mergeSlice rest r -- Shouldn't happen.
    (SDontCare n rest, SDontCare m rest') ->
      let count = min m n
      in sDontCare count (mergeSlice (sDontCare (n - count) rest) (sDontCare (m - count) rest'))
    (SDontCare n rest, SDo m_x slL slR) -> SDo m_x slL (mergeSlice (sDontCare (n - 1) rest) slR)
    
    -- FIXME: does this make sense?
    (SDontCare n rest, SLeaf sl) -> SDo Nothing sl (sDontCare (n - 1) rest)

    (_, SDontCare {})       -> mergeSlice r l

    (SDo x1 slL1 slR1, SDo x2 slL2 slR2) ->
      SDo (x1 <|> x2) (mergeSliceLeaf slL1 slL2) (mergeSlice slR1 slR2)

    (SLeaf sl1, SLeaf sl2) -> SLeaf (mergeSliceLeaf sl1 sl2)
    
    _ -> panic "Merging non-mergeable nodes" [showPP l, showPP r]

-- This assumes the node came from the same statement (i.e., the paths
-- we are comparing are rooted at the same statement).  This means we
-- don't need to compare TC for equality.
--
-- Thus, for some cases, the presence of a node is enough to return True (e.g. for Assertion)
sliceEqv :: Slice -> Slice -> Bool
sliceEqv l r = 
  case (l, r) of
    (SDontCare n rest, SDontCare m rest') -> m == n && sliceEqv rest rest'

    (SDo _x slL1 slR1, SDo _x' slL2 slR2) ->
      sliceLeafEqv slL1 slL2 && sliceEqv slR1 slR2

    (SUnconstrained, SUnconstrained) -> True
    (SLeaf sl1, SLeaf sl2) -> sliceLeafEqv sl1 sl2

    _ -> panic "Comparing non-comparable nodes" [showPP l, showPP r]

--------------------------------------------------------------------------------
-- Instances

instance PP SliceLeaf where
  ppPrec n sl =
    case sl of
      SPure v  -> wrapIf (n > 0) $ "pure" <+> ppPrec 1 v
      SMatch m -> wrapIf (n > 0) $ ppMatch SemYes m
      SAssertion e -> wrapIf (n > 0) $ "assert" <+> ppPrec 1 e
      SChoice cs    -> "choice" <> block "{" "," "}" (map pp cs)
      SCall (CallNode { callName = fname, callPaths = evs }) ->
        wrapIf (n > 0) $ ("call " <> pp fname)
                         <+> (lbrace <> commaSep (map pp (Map.keys evs)) <> rbrace)
      SCase c -> pp c

ppStmt :: Slice -> Doc
ppStmt sl =
  case sl of
    SDo (Just x) e1 e2 -> (pp x <+> "<-" <+> pp e1) $$ ppStmt e2
    SDo Nothing  e1 e2 ->                    pp e1  $$ ppStmt e2 
    _           -> pp sl
      
instance PP Slice where
  ppPrec n ps =
    case ps of
      SDontCare 1 sl  -> wrapIf (n > 0) $ "[..]; " <> pp sl
      SDontCare n' sl -> wrapIf (n > 0) $ "[..]"   <> pp n' <> "; " <> pp sl
      SDo  {}         -> "do" <+> ppStmt ps
      
      SUnconstrained -> "[..]*;"
      SLeaf s     -> ppPrec n s
      
instance PP Assertion where
  pp (GuardAssertion g) = pp g

instance PP SummaryClass where
  pp Assertions     = "Assertions"
  pp FunctionResult = "Result"
        

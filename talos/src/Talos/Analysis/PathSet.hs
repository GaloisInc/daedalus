{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

-- Path set analysis

module Talos.Analysis.PathSet where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Panic
import Daedalus.Type.AST
import Daedalus.Type.PatComplete (PatternCompleteness(..), CaseSummary)

import Talos.Analysis.EntangledVars

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
data PathSet a n =
  Unconstrained -- Base case, all paths are feasible
  | DontCare Int (PathSet a n)
  -- ^ We don't care about the nodes, so they can take any value.

  -- A node we do care about.  The first argument is Just (v, tc) if
  -- the variable is entangled on this path, the tc being the lhs of
  -- the bind that assigns the variable.
  | PathNode n (PathSet a n)
  deriving (Functor)

-- isXs, mainly because we don't always have equality over nodes
isUnconstrained, isDontCare, isPathNode :: PathSet a n -> Bool
isUnconstrained Unconstrained = True
isUnconstrained _             = False

isDontCare (DontCare {}) = True
isDontCare _             = False

isPathNode (PathNode {}) = True
isPathNode _             = False

-- FIXME: too general probably
mergePathSet :: (m -> n -> p)
             -> (m -> p)
             -> (n -> p)
             -> (PathSet a m -> PathSet a p)
             -> (PathSet a n -> PathSet a p)             
             -> PathSet a m -> PathSet a n -> PathSet a p
mergePathSet mergeN inL inR pinL pinR = go 
  where
    go psL psR = 
      case (psL, psR) of
        (Unconstrained, _) -> pinR psR        
        (DontCare 0 rest, _)   -> go rest psR
        (DontCare n rest, DontCare m rest') ->
          let count = min m n
          in dontCare count (go (dontCare (n - count) rest) (dontCare (m - count) rest'))
        (DontCare n rest, PathNode pn ps') -> PathNode (inR pn) (go (dontCare (n - 1) rest) ps')

        (PathNode n1 ps1, PathNode n2 ps2) ->
          PathNode (mergeN n1 n2) (go ps1 ps2)
          
        _ -> mergePathSet (flip mergeN) inR inL pinR pinL psR psL
        
-- smart constructor for dontCares.
dontCare :: Int -> PathSet a n -> PathSet a n
dontCare 0 ps = ps
dontCare  n (DontCare m ps)   = DontCare (n + m) ps
dontCare _n Unconstrained = Unconstrained
dontCare n  ps = DontCare n ps

splitPath :: PathSet a n -> (Maybe n, PathSet a n)
splitPath cp =
  case cp of
    Unconstrained             -> (Nothing, Unconstrained)
    -- Shouldn't happen
    DontCare 0 cp' -> splitPath cp'
    DontCare n cp' -> (Nothing, dontCare (n - 1) cp')
    PathNode n cp' -> (Just n, cp')

-- This is used for comparing summaries when computing fixpoints.  It
-- is only really correct when comparing path sets rooted at the same
-- statement.
pathEqv :: (n -> n -> Bool) -> PathSet a n -> PathSet a n -> Bool
pathEqv eqvNode = go
  where
    go Unconstrained Unconstrained    = True
    go Unconstrained (DontCare _ ps2) = go Unconstrained ps2 -- Probably shouldn't happen
    go Unconstrained _                = False

    go (DontCare 0 ps1) ps2           = go ps1 ps2
    go (DontCare n ps1) (DontCare m ps2) =
      go (dontCare (n - min n m) ps1) (dontCare (m - min n m) ps2)
    go (DontCare {}) (PathNode {})    = False

    go (PathNode n1 ps1) (PathNode n2 ps2) =
      eqvNode n1 n2 && go ps1 ps2
      
    go x y = go y x

--------------------------------------------------------------------------------
-- Nodes for path sets

data Assertion a = GuardAssertion (TC a Value)

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

-- c.f. TCAlt.  FIXME: can we unify them?
data FNAlt a = FNAlt { fnAltPatterns :: [TCPat]
                     , fnAltBody     :: FuturePathSet a
                     }

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

data CallNode a =
  CallNode { callClass         :: SummaryClass           
           , callResultAssign :: Maybe EntangledVar
           -- ^ Just x if this returns a value.(a var in the caller)
           , callAllArgs      :: Map (TCName Value) (TC a Value)
           -- ^ A shared map (across all domain elements) of the args to the call
           , callName         :: Name
           -- ^ The called function
           
           -- FIXME: we probably get into trouble if we have the same var in different classes here.
           , callPaths        :: Map EntangledVar (Wrapped (EntangledVars, FuturePathSet a))
           -- ^ All entangled params for the call, the range (wrapped)
           -- are in the _callee_'s namespace, so we don't merge etc.

           }
  
mergeCallNode :: CallNode a -> CallNode a -> CallNode a
mergeCallNode cn@(CallNode { callClass = cl1, callResultAssign = res1, callPaths = paths1 })
                 (CallNode { callClass = cl2, callResultAssign = res2, callPaths = paths2 })
  | cl1 /= cl2 = panic "Saw different function classes" []
  | otherwise = cn { callResultAssign = res1 <|> res2
                   , callPaths = Map.union paths1 paths2 -- don't have to use unionWith here
                   }

eqvCallNode :: CallNode a -> CallNode a -> Bool
eqvCallNode (CallNode { callClass = cl1, callResultAssign = res1, callPaths = paths1 })
            (CallNode { callClass = cl2, callResultAssign = res2, callPaths = paths2 }) =
  cl1 == cl2 && res1 == res2 && Map.keys paths1 == Map.keys paths2

data CaseNode a =
  CaseNode { caseCompleteness :: PatternCompleteness
           , caseSummary      :: CaseSummary
           , caseTerm         :: TC a Value
           , caseAlts         :: NonEmpty (FNAlt a)
           , caseDefault      :: Maybe (FuturePathSet a)
           }

mergeCaseNode :: CaseNode a -> CaseNode a -> CaseNode a
mergeCaseNode (CaseNode pc cs tm alts1 m_def1) (CaseNode _pc _cs _tm alts2 m_def2) =
  CaseNode pc cs tm (NE.zipWith goAlt alts1 alts2) (mergeFuturePathSet <$> m_def1 <*> m_def2)
  where
    goAlt a1 a2 = a1 { fnAltBody = mergeFuturePathSet (fnAltBody a1) (fnAltBody a2) }

eqvCaseNode :: CaseNode a -> CaseNode a -> Bool
eqvCaseNode (CaseNode _pc _cs _tm alts1 m_def1) (CaseNode _pc' _cs' _tm' alts2 m_def2) =
  all (uncurry $ on futurePathEqv fnAltBody) (NE.toList (NE.zip alts1 alts2))
  && cmpMB m_def1 m_def2
  where
    cmpMB Nothing    Nothing = True
    cmpMB (Just ps1) (Just ps2) = futurePathEqv ps1 ps2
    cmpMB _          _          = False -- Can't happen?

data ManyNode a =
  ManyNode { manyResultAssign :: Maybe EntangledVar
           , manyBounds       :: ManyBounds (TC a Value)
           , manyFrees        :: EntangledVars
           , manyBody         :: FuturePathSet a
           }
  
mergeManyNode :: ManyNode a -> ManyNode a -> ManyNode a
mergeManyNode mn@(ManyNode { manyFrees = f1, manyBody = b1 })
                 (ManyNode { manyFrees = f2, manyBody = b2 }) =
  mn { manyFrees = mergeEntangledVars f1 f2
     , manyBody = mergeFuturePathSet b1 b2 }
  
eqvManyNode :: ManyNode a -> ManyNode a -> Bool
eqvManyNode (ManyNode { manyFrees = f1, manyBody = b1 })
            (ManyNode { manyFrees = f2, manyBody = b2 }) =
  f1 == f2 && futurePathEqv b1 b2
  
-- A path node where we need to do something.  
data FutureNode a =
  -- | A choose node in the DDL.
  Choice [FuturePathSet a]

  -- | A case statement in the DDL.
  | FNCase (CaseNode a)
  
  | Call (CallNode a)

  | Assertion (Assertion a)

  | FNMany (ManyNode a)

  | NestedNode (FuturePathSet a) -- ^ Allows for left-nested binds

  -- We can completely synthesise this value.
  | SimpleNode EntangledVar (TC a Grammar)

type FuturePathSet a = PathSet a (FutureNode a)

mergeFutureNode :: FutureNode a -> FutureNode a -> FutureNode a
mergeFutureNode (Choice cs1)           (Choice cs2) =
  Choice (zipWith mergeFuturePathSet cs1 cs2)
mergeFutureNode (Call cn1) (Call cn2) = Call (mergeCallNode cn1 cn2)
-- b and v should be identical, and the alts and m_defs should have the same shape.
mergeFutureNode (FNCase cn1) (FNCase cn2) = FNCase (mergeCaseNode cn1 cn2)
mergeFutureNode (FNMany mn1) (FNMany mn2) = FNMany (mergeManyNode mn1 mn2)
mergeFutureNode x            _y           = x -- FIXME: check for equality.

-- mergeAnnFutureNode :: AnnFutureNode a -> AnnFutureNode a -> AnnFutureNode a
-- mergeAnnFutureNode (AnnFutureNode n ann) (AnnFutureNode n' ann') =
--   AnnFutureNode (mergeFutureNode n n') (ann <|> ann')

mergeFuturePathSet :: FuturePathSet a -> FuturePathSet a -> FuturePathSet a
mergeFuturePathSet = mergePathSet mergeFutureNode id id id id

-- This assumes the node came from the same statement (i.e., the paths
-- we are comparing are rooted at the same statement).  This means we
-- don't need to compare TC for equality.
--
-- Thus, for some cases, the presence of a node is enough to return True (e.g. for Assertion)
futureNodeEqv :: FutureNode a -> FutureNode a -> Bool
futureNodeEqv = go
  where
    go (Assertion {})  _             = True
    go (SimpleNode {}) _             = True
    go (NestedNode ps1) (NestedNode ps2) = futurePathEqv ps1 ps2
    go (Call cn1) (Call cn2)         = eqvCallNode cn1 cn2
    go (Choice pss1)   (Choice pss2) =
      all (uncurry futurePathEqv) (zip pss1 pss2)
    go (FNCase c1) (FNCase c2) = eqvCaseNode c1 c2
    go (FNMany m1) (FNMany m2) = eqvManyNode m1 m2    
    go _               _              = panic "Unexpected node comparison" []


futurePathEqv :: FuturePathSet a -> FuturePathSet a -> Bool
futurePathEqv = pathEqv futureNodeEqv

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

type ProvenanceTag = Int 
type ProvenanceMap = Map Int ProvenanceTag

randomProvenance :: ProvenanceTag
randomProvenance = 0 

synthVProvenance :: ProvenanceTag -- XXX: this is a placeholder. Need to work out what to do 
synthVProvenance = 1 

firstSolverProvenance :: ProvenanceTag
firstSolverProvenance = 2 

data SelectedNode =
  SelectedChoice Int SelectedPath
  -- ^ We chose an alternative.

  | SelectedCase Int SelectedPath
  -- ^ We have selected an alternative of a case.  The index is also
  -- determined by the value (here to check). The default (if any) is
  -- considered the last element in the list.

  | SelectedCall SummaryClass SelectedPath
  -- ^ We have a function call.

  -- Assertions are ignored at this point
  | SelectedNested SelectedPath

  | SelectedMany [SelectedPath]
  
  | SelectedSimple ProvenanceTag ByteString 
  -- ^ The term has been fully processed and does not contain anything
  -- of interest to other paths.  We still need to execute the term on the bytes.

--  We may merge nested nodes and calls, although choices and cases should occur on only 1 path.
mergeSelectedNode :: SelectedNode -> SelectedNode -> SelectedNode
mergeSelectedNode (SelectedChoice n1 sp1) (SelectedChoice n2 sp2)
  | n1 /= n2  = panic "BUG: Incompatible paths selected in mergeSelectedNode" [show n1, show n2]
  | otherwise = SelectedChoice n1 (mergeSelectedPath sp1 sp2)
mergeSelectedNode (SelectedCase n1 sp1) (SelectedCase n2 sp2)
  | n1 /= n2  = panic "BUG: Incompatible cases selected in mergeSelectedNode" [show n1, show n2]
  | otherwise = SelectedCase n1 (mergeSelectedPath sp1 sp2)
mergeSelectedNode (SelectedCall cl1 sp1) (SelectedCall cl2 sp2)
  | cl1 /= cl2 = panic "BUG: Incompatible function classes" [showPP cl1, showPP cl2]
  | otherwise = SelectedCall cl1 (mergeSelectedPath sp1 sp2)
mergeSelectedNode (SelectedNested sp1) (SelectedNested sp2)
  = SelectedNested (mergeSelectedPath sp1 sp2)
mergeSelectedNode _n1 _n2 = panic "BUG: merging non-mergeable nodes" []

mergeSelectedPath :: SelectedPath -> SelectedPath -> SelectedPath
mergeSelectedPath = mergePathSet mergeSelectedNode id id id id                

type TCSynthAnnot = SourceRange

type SelectedPath = PathSet TCSynthAnnot SelectedNode


instance PP n => PP (PathSet a n) where
  ppPrec n ps =
    case ps of
      Unconstrained -> "[..]*;"
      -- These probably shouldn't have Unconstrained after them, so we
      -- will not try to simplify the output
      DontCare 1 ps' -> wrapIf (n > 0) $ "[..]; " <> pp ps'
      DontCare n' ps' -> wrapIf (n > 0) $ "[..]" <> pp n' <> "; " <> pp ps'
      PathNode n' Unconstrained -> ppPrec n n'
      PathNode n' ps' -> wrapIf (n > 0) $  pp n' <> "; " <> pp ps'

instance PP (Assertion a) where
  pp (GuardAssertion g) = pp g

instance PP SummaryClass where
  pp Assertions     = "Assertions"
  pp FunctionResult = "Result"

instance PP (FNAlt a) where
  ppPrec _ (FNAlt ps e) = lhs <+> "->" <+> pp e
    where lhs = sep $ punctuate comma $ map pp ps

instance PP (FutureNode a) where
  ppPrec n fn =
    case fn of
      Choice cs -> 
        "Choice" <> block "{" "," "}" (map pp cs)

      FNCase (CaseNode c _b e alts m_def) -> 
        wrapIf (n > 0)
        ("case" <> if c == Incomplete then "?" else mempty) <+> pp e <+> "is" $$
          nest 2 (block "{" ";" "}" (addDefault (map pp (NE.toList alts))))
        where
        addDefault xs = case m_def of
                          Nothing -> xs
                          Just d  -> xs ++ ["_" <+> "->" <+> pp d]

      FNMany (ManyNode { manyResultAssign = m_x, manyBody = b }) ->
        wrapIf (n > 0) $ (maybe mempty (\x -> pp x <> " = ") m_x <> "Many " <> ppPrec 1 b)

      Call (CallNode { callResultAssign = m_x, callName = fname, callPaths = evs }) ->
        wrapIf (n > 0) $ (maybe mempty (\x -> pp x <> " = ") m_x <> "Call " <> pp fname)
                         <+> (lbrace <> commaSep (map pp (Map.keys evs)) <> rbrace)
        
      Assertion a       -> wrapIf (n > 0) $ pp a
      NestedNode fps    -> wrapIf (n > 0) $ "Do" <+> parens (pp fps)
      SimpleNode v e -> wrapIf (n > 0) $ pp v <> " = " <> pp e

instance PP SelectedNode where
  ppPrec n sn =
    case sn of
      SelectedChoice n' sp  -> wrapIf (n > 0) $ "choice" <+> pp n' <+> ppPrec 1 sp
      SelectedCase   n' sp  -> wrapIf (n > 0) $ "case" <+> pp n' <+> ppPrec 1 sp
      SelectedCall   _  sp  -> wrapIf (n > 0) $ "call" <+> ppPrec 1 sp
      SelectedNested sp     -> wrapIf (n > 0) $ "do" <+> ppPrec 1 sp
      SelectedMany sps      -> wrapIf (n > 0) $ "many" <+> (vcat $ map (ppPrec 1) sps)
      SelectedSimple _pr bs -> pp bs  -- XXX: print provenance 

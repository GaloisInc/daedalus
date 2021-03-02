{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

-- Path set analysis

module Talos.Analysis.Slice where

import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Panic
import Daedalus.Type.AST hiding (ppStmt)
import Daedalus.Type.PatComplete (PatternCompleteness(..), CaseSummary)

import Talos.Analysis.EntangledVars

type TCSynthAnnot = SourceRange

--------------------------------------------------------------------------------
-- Representation of paths/pathsets
-- smart constructor for dontCares.

dontCare :: Int -> Slice a -> Slice a
dontCare 0 sl = sl
dontCare  n (SDontCare m ps)   = SDontCare (n + m) ps
dontCare _n SUnconstrained = SUnconstrained
dontCare n  ps = SDontCare n ps

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
data SAlt a = SAlt { sAltPatterns :: [TCPat]
                   , sAltBody     :: Slice a
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
  CallNode { callClass        :: SummaryClass           
           , callAllArgs      :: Map (TCName Value) (TC a Value)
           -- ^ A shared map (across all domain elements) of the args to the call
           , callName         :: Name
           -- ^ The called function
           
           -- FIXME: we probably get into trouble if we have the same var in different classes here.
           , callPaths        :: Map EntangledVar (Wrapped (EntangledVars, Slice a))
           -- ^ All entangled params for the call, the range (wrapped)
           -- are in the _callee_'s namespace, so we don't merge etc.
           }
  
mergeCallNode :: CallNode a -> CallNode a -> CallNode a
mergeCallNode cn@(CallNode { callClass = cl1, callPaths = paths1 })
                 (CallNode { callClass = cl2, callPaths = paths2 })
  | cl1 /= cl2 = panic "Saw different function classes" []
  | otherwise = cn { callPaths = Map.union paths1 paths2 -- don't have to use unionWith here
                   }

eqvCallNode :: CallNode a -> CallNode a -> Bool
eqvCallNode (CallNode { callClass = cl1, callPaths = paths1 })
            (CallNode { callClass = cl2, callPaths = paths2 }) =
  cl1 == cl2 && Map.keys paths1 == Map.keys paths2

data CaseNode a =
  CaseNode { caseCompleteness :: PatternCompleteness
           , caseSummary      :: CaseSummary
           , caseTerm         :: TC a Value
           , caseAlts         :: NonEmpty (SAlt a)
           , caseDefault      :: Maybe (Slice a)
           }

mergeCaseNode :: CaseNode a -> CaseNode a -> CaseNode a
mergeCaseNode (CaseNode pc cs tm alts1 m_def1) (CaseNode _pc _cs _tm alts2 m_def2) =
  CaseNode pc cs tm (NE.zipWith goAlt alts1 alts2) (mergeSlice <$> m_def1 <*> m_def2)
  where
    goAlt a1 a2 = a1 { sAltBody = mergeSlice (sAltBody a1) (sAltBody a2) }

eqvCaseNode :: CaseNode a -> CaseNode a -> Bool
eqvCaseNode (CaseNode _pc _cs _tm alts1 m_def1) (CaseNode _pc' _cs' _tm' alts2 m_def2) =
  all (uncurry $ on sliceEqv sAltBody) (NE.toList (NE.zip alts1 alts2))
  && cmpMB m_def1 m_def2
  where
    cmpMB Nothing    Nothing = True
    cmpMB (Just ps1) (Just ps2) = sliceEqv ps1 ps2
    cmpMB _          _          = False -- Can't happen?

data ManyNode a =
  ManyNode { manyBounds       :: ManyBounds (TC a Value)
           , manyFrees        :: EntangledVars
           , manyBody         :: Slice a
           }
  
mergeManyNode :: ManyNode a -> ManyNode a -> ManyNode a
mergeManyNode mn@(ManyNode { manyFrees = f1, manyBody = b1 })
                 (ManyNode { manyFrees = f2, manyBody = b2 }) =
  mn { manyFrees = mergeEntangledVars f1 f2
     , manyBody = mergeSlice b1 b2 }
  
eqvManyNode :: ManyNode a -> ManyNode a -> Bool
eqvManyNode (ManyNode { manyFrees = f1, manyBody = b1 })
            (ManyNode { manyFrees = f2, manyBody = b2 }) =
  f1 == f2 && sliceEqv b1 b2

-- These are the supported leaf nodes -- we could just reuse TC but it
-- is nice to be explicit here.
data SimpleSlice a =
  SPure (TC a Value)
  | SMatchBytes (TC a Value)
  | SGetByte
  | SMatch (TC a Class)
  | SCoerceCheck Type Type (TC a Value)

data Slice a =
  -- Sequencing
  SDontCare Int (Slice a)
  | SDo (TCName Value) (Slice a) (Slice a)
  | SDo_ (Slice a) (Slice a)
  -- Terminals
  | SUnconstrained
  | SSimple (SimpleSlice a)
  -- | A choose node in the DDL.
  | SChoice [Slice a]
  -- | A case statement in the DDL.
  | SCase (CaseNode a)
  | SCall (CallNode a)
  | SAssertion (Assertion a)
  | SMany (ManyNode a)

-- This assumes the slices come from the same program, i.e., we don't
-- simple slices should be identical.
mergeSlice :: Slice a -> Slice a -> Slice a
mergeSlice l r = 
  case (l, r) of
    (_, SUnconstrained)            -> l
    (SUnconstrained, _)            -> r

    (SDontCare 0 rest, _)   -> mergeSlice rest r -- Shouldn't happen.
    (SDontCare n rest, SDontCare m rest') ->
      let count = min m n
      in dontCare count (mergeSlice (dontCare (n - count) rest) (dontCare (m - count) rest'))
    (SDontCare n rest, SDo x slL slR) -> SDo x slL (mergeSlice (dontCare (n - 1) rest) slR)
    (SDontCare n rest, SDo_  slL slR) -> SDo_  slL (mergeSlice (dontCare (n - 1) rest) slR)
    
    -- Terminal case
    (SDontCare n rest, _)             -> SDo_ r (dontCare (n - 1) rest)

    (SDo  {}, SDontCare {}) -> mergeSlice r l
    (SDo_ {}, SDontCare {}) -> mergeSlice r l

    (SDo x slL1 slR1, SDo _x slL2 slR2) ->
      SDo x (mergeSlice slL1 slL2) (mergeSlice slR1 slR2)
    (SDo x slL1 slR1, SDo_ slL2 slR2) ->
      SDo x (mergeSlice slL1 slL2) (mergeSlice slR1 slR2)
    (SDo_ slL1 slR1, SDo x slL2 slR2) ->
      SDo x (mergeSlice slL1 slL2) (mergeSlice slR1 slR2)
    (SDo_ slL1 slR1, SDo_ slL2 slR2) ->
      SDo_ (mergeSlice slL1 slL2) (mergeSlice slR1 slR2)

    (SSimple {}, SSimple {})       -> l
    (SChoice ls, SChoice rs)       -> SChoice (zipWith mergeSlice ls rs)
    (SCase lc, SCase rc)           -> SCase (mergeCaseNode lc rc)
    (SCall lc, SCall rc)           -> SCall (mergeCallNode lc rc)
    (SAssertion {}, SAssertion {}) -> l 
    (SMany lmn, SMany rmn)         -> SMany (mergeManyNode lmn rmn) -- Shouldn't happen
      
    _ -> panic "Merging non-mergeable nodes" [showPP l, showPP r]

-- This assumes the node came from the same statement (i.e., the paths
-- we are comparing are rooted at the same statement).  This means we
-- don't need to compare TC for equality.
--
-- Thus, for some cases, the presence of a node is enough to return True (e.g. for Assertion)
sliceEqv :: Slice a -> Slice a -> Bool
sliceEqv l r = 
  case (l, r) of
    (SDontCare n rest, SDontCare m rest') -> m == n && sliceEqv rest rest'

    (SDo _x slL1 slR1, SDo _x' slL2 slR2) ->
      sliceEqv slL1 slL2 && sliceEqv slR1 slR2
    (SDo_ slL1 slR1, SDo_ slL2 slR2) ->
      sliceEqv slL1 slL2 && sliceEqv slR1 slR2

    (SUnconstrained, SUnconstrained) -> True
    (SSimple {}, SSimple {})       -> True
    (SChoice ls, SChoice rs)       -> all (uncurry sliceEqv) (zip ls rs)
    (SCase lc, SCase rc)           -> eqvCaseNode lc rc
    (SCall lc, SCall rc)           -> eqvCallNode lc rc
    (SAssertion {}, SAssertion {}) -> True
    (SMany lmn, SMany rmn)         -> eqvManyNode lmn rmn
    _ -> False

--------------------------------------------------------------------------------
-- Instances

instance PP (SimpleSlice a) where
  ppPrec n sl =
    case sl of
      SPure v       -> wrapIf (n > 0) $ "pure" <+> ppPrec 1 v
      SMatchBytes v -> wrapIf (n > 0) $ "MatchBytes" <+> ppPrec 1 v
      SGetByte      -> "GetByte"
      SMatch p      -> wrapIf (n > 0) $ "Match" <+> ppPrec 1 p
      SCoerceCheck _ t2 e -> wrapIf (n > 0) (ppPrec 1 e <+> "AS" <+> pp t2)

ppStmt :: Slice a -> Doc
ppStmt sl =
  case sl of
    SDo x e1 e2 -> (pp x <+> "<-" <+> pp e1) $$ ppStmt e2
    SDo_  e1 e2 ->                    pp e1  $$ ppStmt e2 
    _           -> pp sl

      
instance PP (Slice a) where
  ppPrec n ps =
    case ps of
      SDontCare 1 sl  -> wrapIf (n > 0) $ "[..]; " <> pp sl
      SDontCare n' sl -> wrapIf (n > 0) $ "[..]"   <> pp n' <> "; " <> pp sl
      SDo  {}         -> "do" <+> ppStmt ps
      SDo_ {}         -> "do" <+> ppStmt ps
      
      SUnconstrained -> "[..]*;"
      SSimple s     -> ppPrec n s
      SChoice cs    -> "Choice" <> block "{" "," "}" (map pp cs)
      SCase (CaseNode c _b e alts m_def) -> 
        wrapIf (n > 0)
        ("case" <> if c == Incomplete then "?" else mempty) <+> pp e <+> "is" $$
           nest 2 (block "{" ";" "}" (addDefault (map pp (NE.toList alts))))
        where
        addDefault xs = case m_def of
                          Nothing -> xs
                          Just d  -> xs ++ ["_" <+> "->" <+> pp d]

      SCall (CallNode { callName = fname, callPaths = evs }) ->
        wrapIf (n > 0) $ ("Call " <> pp fname)
                         <+> (lbrace <> commaSep (map pp (Map.keys evs)) <> rbrace)

      SAssertion a       -> wrapIf (n > 0) $ "Guard" <+> ppPrec 1 a
      
      SMany (ManyNode { manyBody = b }) ->
        wrapIf (n > 0) $ "Many " <> ppPrec 1 b
      
instance PP (Assertion a) where
  pp (GuardAssertion g) = pp g

instance PP SummaryClass where
  pp Assertions     = "Assertions"
  pp FunctionResult = "Result"

instance PP (SAlt a) where
  ppPrec _ (SAlt ps e) = lhs <+> "->" <+> pp e
    where lhs = sep $ punctuate comma $ map pp ps
        

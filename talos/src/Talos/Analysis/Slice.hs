{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- Path set analysis


module Talos.Analysis.Slice
  ( FInstId(..)
  , assertionsFID
  , SummaryClass(..), isAssertions, isResult, summaryClassToPreds, summaryClassFromPreds
  , Slice'(..), Structural(..)
  ) where

import           Data.Set                        (Set)
import qualified Data.Set                        as Set

import           Control.DeepSeq                 (NFData)
import           GHC.Generics                    (Generic)

import           Daedalus.PP
import           Daedalus.Panic

import           Daedalus.Core
import           Daedalus.Core.Free
import           Daedalus.Core.TraverseUserTypes

import           Talos.Analysis.Eqv
import           Talos.Analysis.Merge

-- import Debug.Trace

--------------------------------------------------------------------------------
-- Slices

-- This tags a particular instance of a function, It is used to name
-- SummaryClasses so that after analysis we can forget which AbsEnv
-- was used to generate the slices.
newtype FInstId = FInstId Int
  deriving (Eq, Ord, Generic, NFData, Eqv)

-- We reserve a well-known id for the assertions class
assertionsFID :: FInstId
assertionsFID = FInstId 0

-- Grammars are summarised w.r.t a (pointed) collection of predicates
-- constraining the post-condition.  If the predicate is bottom then
-- we only care about the constraints inside the grammar.
data SummaryClass p = Assertions | Result (Set p)
  deriving (Ord, Eq, Show, Generic, NFData)

isAssertions, isResult :: SummaryClass p -> Bool
isAssertions Assertions = True
isAssertions _ = False

isResult = not . isAssertions

summaryClassToPreds :: SummaryClass p -> [p]
summaryClassToPreds Assertions = []
summaryClassToPreds (Result p) = Set.toList p

summaryClassFromPreds :: Ord p => [p] -> SummaryClass p
summaryClassFromPreds [] = Assertions
summaryClassFromPreds ps = Result (Set.fromList ps)

-- We represent a Call by a set of the entangled args.  If the
-- args aren't futher entangled by the calling context, then for
-- each argument slice we get a single Call node, where the Set is a
-- singleton containing the representative var as returned by
-- 'explodeDomain'.  The second argument tells us how to instantiate
-- the params used by the call.
--
-- We require a set so we can merge where the callers entangle params.
--
-- The first argument is whether this call is an assigned, in which
-- case the entangled vars must contain ResultVar.  Note that the
-- first argument can also be ResultVar, but that is the result of
-- the current function, not this call (e.g. def Foo = { ...; Bar })
--
-- Note that the set of entangled vars here is a bit different to
-- that which appears in a Domain, if only in intent --- these are
-- entangled by their context, while in a domain they are entangled
-- by use

-- def F x y = { Guard (x > 10); Guard (y > 10); ^ 42 }
--
-- def Q = { a = UInt8; z = F a a; Guard (z > 10); ^ true } -- entangles x and y above
--
-- F will generate slices for 'x', 'y', and the result, but at Q we
-- entangle 'x' and 'y' through 'a'

-- This is a variant of Grammar from Core
data Slice' cn sle =
    SHole -- Type
  | SPure sle
  --  | GetStream
  --  | SetStream Expr

  -- We only really care about a byteset.
  | SMatch ByteSet
  --  | Fail ErrorSource Type (Maybe Expr)
  | SDo Name (Slice' cn sle) (Slice' cn sle)
  --  | Let Name Expr Grammar
  | SChoice [Slice' cn sle] -- This gives better probabilities than nested Ors
  | SCall cn
  | SCase Bool (Case (Slice' cn sle))

   -- | This is the case where the length of the list isn't important,
   -- so we can generate multiple models and combine lazily.  The
   -- lower bound is included so we can assert it is == 0 in the null
   -- case, while the upper bound is included to assert it is > 0 in
   -- the non-null case.
  | SLoopParametric Sem sle (Maybe sle) (Slice' cn sle)
  -- Having 'Structural' here is a bit of a hack
  
  | SLoop Structural (LoopClass' sle (Slice' cn sle))

  -- Extras for synthesis, we don't usually have SLExpr here as we
  -- don't slice the Exprs here.
  | SInverse Name Expr Expr
  -- ^ We have an inverse for this statement; this constructor has a
  -- name for the result (considered bound in this term only), the
  -- inverse expression, and a predicate constraining the value
  -- produced by this node (i.e., result of the original DDL code).
  
  deriving (Generic, NFData)

-- A note on inverses.  The ides is if we have something like
--
-- def F y = {
--   x = UInt8 # UInt8 # UInt8 # UInt 8
--   Guard (x > y && x < 1000)
--   ^ x
-- }
--
-- we can replace calls to F (in slices) like
--
-- ...
-- v = F y
-- ...
-- 
-- with (\result > y && \result < 1000) when synthesising, and then
-- [\result >> 24 as uint 8, \result >> 16 as uint 8, \result >> 8 as uint 8, \result as uint 8 ]
-- when constructing the path (i.e., when getting bytes).

-- | This is for loops -- a loop slice is structural if the structure
-- is important (i.e., the order/number of elements).
data Structural = Structural | StructureInvariant
  deriving (Eq, Ord, Show, Generic, NFData)

--------------------------------------------------------------------------------
-- Domain Instances

-- This assumes the node came from the same statement (i.e., the paths
-- we are comparing are rooted at the same statement).  This means we
-- don't need to compare for equality.
--
-- Thus, for some cases, the presence of a node is enough to return
-- True (e.g. for Assertion)

instance (Eqv cn, PP cn, Eqv sle, PP sle) => Eqv (Slice' cn sle) where
  eqv l r =
    case (l, r) of
      (SHole {}, SHole {}) -> True
      (SHole {}, _)         -> False
      (_       , SHole {}) -> False

      (SPure e, SPure e')  -> eqv e e' -- FIXME: needed?
      (SMatch {}, SMatch {}) -> True
      (SDo _ l1 r1, SDo _ l2 r2)  -> (l1, r1) `eqv` (l2, r2)
      (SChoice ls, SChoice rs)       -> ls `eqv` rs
      (SCall lc, SCall rc)           -> lc `eqv` rc
      (SCase _ lc, SCase _ rc)       -> lc `eqv` rc
      (SLoopParametric _sem lb m_ub sl, SLoopParametric _sem' lb' m_ub' sl') ->
        (lb, sl, m_ub) `eqv` (lb', sl', m_ub')
      (SLoopParametric {}, SLoop {}) -> False
      (SLoop {}, SLoopParametric {}) -> False
      (SLoop str lb, SLoop str' lb') -> (str, lb) `eqv` (str', lb')
      
      (SInverse {}, SInverse {})     -> True
      _                              -> panic "Mismatched terms in eqv (Slice)" ["Left", showPP l, "Right", showPP r]

instance (Merge cn, PP cn, Merge sle, PP sle) => Merge (Slice' cn sle) where
  merge l r =
    case (l, r) of
      (SHole {}, _)                  -> r
      (_       , SHole {})           -> l
      (SPure e, SPure e')            -> SPure (merge e e')
      (SDo x1 slL1 slR1, SDo _x2 slL2 slR2) ->
        SDo x1 (merge slL1 slL2) (merge slR1 slR2)
      (SMatch {}, SMatch {})         -> l

      (SChoice cs1, SChoice cs2)     -> SChoice (zipWith merge cs1 cs2)
      (SCall lc, SCall rc)           -> SCall (merge lc rc)
      (SCase t lc, SCase _ rc)       -> SCase t (merge lc rc)

      ( SLoopParametric sem lb m_lub lc, SLoopParametric _sem rb m_rub rc ) ->
        SLoopParametric sem (merge lb rb) (merge m_lub m_rub) (merge lc rc)
      ( SLoopParametric _sem lb m_lub lc, SLoop str (ManyLoop sem bt rb m_rub rc) ) ->
        SLoop str (ManyLoop sem bt (merge lb rb) (merge m_lub m_rub) (merge lc rc))
      ( SLoopParametric _sem _lb _m_ub lsl, SLoop str (MorphismLoop (MapMorphism lc rsl)) ) ->
        SLoop str (MorphismLoop (MapMorphism lc (merge lsl rsl)))
      ( SLoopParametric _sem _lb _m_ub lsl, SLoop str (MorphismLoop (FoldMorphism n e lc rsl)) ) ->
        SLoop str (MorphismLoop (FoldMorphism n e lc (merge lsl rsl)))
      (SLoop str lc, SLoop str' rc) -> SLoop (merge str str') (merge lc rc)
      (SLoop {}, SLoopParametric {}) -> merge r l
       
      (SInverse {}, SInverse{})      -> l
      _                              -> panic "Mismatched terms in merge"
                                              ["Left", showPP l, "Right", showPP r]

instance Eqv Structural where -- default
instance Merge Structural where
  StructureInvariant `merge` StructureInvariant = StructureInvariant
  _ `merge` _ = Structural

--------------------------------------------------------------------------------
-- Free instances
--
--  Used for getting deps for the SMT solver defs.

instance FreeVars Structural where
  freeVars = mempty
  freeFVars = mempty

instance (FreeVars cn, FreeVars sle) => FreeVars (Slice' cn sle) where
  freeVars sl =
    case sl of
      SHole {}       -> mempty
      SPure   v      -> freeVars v -- FIXME: ignores fset, which night not be what we want
      SDo x l r      -> freeVars l `Set.union` Set.delete x (freeVars r)
      SMatch m       -> freeVars m
      SChoice cs     -> foldMap freeVars cs
      SCall cn       -> freeVars cn
      SCase _ c      -> freeVars c
      SLoopParametric _sem lb m_ub sl' -> freeVars (lb, m_ub, sl')
      SLoop _ lc     -> freeVars lc
      SInverse n f p -> Set.delete n (freeVars (f, p))

  freeFVars sl =
    case sl of
      SHole {}       -> mempty
      SDo _x l r     -> freeFVars l `Set.union` freeFVars r
      SPure v        -> freeFVars v
      SMatch m       -> freeFVars m
      SChoice cs     -> foldMap freeFVars cs
      SCall cn       -> freeFVars cn
      SCase _ c      -> freeFVars c
      SLoopParametric _sem lb m_ub sl' -> freeFVars (lb, m_ub, sl')
      SLoop _ lc     -> freeFVars lc
      -- the functions in f should not be e.g. sent to the solver
      -- FIXME: what about other usages of this function?
      SInverse _ _f p -> {- freeFVars f <> -} freeFVars p

-- -----------------------------------------------------------------------------
-- FreeTCons

instance TraverseUserTypes Structural where
  traverseUserTypes _f s = pure s

instance (TraverseUserTypes cn, TraverseUserTypes sle) => TraverseUserTypes (Slice' cn sle) where
  traverseUserTypes f sl =
    case sl of
      SHole            -> pure SHole
      SPure v          -> SPure <$> traverseUserTypes f v
      SDo x l r        -> SDo  <$> traverseUserTypes f x
                               <*> traverseUserTypes f l
                               <*> traverseUserTypes f r
      SMatch m         -> SMatch <$> traverseUserTypes f m
--      SAssertion e     -> SAssertion <$> traverseUserTypes f e
      SChoice cs       -> SChoice <$> traverseUserTypes f cs
      SCall cn         -> SCall   <$> traverseUserTypes f cn
      SCase b c        -> SCase b <$> traverseUserTypes f c
      SLoopParametric sem lb m_ub sl' -> 
        SLoopParametric sem <$> traverseUserTypes f lb
                            <*> traverseUserTypes f m_ub
                            <*> traverseUserTypes f sl'
      SLoop str lc     -> SLoop str <$> traverseUserTypes f lc
      SInverse n ifn p -> SInverse n <$> traverseUserTypes f ifn <*> traverseUserTypes f p

instance (Ord p, TraverseUserTypes p) => TraverseUserTypes (SummaryClass p) where
  traverseUserTypes _f Assertions = pure Assertions
  traverseUserTypes f (Result r) = Result <$> traverseUserTypes f r

--------------------------------------------------------------------------------
-- PP Instances
-- instance PP CallInstance where
--   ppPrec n (CallInstance { callParams = ps, callSlice' = sl }) =
--     wrapIf (n > 0) $ pp ps <+> "-->" <+> pp sl

-- c.f. PP Grammar
instance (PP cn, PP sle) => PP (Slice' cn sle) where
  pp sl =
    case sl of
      SHole          -> "□"
      SPure e        -> "pure" <+> ppPrec 1 e
      SMatch e       -> "match" <+> pp e
      SDo  {}        -> "do" <+> ppStmts' sl
      SChoice cs     -> "choice" <> block "{" "," "}" (map pp cs)
      SCall cn       -> pp cn
      SCase _ c      -> pp c
      SLoopParametric _sem lb m_ub sl' ->
        "Loop[0/1]" <> parens (pp lb <> ".." <> maybe "" pp m_ub) <> " " <> pp sl'
      SLoop _ lc     -> pp lc -- forget Structural
      SInverse n' ifn p -> -- wrapIf (n > 0) $
        "inverse for" <+> ppPrec 1 n' <+> "is" <+> ppPrec 1 ifn <+> "/" <+> ppPrec 1 p

ppStmts' :: (PP cn, PP sle) => Slice' cn sle -> Doc
ppStmts' sl =
  case sl of
    SDo x g1 g2 -> pp x <+> "<-" <+> pp g1 $$ ppStmts' g2
    _           -> pp sl

instance PP FInstId where
  pp (FInstId i) = pp i

instance PP p => PP (SummaryClass p) where
  pp Assertions = "Assertions"
  pp (Result p) = "Result" <+> brackets (commaSep (map pp (Set.toList p)))

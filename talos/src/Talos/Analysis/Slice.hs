{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLabels #-}

-- Path set analysis


module Talos.Analysis.Slice
  ( FInstId(..)
  , assertionsFID
  , SummaryClass(..), isAssertions, isResult, summaryClassToPreds, summaryClassFromPreds
  , Slice'(..), Structural(..), SLoopClass(..)
  , SHoleSize(..), staticSHoleSize, dynamicSHoleSize
  , sloopClassBody, sloopClassE, mapSLoopClassE, foldMapSLoopClassE
  ) where

import           Control.Applicative             (Const (..), (<|>))
import           Control.Lens                    (Traversal', foldMapOf,
                                                  traversal, traverseOf, mapped, (%~), (*~), traversed)
import           Data.Functor.Identity           (Identity (..))
import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq
import           Data.Set                        (Set)
import qualified Data.Set                        as Set

import           Control.DeepSeq                 (NFData)
import           GHC.Generics                    (Generic)
import           Data.Generics.Labels            ()

import           Daedalus.Panic
import           Daedalus.PP

import           Daedalus.Core
import           Daedalus.Core.Free
import           Daedalus.Core.TraverseUserTypes

import           Talos.Analysis.Eqv
import           Talos.Analysis.Merge
import Data.Foldable (toList)

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
  -- | A hole where we don't care about the value.  The argument is
  -- the number of bytes that the elided grammar uses. The structure
  -- is only used for merging, we could erase when exporting.
    SHole (Maybe (SHoleSize, Slice' cn sle)) -- FIXME: param by the Slice'
  | SPure sle
  
  | SGetStream
  | SSetStream sle
  | SEnd 

  -- We only really care about a byteset.
  | SMatch ByteSet
  --  | Fail ErrorSource Type (Maybe Expr)
  | SDo (Maybe Name) (Slice' cn sle) (Slice' cn sle)
  | SChoice [Slice' cn sle] -- This gives better probabilities than nested Ors
  | SCall cn
  | SCase Bool (Case (Slice' cn sle))

  | SLoop (SLoopClass sle (Slice' cn sle))

  -- Extras for synthesis, we don't usually have SLExpr here as we
  -- don't slice the Exprs here.
  | SInverse Name Expr Expr
  -- ^ We have an inverse for this statement; this constructor has a
  -- name for the result (considered bound in this term only), the
  -- inverse expression, and a predicate constraining the value
  -- produced by this node (i.e., result of the original DDL code).

  deriving (Generic, NFData)

-- -----------------------------------------------------------------------------
-- Sized holes
--
-- These are used to track the size of a part of a grammar for nodes
-- with bounded streams.

staticSHoleSize :: Int -> SHoleSize
staticSHoleSize n = SHoleSize n mempty

dynamicSHoleSize :: Name -> SHoleSize -> SHoleSize
dynamicSHoleSize n mult =
  SHoleSize { shsStatic = 1
            , shsDynamic = Seq.singleton expr
            }
  where
    expr = SHoleSizeExpr { shseSeq = n , shseMult = mult }
    
data SHoleSize = SHoleSize
  { shsStatic  :: Int
  , shsDynamic :: Seq SHoleSizeExpr
  } deriving (Generic, NFData, Eq, Ord)

-- | This summarises size information of a slice, allowing us to
-- avoid merging slices.
data SHoleSizeExpr = SHoleSizeExpr
  { shseSeq  :: Name -- The length of this times the shseMult
  , shseMult :: SHoleSize
  } deriving (Generic, NFData, Eq, Ord)

sHoleSizeNameT :: Traversal' SHoleSize Name
sHoleSizeNameT = traversal go
  where
    go :: Applicative f => (Name -> f Name) -> SHoleSize -> f SHoleSize
    go f = traverseOf (#shsDynamic . traversed) (goE f)
    goE :: Applicative f => (Name -> f Name) -> SHoleSizeExpr -> f SHoleSizeExpr    
    goE f shse = SHoleSizeExpr <$> f (shseSeq shse) <*> go f (shseMult shse)

addSHoleSize :: SHoleSize -> SHoleSize -> SHoleSize
addSHoleSize sh1 sh2 = SHoleSize
  { shsStatic  = shsStatic sh1 + shsStatic sh2
  , shsDynamic = shsDynamic sh1 <> shsDynamic sh2
  }

scaleSHoleSize :: Int -> SHoleSize -> SHoleSize
scaleSHoleSize k =
  (#shsStatic *~ k)
  . (#shsDynamic . mapped %~ scaleSHoleSizeExpr k)

scaleSHoleSizeExpr :: Int -> SHoleSizeExpr -> SHoleSizeExpr
scaleSHoleSizeExpr k = #shseMult %~ scaleSHoleSize k

-- | Smart SDo constructor which constructs compound holes where
-- possible.
sDo :: Maybe Name -> Slice' cn sle -> Slice' cn sle -> Slice' cn sle
-- If m_x is free in rhs then the lhs can't be a hole.
sDo m_x (SHole (Just (shs1, sl1))) (SHole (Just (shs2, sl2))) =
  SHole (Just (addSHoleSize shs1 shs2, SDo m_x sl1 sl2))
sDo m_x lhs rhs = SDo m_x lhs rhs
    

-- A note on inverses.  The idea is if we have something like
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

-- | For sequences, there are three ways we care about loop
-- dependency, apart from the dependency on the elements.
data Structural =
  StructureIndependent
  -- ^ No dependence on the structure, including whether it is empty
  -- or not, for example
  --
  --  xs = Many UInt8
  --  map (x in xs) block
  --    $$ = UInt8
  --    x < $$ is true
  --
  | StructureIsNull
  -- ^ Depends only on whether the list is empty or not, typically
  -- because the usage of the list constrains other parts of the
  -- grammar, and so the emptiness (or not) may be relevant to
  -- e.g. the solvability of the slice, for example
  -- 
  -- > let xs = Many UInt8
  -- > let y = UInt8
  -- > map (x in xs) (x < y is true)
  | StructureDependent
  -- ^ The order and length of the list, for example
  -- 
  -- >  let xs = Many UInt8
  -- >  ^ for (acc = 0; x in xs) (x + acc * 10)

  deriving (Eq, Show, Generic, NFData)

instance Ord Structural where
  StructureIndependent <= _ = True
  StructureIsNull <= StructureIsNull    = True
  StructureIsNull <= StructureDependent = True
  StructureDependent <= StructureDependent = True
  _ <= _ = False

data SLoopClass sle b =
  SLoopPool Sem b
  -- ^ Should generate a set of models for b, and we will pick the
  -- number we need.  The body should not depend on the context or the
  -- collection.  This can occur when the body constructs the output
  -- of the map without inspecting the element, for example
  --
  -- > xs = ...
  -- > ys = map (x in xs) block
  -- >        a = f x
  -- >        b = UInt8
  -- >        b > 10 is true
  --
  -- because even though the slice will have no deps, recall we
  -- consider slices which establish a post-condition to be open (so
  -- we can bind then in a Do to link up with the user of the post-condition).

  | SManyLoop Structural sle (Maybe sle) b
  -- ^ A Many loop where the dependence on the result is determined by
  -- the first parameter: it should be possible to ignore this flag,
  -- e.g. in a simple strategy, but a more complex strategy can take
  -- advantage of this information to produce a set of results (i.e.,
  -- order independent and not all need to be used).

  | SRepeatLoop Structural Name sle b
  -- ^ A Repeat loop where the dependence on the result is determined
  -- by the first parameter, as for 'SManyLoop'.

  | SMorphismLoop (LoopMorphism' sle b)
  -- ^ A 'LoopMorphism', where the structure is determined by the collection.
  deriving (Functor, Foldable, Traversable, Generic, NFData)

sloopClassBody :: SLoopClass sle b -> b
sloopClassBody lc =
  case lc of
    SLoopPool _ b -> b
    SManyLoop _str _lb _m_ub b -> b
    SRepeatLoop _str _n _e b -> b
    SMorphismLoop lm -> morphismBody lm

sloopClassE :: Applicative f => (sle -> f sle') -> (b -> f b') -> SLoopClass sle b ->
                      f (SLoopClass sle' b')
sloopClassE ef bf lc =
  case lc of
    SLoopPool s b -> SLoopPool s <$> bf b
    SManyLoop str lb m_ub b ->
      SManyLoop str <$> ef lb <*> traverse ef m_ub <*> bf b
    SRepeatLoop str n e b ->
      SRepeatLoop str n <$> ef e <*> bf b
    SMorphismLoop lm -> SMorphismLoop <$> morphismE ef bf lm

mapSLoopClassE :: (e -> e') -> (a -> a') ->
                   SLoopClass e a -> SLoopClass e' a'
mapSLoopClassE ef af lc =
  runIdentity (sloopClassE (Identity . ef) (Identity . af) lc)

foldMapSLoopClassE :: Monoid m => (e -> m) -> (a -> m) ->
                       SLoopClass e a -> m
foldMapSLoopClassE ef af lc = m
  where
    Const m = sloopClassE (Const . ef) (Const . af) lc

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
      (SHole {}, SHole {}) -> True -- The sizes should be the same
      (SHole {}, _)         -> False
      (_       , SHole {}) -> False

      (SPure e, SPure e')  -> eqv e e' -- FIXME: needed?
      (SGetStream, SGetStream) -> True
      (SSetStream e, SSetStream e') -> eqv e e'
      (SEnd, SEnd) -> True
      
      (SMatch {}, SMatch {}) -> True
      (SDo _ l1 r1, SDo _ l2 r2)  -> (l1, r1) `eqv` (l2, r2)
      (SChoice ls, SChoice rs)       -> ls `eqv` rs
      (SCall lc, SCall rc)           -> lc `eqv` rc
      (SCase _ lc, SCase _ rc)       -> lc `eqv` rc
      (SLoop lc, SLoop lc')          -> lc `eqv` lc'
      (SInverse {}, SInverse {})     -> True
      _                              -> panic "Mismatched terms in eqv (Slice)" ["Left", showPP l, "Right", showPP r]

instance (Merge cn, PP cn, Merge sle, PP sle) => Merge (Slice' cn sle) where
  merge l r =
    case (l, r) of
      -- Filling in holes.
      
      -- Pick the first non-Nothing, we should never have Just for
      -- both (we can only create 1 bounded-stream slice)
      (SHole m_le, SHole m_re)            -> SHole (m_le <|> m_re)
      (SHole Nothing, _)                  -> r
      (_       , SHole Nothing)           -> l      
      -- These are those nodes which create a wrapped hole.
      (SHole (Just (_, str)), _) -> merge str r
      (_, SHole (Just (_, str))) -> merge l str

      -- Non-holes
      (SPure e, SPure e')            -> SPure (merge e e')
      (SGetStream, SGetStream)       -> SGetStream
      (SSetStream e, SSetStream e')  -> SSetStream (merge e e')
      (SEnd, SEnd)                   -> SEnd
      
      (SDo x1 slL1 slR1, SDo _x2 slL2 slR2) ->
        -- Handle compound sized hole construction.
        sDo x1 (merge slL1 slL2) (merge slR1 slR2)
      (SMatch {}, SMatch {})         -> l

      (SChoice cs1, SChoice cs2)     -> SChoice (zipWith merge cs1 cs2)
      (SCall lc, SCall rc)           -> SCall (merge lc rc)
      (SCase t lc, SCase _ rc)       -> SCase t (merge lc rc)

      (SLoop lc, SLoop lc')          -> SLoop (merge lc lc')
      (SInverse {}, SInverse{})      -> l
      
      _                              -> panic "Mismatched terms in merge"
                                              ["Left", showPP l, "Right", showPP r]

instance Eqv Structural where -- default
instance Merge Structural where
  merge = max

instance (Eqv sle, Eqv b) => Eqv (SLoopClass sle b) where
  SLoopPool _s b `eqv ` SLoopPool _s' b' = b `eqv` b'
  SManyLoop str lb m_ub b `eqv ` SManyLoop str' lb' m_ub' b'
    = (str, lb, m_ub, b) `eqv` (str', lb', m_ub', b')
  SRepeatLoop str _n sle b `eqv` SRepeatLoop str' _n' sle' b' =
    (str, sle, b) `eqv` (str', sle', b')
  SMorphismLoop lm `eqv` SMorphismLoop lm' = lm `eqv` lm'
  _ `eqv` _ = False

instance (Merge sle, Merge b, PP sle, PP b) => Merge (SLoopClass sle b) where
  SLoopPool s b `merge ` SLoopPool _s b' = SLoopPool s (merge b b')

  SLoopPool _ b `merge`  SManyLoop str lb m_ub b' = SManyLoop str lb m_ub (merge b b')
  lc@SManyLoop {} `merge` lc'@SLoopPool {} = merge lc' lc
  SManyLoop str lb m_ub b `merge` SManyLoop str' lb' m_ub' b'
    = SManyLoop (merge str str') (merge lb lb') (merge m_ub m_ub') (merge b b')

  SRepeatLoop str n sle b `merge` SRepeatLoop str' _n sle' b' =
    SRepeatLoop (merge str str') n (merge sle sle') (merge b b')

  SMorphismLoop lm `merge` SMorphismLoop lm' = SMorphismLoop (merge lm lm')
  -- Now for MorphismBody and LoopMorphism
  SLoopPool _ b `merge` SMorphismLoop lm =
    SMorphismLoop $ case lm of
                      MapMorphism lc b' -> MapMorphism lc (merge b b')
                      FoldMorphism n e lc b' -> FoldMorphism n e lc (merge b b')
  lc@SMorphismLoop {} `merge` lc'@SLoopPool {} = merge lc' lc

  lc `merge` lc' = panic "IMPOSSIBLE (merge)" [showPP lc, showPP lc']

--------------------------------------------------------------------------------
-- Free instances
--
--  Used for getting deps for the SMT solver defs.

instance FreeVars Structural where
  freeVars = mempty
  freeFVars = mempty

instance FreeVars SHoleSize where
  freeVars = foldMapOf sHoleSizeNameT freeVars
  freeFVars = foldMapOf sHoleSizeNameT freeFVars

instance (FreeVars cn, FreeVars sle) => FreeVars (Slice' cn sle) where
  freeVars sl =
    case sl of
      SHole e        -> freeVars e -- FIXME: we can probably ignore
                                   -- the slice in this node as the
                                   -- size should have the same frees.
      SPure   v      -> freeVars v -- FIXME: ignores fset, which night not be what we want
      SGetStream     -> mempty
      SSetStream e   -> freeVars e
      SEnd           -> mempty
      SDo m_x l r    -> freeVars l `Set.union` maybe id Set.delete m_x (freeVars r)
      SMatch m       -> freeVars m
      SChoice cs     -> foldMap freeVars cs
      SCall cn       -> freeVars cn
      SCase _ c      -> freeVars c
      SLoop lc       -> freeVars lc
      SInverse n f p -> Set.delete n (freeVars (f, p))

  freeFVars sl =
    case sl of
      SHole e        -> freeFVars e
      SDo _x l r     -> freeFVars l `Set.union` freeFVars r
      SPure v        -> freeFVars v
      SGetStream     -> mempty
      SSetStream e   -> freeFVars e
      SEnd           -> mempty
      SMatch m       -> freeFVars m
      SChoice cs     -> foldMap freeFVars cs
      SCall cn       -> freeFVars cn
      SCase _ c      -> freeFVars c
      SLoop lc       -> freeFVars lc
      -- the functions in f should not be e.g. sent to the solver
      -- FIXME: what about other usages of this function?
      SInverse _ _f p -> {- freeFVars f <> -} freeFVars p


instance (FreeVars sle, FreeVars b) => FreeVars (SLoopClass sle b) where
  freeVars lc =
    case lc of
      SLoopPool _ b -> freeVars b
      SManyLoop _str lb m_ub b -> freeVars (lb, m_ub, b)
      SRepeatLoop _str n e b   -> freeVars e <> Set.delete n (freeVars b)
      SMorphismLoop lm -> freeVars lm

  freeFVars lc =
    case lc of
      SLoopPool _ b -> freeFVars b
      SManyLoop _str lb m_ub b -> freeFVars (lb, m_ub, b)
      SRepeatLoop _str _n e b   -> freeFVars (e, b)
      SMorphismLoop lm -> freeFVars lm

-- -----------------------------------------------------------------------------
-- FreeTCons

instance TraverseUserTypes Structural where
  traverseUserTypes _f s = pure s

instance TraverseUserTypes SHoleSize where
  traverseUserTypes f = traverseOf sHoleSizeNameT (traverseUserTypes f)

instance (TraverseUserTypes cn, TraverseUserTypes sle) => TraverseUserTypes (Slice' cn sle) where
  traverseUserTypes f sl =
    case sl of
      SHole e          -> SHole <$> traverseUserTypes f e
      SPure v          -> SPure <$> traverseUserTypes f v
      SGetStream       -> pure sl
      SSetStream e     -> SSetStream <$> traverseUserTypes f e
      SEnd             -> pure sl
      SDo x l r        -> SDo  <$> traverseUserTypes f x
                               <*> traverseUserTypes f l
                               <*> traverseUserTypes f r
      SMatch m         -> SMatch <$> traverseUserTypes f m
      SChoice cs       -> SChoice <$> traverseUserTypes f cs
      SCall cn         -> SCall   <$> traverseUserTypes f cn
      SCase b c        -> SCase b <$> traverseUserTypes f c
      SLoop lc         -> SLoop <$> traverseUserTypes f lc
      SInverse n ifn p -> SInverse n <$> traverseUserTypes f ifn <*> traverseUserTypes f p

instance (Ord p, TraverseUserTypes p) => TraverseUserTypes (SummaryClass p) where
  traverseUserTypes _f Assertions = pure Assertions
  traverseUserTypes f (Result r) = Result <$> traverseUserTypes f r

instance (TraverseUserTypes sle, TraverseUserTypes b) =>
         TraverseUserTypes (SLoopClass sle b) where
  traverseUserTypes f lc =
    case lc of
      SLoopPool s b -> SLoopPool s <$> traverseUserTypes f b
      SManyLoop str lb m_ub b -> SManyLoop str <$> traverseUserTypes f lb
                                               <*> traverseUserTypes f m_ub
                                               <*> traverseUserTypes f b
      SRepeatLoop str n e b -> SRepeatLoop str <$> traverseUserTypes f n
                                               <*> traverseUserTypes f e
                                               <*> traverseUserTypes f b
      SMorphismLoop lm -> SMorphismLoop <$> traverseUserTypes f lm

--------------------------------------------------------------------------------
-- PP Instances
-- instance PP CallInstance where
--   ppPrec n (CallInstance { callParams = ps, callSlice' = sl }) =
--     wrapIf (n > 0) $ pp ps <+> "-->" <+> pp sl

instance PP SHoleSize where
  pp shs
    | shsStatic shs == 1, Seq.null (shsDynamic shs) = "1"
    | otherwise = stat <> mult <> dyn
    where
      mult | shsStatic shs == 1 || Seq.null (shsDynamic shs) = mempty
           | otherwise = " * "
      stat | shsStatic shs == 1 = mempty
           | otherwise      = pp (shsStatic shs)
      dyn | Seq.null (shsDynamic shs) = mempty
          | [x] <- toList (shsDynamic shs) = pp x
          | otherwise = parens (hsep (punctuate "+" (map pp (toList $ shsDynamic shs))))

instance PP SHoleSizeExpr where
  pp shse = pp (shseSeq shse) <> ppMult (shseMult shse)
    where
      ppMult m | shsStatic m == 1, Seq.null (shsDynamic m) = mempty
               | otherwise = " *" <+> pp (shseMult shse)
            
-- c.f. PP Grammar
instance (PP cn, PP sle) => PP (Slice' cn sle) where
  pp sl =
    case sl of
      SHole sz       -> "□" <> maybe mempty (parens . pp . fst) sz
      SPure e        -> "pure" <+> ppPrec 1 e
      SGetStream     -> "getStream"
      SSetStream e   -> "setStream" <+> pp e
      SEnd           -> "end"
      SMatch e       -> "match" <+> pp e
      SDo  {}        -> "do" <+> ppStmts' sl
      SChoice cs     -> "choice" <> block "{" "," "}" (map pp cs)
      SCall cn       -> pp cn
      SCase _ c      -> pp c
      SLoop lc       -> pp lc -- forget Structural
      SInverse n' ifn p -> -- wrapIf (n > 0) $
        "inverse for" <+> ppPrec 1 n' <+> "is" <+> ppPrec 1 ifn <+> "/" <+> ppPrec 1 p

ppStmts' :: (PP cn, PP sle) => Slice' cn sle -> Doc
ppStmts' sl =
  case sl of
    SDo m_x g1 g2 -> maybe mempty (\x -> pp x <+> "<-") m_x <+> pp g1 $$ ppStmts' g2
    _           -> pp sl

instance PP FInstId where
  pp (FInstId i) = pp i

instance PP p => PP (SummaryClass p) where
  pp Assertions = "Assertions"
  pp (Result p) = "Result" <+> brackets (commaSep (map pp (Set.toList p)))

instance (PP sle, PP b) => PP (SLoopClass sle b) where
  pp lc =
    case lc of
      SLoopPool s b -> ("LoopPool" <> parens pp_s) <+> pp b
        where pp_s = case s of
                       SemNo -> "@"
                       _     -> ""
                
      SManyLoop str l m_h g ->
        "Many" <.> pp str  <.>
        parens (pp l <.> ".." <.> maybe "" pp m_h) <+> pp g
      SRepeatLoop str n e g   ->
        "for" <.> pp str <+> parens (pp n <+> "=" <+> pp e) <+> pp g
      SMorphismLoop lm  -> pp lm

instance PP Structural where
  pp str =
    case str of
      StructureIndependent -> "*"
      StructureIsNull -> "+"
      StructureDependent -> "!"

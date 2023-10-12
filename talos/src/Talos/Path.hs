{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, ParallelListComp #-}

-- Representation of Paths

module Talos.Path where

import           Control.DeepSeq       (NFData)
import           Control.Lens          (ix, (%~), (&))
import           Data.ByteString       (ByteString)
import           Data.List             (transpose)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           GHC.Generics          (Generic)

import           Daedalus.PP
import           Daedalus.Panic
import           Daedalus.GUID (GUID)

import           Talos.Analysis.Merge  (Merge (..))
import           Talos.Analysis.Slice  (FInstId, assertionsFID)

--------------------------------------------------------------------------------
-- Representation of paths/pathsets

-- This datastructure handles the cases like:
--
-- def F = block
--   xs = Many Q <| pure [1, 2, 3]
--   for (x in xs) R
--
-- where choosing for xs means also choosing for the 'for' loop,
-- noting that the Many is nested.  In this case, we mau get a path like
--
-- Do ( Choose[0](Many [ (Q1, [R1]), (Q2, [R2]), ...] [ (0, PCNode PCTarget) ]
--    (SelectedLoop SelectedHole)
--
-- where we don't fill in the Hole until the Many is filled in.

data SelectedPathF ch ca fn lp a =
    SelectedHole
  | SelectedBytes ProvenanceTag a
  --  | Fail ErrorSource Type (Maybe Expr)
  | SelectedDo (SelectedPathF ch ca fn lp a) (SelectedPathF ch ca fn lp a)
  | SelectedChoice (ch (SelectedPathF ch ca fn lp a))
  | SelectedCall (fn (SelectedPathF ch ca fn lp a))
  | SelectedCase (ca (SelectedPathF ch ca fn lp a))
  | SelectedLoop (lp (SelectedPathF ch ca fn lp a))
  deriving (Functor, Foldable, Traversable, Generic)

data PathIndex a  = PathIndex { pathIndex :: Int, pathIndexPath :: a }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)

data CallInstantiation a = CallInstantiation { instantiationId :: FInstId, instantiationVal :: a }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)

assertionsCI :: CallInstantiation SelectedPath
assertionsCI = CallInstantiation assertionsFID SelectedHole

-- We don't really need the index for cases, but it makes life a bit
-- easier if we can just copy choice.
type SelectedPath = SelectedPathF PathIndex PathIndex CallInstantiation SelectedLoopF ByteString

deriving instance NFData SelectedPath

-- -----------------------------------------------------------------------------
-- Loops

type SelectedLoopPool = SelectedLoopPoolF SelectedPath
deriving instance NFData SelectedLoopPool

data SelectedLoopPoolF a = SelectedLoopPoolF
  {
    -- | The possible paths in a Many, one list entry per Many
    -- iteration.  The second element is the following grammar nodes
    -- that depend on the Many.
    smPaths   :: [ (a, [a]) ]
    -- | The cursors for the subsequent nodes, depth is for Do nodes
    -- only, and we have on entry here for each element in the second
    -- entry of smPaths.
  , smCursors :: [ (Int, PathCursor) ]
  }
  deriving (Functor, Foldable, Traversable, Generic)

type SelectedLoop = SelectedLoopF SelectedPath
deriving instance NFData SelectedLoop

data SelectedLoopF a =
  -- | For Many nodes, we have one element for each merged node (i.e.,
  -- slice) --- merging two of these nodes just appends the lists.
  -- The Bool says whether the loop can be empty, the second argument
  -- are the possible elements.
    SelectedLoopPool     LoopGeneratorTag Bool [SelectedLoopPoolF a]
  | SelectedLoopElements (Maybe LoopGeneratorTag) [a]
  -- ^ Elements of a sequence, the generator tag is used to resolve
  -- PCLoopPool paths later on.
  deriving (Functor, Foldable, Traversable, Generic)

-- -----------------------------------------------------------------------------
-- Cursors
--
--  A cursor is an index to a particular node, used to update loop elements.

-- | A 'PathCursorElement' tells how to navigate a 'SelectedPathF'.
-- See also 'PathContextElement'.  Note that if we have multiple
-- slices for loops, we will need to make this follow the structure of
-- the path more closely --- in particular, we will need to be able to
-- extend previously-filled-in holes.
data PathCursorElement =
  PCDoLeft
  | PCDoRight
  | PCSequence Int
  -- ^ An element of a sequence.
  | PCLoopPool LoopGeneratorTag
  -- ^ This allows us to deal with code like the following
  -- @
  --  xss = Many (Many P)
  --  ys = map (xs in xss) (map (x in xs) ...)
  -- @
  --
  -- where the index into a sequence isn't known until thee elements
  -- of the generating loop are selected.
  deriving (Eq, Ord, Show, Generic, NFData)

-- | c.f. 'SequenceTag' and 'SymbolicLoopTag', which are basically this.
type LoopGeneratorTag = GUID

-- | THis is a path inside a SelectedPath to a particlar node, c.f. Zippers
type PathCursor = [PathCursorElement]

resolveLoopGeneratorTags :: Map LoopGeneratorTag Int -> PathCursor -> PathCursor
resolveLoopGeneratorTags tagInst = map fixup
  where
    fixup (PCLoopPool tag)
      | Just i <- Map.lookup tag tagInst = PCSequence i
      | otherwise = panic "Missing LoopGeneratorTag" []
    fixup el = el

-- | This should target a hole in a slice.  Note that any PCLoopPool
-- should have been resolved before this.
fillCursorTarget :: SelectedPath -> PathCursor -> SelectedPath -> SelectedPath
fillCursorTarget fill = go
  where
    -- This is a bit gross?
    go pc' sp =
      case (sp, pc') of
        (SelectedHole, [])                 -> fill
        (SelectedHole, _)                  -> badPath
        (SelectedBytes {}, _)              -> badPath
        (SelectedChoice pidx, _)           -> SelectedChoice (go pc' <$> pidx)
        (SelectedCall (CallInstantiation fid sp'), _)          -> SelectedCall (CallInstantiation fid (go pc' sp'))
        (SelectedCase sp', _)              -> SelectedCase (go pc' <$> sp')
        (SelectedDo l r, PCDoLeft : rest)  -> SelectedDo (go rest l) r
        (SelectedDo l r, PCDoRight : rest) -> SelectedDo l (go rest r)
        (SelectedDo {}, _)                 -> badPath
        (SelectedLoop {}, [])              -> badPath
        (SelectedLoop (SelectedLoopElements tag els), PCSequence i : rest)
          | i < length els -> SelectedLoop (SelectedLoopElements tag (els & ix i %~ go rest))
        (SelectedLoop _, _) -> badPath

    badPath = panic "Unexpected path element" []

-- | Given the generator for a loop element, and a user, this will
-- determine the target of th| cursor _with respect to_ the generating
-- loop.  Here is an example: consider the code
--
--  xs = Many UInt8
--  y  = UInt8
--  zs = map (x in xs) { z = UInt8; z < y is true }
--
-- which generates a 'SelectedPaht' like
-- 
--    +-----+         +------+
--    |     |    x    |      |
--    +-----+    |    +------+
--       |       |       |
--       |       |       |
--       *-------*-------*--------x
--
-- where each '*' is a do node, and the boxes represent the loops.
-- The many (in the first bo) will have a path cursor 'L' for taking a
-- left at the node, while the map (second box) will be at 'RRL'. When
-- updating the 'map' node, we find thee first branch point, and then
-- start from there.  This branch point is above the Many node by the
-- length of the context-generating path elements, less one
-- (representing the point of divergence), so in the above the length
-- will be 1 (as 'L' generates a new path element), so less 1 means
-- when we are updating the paths, we start at the top of the stack,
-- move right, right, then left and update that node.

relitiviseCursors :: PathCursor -> PathCursor -> (Int, PathCursor)
relitiviseCursors gen use
  | depthToBranch > 0 = (depthToBranch - 1, use')
  | otherwise = panic "UNEXPECTED: 0 depth to branch" []
  where
    depthToBranch = length (filter isContextPC gen')

    isContextPC PCDoLeft = True
    isContextPC PCDoRight = False -- Doesn't generate a context element
    isContextPC PCSequence {} = True
    isContextPC PCLoopPool {} = True -- same as PCSequence above.

    (gen', use') = stripCommon gen use

    stripCommon [] ys = ([], ys)
    stripCommon xs []  = (xs, [])
    stripCommon xs@(x : xs') ys@(y : ys')
      | x == y = stripCommon xs' ys'
      | otherwise = (xs, ys)

-- -----------------------------------------------------------------------------
-- SelectedMany

makeManyTargets :: LoopGeneratorTag ->
                   Map LoopGeneratorTag Int ->
                   SelectedLoopPool ->
                   Map Int [(PathCursor, SelectedPath)]
makeManyTargets gentag tagInst sm = Map.fromListWith (<>) els
  where
    els = [ (depth, [( resolveLoopGeneratorTags tagInst pc
                     , SelectedLoop (SelectedLoopElements (Just gentag) sps))])
          | (depth, pc) <- smCursors sm
          | sps <- transpose $ map snd (smPaths sm)
          ]

-- | This handles the lazy merge of Many elements.  This allows
-- decoupling e.g. the count of elements from the synthesis of the
-- elements. In practice (until we stop squashing loop bodies) this
-- will be applied to singleton lists.
selectedMany :: LoopGeneratorTag ->
                Map LoopGeneratorTag Int ->
                [SelectedLoopPool] ->
                ([SelectedPath], Map Int [(PathCursor, SelectedPath)])
selectedMany gentag tagInst ms
  | not (same (map (length . smPaths) ms)) = panic "BUG: selectedMany needs to agree on count" []
  | otherwise = (mps, tgts)
  where
    same [] = True
    same (x : xs) = all (== x) xs

    -- Merge the elements
    mps  = map (foldl1 merge) $ transpose $ map (map fst . smPaths) ms
    tgts = Map.unionsWith (<>) (map (makeManyTargets gentag tagInst) ms)

-- -----------------------------------------------------------------------------
-- PathContext

-- | These are the places in a SelectedPath which can branch (i.e.,
-- sequence points).
data PathContextElement =
  PCEDoRHS SelectedPath
  -- ^ When we enter the LHS of a bind, we push the context for the
  -- RHS so we can update if required.
  | PCELoopBody Int [SelectedPath]
  -- ^ When we enter a loop element, we push the remaining loop
  -- iteration elements along with the current loop element index.
  -- This needs to match up with the use of 'PathCursor' -- when we
  -- would generate a 'PathCursor' in model generation, we need to
  -- push a 'PCELoopBody' during synthesis.

type PathContext = [PathContextElement]

-- | Refines the RHS continuations based on the results of a call to
-- selectedMany.  Note that the PathCursors start with the first
-- different path node, so either PCDoRight or PCSequence as the user
-- of a sequence will be _after_ the generator.
applyManyTargets :: Map Int [(PathCursor, SelectedPath)] ->
                    PathContext -> PathContext
applyManyTargets tgts = go 0 (Map.toList tgts)
  where
    go _here [] stack = stack
    go here ((depth, pcs) : rest) (frame : stack)
      | depth == here = foldl goOne frame pcs : go (here + 1) rest stack
    go here ps (frame : stack) = frame : go (here + 1) ps stack
    go _    (_ : _) [] = bad

    -- A bit inefficient, but it will be rare to have lots of targets.
    goOne frame (pc, tgt) =
      case (frame, pc) of
        -- If we are looking at a RHS
        (PCEDoRHS p', PCDoRight : pc') -> PCEDoRHS (fillCursorTarget tgt pc' p')
        (PCEDoRHS {}, _) -> bad
        (PCELoopBody i ps, PCSequence j : pc')
          | i < j -> PCELoopBody i (ps & ix (j - (i + 1)) %~ fillCursorTarget tgt pc')
        (PCELoopBody {}, _) -> bad

    bad = panic "BUG: mismatched Many target/Do stack" []

splitPath :: SelectedPath -> (SelectedPath, SelectedPath)
splitPath cp =
  case cp of
    SelectedDo l r -> (l, r)
    SelectedHole   -> (SelectedHole, SelectedHole)
    _ -> panic "splitPath: saw a non-{Do,Hole}" []

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

instance Merge a => Merge (SelectedLoopF a) where
  merge lp lp' =
    case (lp, lp') of
      (SelectedLoopPool tag canBeNull pool, SelectedLoopPool tag' canBeNull' pool')
        | tag == tag' -> SelectedLoopPool tag (canBeNull && canBeNull') (pool ++ pool')
        | otherwise -> panic "Mismatched tags" []
      -- FIXME: if we actually hit this (i.e. multiple loop slices) we
      -- might need to have a set of tags.
      (SelectedLoopElements ltag els, SelectedLoopElements _ltag' els')
        | length els == length els' -> SelectedLoopElements ltag (merge els els')
      _ -> panic "BUG: mismatched structure for merging SelectedLoop" []

-- FIXME: too general probably
instance Merge (SelectedPathF PathIndex PathIndex CallInstantiation SelectedLoopF a) where
  merge psL psR =
    case (psL, psR) of
      (SelectedHole, _) -> psR
      (_, SelectedHole) -> psL
      (SelectedChoice (PathIndex n1 sp1), SelectedChoice (PathIndex n2 sp2))
        | n1 /= n2  -> panic "BUG: Incompatible paths selected in merge" [show n1, show n2]
        | otherwise -> SelectedChoice (PathIndex n1 (merge sp1 sp2))
      (SelectedCase (PathIndex n1 sp1), SelectedCase (PathIndex n2 sp2))
        | n1 /= n2  -> panic "BUG: Incompatible paths selected in merge" [show n1, show n2]
        | otherwise -> SelectedCase (PathIndex n1 (merge sp1 sp2))

      (SelectedCall (CallInstantiation cl1 sp1), SelectedCall (CallInstantiation cl2 sp2))
        | cl1 /= cl2 -> panic "BUG: Incompatible function classes"  [] -- [showPP cl1, showPP cl2]
        | otherwise  -> SelectedCall (CallInstantiation cl1 (merge sp1 sp2))
      (SelectedDo l1 r1, SelectedDo l2 r2) -> SelectedDo (merge l1 l2) (merge r1 r2)
      (SelectedLoop lp, SelectedLoop lp') -> SelectedLoop (merge lp lp')
      _ -> panic "BUG: merging non-mergeable nodes" []

instance ( Functor ch, PP (ch Doc)
         , Functor ca, PP (ca Doc)
         , Functor fn, PP (fn Doc)
         , Functor lp, PP (lp Doc)         
         , PP a) => PP ( SelectedPathF ch ca fn lp a ) where
  ppPrec n p =
    case p of
      SelectedHole       -> "□"
      SelectedBytes _ bs -> pp bs
      SelectedDo {}      -> "do" <+> ppStmts' p
      SelectedChoice ch ->
          wrapIf (n > 0) $ "choice" <+> pp (pp <$> ch)
      SelectedCase   cs -> wrapIf (n > 0) $ "case" <+> ppPrec 1 (pp <$> cs)
      SelectedCall   fn -> pp (pp <$> fn)
      SelectedLoop l -> pp (pp <$> l)

instance PP a => PP (CallInstantiation a) where
  ppPrec n (CallInstantiation fid sp) = wrapIf (n > 0) $ ("call" <> parens (pp fid)) <+> pp sp

instance PP a => PP (PathIndex a) where
  pp (PathIndex i p) = pp i <> "@" <> pp p

instance PP a => PP (SelectedLoopF a) where
  pp l =
    case l of
      SelectedLoopPool tag canBeNull els ->
        "(SelectedLoopPool@" <> pp tag <> ")"
        <> (if canBeNull then "*" else "+")
        <> bullets (map pp els)
      SelectedLoopElements _ltag els ->
        "LoopEls" <> brackets (sep (punctuate ", " (map pp els)))

instance PP PathCursorElement where
  pp el =
    case el of
      PCDoLeft     -> "L"
      PCDoRight    -> "R"
      PCSequence i -> brackets (pp i)
      PCLoopPool ltag -> brackets ("?" <> pp ltag)

instance PP a => PP (SelectedLoopPoolF a) where
  pp slp =
    bullets (map ppOne (smPaths slp))
    where
      ppOne (gen, deps) =
        hang (pp gen) 4 (bullets (map pp deps))

instance PP PathContextElement where
  pp el =
    case el of
      PCEDoRHS p -> "[R]" <> pp p
      PCELoopBody i ps ->
        hang (brackets ("L" <> pp i)) 4 (bullets (map pp ps))

ppPathContext :: PathContext -> Doc
ppPathContext = bullets . map pp

ppPathCursor :: PathCursor -> Doc
ppPathCursor = hcat . map pp

ppStmts' :: ( Functor ch, PP (ch Doc)
            , Functor ca, PP (ca Doc)
            , Functor fn, PP (fn Doc)
            , Functor lp, PP (lp Doc)
            , PP a) => SelectedPathF ch ca fn lp a -> Doc
ppStmts' p =
  case p of
    SelectedDo g1 g2 -> pp g1 $$ ppStmts' g2
    _                -> pp p

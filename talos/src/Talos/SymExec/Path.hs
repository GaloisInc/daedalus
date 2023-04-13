{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, ParallelListComp #-}

-- Path set analysis

module Talos.SymExec.Path where

import Control.Lens (ix, (%~))
import           Control.DeepSeq       (NFData)
import           Data.ByteString       (ByteString)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Map              (Map)
import           GHC.Generics          (Generic)

import           Daedalus.PP
import           Daedalus.Panic

import           Talos.Analysis.Merge  (Merge (..))
import           Talos.Analysis.Slice  (FInstId)
import Data.List (transpose)
import qualified Data.Map as Map
import Data.Function (on, (&))

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

data SelectedPathF ch ca lp a = 
    SelectedHole
  | SelectedBytes ProvenanceTag a
  --  | Fail ErrorSource Type (Maybe Expr)
  | SelectedDo (SelectedPathF ch ca lp a) (SelectedPathF ch ca lp a)
  | SelectedChoice (ch (SelectedPathF ch ca lp a))
  | SelectedCall FInstId (SelectedPathF ch ca lp a)
  | SelectedCase (ca (SelectedPathF ch ca lp a))
  | SelectedLoop (lp (SelectedPathF ch ca lp a))
  deriving (Functor, Foldable, Traversable, Generic)

data PathIndex a  = PathIndex { pathIndex :: Int, pathIndexPath :: a }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)

type SelectedPath = SelectedPathF PathIndex Identity SelectedLoopF ByteString 

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
    SelectedLoopPool     Bool [SelectedLoopPoolF a]
  | SelectedLoopElements [a]
  -- ^ Elements of a sequence based upon another sequence.  The Ints
  -- are the indicies.
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
  deriving (Eq, Ord, Show, Generic, NFData)
  
-- | THis is a path inside a SelectedPath to a particlar node, c.f. Zippers
type PathCursor = [PathCursorElement]

-- This should target a hole in a slice.
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
        (SelectedCall fid sp', _)          -> SelectedCall fid (go pc' sp')
        (SelectedCase sp', _)              -> SelectedCase (go pc' <$> sp')
        (SelectedDo l r, PCDoLeft : rest)  -> SelectedDo (go rest l) r
        (SelectedDo l r, PCDoRight : rest) -> SelectedDo l (go rest r)
        (SelectedDo {}, _)                 -> badPath        
        (SelectedLoop {}, [])              -> badPath
        (SelectedLoop (SelectedLoopElements els), PCSequence i : rest)
          | i < length els -> SelectedLoop (SelectedLoopElements (els & ix i %~ go rest))
        (SelectedLoop _, _) -> badPath

    badPath = panic "Unexpected path element" []

-- | Given the generator for a loop element, and a user, this will
-- determine the target of the cursor _with respect to_ the generating
-- loop.
relitiviseCursors :: PathCursor -> PathCursor -> (Int, PathCursor)
relitiviseCursors gen use = (length (filter isContextPC gen'), use')
  where
    isContextPC PCDoLeft = True
    isContextPC PCDoRight = False -- Doesn't generate a context element
    isContextPC PCSequence {} = True
    
    (gen', use') = unzip (dropWhile (uncurry (==)) (zip gen use))

-- -----------------------------------------------------------------------------
-- SelectedMany

fillManyTargets :: SelectedLoopPool -> Map Int [(PathCursor, SelectedPath)]
fillManyTargets sm = Map.fromListWith (<>) els
  where
    els = [ (depth, [(pc , SelectedLoop (SelectedLoopElements sps))])
          | (depth, pc) <- smCursors sm
          | sps <- transpose $ map snd (smPaths sm)
          ]
    
-- | This handles the lazy merge of Many elements.  This allows
-- decoupling e.g. the count of elements from the synthesis of the
-- elements. In practice (until we stop squashing loop bodies) this
-- will be applied to singleton lists.
selectedMany :: [SelectedLoopPool] -> ([SelectedPath], Map Int [(PathCursor, SelectedPath)])
selectedMany ms
  | not (same (map (length . smPaths) ms)) = panic "BUG: selectedMany needs to agree on count" []
  | otherwise = (mps, tgts)
  where
    same [] = True
    same (x : xs) = all (== x) xs

    -- Merge the elements
    mps  = map (foldl1 merge) $ transpose $ map (map fst . smPaths) ms
    tgts = Map.unionsWith merge (map fillManyTargets ms)

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
applyManyTargets :: Map Int [(PathCursor, SelectedPath)] -> PathContext -> PathContext
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
      (SelectedLoopPool m_selection pool, SelectedLoopPool m_selection' pool')
        | Just False <- ((==) `on` length) <$> m_selection <*> m_selection' -> 
          panic "BUG: mismatched SelectedLoopPool lengths" []
        | otherwise ->
          SelectedLoopPool (merge m_selection m_selection') (pool ++ pool')
      (SelectedLoopElements els, SelectedLoopElements els') ->
        SelectedLoopElements (Map.unionWith merge els els')
      _ -> panic "BUG: mismatched structure for merging SelectedLoop" []
      
-- FIXME: too general probably
instance Merge (SelectedPathF PathIndex Identity SelectedLoopF a) where
  merge psL psR =
    case (psL, psR) of
      (SelectedHole, _) -> psR
      (_, SelectedHole) -> psL
      (SelectedChoice (PathIndex n1 sp1), SelectedChoice (PathIndex n2 sp2))
        | n1 /= n2  -> panic "BUG: Incompatible paths selected in merge" [show n1, show n2]
        | otherwise -> SelectedChoice (PathIndex n1 (merge sp1 sp2))
      (SelectedCase (Identity sp1), SelectedCase (Identity sp2))
        -> SelectedCase (Identity (merge sp1 sp2))
      (SelectedCall cl1 sp1, SelectedCall cl2 sp2)
        | cl1 /= cl2 -> panic "BUG: Incompatible function classes"  [] -- [showPP cl1, showPP cl2]
        | otherwise  -> SelectedCall cl1 (merge sp1 sp2)
      (SelectedDo l1 r1, SelectedDo l2 r2) -> SelectedDo (merge l1 l2) (merge r1 r2)
      (SelectedLoop lp, SelectedLoop lp') -> SelectedLoop (merge lp lp')
      _ -> panic "BUG: merging non-mergeable nodes" []

instance ( Functor ch, PP (ch Doc)
         , Functor ca, PP (ca Doc)
         , Functor lp, PP (lp Doc)
         , PP a) => PP ( SelectedPathF ch ca lp a ) where
  ppPrec n p = 
    case p of
      SelectedHole       -> "□"
      SelectedBytes _ bs -> pp bs
      SelectedDo {}      -> "do" <+> ppStmts' p
      SelectedChoice ch ->
          wrapIf (n > 0) $ "choice" <+> pp (pp <$> ch)
      SelectedCase   cs -> wrapIf (n > 0) $ "case" <+> ppPrec 1 (pp <$> cs)
      SelectedCall   fid sp  -> wrapIf (n > 0) $ ("call" <> parens (pp fid)) <+> ppPrec 1 sp
      SelectedLoop l -> pp (pp <$> l)

instance PP a => PP (SelectedLoopF a) where
  pp l =
    case l of
      SelectedLoopPool _m_structural _els -> "SelectedLoopPool" -- FIXME
      SelectedLoopElements els ->
        brackets (sep (punctuate ", " (map ppEl (Map.toList els))))
        where
          ppEl (i, el) = pp i <> ": " <> pp el
  
ppStmts' :: ( Functor ch, PP (ch Doc)
            , Functor ca, PP (ca Doc)
            , Functor lp, PP (lp Doc)
            , PP a) => SelectedPathF ch ca lp a -> Doc
ppStmts' p =
  case p of
    SelectedDo g1 g2 -> pp g1 $$ ppStmts' g2
    _                -> pp p

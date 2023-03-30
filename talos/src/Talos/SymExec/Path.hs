{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, ParallelListComp #-}

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
import           Talos.Analysis.Slice  (FInstId)
import Data.List (transpose)
import qualified Data.Map as Map

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

data SelectedManyF ch ca a = SelectedManyF
  {
    -- | The possible paths in a Many, one list entry per Many
    -- iteration.  The second element is the following grammar nodes
    -- that depend on the Many.
    smPaths   :: [ (SelectedPathF ch ca a, [SelectedPathF ch ca a]) ]
    -- | The cursors for the subsequent nodes, depth is for Do nodes
    -- only, and we have on entry here for each element in the second
    -- entry of smPaths.
  , smCursors :: [ (Int, PathCursor) ]
  }
  deriving (Functor, Foldable, Traversable, Generic)

data SelectedPathF ch ca a = 
    SelectedHole
    -- | Placeholder for cursors, shouldn't really appear when we see
    -- the grammar node.
  | SelectedNode (SelectedPathF ch ca a)
  | SelectedBytes ProvenanceTag a
  --  | Fail ErrorSource Type (Maybe Expr)
  | SelectedDo (SelectedPathF ch ca a) (SelectedPathF ch ca a)
  | SelectedChoice (ch (SelectedPathF ch ca a))
  | SelectedCall FInstId (SelectedPathF ch ca a)
  | SelectedCase (ca (SelectedPathF ch ca a))
  -- | For Many nodes, we have one element for each merged node (i.e.,
  -- slice) --- merging two of these nodes just appends the lists.
  -- The first argument is the (possible) size of the generated list,
  -- if it is known, the second argument can be any size (i.e. we
  -- should pick a random number until we get the count we want).
  | SelectedMany (Maybe [SelectedPathF ch ca a]) [SelectedManyF ch ca a]
  | SelectedLoop (Map Int (SelectedPathF ch ca a))
  -- ^ Elements of a sequence based upon another sequence.  The Ints
  -- are the indicies.
  deriving (Functor, Foldable, Traversable, Generic)

data PathIndex a  = PathIndex { pathIndex :: Int, pathIndexPath :: a }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)

type SelectedPath = SelectedPathF PathIndex Identity ByteString
type SelectedMany = SelectedManyF PathIndex Identity ByteString

deriving instance NFData SelectedPath
deriving instance NFData SelectedMany

-- -----------------------------------------------------------------------------
-- Cursors
--
--  A cursor is an index to a particular node, used to update loop elements.

-- | A 'PathCursorElement' tells how to navigate a 'SelectedPathF'.
-- See also 'PathContextElement'.
data PathCursorElement =
  PCDoLeft
  | PCDoRight
  | PCSequence Int
  -- ^ An element of a sequence, useful for indexing.  We cannot just
  -- use PCNode here as we may need a particular element.
  | PCNode
  deriving (Eq, Ord, Show, Generic, NFData)
  
-- | THis is a path inside a SelectedPath to a particlar node, c.f. Zippers
type PathCursor = [PathCursorElement]

fillCursorTarget :: PathCursor -> [SelectedPath] -> SelectedPath
fillCursorTarget pc els = go pc
  where
    -- This is a bit gross?
    go [] = SelectedLoop (Map.fromList $ zip [0..] els)
    go (el : rest) =
      case el of
        PCDoLeft     -> SelectedDo (go rest) SelectedHole
        PCDoRight    -> SelectedDo SelectedHole (go rest)
        PCSequence i -> SelectedLoop (Map.singleton i (go rest))
        PCNode       -> SelectedNode (go rest)

relitiviseCursors :: PathCursor -> PathCursor -> (Int, PathCursor)
relitiviseCursors gen use = (length (filter isContextPC gen'), use')
  where
    isContextPC PCDoLeft = True
    isContextPC PCDoRight = False -- Doesn't generate a context element
    isContextPC PCSequence {} = True
    isContextPC PCNode = False
    
    (gen', use') = unzip (dropWhile (uncurry (==)) (zip gen use))

-- -----------------------------------------------------------------------------
-- SelectedMany

fillManyTargets :: SelectedMany -> Map Int SelectedPath
fillManyTargets sm = Map.fromListWith merge els
  where
    els = [ (depth, fillCursorTarget pc sps)
          | (depth, pc) <- smCursors sm
          | sps <- transpose $ map snd (smPaths sm)
          ]
    
-- | This handles the lazy merge of Many elements.  This allows
-- decoupling e.g. the count of elements from the synthesis of the elements. 
selectedMany :: [SelectedMany] -> ([SelectedPath], Map Int SelectedPath)
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

-- | Refines the RHS continuations based on the results of a call to selectedMany    
applyManyTargets :: Map Int SelectedPath -> PathContext -> PathContext
applyManyTargets tgts = go 0 (Map.toList tgts)
  where
    go _here [] stack = stack
    go here ((depth, p) : rest) (frame : stack)
      | depth == here =
        let newFrame = case (frame, p) of
              -- In this case we can merge directly              
              (PCEDoRHS p', _) -> PCEDoRHS $ merge p p'
              -- ... while here we need to inspect the path to update
              -- the right elements
              (PCELoopBody i ps, SelectedLoop m) ->
                PCELoopBody i $ goLoop i m ps
              _ -> panic "BUG: mismatched context/path" []
        in newFrame : go (here + 1) rest stack
    go here ps (frame : stack) = frame : go (here + 1) ps stack
    go _    (_ : _) [] = panic "BUG: mismatched Many target/Do stack" []

    -- We may need to update multiple loop paths.  Note that the
    -- indicies in m should all be > i.
    goLoop i m = zipWith (upd m) [i+1 ..]
    upd m j p
      | Just p' <- Map.lookup j m = merge p p'
      | otherwise = p
                         
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

-- FIXME: too general probably
instance Merge (SelectedPathF PathIndex Identity a) where
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
      (SelectedMany m_structural pool, SelectedMany m_structural' pool')
        | Just structural <- m_structural, Just structural' <- m_structural',
          length structural /= length structural' ->
          panic "BUG: mismatched Many lengths" []
        | otherwise -> SelectedMany (merge m_structural m_structural') (pool ++ pool')
      (SelectedLoop els, SelectedLoop els') -> SelectedLoop (Map.unionWith merge els els')
      _ -> panic "BUG: merging non-mergeable nodes" []

instance ( Functor ch, PP (ch Doc)
         , Functor ca, PP (ca Doc)
         , PP a) => PP ( SelectedPathF ch ca a ) where
  ppPrec n p = 
    case p of
      SelectedHole       -> "□"
      SelectedNode p'    -> "<<" <> pp p' <> ">>"
      SelectedBytes _ bs -> pp bs
      SelectedDo {}      -> "do" <+> ppStmts' p
      SelectedChoice ch ->
          wrapIf (n > 0) $ "choice" <+> pp (pp <$> ch)
      SelectedCase   cs -> wrapIf (n > 0) $ "case" <+> ppPrec 1 (pp <$> cs)
      SelectedCall   fid sp  -> wrapIf (n > 0) $ ("call" <> parens (pp fid)) <+> ppPrec 1 sp
      SelectedMany _m_structural _els -> "SelectedMany" -- FIXME
      SelectedLoop els -> brackets (sep (punctuate ", " (map ppEl (Map.toList els))))
        where
          ppEl (i, el) = pp i <> ": " <> pp el

ppStmts' :: ( Functor ch, PP (ch Doc)
            , Functor ca, PP (ca Doc)
            , PP a) => SelectedPathF ch ca a -> Doc
ppStmts' p =
  case p of
    SelectedDo g1 g2 -> pp g1 $$ ppStmts' g2
    _                -> pp p

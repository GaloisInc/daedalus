{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.ReadFrontier where

import qualified Data.List             as List
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Daedalus.Core
import           Daedalus.Core.CFG
import           Daedalus.PP
import           Talos.Polyglot.Util
import           Talos.Polyglot.PolyglotReader

-- | Maps each stream read location to the set of sets of stream read locations
-- downstream from it.  Each set corresponds to an execution path.
type ReadFrontier = Map NodeID ReadFrontierRange
type ReadFrontierRange = Set (Set NodeID)

ppReadFrontier :: ReadFrontier -> Doc
ppReadFrontier rf = vcat $ map doRow (Map.toList rf)
  where
    doRow (nodeID, sets) = pp nodeID <+> text "->" <+> ppSets sets

readFrontierContains :: ReadFrontier -> NodeID -> NodeID -> Bool
readFrontierContains rf id1 id2 = not $ Map.null filteredMap
  where
    filteredMap = Map.filterWithKey (\k sets -> k == id1 && Set.member id2 (Set.unions sets)) rf

mergeReadFrontier :: ReadFrontier -> ReadFrontier -> ReadFrontier
mergeReadFrontier = mapUnion Set.union

-- Add `nodeID` as a dependency to nodes in the read frontier.  If `nodeID` is
-- not in the read frontier, add it with an empty set.
extendReadFrontier :: ReadFrontier -> NodeID -> ReadFrontier
extendReadFrontier rf nodeID =
  let rf' = Map.map (extendReadFrontierRange nodeID) rf in
  if Map.member nodeID rf' then
     rf'
  else
     Map.insert nodeID Set.empty rf'

-- Add `nodeID` to all existing sets and add a singleton set containing `nodeID`.
extendReadFrontierRange :: NodeID -> ReadFrontierRange -> ReadFrontierRange
extendReadFrontierRange nodeID rfr = Set.union extendedSets singletonRFR
  where
    extendedSets = Set.map (Set.insert nodeID) rfr
    singletonRFR = Set.singleton (Set.singleton nodeID)

-- | Updates the read frontier to reflect that `nodeID` is bound.  That is, removes
-- all sets containing nodeID.
applyBound :: ReadFrontier -> NodeID -> ReadFrontier
applyBound state nodeID = Map.map filterSets state
  where
    filterSets :: Set (Set NodeID) -> Set (Set NodeID)
    filterSets sets = Set.filter (\s -> Set.member nodeID s) sets

-- | Updates the read frontier to reflect that all node IDs are bound.  That is,
-- removes all sets overlapping with `nodeIDs`.
applyBounds :: ReadFrontier -> Set NodeID -> ReadFrontier
applyBounds rf nodeIDs = Map.map filterSets rf
  where
    filterSets = Set.filter (Set.disjoint nodeIDs)

-- | Adds bounds to the read frontier for `nodeID` from `pat`, if any.  Constraints
-- are induced from matching literal values but not constructors.
addBoundsFromPattern :: ReadFrontier -> NodeID -> Pattern -> ReadFrontier
addBoundsFromPattern state nodeID (PBool _)   = applyBound state nodeID
addBoundsFromPattern state nodeID (PNum _)    = applyBound state nodeID
addBoundsFromPattern state nodeID (PBytes _)  = applyBound state nodeID
addBoundsFromPattern state _ _             = state

-- | Whether a Match induces bounds on a read location.
inducesBounds :: Match -> PolyglotReader Bool
inducesBounds (MatchByte byteSet) = isByteSetBound byteSet

-- MatchBytes is bounded except in pathological cases (I think).
-- TODO(cns): Handle pathological cases.
inducesBounds (MatchBytes expr) = return True

inducesBounds MatchEnd = return False

isByteSetBound :: ByteSet -> PolyglotReader Bool
isByteSetBound SetAny = return False
isByteSetBound (SetSingle _) = return True
isByteSetBound (SetRange _ _) = return False
isByteSetBound (SetComplement bset) = do
  res <- isByteSetBound bset
  return $ not res

-- Union is bound if both sides are bound.
isByteSetBound (SetUnion left right) = do
  leftRes  <- isByteSetBound left
  rightRes <- isByteSetBound right
  return $ leftRes && rightRes

-- Intersection is bound if either side is bound.
isByteSetBound (SetIntersection left right) = do
  leftRes  <- isByteSetBound left
  rightRes <- isByteSetBound right
  return $ leftRes || rightRes

isByteSetBound (SetLet _ _ byteSet) = do
  isByteSetBound byteSet

isByteSetBound (SetCall fname _) = do
  bfun <- getBFun fname
  case (fDef bfun) of
    -- Be conservative and assume the external function admits a cavity.
    External -> return False
    Def bset -> do
      isByteSetBound bset

isByteSetBound (SetCase Case{..}) = do
  caseResults <- sequence $ map (isByteSetBound . snd) casePats
  return $ List.or caseResults
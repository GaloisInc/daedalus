{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Talos.Strategy.PathSymbolic.PathSet
  ( -- * Path Variables
    PathVar(..)
  , pathVarToSExpr
  , pathVarToSMTVar
  -- * Loops
  , LoopCountVar(..)
  -- , LoopCountConstraint(..)
  , loopCountVarSort
  , loopCountToSExpr
  , loopCountVarToSExpr
  , loopCountVarToSMTVar
  -- * Constraint Constructors
  , choiceConstraint
  , indexConstraint
  , loopCountGtConstraint
  , loopCountGeqConstraint
  , loopCountEqConstraint
  
  -- -- * Path condition type
  -- , PathConditionInfo(..)
  -- , PathCondition(..)
  -- , ValuePathConstraint(..)
  -- -- * Operations
  -- , insertValue, insertChoice, insertLoopCount
  -- * Predicates
  -- , isInfeasible
  -- , isFeasibleMaybe
  , isTrivial
  -- -- * Semantics
  -- , vpcSatisfied, lccSatisfied
  -- * Converstion to SExpr
  , toSExpr
  -- * Path Sets
  , PathSet
  , trivialPathSet
  , conjPathSet
  , disjPathSet
  , disjPathSets
  -- * Helpers
  , andMany -- FIXME: move
  , orMany
  ) where

import           Control.Monad                (guard)
import           Data.Functor                 (($>))
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NE
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Map.Merge.Strict        as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           GHC.Generics                 (Generic)
import qualified SimpleSMT                    as S
import           SimpleSMT                    (SExpr, bvHex, tBits)

import           Daedalus.Core                (Pattern)
import           Daedalus.PP

import           Talos.Solver.SolverT         (SMTVar)
import Data.Foldable (toList)

newtype PathVar = PathVar { getPathVar :: SMTVar }
  deriving (Eq, Ord, Show)

newtype LoopCountVar = LoopCountVar { getLoopCountVar :: SMTVar }
  deriving (Eq, Ord, Show)

-- ----------------------------------------------------------------------------------------
-- Path conditions


pathVarToSExpr :: PathVar -> SExpr
pathVarToSExpr (PathVar v) = S.const v

pathVarToSMTVar :: PathVar -> SMTVar
pathVarToSMTVar (PathVar v) = v

loopCountVarToSExpr :: LoopCountVar -> SExpr
loopCountVarToSExpr = S.const . getLoopCountVar

loopCountVarToSMTVar :: LoopCountVar -> SMTVar
loopCountVarToSMTVar = getLoopCountVar

-- This allows the var to be used for the length.
loopCountVarSort :: SExpr
loopCountVarSort = tBits 64

loopCountToSExpr :: Int -> SExpr
loopCountToSExpr = bvHex 64 . fromIntegral

-- | Subsumes case variables and general program variables (where
-- Pattern should probably be a constant, not a ctor)
data ValuePathConstraint =
  VPCPositive Pattern
  | VPCNegative (Set Pattern)
  deriving (Show, Eq, Ord, Generic)

data LoopCountConstraint = LCCEq Int | LCCGt Int
  deriving (Show, Eq, Ord, Generic)

data PathCondition = PathCondition
  { pcChoices :: Map PathVar Int
    -- ^ We call these out separately as it is easy to figure out if the
    -- conjunction of two guards is unsat.  Wheras pcSymbolicIndices is
    -- probably not going to clash, this may./

    -- Side conditions
  , pcSymbolicIndices :: Map SMTVar Int -- Side condition    
  , pcLoops   :: Map LoopCountVar LoopCountConstraint
  } deriving (Show, Eq, Ord, Generic)

trivialPathCondition :: PathCondition
trivialPathCondition = PathCondition mempty mempty mempty

trivialPathSet :: PathSet
trivialPathSet = PathSet (NE.singleton trivialPathCondition)

-- data PathCondition =
--   FeasibleMaybe PathConditionInfo
--   | Infeasible
--   deriving (Show, Eq, Ord)

isTrivial :: PathSet -> Bool
isTrivial = (==) trivialPathSet

-- conjValues :: Typed ValuePathConstraint -> Typed ValuePathConstraint ->
--               Maybe (Typed ValuePathConstraint)
-- conjValues (Typed ty ci1) (Typed _ ci2) =
--   Typed ty <$> case (ci1, ci2) of
--     (VPCPositive p1, VPCPositive p2) -> guard (p1 == p2) $> ci1
--     (VPCPositive p, VPCNegative nps) -> guard (p `Set.notMember` nps) $> ci1
--     (VPCNegative nps, VPCPositive p) -> guard (p `Set.notMember` nps) $> ci2
--     (VPCNegative nps1, VPCNegative nps2) -> Just (VPCNegative (Set.union nps1 nps2))

conjLoops :: LoopCountConstraint -> LoopCountConstraint ->
             Maybe LoopCountConstraint
conjLoops lc1 lc2 =
  case (lc1, lc2) of
    (LCCEq n1, LCCEq  n2) -> guard (n1 == n2) $> lc1
    (LCCEq n1, LCCGt n2)  -> guard (n1 >  n2) $> lc1
    (LCCGt n1, LCCGt n2)  -> Just (LCCGt (max n1 n2))
    _ -> conjLoops lc2 lc1

-- | Conjoins two guards, returning Nothing if the result is unsat.
-- This is an optimisation, so an unsat guards may still be returned.
conjPathCondition :: PathCondition -> PathCondition -> Maybe PathCondition
conjPathCondition pc1 pc2 =
  PathCondition <$> joinMap (\x y -> guard (x == y) $> x) (pcChoices pc1) (pcChoices pc2)
                <*> joinMap (\x y -> guard (x == y) $> x) (pcSymbolicIndices pc1) (pcSymbolicIndices pc2)
                <*> joinMap conjLoops (pcLoops pc1)   (pcLoops pc2)    
  where
    joinMap f =
      Map.mergeA Map.preserveMissing Map.preserveMissing
         (Map.zipWithAMatched (const f))

indexConstraint :: SMTVar -> Int -> PathSet
indexConstraint v i = PathSet (NE.singleton pc)
  where
    pc = trivialPathCondition { pcSymbolicIndices = Map.singleton v i }

choiceConstraint :: PathVar -> Int -> PathSet
choiceConstraint pv i = PathSet (NE.singleton pc)
  where
    pc = trivialPathCondition { pcChoices = Map.singleton pv i }

loopCountEqConstraint :: LoopCountVar -> Int -> PathSet
loopCountEqConstraint lcv i = PathSet (NE.singleton pc)
  where
    pc = trivialPathCondition { pcLoops = Map.singleton lcv (LCCEq i) }

loopCountGtConstraint :: LoopCountVar -> Int -> PathSet
loopCountGtConstraint lcv i = PathSet (NE.singleton pc)
  where
    pc = trivialPathCondition { pcLoops = Map.singleton lcv (LCCGt i) }

loopCountGeqConstraint :: LoopCountVar -> Int -> PathSet
loopCountGeqConstraint lcv i
  | i == 0    = trivialPathSet -- lccs are unsigned
  | otherwise = loopCountGtConstraint lcv (i - 1)

-- insertValue :: SMTVar -> Typed ValuePathConstraint->
--                PathCondition -> PathCondition
-- insertValue v els = (<>) (FeasibleMaybe (PathConditionInfo (Map.singleton v els) mempty mempty))

-- insertChoice :: PathVar -> Int ->
--                 PathCondition -> PathCondition
-- insertChoice v el = (<>) (FeasibleMaybe (PathConditionInfo mempty (Map.singleton v el) mempty))

-- insertLoopCount :: LoopCountVar -> LoopCountConstraint -> 
--                    PathCondition -> PathCondition
-- insertLoopCount v el = (<>) (FeasibleMaybe (PathConditionInfo mempty mempty (Map.singleton v el)))

-- ----------------------------------------------------------------------------------------
-- Path sets

-- A path set is a disjunction of path conditions

-- FIXME: do we need to keep this in DNF?
newtype PathSet = PathSet { getPathSet :: NonEmpty PathCondition } -- Contains > 1 element
  deriving (Show, Eq, Ord)

-- | Returns Infeasible if the conjunction is unsatisfiable.
conjPathSet :: PathSet -> PathSet -> Maybe PathSet
conjPathSet (PathSet vs1) (PathSet vs2) = PathSet <$> NE.nonEmpty els
  where
    els = [ g | g1 <- toList vs1, g2 <- toList vs2, Just g <- [ g1 `conjPathCondition` g2 ]]

disjPathSet :: PathSet -> PathSet -> PathSet
disjPathSet (PathSet vs1) (PathSet vs2) = PathSet (vs1 <> vs2)

disjPathSets :: [PathSet] -> PathSet
disjPathSets = PathSet . foldl1 (<>) . map getPathSet

pathConditionToSExpr :: PathCondition -> SExpr
pathConditionToSExpr pc 
  | [el] <- allPreds = el
  | otherwise = S.andMany allPreds
  where
    allPreds = symIxs ++ choices ++ loops

    symIxs  = map (\(v, i) -> S.eq (S.const v) (S.int (fromIntegral i)))
                  (Map.toList (pcSymbolicIndices pc))
               
    choices = map choicePred (Map.toList (pcChoices pc))
    choicePred (pv, i) = S.eq (pathVarToSExpr pv) (S.int (fromIntegral i))
    
    loops = map lccPred (Map.toList (pcLoops pc))
    lccPred (v, lcc) =
      case lcc of
        LCCEq n  -> S.eq (loopCountVarToSExpr v) (loopCountToSExpr n)
        LCCGt n  -> S.bvULt (loopCountToSExpr n) (loopCountVarToSExpr v)

toSExpr :: PathSet -> SExpr
toSExpr (PathSet pcs) = orMany (map pathConditionToSExpr (toList pcs))

orMany :: [SExpr] -> SExpr
orMany [x] = x
orMany xs  = S.orMany xs

andMany :: [SExpr] -> SExpr
andMany [x] = x
andMany xs  = S.andMany xs

-- -----------------------------------------------------------------------------
-- Semantics

-- vpcSatisfied :: Typed ValuePathConstraint -> I.Value -> Bool
-- vpcSatisfied (Typed _ (VPCPositive p))  v = I.matches p v
-- vpcSatisfied (Typed _ (VPCNegative ps)) v =
--   not (any (flip I.matches v) (Set.toList ps))

-- lccSatisfied :: LoopCountConstraint -> Int -> Bool
-- lccSatisfied lcc i =
--   case lcc of
--     LCCGt j -> i > j
--     LCCEq j -> i == j

-- ------------------------------------------------------------------------------
-- Instances

-- instance Semigroup PathSet where
--   (<>) = disjPathSet

-- instance Monoid PathCondition where
--   mempty = PathCondition mempty mempty mempty

instance PP PathVar where
  pp = text . getPathVar

instance PP LoopCountVar where
  pp = text . getLoopCountVar

instance PP ValuePathConstraint where
  pp (VPCPositive c)   = "=" <+> pp c
  pp (VPCNegative cs)  = "∉" <+> block "{" "," "}" (pp <$> Set.toList cs)

instance PP PathCondition where
  pp pci | Map.null (pcChoices pci), Map.null (pcSymbolicIndices pci), Map.null (pcLoops pci) = "⊤"
  pp pci = "∧" <> block "{" ";" "}" (chEls ++ ixEls ++ loopEls)
    where
      chEls   = [ pp v <+> "=" <+> pp i | (v, i) <- Map.toList (pcChoices pci) ]
      ixEls   = [ text v <+> "=" <+> pp i | (v, i) <- Map.toList (pcSymbolicIndices pci) ]
      loopEls = [ pp v <+> pp c | (v, c) <- Map.toList (pcLoops pci) ]
        
instance PP PathSet where
  pp (PathSet pcs) = "⋁" <> block "(" ";" ")" (map pp (toList pcs))

instance PP LoopCountConstraint where
  pp (LCCEq  n) = "=" <+> pp n
  pp (LCCGt n)  = ">" <+> pp n
  

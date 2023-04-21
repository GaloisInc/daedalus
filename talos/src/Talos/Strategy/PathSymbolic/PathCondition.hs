{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Talos.Strategy.PathSymbolic.PathCondition
  ( -- * Path Variables
    PathVar(..)
  , pathVarToSExpr
  , pathVarToSMTVar
  -- * Loops
  , LoopCountVar(..)
  , LoopCountConstraint(..)
  , loopCountVarSort
  , loopCountToSExpr
  , loopCountVarToSExpr
  , loopCountVarToSMTVar  
  -- * Path condition type
  , PathConditionInfo(..)
  , PathCondition(..)
  , ValuePathConstraint(..)
  -- * Operations
  , insertValue, insertChoice, insertLoopCount
  -- * Predicates
  , isInfeasible
  , isFeasibleMaybe
  -- * Semantics
  , vpcSatisfied, lccSatisfied
  -- * Converstion to SExpr
  , toSExpr
  ) where

import           Control.Monad                (guard)
import           Data.Functor                 (($>))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Map.Merge.Strict        as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           GHC.Generics                 (Generic)
import           SimpleSMT                    (SExpr, bvHex, tBits)
import qualified SimpleSMT                    as S

import           Daedalus.Core                (Pattern, Typed (..))
import qualified Daedalus.Core.Semantics.Expr as I
import           Daedalus.PP
import qualified Daedalus.Value               as I

import qualified Talos.SymExec.Expr           as SE
import           Talos.SymExec.SolverT        (SMTVar)

newtype PathVar = PathVar { getPathVar :: SMTVar }
  deriving (Eq, Ord, Show)

newtype LoopCountVar = LoopCountVar { getLoopCountVar :: SMTVar }
  deriving (Eq, Ord, Show)


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
  deriving (Eq, Ord, Generic)

data LoopCountConstraint = LCCEq Int | LCCGt Int
  deriving (Eq, Ord, Generic)

data PathConditionInfo = PathConditionInfo
  { pcValues   :: Map SMTVar (Typed ValuePathConstraint)
  -- ^ Case match path conditions.
  , pcChoices :: Map PathVar Int
  -- ^ We call these out separately as it is easy to figure out if the
  -- conjunction of two guards is unsat.
  , pcLoops   :: Map LoopCountVar LoopCountConstraint
  } deriving (Eq, Ord, Generic)

data PathCondition =
  FeasibleMaybe PathConditionInfo
  | Infeasible
  deriving (Eq, Ord)

isInfeasible :: PathCondition -> Bool
isInfeasible = (==) Infeasible

isFeasibleMaybe :: PathCondition -> Bool
isFeasibleMaybe = (/=) Infeasible

conjValues :: Typed ValuePathConstraint -> Typed ValuePathConstraint ->
              Maybe (Typed ValuePathConstraint)
conjValues (Typed ty ci1) (Typed _ ci2) =
  Typed ty <$> case (ci1, ci2) of
    (VPCPositive p1, VPCPositive p2) -> guard (p1 == p2) $> ci1
    (VPCPositive p, VPCNegative nps) -> guard (p `Set.notMember` nps) $> ci1
    (VPCNegative nps, VPCPositive p) -> guard (p `Set.notMember` nps) $> ci2
    (VPCNegative nps1, VPCNegative nps2) -> Just (VPCNegative (Set.union nps1 nps2))

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
conjInfo :: PathConditionInfo -> PathConditionInfo -> Maybe PathConditionInfo
conjInfo pc1 pc2 =
  PathConditionInfo <$> joinMap conjValues (pcValues pc1) (pcValues pc2)
                    <*> joinMap (\x y -> guard (x == y) $> x) (pcChoices pc1) (pcChoices pc2)
                    <*> joinMap conjLoops (pcLoops pc1)   (pcLoops pc2)  
  where
    joinMap f =
      Map.mergeA Map.preserveMissing Map.preserveMissing
         (Map.zipWithAMatched (const f))
    

insertValue :: SMTVar -> Typed ValuePathConstraint->
               PathCondition -> PathCondition
insertValue v els = (<>) (FeasibleMaybe (PathConditionInfo (Map.singleton v els) mempty mempty))

insertChoice :: PathVar -> Int ->
                PathCondition -> PathCondition
insertChoice v el = (<>) (FeasibleMaybe (PathConditionInfo mempty (Map.singleton v el) mempty))

insertLoopCount :: LoopCountVar -> LoopCountConstraint -> 
                   PathCondition -> PathCondition
insertLoopCount v el = (<>) (FeasibleMaybe (PathConditionInfo mempty mempty (Map.singleton v el)))

toSExpr :: PathCondition -> SExpr
toSExpr Infeasible = S.bool False
toSExpr (FeasibleMaybe pc)
  | [el] <- allPreds = el
  | otherwise = S.andMany allPreds
  where
    allPreds = values ++ choices ++ loops
    
    values = concatMap valuePred (Map.toList (pcValues pc))
    valuePred (n, Typed ty v) =
      case v of
        VPCPositive p  -> [ SE.patternToPredicate ty p (S.const n) ]
        VPCNegative ps -> [ S.not (SE.patternToPredicate ty p (S.const n))
                          | p <- Set.toList ps
                          ]
             
    choices = map choicePred (Map.toList (pcChoices pc))
    choicePred (pv, i) = S.eq (pathVarToSExpr pv) (S.int (fromIntegral i))

    loops = map lccPred (Map.toList (pcLoops pc))
    lccPred (v, lcc) =
      case lcc of
        LCCEq n  -> S.eq (loopCountVarToSExpr v) (loopCountToSExpr n)
        LCCGt n  -> S.bvULt (loopCountToSExpr n) (loopCountVarToSExpr v)

-- -----------------------------------------------------------------------------
-- Semantics

vpcSatisfied :: Typed ValuePathConstraint -> I.Value -> Bool
vpcSatisfied (Typed _ (VPCPositive p))  v = I.matches p v
vpcSatisfied (Typed _ (VPCNegative ps)) v =
  not (any (flip I.matches v) (Set.toList ps))

lccSatisfied :: LoopCountConstraint -> Int -> Bool
lccSatisfied lcc i =
  case lcc of
    LCCGt j -> i > j
    LCCEq j -> i == j

-- ------------------------------------------------------------------------------
-- Instances

instance Semigroup PathCondition where
  Infeasible <> _ = Infeasible
  _ <> Infeasible = Infeasible
  (FeasibleMaybe pc1) <> (FeasibleMaybe pc2) =
    maybe Infeasible FeasibleMaybe (conjInfo pc1 pc2)

instance Monoid PathCondition where
  mempty = FeasibleMaybe (PathConditionInfo mempty mempty mempty)

instance PP PathVar where
  pp = text . getPathVar

instance PP LoopCountVar where
  pp = text . getLoopCountVar

instance PP ValuePathConstraint where
  pp (VPCPositive c)   = "=" <+> pp c
  pp (VPCNegative cs)  = "∉" <+> block "{" "," "}" (pp <$> Set.toList cs)

instance PP PathConditionInfo where
  pp pci | Map.null (pcChoices pci), Map.null (pcValues pci), Map.null (pcLoops pci) = "⊤"
  pp pci = "∧" <> block "{" ";" "}" (chEls ++ valueEls ++ loopEls)
    where
      chEls    = [ pp v <+> "=" <+> pp i | (v, i) <- Map.toList (pcChoices pci) ]
      valueEls = [ text v <+> pp c | (v, Typed _ c) <- Map.toList (pcValues pci) ]
      loopEls  = [ pp v <+> pp c | (v, c) <- Map.toList (pcLoops pci) ]
        
instance PP PathCondition where
  pp Infeasible = "⊥"
  pp (FeasibleMaybe pci) = pp pci

instance PP LoopCountConstraint where
  pp (LCCEq  n) = "=" <+> pp n
  pp (LCCGt n)  = ">" <+> pp n
  

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Talos.Strategy.PathCondition
  ( -- * Path Variables
    PathVar(..)
  , pathVarToSExpr
  -- * Path condition type
  , PathConditionInfo(..)
  , PathCondition(..)
  , PathConditionCaseInfo(..)
  -- * Operations
  , insertCase, insertChoice
  -- * Predicates
  , isInfeasible
  , isFeasibleMaybe
  -- * Semantics
  , pcciSatisfied
  -- * Converstion to SExpr
  , toSExpr
  ) where

import           Talos.SymExec.SolverT (SMTVar)
import SimpleSMT (SExpr)
import qualified SimpleSMT as S
import Daedalus.Core (Type, Pattern)
import Data.Set (Set)
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import Control.Monad (guard)
import Data.Functor (($>))
import qualified Talos.SymExec.Expr as SE
import qualified Daedalus.Value as I
import qualified Daedalus.Core.Semantics.Expr as I
import Daedalus.PP

newtype PathVar = PathVar { getPathVar :: SMTVar }
  deriving (Eq, Ord, Show)

pathVarToSExpr :: PathVar -> SExpr
pathVarToSExpr (PathVar v) = S.const v

data PathConditionCaseInfo = PathConditionCaseInfo
  { pcciType       :: Type
  , pcciConstraint :: Either Pattern (Set Pattern)
  } deriving (Eq, Ord, Generic)

data PathConditionInfo = PathConditionInfo
  { pcCases   :: Map SMTVar PathConditionCaseInfo
  -- ^ Case match path conditions.
  , pcChoices :: Map PathVar Int
  -- ^ We call these out separately as it is easy to figure out if the
  -- conjunction of two guards is unsat.
  } deriving (Eq, Ord, Generic)

data PathCondition =
  FeasibleMaybe PathConditionInfo
  | Infeasible
  deriving (Eq, Ord)

isInfeasible :: PathCondition -> Bool
isInfeasible = (==) Infeasible

isFeasibleMaybe :: PathCondition -> Bool
isFeasibleMaybe = (/=) Infeasible

conjCases :: PathConditionCaseInfo -> PathConditionCaseInfo ->
             Maybe PathConditionCaseInfo
conjCases ci1 ci2 =
  case (pcciConstraint ci1, pcciConstraint ci2) of
    (Left p1, Left p2)  -> guard (p1 == p2) $> ci1
    (Left p, Right nps) -> guard (p `Set.notMember` nps) $> ci1
    (Right nps, Left p) -> guard (p `Set.notMember` nps) $> ci2
    (Right nps1, Right nps2) -> Just (ci1 { pcciConstraint = Right (Set.union nps1 nps2) })

-- | Conjoins two guards, returning Nothing if the result is unsat.
-- This is an optimisation, so an unsat guards may still be returned.
conjInfo :: PathConditionInfo -> PathConditionInfo -> Maybe PathConditionInfo
conjInfo pc1 pc2 =
  PathConditionInfo <$> joinCases   (pcCases pc1) (pcCases pc2)
                    <*> joinChoices (pcChoices pc1) (pcChoices pc2)
  where
    joinCases =
      Map.mergeA Map.preserveMissing Map.preserveMissing
          (Map.zipWithAMatched (const conjCases))

    -- Copied from mergeUnifiers in MemoSearch.hs, should probably be a library thing.
    joinChoices =
      Map.mergeA Map.preserveMissing Map.preserveMissing
          (Map.zipWithAMatched (\_k x y -> guard (x == y) $> x))

insertCase :: SMTVar -> PathConditionCaseInfo ->
              PathCondition -> PathCondition
insertCase v els = (<>) (FeasibleMaybe (PathConditionInfo (Map.singleton v els) mempty))

insertChoice :: PathVar -> Int ->
                PathCondition -> PathCondition
insertChoice v el = (<>) (FeasibleMaybe (PathConditionInfo mempty (Map.singleton v el)))

toSExpr :: PathCondition -> SExpr
toSExpr Infeasible = S.bool False
toSExpr (FeasibleMaybe pc)
  | [el] <- allPreds = el
  | otherwise = S.andMany allPreds
  where
    notf b = if b then id else S.not
    allPreds = cases ++ choices
    cases = [ notf b (SE.patternToPredicate (pcciType pcci) p (S.const n))
            | (n, pcci) <- Map.toList (pcCases pc)
            , (b, p) <- either (\p -> [(True, p)])
                               (map ((,) False) . Set.toList)
                               (pcciConstraint pcci)
            ]
    choices = [ S.eq (pathVarToSExpr pv) (S.int (fromIntegral i))
              | (pv, i) <- Map.toList (pcChoices pc)
              ]


-- -----------------------------------------------------------------------------
-- Semantics

pcciSatisfied :: PathConditionCaseInfo -> I.Value -> Bool
pcciSatisfied PathConditionCaseInfo { pcciConstraint = Left p } v =
  I.matches p v
pcciSatisfied PathConditionCaseInfo { pcciConstraint = Right ps } v =
  not (any (flip I.matches v) (Set.toList ps))

-- ------------------------------------------------------------------------------
-- Instances

instance Semigroup PathCondition where
  Infeasible <> _ = Infeasible
  _ <> Infeasible = Infeasible
  (FeasibleMaybe pc1) <> (FeasibleMaybe pc2) =
    maybe Infeasible FeasibleMaybe (conjInfo pc1 pc2)

instance Monoid PathCondition where
  mempty = FeasibleMaybe (PathConditionInfo mempty mempty)

instance PP PathVar where
  pp = text . getPathVar

instance PP PathConditionCaseInfo where
  pp PathConditionCaseInfo { pcciConstraint = Left c }   = "=" <+> pp c
  pp PathConditionCaseInfo { pcciConstraint = Right cs } = "∉" <+> block "{" "," "}" (pp <$> Set.toList cs)

instance PP PathConditionInfo where
  pp pci | Map.null (pcChoices pci), Map.null (pcCases pci) = "⊤"
  pp pci = "∧" <> block "{" ";" "}" (chEls ++ caseEls)
    where
      chEls   = [ pp v <+> "=" <+> pp i | (v, i) <- Map.toList (pcChoices pci) ]
      caseEls = [ text v <+> pp c | (v, c) <- Map.toList (pcCases pci) ]

instance PP PathCondition where
  pp Infeasible = "⊥"
  pp (FeasibleMaybe pci) = pp pci

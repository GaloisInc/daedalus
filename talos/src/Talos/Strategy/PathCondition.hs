{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Talos.Strategy.PathCondition
  ( -- * Path Variables
    PathVar(..)
  , pathVarToSExpr
  -- * Path condition type
  , PathConditionInfo(..)
  , PathCondition(..)
  , PathConditionCaseInfo(..)
  -- * Operations
  , singletonCase, singletonChoice, singletonSExpr
  -- * Predicates
  , isInfeasible
  , isFeasibleMaybe
  -- * Semantics
  , pcciSatisfied
  -- * Converstion to SExpr
  , toSExpr
  , name
  ) where

import           Control.Monad                (guard)
import           Daedalus.Core                (Pattern, Type)
import qualified Daedalus.Core.Semantics.Expr as I
import           Daedalus.PP
import qualified Daedalus.Value               as I
import           Data.Functor                 (($>))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Map.Merge.Strict        as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           GHC.Generics                 (Generic)

import           SimpleSMT                    (SExpr)
import qualified SimpleSMT                    as S

import qualified Talos.SymExec.Expr           as SE
import           Talos.SymExec.SolverT        (SMTVar, defineSymbol, MonadSolver (..))
import Control.Lens (traverseOf)
import Data.Generics.Product (field)

newtype PathVar = PathVar { getPathVar :: SMTVar }
  deriving (Eq, Ord, Show)

pathVarToSExpr :: PathVar -> SExpr
pathVarToSExpr (PathVar v) = S.const v

data PathConditionCaseInfo = PathConditionCaseInfo
  { pcciType       :: Type
  , pcciConstraint :: Either Pattern (Set Pattern)
  } deriving (Eq, Ord, Generic)

data PathConditionInfo = PathConditionInfo
  { pcCases   :: !(Map SMTVar PathConditionCaseInfo)
  -- ^ Case match path conditions.
  , pcChoices :: !(Map PathVar Int)
  -- ^ We call these out separately as it is easy to figure out if the
  -- conjunction of two guards is unsat.
  , pcPred    :: !SExpr
  -- ^ The predicate for this path (the other fields can over-approximate)
  } deriving (Eq, Ord, Generic)

-- FIXME: use False in pcPred so we don't need the sum here
data PathCondition =
  FeasibleMaybe {-# UNPACK #-} !PathConditionInfo
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
                    <*> pure (S.and (pcPred pc1) (pcPred pc2))
  where
    joinCases =
      Map.mergeA Map.preserveMissing Map.preserveMissing
          (Map.zipWithAMatched (const conjCases))

    -- Copied from mergeUnifiers in MemoSearch.hs, should probably be a library thing.
    joinChoices =
      Map.mergeA Map.preserveMissing Map.preserveMissing
          (Map.zipWithAMatched (\_k x y -> guard (x == y) $> x))

singletonCase :: SMTVar -> PathConditionCaseInfo -> PathCondition
singletonCase v pcci = 
  FeasibleMaybe (PathConditionInfo (Map.singleton v pcci) mempty p)
  where
    p = S.andMany [ notf b (SE.patternToPredicate (pcciType pcci) p' (S.const v))
                  | (b, p') <- either (\q -> [(True, q)])
                               (map ((,) False) . Set.toList)
                               (pcciConstraint pcci)
                  ]
    notf b = if b then id else S.not

singletonChoice :: PathVar -> Int -> PathCondition
singletonChoice v i =
  FeasibleMaybe (PathConditionInfo mempty (Map.singleton v i)
                  (S.eq (pathVarToSExpr v) (S.int (fromIntegral i))))

singletonSExpr :: SExpr -> PathCondition
singletonSExpr p = FeasibleMaybe (PathConditionInfo mempty mempty p)

toSExpr :: PathCondition -> SExpr
toSExpr Infeasible = S.bool False
toSExpr (FeasibleMaybe pc) = pcPred pc

name :: MonadSolver m => PathCondition -> m PathCondition
name Infeasible         = pure Infeasible
name (FeasibleMaybe pc) =
  FeasibleMaybe <$> traverseOf (field @"pcPred") def pc
  where
    def = liftSolver . fmap S.const . defineSymbol "pc" S.tBool

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
  mempty = FeasibleMaybe (PathConditionInfo mempty mempty (S.bool True))

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

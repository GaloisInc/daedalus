{-# LANGUAGE DeriveGeneric #-}

module Talos.Strategy.PathSymbolic.Assertion
  ( Assertion(..)
  , toSExpr
  , trivial
  , entail
  , entailMany
  , true
  , false, 
  ) where

import           Data.Foldable                         (toList)
import           Data.List.NonEmpty                    (NonEmpty (..))
import           GHC.Generics                          (Generic)
import qualified SimpleSMT                             as SMT

import           Talos.Lib                             (andMany)
import qualified Talos.Strategy.PathSymbolic.Branching as B
import           Talos.Strategy.PathSymbolic.Branching (Branching)
import qualified Talos.Strategy.PathSymbolic.PathSet   as PS
import           Talos.Strategy.PathSymbolic.PathSet   (PathSet)

-- -----------------------------------------------------------------------------
-- Assertions

-- Should we do something smarter when we have entailment under a
-- Branching? We could split Entail into Conj and Implies and then
-- simplify Implies under Branching.
data Assertion =
  SExprAssert SMT.SExpr -- Try not to use as it is not that informative.
  | PSAssert PathSet
  | BoolAssert Bool
  | BAssert (Branching Assertion)
  | EntailAssert PathSet (NonEmpty Assertion) -- P |= AND assns
  deriving (Ord, Eq, Show, Generic)

toSExpr :: Assertion -> SMT.SExpr
toSExpr assn =
  case assn of
    SExprAssert s -> s
    PSAssert ps -> PS.toSExpr ps
    BoolAssert b -> SMT.bool b
    BAssert  b  -> B.toSExpr (toSExpr <$> b)
    EntailAssert ps assns
      | PS.trivial ps -> rhs
      | otherwise     -> PS.toSExpr ps `SMT.implies` rhs
      where rhs = andMany (map toSExpr (toList assns))
      
trivial :: Assertion -> Bool
trivial assn =
  case assn of
    SExprAssert s -> s == SMT.bool True
    PSAssert ps -> PS.trivial ps
    BoolAssert b -> b
    BAssert  b  -> all trivial b
    EntailAssert _ assns -> all trivial assns

instance Semigroup Assertion where
  -- Unit
  BoolAssert True <> assn = assn
  assn <> BoolAssert True = assn
  -- Absorb
  assn@(BoolAssert False) <> _ = assn
  _ <> assn@(BoolAssert False) = assn

  -- Entailment
  EntailAssert ps1 assns1 <> EntailAssert ps2 assns2
    | PS.trivial ps1, PS.trivial ps2 = EntailAssert PS.true (assns1 <> assns2)
  EntailAssert ps1 assns1 <> assn2
    | PS.trivial ps1 = EntailAssert PS.true (assn2 :| toList assns1)
  assn1 <> EntailAssert ps2 assns2
    | PS.trivial ps2 = EntailAssert PS.true (assn1 :| toList assns2)
  assn1 <> assn2 = EntailAssert PS.true (assn1 :| [assn2])

instance Monoid Assertion where mempty = BoolAssert True

entail :: PathSet -> Assertion -> Assertion
entail ps a
  | PS.trivial ps      = a
  | trivial a = mempty
  | EntailAssert ps' assns <- a = EntailAssert (ps `PS.conj` ps') assns
  | otherwise            = EntailAssert ps (a :| [])

entailMany :: PathSet -> NonEmpty Assertion -> Assertion
entailMany ps (a :| []) = entail ps a
entailMany ps assns     = EntailAssert ps assns

true, false :: Assertion
true = BoolAssert True
false = BoolAssert False


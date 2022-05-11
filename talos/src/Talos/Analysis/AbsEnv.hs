{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Talos.Analysis.AbsEnv where

import           Daedalus.Core         (ByteSet, Expr, Name, Case(..))
import           Data.Bifunctor        (first)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Proxy            (Proxy)

import           Daedalus.PP

import           Talos.Analysis.Eqv
import           Talos.Analysis.SLExpr (SLExpr)
import Daedalus.Panic (panic)

--------------------------------------------------------------------------------
-- Abstract Environments

class (Ord (AbsPred ae), Eqv ae, PP (AbsPred ae), PP ae) => AbsEnv ae where
  type AbsPred ae
  -- (\forall x. absProj x ae = Nothing) --> absNullEnv ae
  absNullEnv     :: ae -> Bool
  absPre         :: AbsPred ae -> Expr -> (ae, SLExpr)
  absGuard       :: Expr -> ae
  -- | This is used to refine the branches of case statements. The
  -- result is the abstract environment for the new case node.
  absCase        :: Case ae -> ae
  absByteSet     :: AbsPred ae -> ByteSet -> ae
  absProj        :: Name -> ae -> Maybe (ae, AbsPred ae)
  absInverse     :: Name -> Expr -> Expr -> ae
  absUnion       :: ae -> ae -> ae
  absEnvOverlaps :: ae -> ae -> Bool
  absSubstEnv    :: Map Name Name -> ae -> ae
  absTop         :: {- Set Name -> -} ae
  
data AbsEnvTy = forall ae. AbsEnv ae => AbsEnvTy (Proxy ae) 

-- Just treat the elements pointwise
newtype LiftAbsEnv p = LiftAbsEnv { getLiftAbsEnv :: Map Name p }

instance Eqv p => Eqv (LiftAbsEnv p) where
  eqv (LiftAbsEnv m1) (LiftAbsEnv m2) = eqv m1 m2

-- This is a convenience class so we can just define things at the
-- predicate level and lift them to maps.
class (Eqv p, Ord p, PP p) => AbsEnvPointwise p where
  absPredPre     :: p -> Expr -> (Map Name p, SLExpr)
  absPredGuard   :: Expr -> Map Name p
  absPredCase    :: Case (Maybe p) -> p
  absPredByteSet :: p -> ByteSet -> Map Name p
  absPredInverse :: Name -> Expr -> Expr -> Map Name p
  absPredUnion   :: p -> p -> p

instance AbsEnvPointwise p => AbsEnv (LiftAbsEnv p) where
  type AbsPred (LiftAbsEnv p) = p
  absNullEnv     = Map.null . getLiftAbsEnv
  absPre p e     = first LiftAbsEnv (absPredPre p e)
  absGuard       = LiftAbsEnv . absPredGuard
  absCase (Case _x [])     = panic "Empty case" []
  absCase c@(Case x _alts) =
    let x_p      = absPredCase (Map.lookup x . getLiftAbsEnv <$> c)
        res_no_x = foldl1 absUnion c
    in LiftAbsEnv (Map.insert x x_p (getLiftAbsEnv res_no_x))
    
  absByteSet p e = LiftAbsEnv (absPredByteSet p e)
  absProj n (LiftAbsEnv m) =
    let (m_r, m') = Map.updateLookupWithKey (\_ _ -> Nothing) n m
    in (,) (LiftAbsEnv m') <$> m_r
  absInverse n e e' = LiftAbsEnv (absPredInverse n e e')
  absUnion (LiftAbsEnv m1) (LiftAbsEnv m2) =
    LiftAbsEnv $ Map.unionWith absPredUnion m1 m2
  absEnvOverlaps (LiftAbsEnv m1) (LiftAbsEnv m2) = Map.disjoint m1 m2
  absSubstEnv subst (LiftAbsEnv m) = LiftAbsEnv $ Map.compose m subst
  absTop  = LiftAbsEnv Map.empty

instance PP p => PP (LiftAbsEnv p) where
  pp (LiftAbsEnv m) = brackets (commaSep (map go (Map.toList m)))
    where
      go (k, v) = pp k <+> "=>" <+> pp v
    

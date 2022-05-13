{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Talos.Analysis.AbsEnv where

import           Daedalus.Core         (ByteSet, Case (..), Expr, Name)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Proxy            (Proxy)

import           Daedalus.PP
import           Daedalus.Panic        (panic)

import           Talos.Analysis.Eqv
import           Talos.Analysis.SLExpr (SLExpr)

--------------------------------------------------------------------------------
-- Abstract Environments

class (Ord (AbsPred ae), Eqv ae, PP (AbsPred ae), PP ae, Semigroup ae) => AbsEnv ae where
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
  absEnvOverlaps :: ae -> ae -> Bool
  absSubstEnv    :: Map Name Name -> ae -> ae
  absTop         :: {- Set Name -> -} ae
  
data AbsEnvTy = forall ae. AbsEnv ae => AbsEnvTy (Proxy ae) 

-- Just treat the elements pointwise
newtype LiftAbsEnv p = LiftAbsEnv { getLiftAbsEnv :: Map Name p }
  deriving (Functor)

mapLiftAbsEnv :: (Map Name p -> Map Name p) -> (LiftAbsEnv p -> LiftAbsEnv p)
mapLiftAbsEnv f (LiftAbsEnv m) = LiftAbsEnv (f m)

instance Eqv p => Eqv (LiftAbsEnv p) where
  eqv (LiftAbsEnv m1) (LiftAbsEnv m2) = eqv m1 m2

-- This is a convenience class so we can just define things at the
-- predicate level and lift them to maps.
class (Eqv p, Ord p, PP p, Semigroup p) => AbsEnvPointwise p where
  absPredPre     :: p -> Expr -> (LiftAbsEnv p, SLExpr)
  absPredGuard   :: Expr -> LiftAbsEnv p
  absPredByteSet :: p -> ByteSet -> LiftAbsEnv p
  absPredInverse :: Name -> Expr -> Expr -> LiftAbsEnv p
  absPredTop     :: p -- Absorbing element wrt <>

instance AbsEnvPointwise p => AbsEnv (LiftAbsEnv p) where
  type AbsPred (LiftAbsEnv p) = p
  absNullEnv     = Map.null . getLiftAbsEnv
  absPre p e     = absPredPre p e
  absGuard       = absPredGuard
  absCase (Case _x [])     = panic "Empty case" []
  -- FIXME: this overapproximates by assuming we want all of x
  absCase c@(Case x _alts) =
    mapLiftAbsEnv (Map.insert x absPredTop) (foldl1 (<>) c)
    
  absByteSet p e = absPredByteSet p e
  absProj n (LiftAbsEnv m) =
    let (m_r, m') = Map.updateLookupWithKey (\_ _ -> Nothing) n m
    in (,) (LiftAbsEnv m') <$> m_r
  absInverse n e e' = absPredInverse n e e'
  absEnvOverlaps (LiftAbsEnv m1) (LiftAbsEnv m2) = Map.disjoint m1 m2
  absSubstEnv subst = mapLiftAbsEnv (Map.mapKeysWith (<>) keyf)
    where
      keyf k | Just k' <- Map.lookup k subst = k'
             | otherwise = panic "Missing key" [showPP k]
      
  absTop  = LiftAbsEnv Map.empty

instance PP p => PP (LiftAbsEnv p) where
  pp (LiftAbsEnv m) = brackets (commaSep (map go (Map.toList m)))
    where
      go (k, v) = pp k <+> "=>" <+> pp v

instance Semigroup p => Semigroup (LiftAbsEnv p) where
  LiftAbsEnv m <> LiftAbsEnv m' =   LiftAbsEnv $ Map.unionWith (<>) m m' 

instance Semigroup p => Monoid (LiftAbsEnv p) where
  mempty = LiftAbsEnv Map.empty
    

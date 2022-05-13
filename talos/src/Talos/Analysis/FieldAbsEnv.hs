{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Talos.Analysis.FieldAbsEnv where

import           Control.DeepSeq       (NFData)
import           Data.Bifunctor        (second)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Proxy            (Proxy (Proxy))
import           GHC.Generics          (Generic)

import           Daedalus.Core
import           Daedalus.Core.Type
import           Daedalus.PP

import           Talos.Analysis.AbsEnv
import           Talos.Analysis.Eqv    (Eqv)
import           Talos.Analysis.SLExpr (SLExpr (..), exprToSLExpr, slExprToExpr)

fieldAbsEnvTy :: AbsEnvTy
fieldAbsEnvTy = AbsEnvTy (Proxy @FieldAbsEnv)

data FieldProj = Whole | FieldProj (Map Label FieldProj)
  deriving (Ord, Eq, Show, Generic)

instance NFData FieldProj -- default
instance Eqv FieldProj -- default

newtype FieldAbsEnv = FieldAbsEnv (LiftAbsEnv FieldProj)
  deriving (Semigroup, Eqv, PP, AbsEnv)

instance AbsEnvPointwise FieldProj where
  absPredPre p e   =
    let sle = projectE p e
        -- We replace all holes by unit, which should be OK for
        -- getting the set of field projections.
        e'  = slExprToExpr (const unit) sle
    in (exprToAbsEnv e', sle)
  absPredGuard     = exprToAbsEnv
  absPredByteSet _ = byteSetToAbsEnv
  absPredTop       = Whole
  absPredInverse n e1 e2 =
    mapLiftAbsEnv (Map.delete n) (exprToAbsEnv e1 <> exprToAbsEnv e2)

explodeFieldProj :: FieldProj -> [ [Label] ]
explodeFieldProj Whole = [ [] ]
explodeFieldProj (FieldProj m) =
  [ l : ls | (l, fs') <- Map.toList m, ls <- explodeFieldProj fs' ]

instance PP FieldProj where
  pp Whole = "Whole"
  pp fs    = braces (commaSep (map (hcat . punctuate "." . map pp) (explodeFieldProj fs)))

-- -----------------------------------------------------------------------------
-- Expression level analysis
--
-- Calculate the EVs used in an expression relative to a given
-- projection (field set) on the result.  We can always just return
-- freeVars for the argument, but the hope is that we can do a bit
-- better.

projectStruct :: Map Label FieldProj -> [(Label, Expr)] -> [(Label, SLExpr)]
projectStruct fp = map go
  where
    go (l, e) = case Map.lookup l fp of
      Nothing  -> (l, EHole (typeOf e))
      Just fp' -> (l, projectE fp'  e)

-- This function is pretty simple at the moment --- it just projects
-- out of structs (and under e.g. let and case)
projectE :: FieldProj -> Expr -> SLExpr
projectE Whole          expr = exprToSLExpr expr
projectE (FieldProj fp) expr = go expr
  where
    go e = case e of
      Var {}           -> exprToSLExpr e
      PureLet n e' e'' -> SPureLet n (exprToSLExpr e') (go e'')
      Struct ut flds   -> SStruct ut (projectStruct fp flds)
      ECase cs       -> SECase (go <$> cs)
      Ap0 {}         -> exprToSLExpr e
      Ap1 {}         -> exprToSLExpr e
      Ap2 {}         -> exprToSLExpr e
      Ap3 {}         -> exprToSLExpr e
      ApN {}         -> exprToSLExpr e


exprToProj :: Expr -> Maybe (Name, FieldProj)
exprToProj = go
  where
    go (Var n) = Just (n, Whole)
    go (Ap1 (SelStruct _ l) e) = second (FieldProj . Map.singleton l) <$> go e
    go _ = Nothing

-- | composeFieldProj d.e.f a.b.c  = a.b.c.d.e.f
composeFieldProj :: FieldProj -> FieldProj -> FieldProj
composeFieldProj inner = go
  where
    go Whole = inner
    go (FieldProj m) = FieldProj (go <$> m)

letLikeToAbsEnv :: Name -> Expr -> LiftAbsEnv FieldProj -> LiftAbsEnv FieldProj
letLikeToAbsEnv n e m@(LiftAbsEnv e'_env)
  | Just (n', n'_fp) <- exprToProj e
  , (Just n_fp, e_env_no_n') <- Map.updateLookupWithKey (\_ _ -> Nothing) n e'_env
  = LiftAbsEnv $ Map.insert n' (composeFieldProj n_fp n'_fp) e_env_no_n'
  | otherwise =  exprToAbsEnv e <> mapLiftAbsEnv (Map.delete n) m

exprToAbsEnv :: Expr -> LiftAbsEnv FieldProj
exprToAbsEnv = go
  where
    go expr
      | Just (n, fp) <- exprToProj expr = LiftAbsEnv (Map.singleton n fp)
    -- special case for let x = a.b.c in y x, FWIW
    go (PureLet n e e')     = letLikeToAbsEnv n e (go e')
    go (ECase c@(Case x _)) = mapLiftAbsEnv (Map.insert x Whole) (foldMap go c)
    go expr = foldMapChildrenE go expr

byteSetToAbsEnv :: ByteSet -> LiftAbsEnv FieldProj
byteSetToAbsEnv = go
  where
    go (SetLet n e b)         = letLikeToAbsEnv n e (go b)
    go (SetCase c@(Case x _)) = mapLiftAbsEnv (Map.insert x Whole) (foldMap go c)
    go b = ebFoldMapChildrenB exprToAbsEnv go b

--------------------------------------------------------------------------------
-- Instances

instance Semigroup FieldProj where
  Whole <> _ = Whole
  _ <> Whole = Whole
  FieldProj m <> FieldProj m' = FieldProj $ Map.unionWith (<>) m m'

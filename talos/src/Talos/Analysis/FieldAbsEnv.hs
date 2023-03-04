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
import qualified Data.Map.Merge.Lazy   as Map
import           Data.Maybe            (fromMaybe)
import           Data.Proxy            (Proxy (Proxy))
import           GHC.Generics          (Generic)

import           Daedalus.Core
import           Daedalus.Core.Type
import           Daedalus.PP

import           Talos.Analysis.AbsEnv
import           Talos.Analysis.Eqv    (Eqv)
import           Talos.Analysis.Merge  (Merge (..))
import           Talos.Analysis.SLExpr (SLExpr (..))


fieldAbsEnvTy :: AbsEnvTy
fieldAbsEnvTy = AbsEnvTy (Proxy @FieldAbsEnv)

data FieldProj = Whole | FieldProj (Map Label FieldProj)
  deriving (Ord, Eq, Show, Generic)

instance NFData FieldProj -- default
instance Eqv FieldProj -- default

instance Merge FieldProj where
  Whole `merge` _ = Whole
  _ `merge` Whole = Whole
  FieldProj m `merge` FieldProj m' = FieldProj $ merge m m'

instance AbsEnvPred FieldProj where
  absPredTop = Whole
  absPredOverlaps Whole _ = True
  absPredOverlaps _ Whole = True
  absPredOverlaps (FieldProj m1) (FieldProj m2) = mapPredOverlaps m1 m2

  absPredEntails Whole _ = True
  -- probably, unless we have the whole record?
  absPredEntails (FieldProj {}) Whole = False
  absPredEntails (FieldProj m1) (FieldProj m2) =
    and (Map.merge Map.dropMissing {- in m1 not m2 -}
                   (Map.mapMissing (\_ _ -> False)) {- in m2 not m1 -}
                   (Map.zipWithMatched (const absPredEntails)) {- in both -}
                   m1 m2)

  -- Nothing special for lists.
  absPredIsStructural _ = True
  absPredListElement _ = Whole
  absPredCollection _ _ _ _ = Whole

newtype FieldAbsEnv = FieldAbsEnv (LiftAbsEnv FieldProj)
  deriving (Merge, Eqv, PP, AbsEnv)

instance AbsEnvPointwise FieldProj where
  absPredPre p e   = exprToAbsEnv p e
  absPredByteSet _ = byteSetToAbsEnv
  absPredInverse n e1 e2 =
    mapLiftAbsEnv (Map.delete n)
                  (fst (exprToAbsEnv Whole e1) `merge` fst (exprToAbsEnv Whole e2))

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

-- projectStruct :: Map Label FieldProj -> [(Label, Expr)] -> [(Label, SLExpr)]
-- projectStruct fp = map go
--   where
--     go (l, e) = case Map.lookup l fp of
--       Nothing  -> (l, EHole (typeOf e))
--       Just fp' -> (l, projectE fp'  e)

-- -- This function is pretty simple at the moment --- it just projects
-- -- out of structs (and under e.g. let and case)
-- projectE :: FieldProj -> Expr -> SLExpr
-- projectE Whole          expr = exprToSLExpr expr
-- projectE (FieldProj fp) expr = go expr
--   where
--     go e = case e of
--       Var {}           -> exprToSLExpr e
--       PureLet n e' e'' -> SPureLet n (exprToSLExpr e') (go e'')
--       Struct ut flds   -> SStruct ut (projectStruct fp flds)
--       ECase cs       -> SECase (go <$> cs)
--       ELoop (FoldMorphism n e lc b) ->
--         let (fp' = exprFixpoint 
--       -- This case shouldn't happen here as we should only have Whole
--       -- for lists, but this should be correct in any case.
--       ELoop (MapMorphism lc b) ->        
--         SELoop (MapMorphism (lc { lcCol = exprToSLExpr (lcCol lc)})
--                             (exprToSLExpr b))           
--       Ap0 {}         -> exprToSLExpr e
--       Ap1 {}         -> exprToSLExpr e
--       Ap2 {}         -> exprToSLExpr e
--       Ap3 {}         -> exprToSLExpr e
--       ApN {}         -> exprToSLExpr e

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

-- letLikeToAbsEnv :: Name -> Expr -> LiftAbsEnv FieldProj -> LiftAbsEnv FieldProj
-- letLikeToAbsEnv n e m@(LiftAbsEnv e'_env)
--   | Just (n', n'_fp) <- exprToProj e
--   , (Just n_fp, e_env_no_n') <- Map.updateLookupWithKey (\_ _ -> Nothing) n e'_env
--   = LiftAbsEnv $ Map.insert n' (composeFieldProj n_fp n'_fp) e_env_no_n'
--   | otherwise =  exprToAbsEnv e `merge` mapLiftAbsEnv (Map.delete n) m

-- The impl here sometimes uses the ((,) a) instance of Applicative
-- (and a is a Monoid).  Pretty cool, pretty obscure.
exprToAbsEnv :: FieldProj -> Expr -> (LiftAbsEnv FieldProj, SLExpr)
exprToAbsEnv fp expr =
  case expr of
    Var n            -> (LiftAbsEnv $ Map.singleton n fp, SVar n)
    PureLet n e e' ->
      let (env, sle') = go fp e'
          (env', fpe) = fromMaybe (absEmptyEnv, Whole) (absProj n env)
          (enve, sle) = go fpe e
      in (merge env' enve, SPureLet n sle sle')
    Struct ut flds   ->
      let mk = case fp of
            Whole        -> \_l e -> go Whole e
            FieldProj m -> \l  e ->
              maybe (absEmptyEnv, EHole (typeOf e))
                    (flip go e) (Map.lookup l m)
          mk' (l, e) = (,) l <$> mk l e
      in SStruct ut <$> traverse mk' flds

    ECase cs       -> second SECase (traverse (go fp) cs)
    ELoop (FoldMorphism n e lc b) ->
      let (env, fp', slb) = exprFixpoint n b fp
          (enve, sle) = go fp' e
          (lcenv, sllc) = go Whole (lcCol lc)
          lc' = lc {lcCol = sllc }          
      in (env `merge` enve `merge` lcenv
         , SELoop (FoldMorphism n sle lc' slb)
         )

    -- fp should be Whole here.
    ELoop (MapMorphism lc b) ->
      -- This is an over-approximate, as the body doesn't _have_
      -- to reference the list, but probably it does.
      let (lcenv, sllc) = go Whole (lcCol lc)
          lc' = lc {lcCol = sllc }
          (benv, bsl) = go Whole b

          -- ignores pred (should be Whole)
          benvNoEl =
            maybe benv fst (absProj (lcElName lc) benv)
          benvNoElK =
            maybe benvNoEl fst (flip absProj benvNoEl =<< lcKName lc)
      in (merge lcenv benvNoElK, SELoop (MapMorphism lc' bsl))
          
        -- Apart from SelStruct, none of the below are interesting (should be Whole)
    Ap0 op         -> (absEmptyEnv, SAp0 op)
    Ap1 (SelStruct ty l) e ->
      SAp1 (SelStruct ty l) <$> go (FieldProj $ Map.singleton l fp) e
    Ap1 op e        -> SAp1 op <$> go Whole e
    Ap2 op e1 e2    -> SAp2 op <$> go Whole e1 <*> go Whole e2
    Ap3 op e1 e2 e3 -> SAp3 op <$> go Whole e1 <*> go Whole e2 <*> go Whole e3

    -- We could (should?) unfold functions etc. here, but this is simpler.
    ApN op es       -> SApN op <$> traverse (go Whole) es
  where
    go = exprToAbsEnv

    exprFixpoint n e fp' =
      let (env, sle) = exprToAbsEnv fp' e
          (env', fp'') = fromMaybe (env, fp') (absProj n env)
      in if absPredEntails fp fp''
         then (env', fp, sle)
         else exprFixpoint n e (fp `merge` fp'')

-- FIXME: a bit simplistic.
byteSetToAbsEnv :: ByteSet -> LiftAbsEnv FieldProj
byteSetToAbsEnv = ebFoldMapChildrenB (fst . exprToAbsEnv Whole) byteSetToAbsEnv 

--------------------------------------------------------------------------------
-- Instances

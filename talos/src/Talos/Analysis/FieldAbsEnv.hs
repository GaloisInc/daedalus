{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Talos.Analysis.FieldAbsEnv (fieldAbsEnvTy) where

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

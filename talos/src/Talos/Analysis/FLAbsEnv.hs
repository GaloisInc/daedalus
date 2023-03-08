{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Talos.Analysis.FLAbsEnv (flAbsEnvTy) where

import           Control.DeepSeq       (NFData)
import           Data.Bifunctor        (second)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import qualified Data.Map.Merge.Lazy   as Map
import           Data.Maybe            (fromMaybe)
import           Data.Proxy            (Proxy (Proxy))
import qualified Data.Set              as Set
import           GHC.Generics          (Generic)

import           Daedalus.Core
import           Daedalus.Core.Type
import           Daedalus.PP

import           Talos.Analysis.AbsEnv
import           Talos.Analysis.Eqv    (Eqv)
import           Talos.Analysis.Merge  (Merge (..))
import           Talos.Analysis.SLExpr (SLExpr (..))
import Talos.Analysis.Slice (Structural(..))
import Daedalus.Panic (panic)

flAbsEnvTy :: AbsEnvTy
flAbsEnvTy = AbsEnvTy (Proxy @FLAbsEnv)

data FLProj =
  Whole
  | FieldProj [Label] (Map Label FLProj)
  | ListProj Structural FLProj
  deriving (Ord, Eq, Show, Generic)

instance NFData FLProj -- default
instance Eqv FLProj -- default

instance Merge FLProj where
  Whole `merge` _ = Whole
  _ `merge` Whole = Whole

  ListProj str1 fp1 `merge` ListProj str2 fp2 =
    listProj (merge str1 str2) (merge fp1 fp2)

  -- FIXME: push the set of labels into the type.
  FieldProj ls m1 `merge` FieldProj _ls m2 = fieldPrpj ls (merge m1 m2)

  _ `merge` _ = panic "BUG: malformed FLProj" []

fieldPrpj :: [Label] -> Map Label FLProj -> FLProj
fieldPrpj ls m
  | all (== Whole) m, Map.keysSet m == Set.fromList ls = Whole
  | otherwise = FieldProj ls m

listProj :: Structural -> FLProj -> FLProj
listProj str fp
  | Whole <- fp, Structural <- str = Whole
  | otherwise = ListProj str fp

instance AbsEnvPred FLProj where
  absPredTop = Whole
  absPredOverlaps fp1 fp2 =
    case (fp1, fp2) of
      (Whole, _) -> True
      (_, Whole) -> True
      (ListProj _str1 fp1', ListProj _str2 fp2') ->
        absPredOverlaps fp1' fp2'
      (FieldProj _ m1, FieldProj _ m2) ->
        mapPredOverlaps m1 m2
        
      _ -> panic "BUG: malformed FLProj" []
      
  absPredEntails Whole _ = True
  -- Assumes we normalise (\_ -> Whole) to Whole
  absPredEntails _ Whole = False
  absPredEntails (ListProj str1 fp1) (ListProj str2 fp2) =
    absPredEntails fp1 fp2 &&
    case (str1, str2) of
      (Structural, _)                          -> True
      (StructureInvariant, StructureInvariant) -> True
      (StructureInvariant, Structural)         -> False
  
  absPredEntails (FieldProj _ m1) (FieldProj _ m2) =
    and (Map.merge Map.dropMissing {- in m1 not m2 -}
                   (Map.mapMissing (\_ _ -> False)) {- in m2 not m1 -}
                   (Map.zipWithMatched (const absPredEntails)) {- in both -}
                   m1 m2)

  absPredEntails _ _ = panic "BUG: malformed FLProj" []

  -- Nothing special for lists.
  absPredStructural (ListProj str _) = str
  absPredStructural _ = Structural
  
  absPredListElement (ListProj _ fp) = fp
  absPredListElement _ = Whole
  
  absPredCollection _ StructureInvariant Nothing Nothing = Nothing
  absPredCollection (TArray {}) str m_kp (Just fp) =
    Just $ listProj str' fp
    where
      str' = str `merge` maybe StructureInvariant (const Structural) m_kp
  absPredCollection _ _ _ _ = Just Whole

newtype FLAbsEnv = FLAbsEnv (LiftAbsEnv FLProj)
  deriving (Merge, Eqv, PP, AbsEnv)

instance AbsEnvPointwise FLProj where
  absPredPre p e   = exprToAbsEnv p e
  absPredByteSet _ = byteSetToAbsEnv
  absPredInverse n e1 e2 =
    mapLiftAbsEnv (Map.delete n)
                  (fst (exprToAbsEnv Whole e1) `merge` fst (exprToAbsEnv Whole e2))

explodeFieldProj :: FLProj -> [ [Label] ]
explodeFieldProj (FieldProj _ m) =
  [ l : ls | (l, fs') <- Map.toList m, ls <- explodeFieldProj fs' ]
explodeFieldProj _ = [ [] ]


instance PP FLProj where
  pp Whole = "Whole"
  pp (ListProj str fp) = ("List" <> if str == Structural then "!" else "") <+> pp fp
  pp fs    = braces (commaSep (map (hcat . punctuate "." . map pp) (explodeFieldProj fs)))

-- -----------------------------------------------------------------------------
-- Expression level analysis
--

-- The impl here sometimes uses the ((,) a) instance of Applicative
-- (and a is a Monoid).  Pretty cool, pretty obscure.
exprToAbsEnv :: FLProj -> Expr -> (LiftAbsEnv FLProj, SLExpr)
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
            Whole         -> \_l e -> go Whole e
            ListProj {} -> panic "BUG: malformed FLProj" []
            FieldProj _ m -> \l  e ->
              maybe (absEmptyEnv, EHole (typeOf e))
                    (flip go e) (Map.lookup l m)
          mk' (l, e) = (,) l <$> mk l e
      in SStruct ut <$> traverse mk' flds

    ECase cs       -> second SECase (traverse (go fp) cs)
    ELoop (FoldMorphism n e lc b) ->
      let (env, fp', slb) = exprFixpoint n b fp
          (enve, sle) = go fp' e
          (elenv, m_elfp) = projectMaybe (lcElName lc) env
          (kenv, m_kfp)   = maybe (elenv, Nothing) (flip projectMaybe elenv) (lcKName lc)
          (lcenv, sllc) = go (listProj  m_kfp m_elfp) (lcCol lc)
          lc' = lc {lcCol = sllc }
      in (env `merge` enve `merge` lcenv
         , SELoop (FoldMorphism n sle lc' slb)
         )

    ELoop (MapMorphism lc b) ->
      let (lcenv, sllc) = go Whole (lcCol lc)
          lc' = lc {lcCol = sllc }
          (benv, bsl) = go Whole b

          -- ignores pred (should be Whole)
          benvNoEl = fst $ projectMaybe (lcElName lc) benv
          benvNoElK = 
            maybe benvNoEl fst (flip absProj benvNoEl =<< lcKName lc)
      in (merge lcenv benvNoElK, SELoop (MapMorphism lc' bsl))

        -- Apart from SelStruct, none of the below are interesting (should be Whole)
    Ap0 op         -> (absEmptyEnv, SAp0 op)
    Ap1 (SelStruct ty l) e
      | TUser UserType { utName = TName { tnameFlav = TFlavStruct ls }} <- ty ->
        SAp1 (SelStruct ty l) <$> go (FieldProj ls $ Map.singleton l fp) e
    Ap1 op e        -> SAp1 op <$> go Whole e
    Ap2 op e1 e2    -> SAp2 op <$> go Whole e1 <*> go Whole e2
    Ap3 op e1 e2 e3 -> SAp3 op <$> go Whole e1 <*> go Whole e2 <*> go Whole e3

    -- We could (should?) unfold functions etc. here, but this is simpler.
    ApN op es       -> SApN op <$> traverse (go Whole) es
  where
    go = exprToAbsEnv

    projectMaybe x env = maybe (env, Nothing) (second Just) (absProj x env)

    exprFixpoint n e fp' =
      let (env, sle) = exprToAbsEnv fp' e
          (env', fp'') = fromMaybe (env, fp') (absProj n env)
      in if absPredEntails fp fp''
         then (env', fp, sle)
         else exprFixpoint n e (fp `merge` fp'')

-- FIXME: a bit simplistic.
byteSetToAbsEnv :: ByteSet -> LiftAbsEnv FLProj
byteSetToAbsEnv = ebFoldMapChildrenB (fst . exprToAbsEnv Whole) byteSetToAbsEnv

--------------------------------------------------------------------------------
-- Instances

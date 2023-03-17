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
import           Data.Maybe            (fromMaybe, isJust)
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
  
  -- Maybe FLProj is the element pred, it is Nothing in the case of
  -- e.g. Length.  Having a special case for just structure avoids
  -- spurious cases.
  | ListProj Structural FLProj
  | ListStructure
  deriving (Ord, Eq, Show, Generic)

instance NFData FLProj -- default
instance Eqv FLProj -- default

instance Merge FLProj where
  Whole `merge` _ = Whole
  _ `merge` Whole = Whole

  ListProj str1 fp1 `merge` ListProj str2 fp2 =
    listProj (merge str1 str2) (merge fp1 fp2)
  ListProj _str1 fp1 `merge` ListStructure = listProj Structural fp1
  ListStructure `merge` ListProj _str1 fp1 = listProj Structural fp1
  ListStructure `merge` ListStructure = ListStructure
  
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

listLike :: Type -> Bool
listLike (TArray {})   = True
listLike (TBuilder {}) = True
listLike _ = False

instance AbsEnvPred FLProj where
  absPredTop = Whole
  absPredOverlaps fp1 fp2 =
    case (fp1, fp2) of
      (Whole, _) -> True
      (_, Whole) -> True
      
      -- In this case the strs should both be structural.
      (ListStructure, ListProj str _) -> str == Structural
      (ListProj str _, ListStructure) -> str == Structural
      (ListStructure, ListStructure) -> True
      -- Only allow a single structural pred.
      (ListProj str1 fp1', ListProj str2 fp2') ->
        (str1 == Structural && str2 == Structural)
        || absPredOverlaps fp1' fp2'        
      (FieldProj _ m1, FieldProj _ m2) ->
        mapPredOverlaps m1 m2
        
      _ -> panic "BUG: malformed FLProj" []
      
  absPredEntails Whole _ = True
  -- Assumes we normalise (\_ -> Whole) to Whole
  absPredEntails _ Whole = False
  absPredEntails (ListProj str _) ListStructure = str == Structural
  -- We don't have empty preds, hence this case
  absPredEntails ListStructure (ListProj {})    = False
  absPredEntails ListStructure ListStructure    = True  
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
  
  absPredListElement (ListProj _ fp) = Just fp
  absPredListElement ListStructure = Nothing
  absPredListElement _ = Just Whole
  
  absPredCollection _ StructureInvariant Nothing Nothing = Nothing
  absPredCollection ty str m_kp Nothing
    | listLike ty, str == Structural || isJust m_kp
    = Just ListStructure
  absPredCollection ty str m_kp (Just fp) | listLike ty
    = Just $ listProj str' fp
    where
      -- If the key pred is non-empty then we care about the list
      -- index, and so the structure.
      str' = maybe str (const Structural) m_kp
      
  -- We take the whole dictionary    
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
  pp ListStructure = "ListStructure"
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
            FieldProj _ m -> \l  e -> goM (Map.lookup l m) e
            _ -> panic "BUG: malformed FLProj" []            
          mk' (l, e) = (,) l <$> mk l e
      in SStruct ut <$> traverse mk' flds

    ECase cs       -> second SECase (traverse (go fp) cs)
    ELoop (FoldMorphism n e lc b) ->
      let (env, m_fp', slb) = exprFixpoint n b fp
          (enve, sle) = goM m_fp' e
          
          str = maybe StructureInvariant absPredStructural m_fp'
          (lcenv, lc', _str') = projectForLoopCollection str env lc
      in (env `merge` enve `merge` lcenv
         , SELoop (FoldMorphism n sle lc' slb)
         )

    ELoop (MapMorphism lc b) ->
      let m_fp' = absPredListElement fp
          str   = absPredStructural fp
          (env, slb) = goM m_fp' b
          (lcenv, lc', _str') = projectForLoopCollection str env lc
      in (env `merge` lcenv, SELoop (MapMorphism lc' slb))

    -- We care about SelStruct and list/builder ops only, we don't do
    -- anything special for iterators (FIXME: maybe we should).
    Ap0 op         -> (absEmptyEnv, SAp0 op)
    Ap1 (SelStruct ty l) e
      | TUser UserType { utName = TName { tnameFlav = TFlavStruct ls }} <- ty ->
        SAp1 (SelStruct ty l) <$> go (FieldProj ls $ Map.singleton l fp) e

    Ap1 ArrayLen e -> SAp1 ArrayLen <$> go ListStructure e
      
    -- We just push fp into the elements of e    
    Ap1 Concat e ->
      SAp1 Concat <$> go (listProj (absPredStructural fp) fp) e
      
    -- We treat builders like lists    
    Ap1 FinishBuilder e -> SAp1 FinishBuilder <$> go fp e
    Ap1 op e        -> SAp1 op <$> go Whole e

    Ap2 ArrayIndex arre ixe ->
      SAp2 ArrayIndex <$> go (listProj Structural fp) arre
                      <*> go Whole ixe
    Ap2 Emit bldre ve -> 
      SAp2 Emit <$> go fp bldre <*> goM (absPredListElement fp) ve

    -- Appends the array to the builder, so we have the same
    -- projection for both.
    Ap2 EmitArray bldre arre -> 
      SAp2 EmitArray <$> go fp bldre <*> go fp arre

    -- Similarly, appends two builders.
    Ap2 EmitBuilder bldre1 bldre2  -> 
      SAp2 EmitBuilder <$> go fp bldre1 <*> go fp bldre2
    Ap2 op e1 e2    -> SAp2 op <$> go Whole e1 <*> go Whole e2

    -- Range ops are just Whole
    Ap3 op e1 e2 e3 -> SAp3 op <$> go Whole e1 <*> go Whole e2 <*> go Whole e3

    ApN (ArrayL ty) es ->
      SApN (ArrayL ty) <$> traverse (goM (absPredListElement fp)) es
      
    -- We could (should?) unfold functions etc. here, but this is simpler.    
    ApN op es       -> SApN op <$> traverse (go Whole) es
  where
    goM m_fp e' = maybe (absEmptyEnv, EHole (typeOf e')) (flip go e') m_fp
     
    go = exprToAbsEnv

    exprFixpoint n e fp' =
      let (env, sle) = exprToAbsEnv fp' e
          (env', m_fp'') = maybe (env, Nothing) (second Just) (absProj n env)
      in case m_fp'' of
        Just fp'' | not (absPredEntails fp' fp'') ->
                    exprFixpoint n e (fp' `merge` fp'')
        _ -> (env', m_fp'', sle)

-- FIXME: a bit simplistic.
byteSetToAbsEnv :: ByteSet -> LiftAbsEnv FLProj
byteSetToAbsEnv = ebFoldMapChildrenB (fst . exprToAbsEnv Whole) byteSetToAbsEnv

--------------------------------------------------------------------------------
-- Instances

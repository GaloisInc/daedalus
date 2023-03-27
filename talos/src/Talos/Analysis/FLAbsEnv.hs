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
import           Daedalus.Panic        (panic)

import           Talos.Analysis.AbsEnv
import           Talos.Analysis.Eqv    (Eqv)
import           Talos.Analysis.Merge  (Merge (..))
import           Talos.Analysis.SLExpr (SLExpr (..))
import           Talos.Analysis.Slice  (Structural (..))


flAbsEnvTy :: AbsEnvTy
flAbsEnvTy = AbsEnvTy (Proxy @FLAbsEnv)

data FLProj =
  Whole
  | FieldProj [Label] (Map Label FLProj)
  
  -- Maybe FLProj is the element pred, it is Nothing in the case of
  -- e.g. Length.  ListProj StructureInvariant Nothing should not
  -- occur, as it means we don't care about the list at all.
  | ListProj Structural (Maybe FLProj)
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

listProj :: Structural -> Maybe FLProj -> FLProj
listProj str m_fp
  | Just Whole <- m_fp, StructureDependent <- str = Whole
  | Nothing <- m_fp, StructureIndependent <- str =
      panic "Malformed list projection" []
  | otherwise = ListProj str m_fp

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
      (ListProj str1 m_fp1, ListProj str2 m_fp2) ->
        case (str1, m_fp1, str2, m_fp2) of
          (StructureIndependent, Nothing, _, _) -> panic "IMPOSSIBLE (absPredOverlaps)" []
          (_, _, StructureIndependent, Nothing) -> panic "IMPOSSIBLE (absPredOverlaps)" []
          (StructureIndependent, _, _, Nothing) -> False
          (_, Nothing, StructureIndependent, _) -> False
          (StructureIndependent, Just fp1', _, Just fp2') ->
            absPredOverlaps fp1' fp2'
          (_, Just fp1', StructureIndependent, Just fp2') ->
            absPredOverlaps fp1' fp2'
          _ -> True
          
      (FieldProj _ m1, FieldProj _ m2) ->
        mapPredOverlaps m1 m2
        
      _ -> panic "BUG: malformed FLProj" []
      
  absPredEntails Whole _ = True
  -- Assumes we normalise (\_ -> Whole) to Whole
  absPredEntails _ Whole = False
  absPredEntails (ListProj str1 m_fp1) (ListProj str2 m_fp2) =
    str2 <= str1 &&
    case (m_fp1, m_fp2) of
      (Nothing, Nothing) -> True
      (Just {}, Nothing) -> True
      (Nothing, Just {}) -> False
      (Just fp1, Just fp2) -> absPredEntails fp1 fp2
  
  absPredEntails (FieldProj _ m1) (FieldProj _ m2) =
    and (Map.merge Map.dropMissing {- in m1 not m2 -}
                   (Map.mapMissing (\_ _ -> False)) {- in m2 not m1 -}
                   (Map.zipWithMatched (const absPredEntails)) {- in both -}
                   m1 m2)

  absPredEntails _ _ = panic "BUG: malformed FLProj" []

  -- Nothing special for lists.
  absPredStructural (ListProj str _) = str
  absPredStructural _ = StructureDependent
  
  absPredListElement (ListProj _ fp) = fp
  absPredListElement _ = Just Whole
  
  absPredCollection _ StructureIndependent Nothing Nothing = Nothing
  absPredCollection ty str m_kp m_elp
    | listLike ty = Just $ listProj str' m_elp
    where
      -- If the key pred is non-empty then we care about the list
      -- index, and so the structure.
      str' = maybe str (const StructureDependent) m_kp
      
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
  pp (ListProj str fp) = ("List" <> pp str) <+> maybe "⊥" pp fp
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
    ELoop {}       -> panic "Saw a Loop in exprToAbsEnv" []
    -- We care about SelStruct and list/builder ops only, we don't do
    -- anything special for iterators (FIXME: maybe we should).
    Ap0 op         -> (absEmptyEnv, SAp0 op)
    Ap1 (SelStruct ty l) e
      | TUser UserType { utName = TName { tnameFlav = TFlavStruct ls }} <- ty ->
        SAp1 (SelStruct ty l) <$> go (FieldProj ls $ Map.singleton l fp) e

    Ap1 ArrayLen e ->
      SAp1 ArrayLen <$> go (listProj StructureDependent Nothing) e
      
    -- We just push fp into the elements of e    
    Ap1 Concat e ->
      SAp1 Concat <$> go (listProj (absPredStructural fp) (Just fp)) e
      
    -- We treat builders like lists    
    Ap1 FinishBuilder e -> SAp1 FinishBuilder <$> go fp e
    Ap1 op e        -> SAp1 op <$> go Whole e

    Ap2 ArrayIndex arre ixe ->
      SAp2 ArrayIndex <$> go (listProj StructureDependent (Just fp)) arre
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
      SArrayL ty <$> traverse (goM (absPredListElement fp)) es
    ApN {}       -> panic "Saw a Call in exprToAbsEnv" []
  where
    goM m_fp e' = maybe (absEmptyEnv, EHole (typeOf e')) (flip go e') m_fp
     
    go = exprToAbsEnv

-- FIXME: a bit simplistic.
byteSetToAbsEnv :: ByteSet -> LiftAbsEnv FLProj
byteSetToAbsEnv = ebFoldMapChildrenB (fst . exprToAbsEnv Whole) byteSetToAbsEnv

--------------------------------------------------------------------------------
-- Instances

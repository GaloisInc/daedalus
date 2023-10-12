{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Talos.Analysis.FieldAbsEnv (fieldAbsEnvTy) where

import           Control.Arrow         ((***))
import           Control.DeepSeq       (NFData)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import qualified Data.Map.Merge.Lazy   as Map
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

fieldAbsEnvTy :: AbsEnvTy
fieldAbsEnvTy = AbsEnvTy (Proxy @FieldAbsEnv)

data FieldProj = Whole | FieldProj [Label] (Map Label FieldProj)
  deriving (Ord, Eq, Show, Generic)

instance NFData FieldProj -- default
instance Eqv FieldProj -- default

instance Merge FieldProj where
  Whole `merge` _ = Whole
  _ `merge` Whole = Whole

  -- FIXME: push the set of labels into the type.
  FieldProj ls m1 `merge` FieldProj _ls m2
    | all (== Whole) m, Map.keysSet m == Set.fromList ls = Whole
    | otherwise = FieldProj ls m
    where
      m = merge m1 m2

instance AbsEnvPred FieldProj where
  absPredTop = Whole
  absPredOverlaps Whole _ = True
  absPredOverlaps _ Whole = True
  absPredOverlaps (FieldProj _ m1) (FieldProj _ m2) = mapPredOverlaps m1 m2

  absPredEntails Whole _ = True
  -- probably, unless we have the whole record?
  absPredEntails (FieldProj {}) Whole = False
  absPredEntails (FieldProj _ m1) (FieldProj _ m2) =
    and (Map.merge Map.dropMissing {- in m1 not m2 -}
                   (Map.mapMissing (\_ _ -> False)) {- in m2 not m1 -}
                   (Map.zipWithMatched (const absPredEntails)) {- in both -}
                   m1 m2)

  -- Nothing special for lists.
  absPredStructural _  = StructureDependent
  absPredListElement _ = Just Whole
  
  absPredCollection _ StructureIndependent Nothing Nothing = Nothing
  absPredCollection _ _                    _       _       = Just Whole

newtype FieldAbsEnv = FieldAbsEnv (LiftAbsEnv FieldProj)
  deriving (Merge, Eqv, PP, AbsEnv)

instance AbsEnvPointwise FieldProj where
  absPredPre p e   = exprToAbsEnv p e
  absPredByteSet _ = byteSetToAbsEnv
  absPredInverse n e1 e2 =
    mapLiftAbsEnv (Map.delete n)
                  (fst (exprToAbsEnv Whole e1) `merge` fst (exprToAbsEnv Whole e2))
  absPredNonStructural _ = False

explodeFieldProj :: FieldProj -> [ [Label] ]
explodeFieldProj Whole = [ [] ]
explodeFieldProj (FieldProj _ m) =
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
      in case absProj n env of
           Nothing -> (env, sle') -- Drop the Let
           Just (env', fpe) ->
             let (enve, sle) = go fpe e
             in (merge env' enve, SPureLet n sle sle')
    Struct ut flds   ->
      let mk = case fp of
            Whole         -> \_l e -> go Whole e
            FieldProj _ m -> \l  e ->
              maybe (absEmptyEnv, EHole (typeOf e))
                    (flip go e) (Map.lookup l m)
          mk' (l, e) = (,) l <$> mk l e
      in SStruct ut <$> traverse mk' flds

    ECase cs@(Case x _) -> (mapLiftAbsEnv (Map.insert x Whole) *** SECase) (traverse (go fp) cs)
    ELoop {}       -> panic "Saw a Loop in exprToAbsEnv" [showPP expr]
    -- Apart from SelStruct, none of the below are interesting (should be Whole)
    Ap0 op         -> (absEmptyEnv, SAp0 op)
    Ap1 (SelStruct ty l) e
      | TUser UserType { utName = TName { tnameFlav = TFlavStruct ls }} <- typeOf e ->
        SAp1 (SelStruct ty l) <$> go (FieldProj ls $ Map.singleton l fp) e
    Ap1 op e        -> SAp1 op <$> go Whole e
    Ap2 op e1 e2    -> SAp2 op <$> go Whole e1 <*> go Whole e2
    Ap3 op e1 e2 e3 -> SAp3 op <$> go Whole e1 <*> go Whole e2 <*> go Whole e3

    -- We could (should?) unfold functions etc. here, but this is simpler.
    ApN (ArrayL ty) es -> SArrayL ty <$> traverse (go Whole) es
    
    -- Mainly for inverses
    ApN (CallF f) es -> SCallF f <$> traverse (go Whole) es
    
  where
    go = exprToAbsEnv

-- FIXME: a bit simplistic.
byteSetToAbsEnv :: ByteSet -> LiftAbsEnv FieldProj
byteSetToAbsEnv = ebFoldMapChildrenB (fst . exprToAbsEnv Whole) byteSetToAbsEnv

--------------------------------------------------------------------------------
-- Instances

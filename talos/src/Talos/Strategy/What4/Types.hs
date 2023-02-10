{-# Language OverloadedStrings #-}
{-# Language GADTs #-}
{-# Language GeneralisedNewtypeDeriving #-}
{-# Language RankNTypes #-}
{-# Language PatternSynonyms #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language KindSignatures #-}
{-# Language ScopedTypeVariables #-}
{-# Language ViewPatterns #-}
{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}
{-# Language MultiParamTypeClasses #-}

module Talos.Strategy.What4.Types(
  -- BaseMaybe
    BaseMaybeType
  , BaseMaybe
  , pattern BaseMaybeRepr
  , getBaseMaybe
  , mkBaseMaybe
  -- BaseUnion
  , BaseUnionType
  , BaseUnion
  , BaseUnionRepr
  , pattern BaseUnionRepr
  , mkBaseUnion
  , getBaseUnion
  , muxBaseUnion
  -- interpreting Daedalus types
  , typeToRepr
  , lookupTName
  , getFieldIndex
  , mkFlds

) where

import qualified Data.BitVector.Sized as BVS
import qualified Data.Map                        as Map

import           Data.Parameterized.NatRepr
import qualified Data.Parameterized.Context      as Ctx
import           Data.Parameterized.Some
import qualified What4.Interface                 as W4

import           Daedalus.Core                   hiding (streamOffset)
import qualified Daedalus.Core.Basics              as I
import           Daedalus.Panic
import           Daedalus.PP
import           Talos.Strategy.Monad

import           Talos.Strategy.What4.SymM

------------------------------------------------------------
-- Daedalus types encoded as What4 types

-- Maybe type

type BaseMaybeType tp = W4.BaseStructType (Ctx.EmptyCtx Ctx.::> W4.BaseBoolType Ctx.::> tp)
type BaseMaybe sym tp = W4.SymExpr sym (BaseMaybeType tp)

getBaseMaybe :: 
  W4.IsSymExprBuilder sym =>
  sym -> 
  BaseMaybe sym tp -> 
  IO (W4.SymExpr sym tp, W4.Pred sym)
getBaseMaybe sym e = do
  p <- W4.structField sym e Ctx.i2of2
  e' <- W4.structField sym e Ctx.i1of2
  return (p, e')

mkBaseMaybe ::
  W4.IsSymExprBuilder sym =>
  sym ->
  W4.SymExpr sym tp ->
  W4.Pred sym ->
  IO (W4.SymExpr sym (BaseMaybeType tp))  
mkBaseMaybe sym e p = W4.mkStruct sym (Ctx.Empty Ctx.:> p Ctx.:> e)

pattern BaseMaybeRepr :: () => (tp_outer ~ BaseMaybeType tp_inner) => W4.BaseTypeRepr tp_inner -> W4.BaseTypeRepr tp_outer
pattern BaseMaybeRepr x = W4.BaseStructRepr (Ctx.Empty Ctx.:> W4.BaseBoolRepr Ctx.:> x)


-- Struct index type

-- Index into an n-field struct represented as an n-width bitvector with exactly
-- one bit set

-- For completeness, we use a type family where an index into an empty struct is just
-- an empty struct. This just avoids the need for carrying around non-empty assumptions.
type family StructIdxType (ctx :: Ctx.Ctx W4.BaseType) :: W4.BaseType
type instance StructIdxType (ctx Ctx.::> _) = W4.BaseBVType (1 + Ctx.CtxSize ctx)
type instance StructIdxType Ctx.EmptyCtx = W4.BaseStructType Ctx.EmptyCtx

type StructIdx sym (ctx :: Ctx.Ctx W4.BaseType) = W4.SymExpr sym (StructIdxType ctx)
type StructIdxRepr ctx = W4.BaseTypeRepr (StructIdxType ctx)


-- inverting a struct repr back into a context size
data StructIdxReprProof (tp :: W4.BaseType) where
  StructIdxReprProof :: Ctx.Size tps -> StructIdxReprProof (StructIdxType tps)

-- FIXME: add unsafe variant?
asStructIdxReprProof :: forall tp. W4.BaseTypeRepr tp -> Maybe (StructIdxReprProof tp)
asStructIdxReprProof repr = case repr of
  W4.BaseStructRepr Ctx.Empty -> Just (StructIdxReprProof Ctx.zeroSize)
  W4.BaseBVRepr w -> case testEquality w (knownNat @1) of
    Just Refl -> Just (StructIdxReprProof (Ctx.incSize Ctx.zeroSize))
    Nothing | w' <- W4.decNat w, Just LeqProof <- W4.isPosNat w',
      Just (StructIdxReprProof sz) <- asStructIdxReprProof (W4.BaseBVRepr w'),
      Just Refl <- testEquality (Ctx.sizeToNatRepr sz) w',
      Refl <- W4.plusComm w' (knownNat @1),
      Refl <- W4.minusPlusCancel w (knownNat @1) ->
      Just (StructIdxReprProof (Ctx.incSize sz))
    Nothing -> Nothing
  _ -> Nothing

lenPos :: Ctx.Size (tps Ctx.::> tp) -> LeqProof 1 (Ctx.CtxSize (tps Ctx.::> tp))
lenPos sz = case Ctx.viewSize sz of
  Ctx.IncSize sz' | sz'_n <- Ctx.sizeToNatRepr sz'
    , W4.LeqProof <- W4.addPrefixIsLeq sz'_n (knownNat @1) 
    , Refl <- W4.plusComm sz'_n (knownNat @1) 
    -> LeqProof

structIdxRepr :: Ctx.Size tps -> StructIdxRepr tps
structIdxRepr sz = case Ctx.viewSize sz of
  Ctx.ZeroSize -> W4.BaseStructRepr Ctx.Empty
  Ctx.IncSize{} | W4.LeqProof <- lenPos sz -> W4.BaseBVRepr (Ctx.sizeToNatRepr sz)

pattern StructIdxRepr :: () => (tp_outer ~ StructIdxType tps) => Ctx.Size tps -> W4.BaseTypeRepr tp_outer
pattern StructIdxRepr sz <- ((\x -> asStructIdxReprProof x) -> Just (StructIdxReprProof sz)) where
  StructIdxRepr sz = structIdxRepr sz

mkStructIdx ::
  W4.IsSymExprBuilder sym =>
  sym ->
  Ctx.Size tps ->
  Ctx.Index tps tp ->
  IO (StructIdx sym tps)
mkStructIdx sym sz idx =
  case Ctx.viewSize sz of
  Ctx.IncSize _ -> do
    W4.LeqProof <- return $ lenPos sz
    let w = (Ctx.sizeToNatRepr sz)
    zero <- W4.bvLit sym w (BVS.mkBV w 0)
    W4.bvSet sym zero (fromIntegral (Ctx.indexVal idx)) (W4.truePred sym)
  Ctx.ZeroSize -> fail "Impossible (found index into zero-width context)"

-- Unions

-- A struct with an additional field indicating which value the union represents
type BaseUnionType tps = W4.BaseStructType (tps Ctx.::> StructIdxType tps)
type BaseUnion sym tps = W4.SymExpr sym (BaseUnionType tps)
type BaseUnionRepr tps = W4.BaseTypeRepr (BaseUnionType tps)

data BaseUnionReprProof (tp :: W4.BaseType) where
  BaseUnionReprProof :: 
    Ctx.Assignment W4.BaseTypeRepr tps -> 
    BaseUnionReprProof (BaseUnionType tps)


data SameStructIdxType tps1 tps2 where
  SameStructIdxType :: 
    Ctx.Size tps1 -> 
    Ctx.Size tps2 -> 
    Ctx.CtxSize tps1 :~: Ctx.CtxSize tps2 ->
    StructIdxType tps1 :~: StructIdxType tps2 -> 
    SameStructIdxType tps1 tps2

-- FIXME: add safe/unsafe variants?
sameStructIdxType :: Ctx.Size ctx1 -> Ctx.Size ctx2 -> Maybe (SameStructIdxType ctx1 ctx2)
sameStructIdxType sz1 sz2 = case (Ctx.viewSize sz1, Ctx.viewSize sz2) of
  (Ctx.ZeroSize, Ctx.ZeroSize) -> return $ SameStructIdxType sz1 sz2 Refl Refl
  (Ctx.IncSize sz1_, Ctx.IncSize sz2_) -> do
    SameStructIdxType _ _ Refl Refl <- sameStructIdxType sz1_ sz2_
    return $ SameStructIdxType sz1 sz2 Refl Refl
  _ -> Nothing

data IsStructIdxPrfFor tps tp where
  IsStructIdxPrfFor :: Ctx.Size tps -> IsStructIdxPrfFor tps (StructIdxType tps)

isValidIdxRepr :: Ctx.Size ctx -> W4.BaseTypeRepr tp -> Maybe (IsStructIdxPrfFor ctx tp)
isValidIdxRepr sz repr = case repr of
  StructIdxRepr sz' -> do
    SameStructIdxType _ _ Refl Refl <- sameStructIdxType sz sz'
    return $ IsStructIdxPrfFor sz
  _ -> Nothing

asBaseUnion :: W4.BaseTypeRepr tp -> Maybe (BaseUnionReprProof tp)
asBaseUnion (W4.BaseStructRepr (vs Ctx.:> idxRepr)) = do
  IsStructIdxPrfFor _ <- isValidIdxRepr (Ctx.size vs) idxRepr
  return $ BaseUnionReprProof vs
asBaseUnion _ = Nothing

baseUnionRepr :: Ctx.Assignment W4.BaseTypeRepr tps -> BaseUnionRepr tps
baseUnionRepr ctx = W4.BaseStructRepr (ctx Ctx.:> structIdxRepr (Ctx.size ctx))

pattern BaseUnionRepr :: () => (tp_outer ~ BaseUnionType tps) => Ctx.Assignment W4.BaseTypeRepr tps -> W4.BaseTypeRepr tp_outer
pattern BaseUnionRepr tps <- ((\x -> asBaseUnion x) -> Just (BaseUnionReprProof tps)) where
  BaseUnionRepr tps = baseUnionRepr tps

mkBaseUnion ::
  W4.IsSymExprBuilder sym =>
  sym ->
  Ctx.Assignment W4.BaseTypeRepr tps ->
  W4.SymExpr sym tp ->
  Ctx.Index tps tp ->
  IO (BaseUnion sym tps)
mkBaseUnion sym reprs v idx = do
  structIdx <- mkStructIdx sym (Ctx.size reprs) idx
  vls <- Ctx.traverseWithIndex (\idx_ repr -> case testEquality idx idx_ of
    Just Refl -> return v
    -- other values don't matter here
    Nothing -> W4.baseDefaultValue sym repr) reprs
  W4.mkStruct sym (vls Ctx.:> structIdx)

getBaseUnion ::
  forall tps tp sym.
  W4.IsSymExprBuilder sym =>
  sym ->
  BaseUnion sym tps ->
  Ctx.Index tps tp ->
  IO (W4.SymExpr sym tp, W4.Pred sym)
getBaseUnion sym e idx = do
  W4.BaseStructRepr repr <- return $ W4.exprType e
  e' <- W4.structField sym e (Ctx.skipIndex idx)
  structIdx <- W4.structField sym e (Ctx.lastIndex (Ctx.size repr))
  p <- case W4.exprType structIdx of
    W4.BaseBVRepr{} -> do
      W4.testBitBV sym (fromIntegral (Ctx.indexVal idx)) structIdx
    _ -> return $ W4.falsePred sym
  return (e', p)

muxBaseUnion ::
  forall tps sym.
  W4.IsSymExprBuilder sym =>
  sym ->
  W4.Pred sym ->
  BaseUnion sym tps ->
  BaseUnion sym tps ->
  IO (BaseUnion sym tps)
muxBaseUnion sym p uT uF = do
  BaseUnionRepr reprs <- return $ W4.exprType uT
  -- zip the values together, but we only need to mux
  -- entries which are present in both unions
  vls <- Ctx.traverseWithIndex (\idx _repr -> do
        (vT, pT) <- getBaseUnion sym uT idx
        (vF, pF) <- getBaseUnion sym uF idx
        case (W4.asConstantPred pT, W4.asConstantPred pF) of
          (Just True, Just False) -> return vT
          (Just False, Just True) -> return vF
          _ -> W4.baseTypeIte sym p vT vF)
         reprs
  let lastIdx = Ctx.lastIndex (Ctx.incSize (Ctx.size reprs))
  -- the union index itself is the actual mux
  structIdxT <- W4.structField sym uT lastIdx
  structIdxF <- W4.structField sym uF lastIdx
  structIdx <- W4.baseTypeIte sym p structIdxT structIdxF
  W4.mkStruct sym (vls Ctx.:> structIdx)


-- Resolving types

sizeToRepr :: I.SizeType -> Maybe (Some (W4.NatRepr))
sizeToRepr (I.TSize n) = W4.someNat n
sizeToRepr (I.TSizeParam{}) = Nothing

typeToRepr ::
  forall sym m.
  I.Type -> 
  W4SolverT sym m (Some (W4.BaseTypeRepr))
typeToRepr v = go v
  where
    go :: I.Type -> W4SolverT sym m (Some (W4.BaseTypeRepr))
    go (I.TUInt n) = do
      Some nr <- liftMaybe $ sizeToRepr n
      W4.LeqProof <- liftMaybe $ W4.isPosNat nr
      return $ Some (W4.BaseBVRepr nr)
    go (I.TSInt n) = do
      Some nr <- liftMaybe $ sizeToRepr n
      W4.LeqProof <- liftMaybe $ W4.isPosNat nr
      return $ Some (W4.BaseBVRepr nr)
    go (TInteger) = return (Some W4.BaseIntegerRepr)
    go (TBool) = return (Some W4.BaseBoolRepr)
    go TUnit = return (Some (W4.BaseStructRepr Ctx.Empty))
    go (TMaybe t) = do
      Some repr <- go t
      return $ (Some (W4.BaseStructRepr (Ctx.Empty Ctx.:> W4.BaseBoolRepr Ctx.:> repr)))
    go (TMap tkey tvalue) = do
      Some rkey <- go tkey
      Some rvalue <- go tvalue
      return $ (Some (W4.BaseArrayRepr (Ctx.Empty Ctx.:> rkey) rvalue))
    go (TUser ut) | [] <- utTyArgs ut, [] <- utNumArgs ut = do
      lookupTName (utName ut)
    go _ = panic "typeToRepr: unsupported type" [showPP v]

mkFlds :: [(Label, Type)] -> W4SolverT sym m (Some (Ctx.Assignment W4.BaseTypeRepr))
mkFlds lbls = Ctx.fromList <$> mapM (\(_,t) -> typeToRepr t) lbls

lookupTName :: TName -> W4SolverT sym m (Some (W4.BaseTypeRepr))
lookupTName nm = do
  tdefs <- getTypeDefs
  case Map.lookup nm tdefs of
    Just tdecl -> case tDef tdecl of
      TUnion flds -> do
        Some reprs <- mkFlds flds
        return $ Some (baseUnionRepr reprs)
      _ -> panic "lookupTName: unsupported type" [showPP nm]
    _ -> panic "lookupTName: missing type" [showPP nm]
  
getFieldIndex ::
  [(Label, Type)] -> 
  Ctx.Size tps ->
  Label -> 
  W4SolverT sym m (Some (Ctx.Index tps), Type)
getFieldIndex ((lbl,t):lbls) sz lblCheck = case Ctx.viewSize sz of
  Ctx.IncSize sz' -> case lbl == lblCheck of
    True -> return $ (Some (Ctx.lastIndex sz), t)
    False -> do
      (Some idx, t') <- getFieldIndex lbls sz' lblCheck
      return (Some (Ctx.skipIndex idx), t')
  Ctx.ZeroSize -> panic "getFieldIndex" ["Missing field:", showPP lblCheck]
getFieldIndex [] _ lblCheck = panic "getFieldIndex" ["Missing field:", showPP lblCheck]
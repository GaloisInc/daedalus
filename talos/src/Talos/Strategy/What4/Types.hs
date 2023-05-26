{-|
 
Conversion from Daedalus types into What4 types

-}


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
{-# Language MultiParamTypeClasses #-}

module Talos.Strategy.What4.Types(
  -- interpreting Daedalus types
    typeToRepr
  , lookupTName
  , getFieldIndex
  , mkFlds
  , module Talos.Strategy.What4.BaseTypes
) where

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
import           Talos.Strategy.What4.BaseTypes

-- Resolving types

sizeToRepr :: I.SizeType -> Maybe (Some (W4.NatRepr))
sizeToRepr (I.TSize n) = W4.someNat n
sizeToRepr (I.TSizeParam{}) = Nothing

-- | Converts a 'I.Type' into a 'W4.BaseTypeRepr', using the typing context
--   from 'W4SolverT' to resolve user-defined types.
--   FIXME: Is not nearly complete, but supports basic primitive types
--   as well as maps, arrays and Maybe types.
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
    go (TArray tvalue) = do
      Some rvalue <- go tvalue
      return $ (Some (ArrayLenRepr rvalue))
    go (TUser ut) | [] <- utTyArgs ut, [] <- utNumArgs ut = do
      lookupTName (utName ut)
    go t = panic "typeToRepr: unsupported type" [showPP v, showPP t]

-- | Converts a labeled list of fields into a 'Ctx.Assignment' of 'W4.BaseTypeRepr's
--   based on their corresponding 'I.Type'
mkFlds :: [(Label, Type)] -> W4SolverT sym m (Some (Ctx.Assignment W4.BaseTypeRepr))
mkFlds lbls = Ctx.fromList <$> mapM (\(_,t) -> typeToRepr t) lbls

-- | Resolve a 'TName' to a used-defined type and then convert it into
--   a 'W4.BaseTypeRepr'.
--   FIXME: currently only handles union types
--   FIXME: panics if the name is not bound in the environment
lookupTName :: TName -> W4SolverT sym m (Some (W4.BaseTypeRepr))
lookupTName nm = do
  tdefs <- getTypeDefs
  case Map.lookup nm tdefs of
    Just tdecl -> case tDef tdecl of
      TUnion flds -> do
        Some reprs <- mkFlds flds
        return $ Some (BaseUnionRepr reprs)
      _ -> panic "lookupTName: unsupported type" [showPP nm]
    _ -> panic "lookupTName: missing type" [showPP nm]
  
-- | Given a list of labelled types and a target 'Ctx.Size' (i.e. the size of the
--   corresponding 'Ctx.Assignment' that resulted from calling 'mkFlds'),
--   takes a 'Label' and returns the index into the 'Ctx.Assignment' and the corresponding 'Type'
--   in the original list associated with that 'Label'.
--   FIXME: panics if there is a length mismatch or if the label is missing
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
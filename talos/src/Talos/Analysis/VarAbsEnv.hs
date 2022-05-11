{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Abstract environments based on sets of variables

module Talos.Analysis.VarAbsEnv where

import           Data.Map              (Map)
import qualified Data.Map              as Map

import           Daedalus.Core         (Name)

import Talos.Analysis.AbsEnv
import Daedalus.Core.Free (FreeVars(..))
import Talos.Analysis.Eqv (Eqv)
import Data.Proxy (Proxy(Proxy))
import Talos.Analysis.SLExpr (exprToSLExpr)
import Daedalus.PP

varAbsEnvTy :: AbsEnvTy
varAbsEnvTy = AbsEnvTy (Proxy @VarAbsEnv)

-- We want the whole value
data Whole = Whole
  deriving (Eq, Ord)

instance Eqv Whole -- default

newtype VarAbsEnv = VarAbsEnv (LiftAbsEnv Whole)
  deriving (Eqv, PP, AbsEnv)

freeToAbsEnv :: FreeVars a => a -> Map Name Whole
freeToAbsEnv = Map.fromSet (const Whole) . freeVars

instance AbsEnvPointwise Whole where
  absPredPre _ e   = (freeToAbsEnv e, exprToSLExpr e)
  absPredGuard     = freeToAbsEnv
  absPredByteSet _ = freeToAbsEnv
  absPredCase _    = Whole
  absPredInverse n e1 e2 =
    Map.delete n $ Map.union (freeToAbsEnv e1) (freeToAbsEnv e2)
  absPredUnion _ _ = Whole

instance PP Whole where
  pp _ = "Whole"

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
import Talos.Analysis.Merge (Merge(..))

varAbsEnvTy :: AbsEnvTy
varAbsEnvTy = AbsEnvTy (Proxy @VarAbsEnv)

-- We want the whole value
data Whole = Whole
  deriving (Eq, Ord)

instance Eqv Whole -- default
instance Merge Whole where
  merge _ _ = Whole

instance AbsEnvPred Whole where
  absPredTop = Whole
  absPredOverlaps _ _ = True

  absPredEntails _ _ = True
  absPredIsStructural _ = True
  absPredListElement _ = Whole
  absPredCollection _ _ _ _ = Whole
  
newtype VarAbsEnv = VarAbsEnv (LiftAbsEnv Whole)
  deriving (Eqv, PP, Merge, AbsEnv)

freeToAbsEnv :: FreeVars a => a -> Map Name Whole
freeToAbsEnv = Map.fromSet (const Whole) . freeVars

instance AbsEnvPointwise Whole where
  absPredPre _ e   = (LiftAbsEnv (freeToAbsEnv e), exprToSLExpr e)
  absPredByteSet _ = LiftAbsEnv . freeToAbsEnv
  absPredInverse n e1 e2 =
    LiftAbsEnv $ Map.delete n $
      Map.union (freeToAbsEnv e1) (freeToAbsEnv e2)

instance PP Whole where
  pp _ = "Whole"


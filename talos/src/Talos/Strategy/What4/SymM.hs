
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Talos.Strategy.What4.SymM(
    SymM(..)
  , SomeSymFn(..)
) where

import           Control.Monad.IO.Class
import           Data.Kind                       as DK
import           Data.Parameterized.Some

import qualified What4.Interface                 as W4

import           Daedalus.Core                   hiding (streamOffset)

import           Talos.Strategy.Monad
import           Data.Parameterized.SymbolRepr

data SomeSymFn sym = forall args ret. SomeSymFn (W4.SymFn sym args ret)

class (MonadFail m, W4.IsSymExprBuilder sym, Monad m, MonadIO m, LiftStrategyM m) => SymM sym m | m -> sym where
  type CacheKey sym (nm :: Symbol) :: DK.Type
  type CacheValue sym (nm :: Symbol) :: DK.Type

  withSym :: (sym -> m a) -> m a
  liftMaybe :: Maybe a -> m a
  bindVarIn :: Name -> W4.SymExpr sym tp -> m a -> m a
  getVar :: Name -> m (Some (W4.SymExpr sym))
  withFNameCache :: FName -> m (SomeSymFn sym, Fun Expr) -> m (SomeSymFn sym, Fun Expr)
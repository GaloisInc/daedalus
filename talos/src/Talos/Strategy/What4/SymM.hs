
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Talos.Strategy.What4.SymM(
   SomeSymFn(..)
 , SomeW4SolverT
 , W4SolverT_
 , W4SolverT
 , W4SolverEnv
 , SomeW4SolverEnv
 , mkSomeW4SolverEnv
 , initSomeW4SolverEnv
 , asSomeSolver
 , runSomeW4Solver
 , withSym
 , bindVarIn
 , getVar
 , liftMaybe
 , withFNameCache
) where

import           Control.Monad.IO.Class
import           Data.Parameterized.Some

import qualified What4.Interface                 as W4

import           Daedalus.Core                   hiding (streamOffset)
import           Daedalus.Panic
import           Daedalus.PP

import           Talos.Strategy.Monad
import Data.Parameterized.Classes
import qualified What4.ProgramLoc as W4
import qualified Data.IORef as IO
import qualified Data.Map as Map
import Control.Monad.Reader
import Unsafe.Coerce (unsafeCoerce)
import qualified System.IO.Unsafe as IO
import Control.Applicative (Alternative)


data SomeSymFn sym = forall args ret. SomeSymFn (W4.SymFn sym args ret)

type NameEnv sym = Map.Map Name (Some (W4.SymExpr sym))

type FnEnv sym = Map.Map FName (SomeSymFn sym, Fun Expr)

-- This could also do a better job of having context-sensitive expression caches, which would cut
-- down translation time, depending on how much it ends up dominating time of the strategy
data W4SolverEnv sym = W4.IsSymExprBuilder sym => W4SolverEnv { sym_ :: sym, nameEnv :: NameEnv sym, fnCache :: IO.IORef (FnEnv sym) }

newtype W4SolverT_ sym m a = W4SolverT_ { _unW4SolverT :: ReaderT (W4SolverEnv sym) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (W4SolverEnv sym), 
            MonadTrans, LiftStrategyM, Alternative, MonadPlus
          )

type W4SolverT sym m a = (W4.IsSymExprBuilder sym, LiftStrategyM m, MonadIO m, Monad m) => W4SolverT_ sym m a

instance MonadIO m => MonadFail (W4SolverT_ sym m) where
  fail msg = liftIO $ fail msg

-- Opaque newtype that stands for our top-level expression builder
-- This is never exported, since the interface abstracts it away (see: 'asSomeSolver')
data Sym

unsafeCoerceToSym :: W4.IsSymExprBuilder sym => sym -> (sym :~: Sym)
unsafeCoerceToSym _ = unsafeCoerce Refl

data SomeSym = forall sym. W4.IsSymExprBuilder sym => SomeSym sym

-- | Singleton reference for a global handle on a single expression builder
singletonSym_ref :: IO.IORef (Maybe SomeSym)
singletonSym_ref = IO.unsafePerformIO (IO.newIORef Nothing)

-- | This is a sanity check that initializes the global expression builder,
--   and checks that any subsequent attempts at initialization necessarily use
--   the same builder.
initSym :: W4.IsSymExprBuilder sym => sym -> IO (sym :~: Sym)
initSym sym1 = do
  msym2 <- IO.atomicModifyIORef' singletonSym_ref $ \msym -> case msym of
    Just (SomeSym sym2) -> (msym, Just (SomeSym sym2))
    Nothing -> (Just (SomeSym sym1), Nothing)
  case msym2 of
    Just (SomeSym sym2) -> testSymEquality sym1 sym2 >>= \case
      Just Refl -> return $ unsafeCoerceToSym sym1
      Nothing -> fail "attempted to initialize Sym to different builder"
    Nothing -> return $ unsafeCoerceToSym sym1

newtype SomeW4SolverT m a = SomeW4SolverT (W4SolverT_ Sym m a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, LiftStrategyM, Alternative, MonadPlus)

newtype SomeW4SolverEnv = SomeW4SolverEnv (W4SolverEnv Sym)

initW4SolverEnv ::
  W4.IsSymExprBuilder sym =>
  sym ->
  IO (W4SolverEnv sym)
initW4SolverEnv sym = do
  fnCacheRef <- IO.newIORef mempty
  return $ W4SolverEnv sym mempty fnCacheRef

mkSomeW4SolverEnv :: 
  W4.IsSymExprBuilder sym =>
  W4SolverEnv sym ->
  IO (SomeW4SolverEnv)
mkSomeW4SolverEnv env = do
  Refl <- initSym (sym_ env)
  return $ SomeW4SolverEnv env

asSomeSolver ::
  (forall sym. W4SolverT_ sym m a) ->
  SomeW4SolverT m a
asSomeSolver f = SomeW4SolverT f

runSomeW4Solver ::
  MonadIO m =>
  SomeW4SolverEnv ->
  SomeW4SolverT m a -> 
  m a
runSomeW4Solver (SomeW4SolverEnv env) (SomeW4SolverT (W4SolverT_ f)) = runReaderT f env

initSomeW4SolverEnv ::
  W4.IsSymExprBuilder sym =>
  MonadIO m =>
  sym ->
  m SomeW4SolverEnv
initSomeW4SolverEnv sym = do
  env <- liftIO $ initW4SolverEnv sym
  liftIO $ mkSomeW4SolverEnv env


withSym :: Monad m => (sym -> W4SolverT_ sym m a) -> W4SolverT_ sym m a
withSym f = do
  W4SolverEnv{} <- ask
  sym <- asks sym_
  f sym

bindVarIn :: Monad m => Name -> W4.SymExpr sym tp -> W4SolverT_ sym m a -> W4SolverT_ sym m a
bindVarIn nm e f = local (\env -> env { nameEnv = Map.insert nm (Some e) (nameEnv env)}) f

getVar :: Monad m => Name -> W4SolverT_ sym m (Some (W4.SymExpr sym))
getVar nm = do
  env <- asks nameEnv
  case Map.lookup nm env of
    Just se -> return se
    Nothing -> panic "Unbound variable" [showPP nm]

liftMaybe :: Monad m => Maybe a -> W4SolverT_ sym m a
liftMaybe (Just a) = return a
liftMaybe Nothing = panic "liftMaybe" []

withFNameCache :: MonadIO m => FName -> W4SolverT_ sym m (SomeSymFn sym, Fun Expr) -> W4SolverT_ sym m (SomeSymFn sym, Fun Expr)
withFNameCache fname f = do
  ref <- asks fnCache
  cache <- liftIO $ IO.readIORef ref
  case Map.lookup fname cache of
    Just a -> return a
    Nothing -> do
      a <- f
      liftIO $ IO.modifyIORef' ref (Map.insert fname a)
      return a

-- | Hack to check if both expression builders are the same.
--   If they have the same IO reference to control the "current" program location
--   then they are necessarily the same builder.
testSymEquality ::
  W4.IsSymExprBuilder sym1 =>
  W4.IsSymExprBuilder sym2 =>
  sym1 ->
  sym2 ->
  IO (Maybe (sym1 :~: sym2))
testSymEquality sym1 sym2 = do
  loc1 <- W4.getCurrentProgramLoc sym1
  W4.setCurrentProgramLoc sym1 (W4.mkProgramLoc "loc1" W4.InternalPos)
  loc2 <- W4.getCurrentProgramLoc sym2
  let loc2' = W4.mkProgramLoc "loc2" W4.InternalPos
  W4.setCurrentProgramLoc sym2 loc2'
  loc1' <- W4.getCurrentProgramLoc sym1
  -- undo the program loc mangling
  W4.setCurrentProgramLoc sym1 loc1
  W4.setCurrentProgramLoc sym2 loc2
  case loc1' == loc2' of
    True -> return (Just (unsafeCoerce Refl))
    False -> return Nothing
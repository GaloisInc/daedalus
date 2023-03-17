
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
{-# LANGUAGE FlexibleInstances #-}

module Talos.Strategy.What4.SymM(
   SomeSymFn(..)
 , W4SolverT_
 , W4SolverT
 , W4SolverEnv
 , SomeW4SolverEnv(..)
 , withSomeSolverEnv
 , runW4Solver
 , withSym
 , bindVarIn
 , getVar
 , liftMaybe
 , withFNameCache
 , withLocalFunction
 , addAssumption
 , collapseAssumptions
) where

import           Control.Applicative (Alternative)
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified System.IO.Unsafe                as IO

import qualified Data.IORef                      as IO
import qualified Data.Map                        as Map

import           Data.Parameterized.Some
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce        as N

import qualified What4.Interface                 as W4
import qualified What4.ProgramLoc                as W4
import qualified What4.Expr                      as WE

import           Daedalus.Core                   hiding (streamOffset)
import           Daedalus.Panic
import           Daedalus.PP

import           Talos.Strategy.Monad
import           Talos.Strategy.What4.Solver

import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Catch
import qualified Data.Kind as DK
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class


data SomeSymFn sym = forall args ret. SomeSymFn
  { someSymFn :: W4.SymFn sym args ret
  , someSymFnAsms :: W4.SymFn sym args W4.BaseBoolType
  }

type NameEnv sym = Map.Map Name (Some (W4.SymExpr sym))

type FnEnv sym = Map.Map FName (SomeSymFn sym, Fun Expr)

-- This could also do a better job of having context-sensitive expression caches, which would cut
-- down translation time, depending on how much it ends up dominating time of the strategy
data W4SolverEnv sym = W4.IsSymExprBuilder sym => 
  W4SolverEnv { solver :: SolverSym sym, nameEnv :: NameEnv sym, fnCache :: IO.IORef (FnEnv sym),
                local_fns :: Map.Map FName (SomeSymFn sym, Fun Expr)
              }

withInitEnv :: 
  (MonadIO m, MonadMask m) =>
  (forall sym. W4SolverEnv sym -> m a) -> m a
withInitEnv f = do
  Some gen <- liftIO N.newIONonceGenerator
  sym <- liftIO $ WE.newExprBuilder WE.FloatRealRepr WE.EmptyExprBuilderState gen
  liftIO $ WE.startCaching sym
  withOnlineSolver Yices Nothing sym $ \bak -> do
    let ssym = SolverSym sym bak
    fnCacheRef <- liftIO $ IO.newIORef mempty
    f (W4SolverEnv ssym mempty fnCacheRef mempty)

withSymEnv :: W4SolverEnv sym -> (W4.IsSymExprBuilder sym => sym -> a) -> a
withSymEnv (W4SolverEnv (SolverSym sym _) _ _ _) f = f sym


withSomeSolverEnv ::
  (MonadIO m, MonadMask m) =>
  (SomeW4SolverEnv -> m a) -> m a
withSomeSolverEnv f = withInitEnv $ \env -> withSymEnv env $ \_sym ->
  f (SomeW4SolverEnv env)

data Assumption sym = OrdF (W4.SymExpr sym) => Assumption (W4.Pred sym)

instance Eq (Assumption sym) where
  (Assumption p1) == (Assumption p2) = case testEquality p1 p2 of
    Just Refl -> True
    _ -> False

instance Ord (Assumption sym) where
  compare (Assumption p1) (Assumption p2) = toOrdering (compareF p1 p2)

newtype Assumptions sym = Assumptions (Set (Assumption sym))
  deriving (Monoid, Semigroup)

newtype W4SolverT_ sym m a = W4SolverT_ { _unW4SolverT :: WriterT (Assumptions sym) (ReaderT (W4SolverEnv sym) m) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (W4SolverEnv sym), 
            LiftStrategyM, Alternative, MonadPlus, MonadCatch, MonadThrow, MonadMask,
            MonadWriter (Assumptions sym)
          )

instance MonadTrans (W4SolverT_ sym) where
  lift m = W4SolverT_(lift $ lift m)

instance (MonadIO m, MonadCatch m, MonadThrow m, MonadMask m) => SolverM sym (W4SolverT_ sym m) where
  getSolverSym = asks solver

type W4SolverT sym m a = 
  (W4.IsSymExprBuilder sym, LiftStrategyM m, MonadIO m, Monad m, MonadMask m, MonadCatch m, MonadThrow m) => 
    W4SolverT_ sym m a

instance MonadIO m => MonadFail (W4SolverT_ sym m) where
  fail msg = liftIO $ fail msg

-- Opaque type that stands for our top-level expression builder
-- This is never exported, since the interface abstracts it away (see: 'asSomeSolver')
-- FIXME: unused currently

type Sym = SymOf SomeSym
type family SymOf t :: DK.Type

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

data SomeW4SolverEnv = forall sym. W4.IsSymExprBuilder sym => SomeW4SolverEnv (W4SolverEnv sym)

runW4Solver ::
  MonadIO m =>
  W4SolverEnv sym ->
  W4SolverT_ sym m a ->
  m (a, W4.Pred sym)
runW4Solver env (W4SolverT_ f) = do
  (a, asms) <- runReaderT (runWriterT f) env
  p <- withSymEnv env $ \sym -> collapseAssumptions sym asms
  return (a, p)

addAssumption ::
  W4.Pred sym ->
  W4SolverT sym m ()
addAssumption p = tell (Assumptions (Set.singleton (Assumption p)))

collapseAssumptions ::
  MonadIO m =>
  W4.IsSymExprBuilder sym =>
  sym ->
  Assumptions sym ->
  m (W4.Pred sym)
collapseAssumptions sym (Assumptions asms) =
  foldM (\a (Assumption b) -> liftIO (W4.andPred sym a b)) (W4.truePred sym) asms

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

-- | withLocalFunction overrides the result of 'withFNameCache'
withLocalFunction :: Monad m => FName -> SomeSymFn sym -> Fun Expr -> W4SolverT_ sym m a -> W4SolverT_ sym m a
withLocalFunction fname fn fne f = local (\env -> env { local_fns = Map.insert fname (fn,fne) (local_fns env )}) f

withFNameCache :: MonadIO m => FName -> W4SolverT_ sym m (SomeSymFn sym, Fun Expr) -> W4SolverT_ sym m (SomeSymFn sym, Fun Expr)
withFNameCache fname f = do
  locals <- asks local_fns
  case Map.lookup fname locals of
    Just a -> return a
    Nothing -> do
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
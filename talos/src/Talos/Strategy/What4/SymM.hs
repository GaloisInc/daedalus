{-|
 
Provides the 'W4SolverT' monad transformer for adding a solver connection
and variable binding environment to a given monad. Additionally it
supports adding assumptions that are introduced along any execution path
(i.e. with multiple paths for an 'Alternative' monad).

-}


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
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Talos.Strategy.What4.SymM(
   SomeSymFn(..)
 , W4SolverT_
 , W4SolverT
 , W4SolverEnv
 , rec_limit
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
 , checkAsms
 , viewSolverEnv
 , mkW4SolverT
, runW4Solver') where

import           Control.Applicative (Alternative)
import           Control.Monad.Reader
import qualified Data.IORef                      as IO
import qualified Data.Map                        as Map

import           Data.Parameterized.Some
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce        as N

import qualified What4.Interface                 as W4
import qualified What4.Expr                      as WE

import           Daedalus.Core                   hiding (streamOffset)
import           Daedalus.Panic
import           Daedalus.PP

import           Talos.Strategy.Monad
import           Talos.Strategy.What4.Solver

import Control.Monad.Catch
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Writer (WriterT (WriterT), runWriterT)
import Control.Monad.Writer.Class

-- | A wrapper around 'W4.SymFn' that existentially quantifies over the argument
-- and return types for the function.
-- Additionally it contains a secondary 'W4.SymFn' that represents any assumptions
-- that should be added to current execution path if this function is executed.
data SomeSymFn sym = forall args ret. SomeSymFn
  { someSymFn :: W4.SymFn sym args ret
  , someSymFnAsms :: W4.SymFn sym args W4.BaseBoolType
  }

type NameEnv sym = Map.Map Name (Some (W4.SymExpr sym))

type FnEnv sym = Map.Map FName (SomeSymFn sym, Fun Expr)

-- | The execution environment that is attached when using 'W4SolverT'.
data W4SolverEnv sym = W4.IsSymExprBuilder sym => 
  W4SolverEnv {   solver :: SolverSym sym
                -- ^ expression builder and online solver connection
                , nameEnv :: NameEnv sym
                -- ^ binding environment for local variables
                , fnCache :: IO.IORef (FnEnv sym)
                -- ^ cached results for function translations
                , local_fns :: FnEnv sym
                -- ^ a local function-binding environment, which takes
                -- precedence over the cache
                , rec_limit :: Integer
                -- ^ limit to use when unwinding recursive functions.
                -- FIXME: recursion support is still highly experimental
              }

-- | Initialize a fresh 'W4SolverEnv' with empty binding environments, 
-- a new expression builder and default online solver connection (currently Z3),
-- which is valid in the given continuation.
withInitEnv :: 
  (MonadIO m, MonadMask m) =>
  (forall sym. W4.IsSymExprBuilder sym => W4SolverEnv sym -> m a) -> m a
withInitEnv f = do
  Some gen <- liftIO N.newIONonceGenerator
  (sym :: sym) <- liftIO $ WE.newExprBuilder WE.FloatRealRepr WE.EmptyExprBuilderState gen
  liftIO $ WE.startCaching sym
  withOnlineSolver Z3 Nothing sym $ \bak -> do
    let ssym = SolverSym sym bak
    fnCacheRef <- liftIO $ IO.newIORef mempty
    f (W4SolverEnv ssym mempty fnCacheRef mempty 1)

-- | Retrieve the expression builder and associated type constraint from
-- a 'W4SolverEnv'
withSymEnv :: W4SolverEnv sym -> (W4.IsSymExprBuilder sym => sym -> a) -> a
withSymEnv (W4SolverEnv (SolverSym sym _) _ _ _ _) f = f sym

-- Wraps 'withInitEnv' with an existentially-quantified 'W4SolverEnv'
withSomeSolverEnv ::
  (MonadIO m, MonadMask m) =>
  (Some W4SolverEnv -> m a) -> m a
withSomeSolverEnv f = withInitEnv $ \env -> f (Some env)

-- Establish 'W4.IsSymExprBuilder' for 'sym' from a 'W4SolverEnv'
viewSolverEnv ::
  W4SolverEnv sym -> (W4.IsSymExprBuilder sym => a) -> a
viewSolverEnv (W4SolverEnv{}) f = f

-- | Internal assumption type, used simply to establish an 'Ord' instance
-- for 'W4.Pred' so we can use it in a 'Set'
data Assumption sym = OrdF (W4.SymExpr sym) => Assumption (W4.Pred sym)

instance Eq (Assumption sym) where
  (Assumption p1) == (Assumption p2) = case testEquality p1 p2 of
    Just Refl -> True
    _ -> False

instance Ord (Assumption sym) where
  compare (Assumption p1) (Assumption p2) = toOrdering (compareF p1 p2)

-- | A collection of What4 predicates (set union is implicitly conjunction)
newtype Assumptions sym = Assumptions (Set (Assumption sym))
  deriving (Monoid, Semigroup)

-- | A simple monad transformer that can collect ad-hoc assumptions as part of its output, with an associated
-- online solver connection and variable binding environments.
-- NOTE: We have 'W4SolverT_' and 'W4SolverT'. This (the former) defines the actual type, while
-- the latter adds the collection of type constraints that are normally applied.
newtype W4SolverT_ sym m a = W4SolverT_ { _unW4SolverT :: WriterT (Assumptions sym) (ReaderT (W4SolverEnv sym) m) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (W4SolverEnv sym), 
            LiftStrategyM, Alternative, MonadPlus, MonadCatch, MonadThrow, MonadMask,
            MonadWriter (Assumptions sym)
          )

mkW4SolverT ::
  (W4SolverEnv sym -> m (a, Assumptions sym)) ->
  W4SolverT_ sym m a
mkW4SolverT f = W4SolverT_ (WriterT (ReaderT f))

instance MonadTrans (W4SolverT_ sym) where
  lift m = W4SolverT_(lift $ lift m)

instance (MonadIO m, MonadCatch m, MonadThrow m, MonadMask m) => SolverM sym (W4SolverT_ sym m) where
  getSolverSym = asks solver

-- | Alias to 'W4SolverT_' with a default set of type constraints.
--   This can be used for most function signatures, but the 'W4SolverT_' is
--   needed for type class instances, etc.
type W4SolverT sym m a = 
  (W4.IsSymExprBuilder sym, LiftStrategyM m, MonadIO m, Monad m, MonadMask m, MonadCatch m, MonadThrow m) => 
    W4SolverT_ sym m a

instance MonadIO m => MonadFail (W4SolverT_ sym m) where
  fail msg = liftIO $ fail msg

-- | Run a 'W4SolverT' in the given environment, and return the result along
-- with a conjunction of any assumptions that were emitted during execution
-- (i.e. with 'addAssumption')
runW4Solver ::
  MonadIO m =>
  W4SolverEnv sym ->
  W4SolverT_ sym m a ->
  m (a, W4.Pred sym)
runW4Solver env (W4SolverT_ f) = do
  (a, asms) <- runReaderT (runWriterT f) env
  p <- withSymEnv env $ \sym -> collapseAssumptions sym asms
  return (a, p)

-- | Same as 'runW4Solver' but leaves the result as an 'Assumptions' instead'
--   of producing the predicate
runW4Solver' ::
  MonadIO m =>
  W4SolverEnv sym ->
  W4SolverT_ sym m a ->
  m (a, Assumptions sym)
runW4Solver' env (W4SolverT_ f) = runReaderT (runWriterT f) env

-- | Add an assumption to the current computation path. Note that this does not
-- affect the context of the solver (i.e. in contrast to 'What4.Solver.withAssumption'),
-- but simply adds to the output of the 'W4SolverT' execution.
addAssumption ::
  W4.Pred sym ->
  W4SolverT sym m ()
addAssumption p = case W4.asConstantPred p of
  Just False -> panic "Cannot add false assumption" []
  Just True -> return ()
  _ -> tell (Assumptions (Set.singleton (Assumption p)))

collapseAssumptions ::
  MonadIO m =>
  W4.IsSymExprBuilder sym =>
  sym ->
  Assumptions sym ->
  m (W4.Pred sym)
collapseAssumptions sym (Assumptions asms) =
  foldM (\a (Assumption b) -> liftIO (W4.andPred sym a b)) (W4.truePred sym) asms

-- | Collect the assumptions emitted from the inner computation and
-- check that they are not inconsistent (i.e. trivially false) 
-- before returning, panicking otherwise.
-- FIXME: We can consider using the solver here as well.
checkAsms ::
  [String] {- ^ debugging message to attach if the resulting assumptions are inconsistent -} ->
  W4SolverT_ sym m a ->
  W4SolverT sym m a
checkAsms msg f = withSym $ \sym -> do
  (a, asms) <- listen f
  p <- collapseAssumptions sym asms
  case W4.asConstantPred p of
    Just False -> panic "False assumptions" msg
    _ -> return a  
 
-- | Execute the given continuation in a context where the given
-- 'Name' is bound to the given 'W4.SymExpr'.
-- NOTE: This does not validate the type in the 'Name' (i.e. 'nameType')
-- against the type of the expression (i.e. 'tp')
bindVarIn :: 
  Monad m => 
  Name {- ^ name to bind -} -> 
  W4.SymExpr sym tp {- ^ expression to bind to the name -} -> 
  W4SolverT_ sym m a {- ^ continuation to run with the binding in scope -} -> 
  W4SolverT_ sym m a
bindVarIn nm e f = local (\env -> env { nameEnv = Map.insert nm (Some e) (nameEnv env)}) f

-- | Retrieves the 'W4.SymExpr' bound to the given 'Name' in the current environment.
-- Panics if the name is not bound.
-- TODO: Make a 'Maybe' variant of this.
getVar :: Monad m => Name -> W4SolverT_ sym m (Some (W4.SymExpr sym))
getVar nm = do
  env <- asks nameEnv
  case Map.lookup nm env of
    Just se -> return se
    Nothing -> panic "Unbound variable" [showPP nm]

liftMaybe :: Monad m => Maybe a -> W4SolverT_ sym m a
liftMaybe (Just a) = return a
liftMaybe Nothing = panic "liftMaybe" []

-- | Bind a 'FName' (function name) to a function to a 'SomeSymFn' (and corresponding 'Fun Expr')
-- in the current environment. In particular this is a local binding which overrides
-- any cached functions (i.e. as a result of using 'withFNameCache').
withLocalFunction :: Monad m => FName -> SomeSymFn sym -> Fun Expr -> W4SolverT_ sym m a -> W4SolverT_ sym m a
withLocalFunction fname fn fne f = local (\env -> env { local_fns = Map.insert fname (fn,fne) (local_fns env )}) f

-- | Returns the 'SomeSymFn' and 'Fun Expr' associated with the provided 'FName',
-- either by running the given 'f' or by finding the given function in the cache.
-- NOTE: This function is stateful, so a function definition should only be translated
-- once across all explored branches.
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
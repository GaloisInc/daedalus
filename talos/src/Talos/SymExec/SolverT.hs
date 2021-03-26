{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving #-}
{-# Language OverloadedStrings #-}

-- | Defines the symbolic parser API.  This wraps the SimpleSMT API

module Talos.SymExec.Monad (
  -- * Solver interaction monad
  SolverT, runSolverT, mapSolverT, emptySolverState,
  freshNameIn,
  nameToSMTName,
  fnameToSMTName,
  getName,
  SolverState,
  withSolver, scoped,
  -- assert, declare, check,
  liftSolverOp, solverState,
  
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Data.Set (Set)

import SimpleSMT (Solver, SExpr)
import qualified SimpleSMT as S

import Daedalus.GUID
import Daedalus.Panic
import Daedalus.PP

import Daedalus.Core hiding (freshName)
import qualified Daedalus.Core as C

data SolverState =
  SolverState { solver     :: Solver
              , knownTypes :: Set TName -- ^ We lazily define types, but their names are mapped directly
              , knownFuns  :: Set FName -- ^ Similarly for (pure) functions (incl. bytesets)
              , boundNames :: Map Name SExpr
              -- ^ May include names bound in closed scopes, to allow for
              --
              -- def P = { x = { $$ = UInt8; $$ > 10 }, ...}
              --
              -- where the execution of the body of x will return
              -- '$$' as it's symbolc result
              }

emptySolverState :: Solver -> SolverState
emptySolverState s = SolverState s mempty mempty mempty 

bindName :: Name -> SExpr -> SolverState -> SolverState
bindName k v st = st { boundNames = Map.insert k v (boundNames st) }

lookupName :: Name -> SolverState -> Maybe SExpr
lookupName k = Map.lookup k . boundNames

newtype SolverT m a = SolverT { getSolverT :: StateT SolverState m a }
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

withSolver :: Monad m => (Solver -> SolverT m a) -> SolverT m a
withSolver f = do
  s <- SolverT $ gets solver
  f s

-- -----------------------------------------------------------------------------
-- Solver operations, over SExprs 

liftSolverOp :: MonadIO m => (Solver -> IO a) -> SolverT m a
liftSolverOp f = SolverT $ do
  s <- gets solver
  liftIO (f s)
  
-- -- MonadIO would be enough here.
-- assert :: LiftStrategyM m => SExpr -> SolverT m ()
-- assert assn = liftSolverOp (\s -> S.assert s assn)

-- declare :: LiftStrategyM m => String -> SExpr -> SolverT m ()
-- declare n ty = SolverT $ do
--   s <- gets solver
--   void $ liftStrategy (liftIO (S.declare s n ty))

-- check :: LiftStrategyM m => String -> SExpr -> SolverT m S.Result
-- check 

-- -----------------------------------------------------------------------------
-- Names

nameToSMTName :: Name -> String
nameToSMTName n = show (maybe "_" pp (nameText n) <> "@" <> pp (nameId n))

fnameToSMTName :: FName -> String
fnameToSMTName n = show (maybe "_" pp (fnameText n) <> "@" <> pp (fnameId n))

-- symExecName :: Name -> SExpr
-- symExecName =  S.const . nameToSMTName

-- symExecFName :: FName -> SExpr
-- symExecFName =  S.const . fnameToSMTName


getName :: Monad m => Name -> SolverT m SExpr
getName n = do
  m_s <- SolverT (gets (lookupName n))
  case m_s of
    Just s -> pure s
    Nothing -> panic "Missing name" [showPP n]

freshNameIn :: (Monad m, HasGUID m) => Name -> (SExpr -> SolverT m a) -> SolverT m (SExpr, a)
freshNameIn n f = do
  n' <- lift (C.freshName n)
  let s = S.const (nameToSMTName n')
  (,) s <$> SolverT (withStateT (bindName n s) (getSolverT (f s)))

solverState :: Monad m => (SolverState -> m (a, SolverState)) -> SolverT m a
solverState f = do
  s <- SolverT get
  (a, s') <- lift (f s)
  SolverT (put s')
  pure a


-- FIXME: might be nicer if we have this monad take care of push/pop
-- and have an explicit stack of bound names etc.

-- Execute the monadic action and clean up the scope and state when it
-- completes.
scoped :: MonadIO m => SolverT m a -> SolverT m a
scoped m = do
  s  <- SolverT get
  -- Get the initial stack level from the solver to restore to later.
  l <- getLevel (solver s)
  liftIO (S.push (solver s))
  r <- m
  l' <- getLevel (solver s)
  liftIO (S.popMany (solver s) (l' - l))
  
  -- restore state as well, note that everything in the state is
  -- contextual or read-only  
  SolverT (put s)
  pure r
  where
    getLevel s = liftIO $ do
      res <- S.command s (S.fun "get-info" [S.const ":assertion-stack-levels"])
      pure $ case res of
        S.List [S.Atom ":assertion-stack-levels", S.Atom n]
          | Just lvl <- readMaybe n -> lvl
          | otherwise -> panic "Malformed level count" [n]
        _ -> panic "Unexpected get-info result" [show res]

runSolverT :: SolverT m a -> SolverState -> m (a, SolverState)
runSolverT (SolverT m) s = runStateT m s

mapSolverT :: (m (a, SolverState) -> n (b, SolverState)) -> SolverT m a -> SolverT n b
mapSolverT f (SolverT m) = SolverT (mapStateT f m)
  
-- -----------------------------------------------------------------------------
-- instances

instance MonadTrans SolverT where
  lift m = SolverT (lift m)

instance (Monad m, HasGUID m) => HasGUID (SolverT m) where
  guidState f = lift (guidState f)


-- instance Alternative m => Alternative (SolverT m) where
  

-- instance MonadPlus m => MonadPlus (SolverT m) where
  

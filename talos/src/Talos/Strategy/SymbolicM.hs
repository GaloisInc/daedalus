{-# Language GeneralizedNewtypeDeriving #-}

module Talos.Strategy.SymbolicM where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           SimpleSMT               (SExpr)

import           Daedalus.Core           (Name)

import           Talos.Strategy.DFST
import           Talos.Strategy.Monad
import           Talos.SymExec.Path
import           Talos.SymExec.SemiValue
import           Talos.SymExec.SolverT   (SolverT, pop, push)
import qualified Talos.SymExec.SolverT   as Solv

-- =============================================================================
-- Symbolic monad
--
-- We need:

--  * An environment mapping DDL variables to SMT variables --- note
--    we unfold loops etc. so we can have multiple occurences of the
--    same (DDL) variable.
--  * A continuation for the failure and success cases.  We will need
--    to be careful to pop contexts appropriately.
--  * A StrategyM

-- Records local definitions
type SymbolicEnv = Map Name (SemiValue SExpr)

emptySymbolicEnv :: SymbolicEnv
emptySymbolicEnv = mempty

newtype SymbolicM a =
  SymbolicM { _getSymbolicM :: ReaderT SymbolicEnv (DFST (Maybe SelectedPath) (SolverT StrategyM)) a }
  deriving (Applicative, Functor, Monad, MonadIO, LiftStrategyM, MonadReader SymbolicEnv)

runSymbolicM :: SymbolicM SelectedPath -> SolverT StrategyM (Maybe SelectedPath)
runSymbolicM (SymbolicM m) =
  runDFST (runReaderT m emptySymbolicEnv) (pure . Just) (pure Nothing)

bindNameIn :: Name -> SemiValue SExpr -> SymbolicM a -> SymbolicM a
bindNameIn n v (SymbolicM m) = SymbolicM $ local (Map.insert n v) m

getName :: Name -> SymbolicM (SemiValue SExpr)
getName n = SymbolicM $ do
  m_local <- asks (Map.lookup n)
  case m_local of
    Nothing -> lift (lift (VOther <$> Solv.getName n))
    Just r  -> pure r


instance Alternative SymbolicM where
  (SymbolicM m1) <|> (SymbolicM m2) = SymbolicM $ do
    env <- ask
    lift $ bracketS (runReaderT m1 env) <|> bracketS (runReaderT m2 env)
    where
      bracketS m = do
        lift push
        m `onBacktrack` popFail
      popFail = lift pop >> mzero

  empty = SymbolicM empty

instance MonadPlus SymbolicM where -- default body (Alternative)

instance Semigroup a => Semigroup (SymbolicM a) where
  m1 <> m2 = (<>) <$> m1 <*> m2

instance Monoid a => Monoid (SymbolicM a) where
  mempty = pure mempty

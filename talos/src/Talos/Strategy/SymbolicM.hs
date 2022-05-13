{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Talos.Strategy.SymbolicM where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           SimpleSMT              (SExpr)

import           Daedalus.Core      (Name, Typed (..))
import           Daedalus.Core.Type (typeOf)

import           Talos.Strategy.Monad
import           Talos.Strategy.SearchT    (SearchT)
import qualified Talos.Strategy.SearchT    as ST
import           Talos.Strategy.SearchTree
import           Talos.SymExec.Path
import           Talos.SymExec.SemiValue
import           Talos.SymExec.SolverT     (SolverT)
import qualified Talos.SymExec.SolverT     as Solv


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

-- FIXME: this type is repeated from SemiExpr
type SymbolicEnv = Map Name (SemiValue (Typed SExpr))

emptySymbolicEnv :: SymbolicEnv
emptySymbolicEnv = mempty

newtype SymbolicM a =
  SymbolicM { _getSymbolicM :: ReaderT SymbolicEnv (SearchT (SolverT StrategyM)) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader SymbolicEnv)

instance LiftStrategyM SymbolicM where
  liftStrategy m = SymbolicM (liftStrategy m)

runSymbolicM :: SearchStrat ->
                SymbolicM SelectedPath ->
                SolverT StrategyM (Maybe SelectedPath)
runSymbolicM sstrat (SymbolicM m) = runSearchStrat sstrat (runReaderT m emptySymbolicEnv)

--------------------------------------------------------------------------------
-- Names

bindNameIn :: Name -> SemiValue (Typed SExpr) -> SymbolicM a -> SymbolicM a
bindNameIn n v (SymbolicM m) = SymbolicM $ local (Map.insert n v) m

getName :: Name -> SymbolicM (SemiValue (Typed SExpr))
getName n = SymbolicM $ do
  m_local <- asks (Map.lookup n)
  case m_local of
    Nothing -> lift (lift (VOther . Typed (typeOf n) <$> Solv.getName n))
    Just r  -> pure r

--------------------------------------------------------------------------------
-- Search operaations

-- -- Backtracking choice + random permutation
choose :: [a] -> SymbolicM a
choose bs = do
  sctxt <- inSolver Solv.getContext
  a <- SymbolicM (lift $ ST.choose bs)
  inSolver (Solv.restoreContext sctxt)
  pure a
  
backtrack :: SymbolicM a
backtrack = SymbolicM (lift ST.backtrack)

--------------------------------------------------------------------------------
-- Search strtegies

newtype SearchStrat = SearchStrat { runSearchStrat :: ST.SearchStrat (SolverT StrategyM) }

dfs, bfs, randDFS, randRestart :: SearchStrat
dfs = SearchStrat ST.dfs
bfs = SearchStrat ST.bfs

randDFS = SearchStrat $ ST.tree ch bt
  where
    ch :: forall n m. LiftStrategyM m => Location n () -> m (Location n ())
    ch loc =
      case locBranches loc of
        0 -> pure loc
        n -> do
          i <- randR (0, n - 1)
          ch (tryMove (downward i) loc)
      
    bt :: forall n m. LiftStrategyM m => Location n () -> m (Maybe (Location n ()))
    bt loc = traverse ch (forgetGoUp loc)

randRestart = SearchStrat $ ST.tree ch bt
  where
    ch :: forall n m. LiftStrategyM m => Location n () -> m (Location n ())
    ch loc =
      case locBranches loc of
        0 -> pure loc
        n -> do
          i <- randR (0, n - 1)
          ch (tryMove (downward i) loc)
      
    bt :: forall n m. LiftStrategyM m => Location n () -> m (Maybe (Location n ()))
    bt loc = traverse (ch . maximally upward) (forgetGoUp loc)

--------------------------------------------------------------------------------
-- Utilities

inSolver :: SolverT StrategyM a -> SymbolicM a
inSolver = SymbolicM . lift . lift

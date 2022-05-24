{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Talos.Strategy.SymbolicM where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Free
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import           SimpleSMT                (SExpr)

import           Daedalus.Core            (Expr, Name, Typed (..))
import           Daedalus.Core.Type       (typeOf)

import           Talos.Analysis.Exported  (ExpSlice)
import           Talos.Strategy.Monad
import           Talos.SymExec.Path
import           Talos.SymExec.SemiExpr   (SemiSExpr)
import           Talos.SymExec.SemiValue
import           Talos.SymExec.SolverT    (SolverT)
import qualified Talos.SymExec.SolverT    as Solv

--------------------------------------------------------------------------------
-- The free monad for searching

-- c.f. https://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html

type Result = (SemiSExpr, PathBuilder)

data ThreadF next =
  Choose [next]
  | Bind Name SymbolicEnv (SymbolicM Result) (Result -> next)
  | Backtrack (Set Name)
  deriving (Functor)

newtype SearchT m a = SearchT { getSearchT :: FreeT ThreadF m a }
  deriving (Applicative, Functor, Monad, MonadIO, LiftStrategyM, MonadTrans)

chooseST :: Monad m => [a] -> SearchT m a
chooseST xs = SearchT $ liftF (Choose xs)

backtrackST :: Monad m => Set Name -> SearchT m a
backtrackST = SearchT . liftF . Backtrack

bindST :: Monad m => Name -> SymbolicEnv -> SymbolicM Result ->
          (Result -> a) -> SearchT m a
bindST n e lhs rhs = SearchT $ liftF (Bind n e lhs rhs)

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

data SolverResultF a =
  ByteResult a
  | InverseResult (Map Name a) Expr -- The env. includes the result var.
  deriving (Functor, Foldable, Traversable)

-- Just so we can get fmap/traverse/etc.
type SolverResult = SolverResultF SemiSExpr

type PathBuilder = SelectedPathF SolverResult
type SearchT'  = SearchT (SolverT StrategyM)

emptySymbolicEnv :: SymbolicEnv
emptySymbolicEnv = mempty

newtype SymbolicM a =
  SymbolicM { getSymbolicM :: ReaderT SymbolicEnv SearchT' a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader SymbolicEnv)

instance LiftStrategyM SymbolicM where
  liftStrategy m = SymbolicM (liftStrategy m)

runSymbolicM :: SearchStrat ->
                -- | Slices for pre-run analysis
                [ExpSlice] ->
                SymbolicM Result ->
                SolverT StrategyM (Maybe Result)
runSymbolicM sstrat sls (SymbolicM m) = do
  runSearchStrat sstrat sls (runReaderT m emptySymbolicEnv)
  
--------------------------------------------------------------------------------
-- Names

bindNameIn :: Name -> SymbolicM Result
           -> (PathBuilder -> SymbolicM a) -> SymbolicM a
bindNameIn n lhs rhs = join (SymbolicM res)
  where
    res = do
      e <- ask
      lift $ bindST n e lhs (\(v, pb) -> local (Map.insert n v) (rhs pb))
 
getName :: Name -> SymbolicM (SemiValue (Typed SExpr))
getName n = SymbolicM $ do
  m_local <- asks (Map.lookup n)
  case m_local of
    Nothing -> lift (lift (VOther . Typed (typeOf n) <$> Solv.getName n))
    Just r  -> pure r

--------------------------------------------------------------------------------
-- Search operaations

choose :: [a] -> SymbolicM a
choose bs = SymbolicM (lift (chooseST bs))
  
backtrack :: Set Name -> SymbolicM a
backtrack xs = SymbolicM (lift (backtrackST xs))

--------------------------------------------------------------------------------
-- Search strtegies

-- Note the search strat is responsible for doing solver context mgmt.

newtype SearchStrat = SearchStrat
  { runSearchStrat :: [ExpSlice] -> SearchT' Result ->
                      SolverT StrategyM (Maybe Result) }

dfs :: SearchStrat
dfs = SearchStrat $ \_ m -> do
  sc <- Solv.getContext
  go [(sc, m)]
  where
    go :: forall r. [(Solv.SolverContext, SearchT' r)] ->
          SolverT StrategyM (Maybe r)
    go [] = pure Nothing
    go ((sc, x) : xs) = do
      Solv.restoreContext sc
      r <- runFreeT (getSearchT x)
      case r of
        Free (Choose xs') -> do
          sc' <- Solv.getContext
          go (map ((,) sc' . SearchT) xs' ++ xs)
        Free (Backtrack {})     -> go xs
        Free (Bind _n e lhs rhs) -> do
          sc' <- Solv.getContext
          let m' = runReaderT (getSymbolicM lhs) e >>= SearchT . rhs
          go ((sc', m') : xs)
        Pure res -> pure (Just res)

-- dfs, bfs, randDFS, randRestart :: SearchStrat
-- dfs = SearchStrat ST.dfs
-- bfs = SearchStrat ST.bfs

-- randDFS = SearchStrat $ ST.tree ch bt
--   where
--     ch :: forall n m. LiftStrategyM m => Location n () -> m (Location n ())
--     ch loc =
--       case locBranches loc of
--         0 -> pure loc
--         n -> do
--           i <- randR (0, n - 1)
--           ch (tryMove (downward i) loc)
      
--     bt :: forall n m. LiftStrategyM m => Location n () -> m (Maybe (Location n ()))
--     bt loc = traverse ch (forgetGoUp loc)

-- randRestart = SearchStrat $ ST.tree ch bt
--   where
--     ch :: forall n m. LiftStrategyM m => Location n () -> m (Location n ())
--     ch loc =
--       case locBranches loc of
--         0 -> pure loc
--         n -> do
--           i <- randR (0, n - 1)
--           ch (tryMove (downward i) loc)
      
--     bt :: forall n m. LiftStrategyM m => Location n () -> m (Maybe (Location n ()))
--     bt loc = traverse (ch . maximally upward) (forgetGoUp loc)

--------------------------------------------------------------------------------
-- Utilities

inSolver :: SolverT StrategyM a -> SymbolicM a
inSolver = SymbolicM . lift . lift

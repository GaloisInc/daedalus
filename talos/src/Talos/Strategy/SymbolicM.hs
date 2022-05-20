{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Talos.Strategy.SymbolicM where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           SimpleSMT              (SExpr)
import qualified Data.Set                     as Set
import Data.Set (Set)

import           Daedalus.Core      (Name, Typed (..), Expr)
import           Daedalus.Core.Type (typeOf)

import           Talos.Strategy.Monad
-- import           Talos.Strategy.SearchT    (SearchT)
-- import qualified Talos.Strategy.SearchT    as ST
import           Talos.Strategy.SearchTree
import           Talos.SymExec.Path
import           Talos.SymExec.SemiValue
import           Talos.SymExec.SolverT     (SolverT)
import qualified Talos.SymExec.SolverT     as Solv

import           Control.Monad.Trans.Free
import Talos.SymExec.SemiExpr (SemiSExpr)
 
--------------------------------------------------------------------------------
-- The free monad for searching

-- c.f. https://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html

data ThreadF next =
  Choose [next]
  | Bind Name SymbolicEnv (SymbolicM Solution) (Solution -> next)
  | Backtrack (Set Name)
  deriving (Functor)

newtype SearchT m a = SearchT { getSearchT :: FreeT ThreadF m a }
  deriving (Applicative, Functor, Monad, MonadIO, LiftStrategyM, MonadTrans)

chooseST :: Monad m => [a] -> SearchT m a
chooseST xs = SearchT $ liftF (Choose xs)

backtrackST :: Monad m => Set Name -> SearchT m a
backtrackST = SearchT . liftF . Backtrack

bindST :: Monad m => Name -> SymbolicEnv -> SymbolicM Solution ->
          (Solution -> a) -> SearchT m a
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

data Solution = Solution
  { sValue   :: SemiSExpr
  , sPath    :: PathBuilder
  , sContext :: Solv.SolverContext 
  }

-- FIXME: this type is repeated from SemiExpr
type SymbolicEnv = Map Name (SemiValue (Typed SExpr))

data SolverResult =
  ByteResult SemiSExpr
  | InverseResult SymbolicEnv Expr -- The env. includes the result var.

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
                SymbolicM SelectedPath ->
                SolverT StrategyM (Maybe SelectedPath)
runSymbolicM sstrat (SymbolicM m) = do
  runSearchStrat sstrat (runReaderT m emptySymbolicEnv)
  
--------------------------------------------------------------------------------
-- Names

bindNameIn :: Name -> SymbolicM (SemiSExpr, PathBuilder)
           -> (PathBuilder -> SymbolicM a) -> SymbolicM a
bindNameIn n lhs rhs = join (SymbolicM res)
  where
    res = do
      e <- ask
      let lhs' = do
            (v, p) <- lhs
            ctx <- inSolver Solv.getContext
            pure (Solution { sValue = v, sPath = p, sContext = ctx})
            
      lift $ bindST n e lhs' (\s -> local (Map.insert n (sValue s)) (rhs (sPath s)))
 
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
  a <- SymbolicM (lift $ chooseST bs)
  inSolver (Solv.restoreContext sctxt)
  pure a
  
backtrack :: Set Name -> SymbolicM a
backtrack xs = SymbolicM (lift (backtrackST xs))

--------------------------------------------------------------------------------
-- Search strtegies

newtype SearchStrat = SearchStrat
  { runSearchStrat :: SearchT' SelectedPath -> SolverT StrategyM (Maybe SelectedPath) }

dfs :: SearchStrat
dfs = SearchStrat $ \m -> go [m]
  where
    go :: forall r. [SearchT' r] -> SolverT StrategyM (Maybe r)
    go [] = pure Nothing
    go (x : xs) = do
      r <- runFreeT (getSearchT x)
      case r of
        Free (Choose xs')       -> go (map SearchT xs' ++ xs)
        Free (Backtrack {})     -> go xs
        Free (Bind n e lhs rhs) -> go ((runReaderT (getSymbolicM lhs) e >>= SearchT . rhs) : xs)
        Pure res              -> pure (Just res)

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

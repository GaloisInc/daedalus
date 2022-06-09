{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Talos.Strategy.SymbolicM where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Free
import           Data.Generics.Product    (field)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           GHC.Generics             (Generic)
import           SimpleSMT                (ppSExpr)

import           Daedalus.Core            (Expr, Name, Typed (..), nameId)
import           Daedalus.GUID            (GUID, HasGUID, getNextGUID)
import           Daedalus.PP
import           Daedalus.Panic           (panic)
import           Daedalus.Rec             (Rec)

import           Talos.Analysis.Exported  (ExpSlice, SliceId)
import           Talos.Strategy.Monad
import           Talos.SymExec.Path
import           Talos.SymExec.SemiExpr   (SemiSExpr, SemiSolverM, runSemiSolverM)
import           Talos.SymExec.SolverT    (SolverT)
import qualified Talos.SymExec.SolverT    as Solv

--------------------------------------------------------------------------------
-- The free monad for searching

-- c.f. https://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html

type Result = (SemiSExpr, Set ChoiceId, PathBuilder)

data ThreadF next =
  Choose ChoiceId (Set ChoiceId) [next]
  | Bind Name SymbolicEnv (SymbolicM Result) (Result -> next)
  | Backtrack BacktrackReason (Set ChoiceId)
  deriving (Functor)

newtype SearchT m a = SearchT { getSearchT :: FreeT ThreadF m a }
  deriving (Applicative, Functor, Monad, MonadIO, LiftStrategyM, MonadTrans)

-- FIXME: maybe we should deal with the path to here differently, by
-- making it explicit in the search tree for example.
chooseST :: Monad m => ChoiceId -> Set ChoiceId -> [a] -> SearchT m a
chooseST cid pathToHere xs = SearchT $ liftF (Choose cid pathToHere xs)

backtrackST :: Monad m => BacktrackReason -> Set ChoiceId -> SearchT m a
backtrackST reason pathToFailure = SearchT (liftF (Backtrack reason pathToFailure))

bindST :: Monad m => Name -> SymbolicEnv -> SymbolicM Result ->
          (Result -> a) -> SearchT m a
bindST n e lhs rhs = SearchT $ liftF (Bind n e lhs rhs)

--------------------------------------------------------------------------------
-- Backtracking
--
-- def Ex1 = block
--   x = false | true
--   y = P
--   case x of
--     true -> ^ {}
--  
-- when we get to the case after choosing false for x, we will have to
-- backtrack.  In this case, the only effective backtracking strategy
-- is to re-choose x; in particular, re-visiting y will have no effect
-- on the feasibility of the current path.  Now consider
--
-- def Ex2 = block
--   a = block
--     b = P
--     First
--       l1 = ...
--       l2 = ...
--   case a of
--     l1 -> ...
--
-- If we pick l2 for a then we will have to backtrack in to the block
-- for a, and choose a different path for the alternation.  In
-- general, we may need to backtrack to variables which are out of
-- scope, or are shadowed (i.e., due to recursion).
--
-- Notes:
--  - What about e.g.
--
--  def Ex3 = block
--    x = First
--      l1 = { x = P }
--      l2 = ...
--    case x of
--      l1 v -> case v of ...
--
--  where the value wrapped by the constructor l1 is also constrained.
--  In the limit we could have each value have choices for the
--  sub-values.  We could also store enough information in the search
--  tree, along with a description of what was wrong with the value
--  when we backtrack.

-- We (ab)use guids to get unique ids, we could also add it to the state
newtype ChoiceId = ChoiceId { getChoiceId :: GUID }
  deriving (Eq, Ord)

type ChoicePath = [Set ChoiceId]

data BacktrackReason =
  ConcreteFailure (Set ChoiceId)
  | OtherFailure

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

type SymVarEnv = Map Name SemiSExpr
type SymVarDeps = Map Name (Set ChoiceId)

data SymbolicEnv = SymbolicEnv
  { sVarEnv  :: SymVarEnv
  -- ^ This binds names to symbolic values in the current scope
  , sVarDeps :: SymVarDeps
  , sPath :: ChoicePath -- ^ Current path.
  } deriving (Generic)

ppSymbolicEnv :: SymbolicEnv -> Doc
ppSymbolicEnv e =
  block "{" ", " "}" [ ppN k <+> "->" <+> ppPrec 1 (text . flip ppSExpr ""  . typedThing <$> v)
                     | (k,v) <- Map.toList (sVarEnv e) ]
  $+$
  block "{" ", " "}" [ pp k <+> "->" <+> ppS v
                     | (k,v) <- Map.toList (sVarDeps e) ]
  
  where
    ppN n = ppPrec 1 n <> parens (pp (nameId n))

    ppS :: Set ChoiceId -> Doc
    ppS e = block "{" ", " "}" (map (pp . getChoiceId) (Set.toList e))

data SolverResultF a =
  ByteResult a
  | InverseResult (Map Name a) Expr -- The env. includes the result var.
  deriving (Functor, Foldable, Traversable)

-- Just so we can get fmap/traverse/etc.
type SolverResult = SolverResultF SemiSExpr

type PathBuilder = SelectedPathF SolverResult
type SearchT'  = SearchT (SolverT StrategyM)

emptySymbolicEnv :: SymbolicEnv
emptySymbolicEnv = SymbolicEnv mempty mempty mempty

newtype SymbolicM a =
  SymbolicM { getSymbolicM :: ReaderT SymbolicEnv SearchT' a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader SymbolicEnv)

instance LiftStrategyM SymbolicM where
  liftStrategy m = SymbolicM (liftStrategy m)

freshChoiceId :: LiftStrategyM m => m ChoiceId
freshChoiceId = ChoiceId <$> liftStrategy getNextGUID

runSymbolicM :: SearchStrat ->
                -- | Slices for pre-run analysis
                (ExpSlice, [ Rec (SliceId, ExpSlice) ]) ->
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
      lift $ bindST n e lhs rhs'
    rhs' (v, cids, pb) =
      primBindName n v cids (rhs pb)

primBindName :: Name ->  SemiSExpr -> Set ChoiceId -> SymbolicM a -> SymbolicM a
primBindName n v cids = 
      local ( set (field @"sVarDeps" . at n) (Just cids)
            . set (field @"sVarEnv"  . at n) (Just v))

getName :: Name -> SymbolicM SemiSExpr
getName n = SymbolicM $ do
  m_local <- asks (view (field @"sVarEnv" . at n))
  case m_local of
    Nothing -> panic "Missing variable" [showPP n]
    Just r  -> pure r

getNameDeps :: Name -> SymbolicM (Set ChoiceId)
getNameDeps n = SymbolicM $ do
  m_deps <- asks (view (field @"sVarDeps" . at n))
  case m_deps of
    Nothing -> do
      e <- ask
      panic "Missing variable" [showPP n, show (ppSymbolicEnv e) ]
    Just r  -> pure r

getPathDeps :: SymbolicM (Set ChoiceId)
getPathDeps = SymbolicM $ asks (mconcat . sPath)

--------------------------------------------------------------------------------
-- Search operaations

choose :: [a] -> SymbolicM (ChoiceId, a)
choose bs = do
  pathToHere <- getPathDeps
  cid <- freshChoiceId
  SymbolicM $ (,) cid <$> lift (chooseST cid pathToHere bs)

backtrack :: BacktrackReason -> SymbolicM a
backtrack reason = do
  pathToFailure <- getPathDeps
  -- liftIO $ putStrLn "Backtracking ..."
  SymbolicM (lift (backtrackST reason pathToFailure))

enterPathNode :: Set ChoiceId -> SymbolicM a -> SymbolicM a
enterPathNode deps = local (over (field @"sPath") (deps :))

enterFunction :: Map Name Name ->
                 SymbolicM a -> SymbolicM a
enterFunction argMap = local upd
  where
    upd e = e { sVarEnv  = Map.compose (sVarEnv  e) argMap
              , sVarDeps = Map.compose (sVarDeps e) argMap
              }
            
--------------------------------------------------------------------------------
-- Search strtegies

-- Note the search strat is responsible for doing solver context mgmt.

newtype SearchStrat = SearchStrat
  { runSearchStrat :: (ExpSlice, [ Rec (SliceId, ExpSlice) ]) -> SearchT' Result ->
                      SolverT StrategyM (Maybe Result) }

dfs :: SearchStrat
dfs = SearchStrat $ \_ -> next []
  where
    go :: forall r. [(Solv.SolverContext, SearchT' r)] ->
          SolverT StrategyM (Maybe r)
    go [] = pure Nothing
    go ((sc, x) : xs) = do
      Solv.restoreContext sc
      next xs x
      
    next xs m = do
      r <- runFreeT (getSearchT m)
      case r of
        Free (Choose _cid _pathToHere xs') -> do
          sc' <- Solv.getContext
          go (map ((,) sc' . SearchT) xs' ++ xs)
        Free (Backtrack {})     -> go xs
        Free (Bind _n e lhs rhs) -> do
          let m' = runReaderT (getSymbolicM lhs) e >>= SearchT . rhs
          next xs m'
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

liftSemiSolverM :: SemiSolverM StrategyM a -> SymbolicM a
liftSemiSolverM m = do
  funs <- getFunDefs
  lenv <- asks sVarEnv
  env  <- getIEnv
  inSolver (runSemiSolverM funs lenv env m)

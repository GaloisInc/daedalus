{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Talos.Strategy.PathSymbolicM where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe    (MaybeT, runMaybeT)
import           Data.Generics.Product        (field)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes)
import           GHC.Generics                 (Generic)
import qualified SimpleSMT                    as SMT
-- FIXME: use .CPS
import           Control.Monad.Writer         (MonadWriter, WriterT, runWriterT,
                                               tell)

import           Daedalus.Core                (Expr, Name, Pattern)
import qualified Daedalus.Core.Semantics.Env  as I
import           Daedalus.PP
import           Daedalus.Panic               (panic)
import           Daedalus.Rec                 (Rec)

import           Talos.Analysis.Exported      (ExpSlice, SliceId)
import           Talos.Strategy.Monad
import           Talos.Strategy.MuxValue      (GuardedSemiSExprs, SemiSolverM,
                                               runSemiSolverM)
import qualified Talos.Strategy.MuxValue      as MV
import           Talos.Strategy.PathCondition (PathVar (..))
import qualified Talos.SymExec.Expr           as SE
import           Talos.SymExec.Path
import           Talos.SymExec.SolverT        (MonadSolver, SMTVar, SolverT,
                                               liftSolver)
import qualified Talos.SymExec.SolverT        as Solv


-- =============================================================================
-- (Path) Symbolic monad

type Result = (GuardedSemiSExprs, PathBuilder)

type SymVarEnv = Map Name GuardedSemiSExprs

data SymbolicEnv = SymbolicEnv
  { sVarEnv  :: SymVarEnv
  -- The path isn't required as we add it on post-facto
  -- , sPath    :: ValueGuard -- ^ Current path.
  } deriving (Generic)

-- ppSymbolicEnv :: SymbolicEnv -> Doc
-- ppSymbolicEnv e =
--   block "{" ", " "}" [ ppN k <+> "->" <+> ppPrec 1 (text . flip ppSExpr ""  . typedThing <$> v)
--                      | (k,v) <- Map.toList (sVarEnv e) ]
--   where
--     ppN n = ppPrec 1 n <> parens (pp (nameId n))

data SolverResult =
  ByteResult SMTVar
  | InverseResult (Map Name GuardedSemiSExprs) Expr -- The env. includes the result var.

 -- Not all paths are necessarily feasible.
data PathChoiceBuilder a = PathChoice PathVar           [(Int, a)]
data PathCaseBuilder a   = PathCase   GuardedSemiSExprs [(Pattern, a)]

type PathBuilder = SelectedPathF PathChoiceBuilder PathCaseBuilder SolverResult

emptySymbolicEnv :: SymbolicEnv
emptySymbolicEnv = SymbolicEnv mempty

newtype SymbolicM a =
  SymbolicM { getSymbolicM :: MaybeT (WriterT [SMT.SExpr] (ReaderT SymbolicEnv (SolverT StrategyM))) a }
  deriving (Applicative, Functor, Monad, MonadIO
           , MonadReader SymbolicEnv, MonadWriter [SMT.SExpr], MonadSolver)

instance LiftStrategyM SymbolicM where
  liftStrategy m = SymbolicM (liftStrategy m)

runSymbolicM :: -- | Slices for pre-run analysis
                (ExpSlice, [ Rec (SliceId, ExpSlice) ]) ->
                SymbolicM Result ->
                SolverT StrategyM (Maybe Result, [SMT.SExpr])
runSymbolicM _sls (SymbolicM m) = runReaderT (runWriterT (runMaybeT m)) emptySymbolicEnv

--------------------------------------------------------------------------------
-- Names

bindNameIn :: Name -> SymbolicM Result
           -> (PathBuilder -> SymbolicM a) -> SymbolicM a
bindNameIn n lhs rhs = lhs >>= \(v, p) -> primBindName n v (rhs p)

primBindName :: Name -> GuardedSemiSExprs -> SymbolicM a -> SymbolicM a
primBindName n v = locally (field @"sVarEnv"  . at n) (const (Just v))

getName :: Name -> SymbolicM GuardedSemiSExprs
getName n = SymbolicM $ do
  m_local <- asks (view (field @"sVarEnv" . at n))
  case m_local of
    Nothing -> panic "Missing variable" [showPP n]
    Just r  -> pure r

pathVarSort :: SMT.SExpr
pathVarSort = SMT.tInt

freshPathVar :: Int -> SymbolicM PathVar
freshPathVar bnd = do
  sym <- liftSolver $ Solv.declareSymbol "c" pathVarSort
  assertSExpr $ SMT.and (SMT.leq (SMT.int 0) (SMT.const sym))
                        (SMT.lt (SMT.const sym) (SMT.int (fromIntegral bnd)))
  pure (PathVar sym)

-- extendPath ::  -> SymbolicM a -> SymbolicM a
-- extendPath el = locally (field @"sPath") (el :)
    
--------------------------------------------------------------------------------
-- Assertions

assertSExpr :: SMT.SExpr -> SymbolicM ()
assertSExpr p = tell [p]
  -- pe <- asks (valueGuardToSExpr . sPath)
  -- inSolver (Solv.assert (SMT.implies pe p))
  
-- assert :: GuardedSemiSExprs -> SymbolicM ()
-- assert sv = do
--   pe <- asks (pathToSExpr . sPath)
--   case sv of
--     VOther p -> inSolver (Solv.assert (SMT.implies pe (typedThing p)))
--     VValue (V.VBool True) -> pure ()
--     -- If we assert false, we can't get here (negate path cond)
--     VValue (V.VBool False) -> inSolver (Solv.assert (SMT.not pe))
--     _ -> panic "Malformed boolean" [show sv]

infeasible :: SymbolicM a
infeasible = SymbolicM $ fail "UNUSED"

--------------------------------------------------------------------------------
-- Search operaations

-- choose :: [a] -> SymbolicM (ChoiceId, a)
-- choose bs = do
--   pathToHere <- getPathDeps
--   cid <- freshChoiceId
--   SymbolicM $ (,) cid <$> lift (chooseST cid pathToHere bs)

-- backtrack :: BacktrackReason -> SymbolicM a
-- backtrack reason = do
--   pathToFailure <- getPathDeps
--   -- liftIO $ putStrLn "Backtracking ..."
--   SymbolicM (lift (backtrackST reason pathToFailure))

-- enterPathNode :: Set ChoiceId -> SymbolicM a -> SymbolicM a
-- enterPathNode deps = local (over (field @"sPath") (deps :))

enterFunction :: Map Name Name ->
                 SymbolicM a -> SymbolicM a
enterFunction argMap = local upd
  where
    upd e = e { sVarEnv  = Map.compose (sVarEnv  e) argMap
              }

--------------------------------------------------------------------------------
-- Utilities

liftSemiSolverM :: SemiSolverM StrategyM a -> SymbolicM a
liftSemiSolverM m = do
  funs <- getFunDefs
  bfuns <- getBFunDefs
  lenv <- asks sVarEnv
  env  <- getIEnv
  hoistMaybe =<< liftSolver (runSemiSolverM funs bfuns lenv env m)

liftSymExecM :: SE.SymExecM StrategyM a -> SymbolicM a
liftSymExecM m = do
  ienv  <- getIEnv
  SymbolicM . lift . lift . withReaderT (envf (I.tEnv ienv)) $ m
  where
    -- FIXME: probably these should live outside MuxValue
    envf tenv env = MV.envToSymEnv tenv (sVarEnv env)

-- FIXME: copied from MuxValue
getMaybe :: SymbolicM a -> SymbolicM (Maybe a)
getMaybe = SymbolicM . lift . runMaybeT . getSymbolicM

putMaybe :: SymbolicM (Maybe a) -> SymbolicM a
putMaybe m = hoistMaybe =<< m

hoistMaybe :: Maybe a -> SymbolicM a
hoistMaybe r = 
  case r of
    Nothing -> SymbolicM $ fail "Ignored"
    Just v  -> pure v

collectMaybes :: [SymbolicM a] -> SymbolicM [a]
collectMaybes = fmap catMaybes . mapM getMaybe

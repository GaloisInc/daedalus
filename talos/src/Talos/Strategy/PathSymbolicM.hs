{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Data.Set                     (Set)

import           Daedalus.Core                (Expr, Name, Pattern, typedThing)
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
import qualified Data.Set as Set



-- =============================================================================
-- (Path) Symbolic monad

type Result = (GuardedSemiSExprs, PathBuilder)

type SymVarEnv = Map Name GuardedSemiSExprs

data SymbolicEnv = SymbolicEnv
  { sVarEnv     :: SymVarEnv
  , sCurrentSCC :: Maybe (Set SliceId)
  -- ^ The set of slices in the current SCC, if any
  , sBackEdges :: Map SliceId (Set SliceId)
  -- ^ All back edges from the current SCC
  , sSliceId :: Maybe SliceId
  -- ^ Current slice

  , sRecDepth :: Int
  , sMaxRecDepth :: Int
  
  -- ^ Current recursive depth (basically the sum of all back edge
  -- calls, irrespective of source/target).
  
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
data PathChoiceBuilder a =
  SymbolicChoice   PathVar           [(Int, a)]
  | ConcreteChoice Int               a
  deriving (Functor)

data PathCaseBuilder a   =
  SymbolicCase   GuardedSemiSExprs [(Pattern, a)]
  | ConcreteCase                   a
  deriving (Functor)

type PathBuilder = SelectedPathF PathChoiceBuilder PathCaseBuilder SolverResult

emptySymbolicEnv :: Int -> SymbolicEnv
emptySymbolicEnv = SymbolicEnv mempty mempty mempty Nothing 0 

newtype SymbolicM a =
  SymbolicM { getSymbolicM :: MaybeT (WriterT [SMT.SExpr] (ReaderT SymbolicEnv (SolverT StrategyM))) a }
  deriving (Applicative, Functor, Monad, MonadIO
           , MonadReader SymbolicEnv, MonadWriter [SMT.SExpr], MonadSolver)

instance LiftStrategyM SymbolicM where
  liftStrategy m = SymbolicM (liftStrategy m)

runSymbolicM :: -- | Slices for pre-run analysis
                (ExpSlice, [ Rec (SliceId, ExpSlice) ]) ->
                Int ->
                SymbolicM Result ->
                SolverT StrategyM (Maybe Result, [SMT.SExpr])
runSymbolicM _sls maxRecDepth (SymbolicM m) = runReaderT (runWriterT (runMaybeT m)) (emptySymbolicEnv maxRecDepth)

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


-- We have a number of cases here
-- 1. Normal function call to non-rec. function
-- 2. Normal function to recursive function
-- 3. Non back-edge call in current SCC
-- 4. Back-edge calls for which we might need to fail (if the depth is too great)

enterFunction :: SliceId -> Map Name Name -> 
                 SymbolicM a -> SymbolicM a
enterFunction tgt argMap m = do
  env <- ask
  let m_myId = sSliceId env
      isBackEdge = maybe False (Set.member tgt) (flip Map.lookup (sBackEdges env) =<< m_myId)
      belowMaxDepth = sRecDepth env < sMaxRecDepth env
      tgtInSCC = maybe False (Set.member tgt) (sCurrentSCC env)
  
  if tgtInSCC
    then sameSCC isBackEdge belowMaxDepth
    else outsideSCC
  where
    -- Cases 1 and 2 above
    outsideSCC = do
      m_sccs <- sccsFor tgt
      backEdges <- backEdgesFor tgt
      let upd e = e { sCurrentSCC = m_sccs
                    , sRecDepth   = 0
                    , sBackEdges  = backEdges
                    }
      local upd m_with_args

    -- Case 3
    sameSCC False _ = m_with_args    
    -- Case 4 back edge, beyond the max
    sameSCC True False = infeasible
    -- Case 4 back edge, allowed, so just increase depth
    sameSCC True True = 
      locally (field @"sRecDepth") (+ 1) m_with_args

    m_with_args =
      locally (field @"sSliceId") (const (Just tgt)) .
      locally (field @"sVarEnv") (flip Map.compose argMap) $ m

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

-- -----------------------------------------------------------------------------
-- Instances

instance PP (PathChoiceBuilder Doc) where
  pp (SymbolicChoice pv ps) = pp pv <> ": " <> block "{" "," "}" (map pp1 ps)
    where
      pp1 (i, p) = pp i <+> "=" <+> p
  pp (ConcreteChoice i p) = pp i <+> "=" <+> p


instance PP (PathCaseBuilder Doc) where
  pp (SymbolicCase gses ps) = block "[[" "," "]]" [ pp (text . typedThing <$> gses)
                                                  , block "{" "," "}" (map pp1 ps) ]
    where
      pp1 (pat, p) = pp pat <+> "=" <+> p
      
  pp (ConcreteCase p) = p

instance PP SolverResult where
  pp (ByteResult v) = text v
  pp (InverseResult _e _v) = "inv"

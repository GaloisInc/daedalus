{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Talos.Strategy.PathSymbolicM where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Generics.Product   (field)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           GHC.Generics            (Generic)
import           SimpleSMT               (ppSExpr)
import qualified SimpleSMT               as SMT

import           Daedalus.Core           (Expr, Name, Pattern, Type (TBool),
                                          Typed (..), nameId)
import           Daedalus.PP
import           Daedalus.Panic          (panic)
import           Daedalus.Rec            (Rec)
import qualified Daedalus.Value          as V

import           Talos.Analysis.Exported (ExpSlice, SliceId)
import           Talos.Strategy.Monad
import           Talos.SymExec.Path
-- import           Talos.SymExec.SemiExpr   (SemiSExpr, SemiSolverM, runSemiSolverM)
import           Talos.SymExec.SolverT   (SolverT)
import qualified Talos.SymExec.SolverT   as Solv
-- import Talos.SymExec.SemiValue (SemiValue(..))
import           Talos.SymExec.Expr      (patternToPredicate)



-- =============================================================================
-- (Path) Symbolic monad

type GuardedSemiSExpr = GuardedValues SymPath SMT.SExpr

type Result = (GuardedSemiSExpr, PathBuilder)

type SymVarEnv = Map Name GuardedSemiSExpr

data SymbolicEnv = SymbolicEnv
  { sVarEnv  :: SymVarEnv
  , sPath    :: SymPath -- ^ Current path.
  } deriving (Generic)

-- ppSymbolicEnv :: SymbolicEnv -> Doc
-- ppSymbolicEnv e =
--   block "{" ", " "}" [ ppN k <+> "->" <+> ppPrec 1 (text . flip ppSExpr ""  . typedThing <$> v)
--                      | (k,v) <- Map.toList (sVarEnv e) ]
--   where
--     ppN n = ppPrec 1 n <> parens (pp (nameId n))

data SolverResultF a =
  ByteResult a
  | InverseResult (Map Name a) Expr -- The env. includes the result var.
  deriving (Functor, Foldable, Traversable)

-- Just so we can get fmap/traverse/etc.
type SolverResult = SolverResultF SMT.SExpr

type PathBuilder = SelectedPathF [] SMT.SExpr SolverResult

emptySymbolicEnv :: SymbolicEnv
emptySymbolicEnv = SymbolicEnv mempty mempty

newtype SymbolicM a =
  SymbolicM { getSymbolicM :: ReaderT SymbolicEnv (SolverT StrategyM) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader SymbolicEnv)

instance LiftStrategyM SymbolicM where
  liftStrategy m = SymbolicM (liftStrategy m)

runSymbolicM :: -- | Slices for pre-run analysis
                (ExpSlice, [ Rec (SliceId, ExpSlice) ]) ->
                SymbolicM Result ->
                SolverT StrategyM Result
runSymbolicM _sls (SymbolicM m) = runReaderT m emptySymbolicEnv

--------------------------------------------------------------------------------
-- Names

bindNameIn :: Name -> SymbolicM Result
           -> (PathBuilder -> SymbolicM a) -> SymbolicM a
bindNameIn n lhs rhs = lhs >>= \(v, p) -> primBindName n v (rhs p)

primBindName :: Name -> GuardedSemiSExpr -> SymbolicM a -> SymbolicM a
primBindName n v = locally (field @"sVarEnv"  . at n) (const (Just v))

getName :: Name -> SymbolicM GuardedSemiSExpr
getName n = SymbolicM $ do
  m_local <- asks (view (field @"sVarEnv" . at n))
  case m_local of
    Nothing -> panic "Missing variable" [showPP n]
    Just r  -> pure r

type PathVar     = SMT.SExpr

pathVarSort :: SMT.SExpr
pathVarSort = SMT.tInt

freshPathVar :: Int -> SymbolicM PathVar
freshPathVar bnd = do
  sym <- inSolver $  Solv.declareSymbol "c" pathVarSort
  assert (VOther (Typed TBool $ SMT.lt sym (SMT.int (fromIntegral bnd))))
  pure sym

extendPath :: SymPathElement -> SymbolicM a -> SymbolicM a
extendPath el = locally (field @"sPath") (el :)
  
--------------------------------------------------------------------------------
-- Assertions

assert :: GuardedSemiSExpr -> SymbolicM ()
assert sv = do
  pe <- asks (pathToSExpr . sPath)
  case sv of
    VOther p -> inSolver (Solv.assert (SMT.implies pe (typedThing p)))
    VValue (V.VBool True) -> pure ()
    -- If we assert false, we can't get here (negate path cond)
    VValue (V.VBool False) -> inSolver (Solv.assert (SMT.not pe))
    _ -> panic "Malformed boolean" [show sv]

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

-- enterFunction :: Map Name Name ->
--                  SymbolicM a -> SymbolicM a
-- enterFunction argMap = local upd
--   where
--     upd e = e { sVarEnv  = Map.compose (sVarEnv  e) argMap
--               , sVarDeps = Map.compose (sVarDeps e) argMap
--               }

--------------------------------------------------------------------------------
-- Utilities

inSolver :: SolverT StrategyM a -> SymbolicM a
inSolver = SymbolicM . lift 

-- liftSemiSolverM :: SemiSolverM StrategyM a -> SymbolicM a
-- liftSemiSolverM m = do
--   funs <- getFunDefs
--   lenv <- asks sVarEnv
--   env  <- getIEnv
--   inSolver (runSemiSolverM funs lenv env m)


--------------------------------------------------------------------------------
-- Values (move)

data GuardedValues p a = GuardedValues [ (p, MuxValue (GuardedValues p) a) ]


data MuxValue f a =
    VValue                 !V.Value
  | VOther                 !a
  | VUnionElem             !V.Label !(f a)
  -- We don't need to support partial updates (e.g. x { foo = bar }
  -- where x is symbolic) as Daedalus doesn't (yet) support updates.
  | VStruct                ![(V.Label, f a)]

  -- Bool is true if this is a builder, false if an array
  | VSequence              !Bool ![f a]
  | VMaybe                 !(Maybe (f a))

  -- For iterators and maps
  | VPair                  !(f a) !(f a)
  
  -- We support symbolic keys, so we can't use Map here

  | VMap                   ![f a]
  | VIterator              ![f a]
    deriving (Show, Eq, Ord, Foldable, Traversable, Functor)

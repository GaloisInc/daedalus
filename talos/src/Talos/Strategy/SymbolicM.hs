{-# Language GeneralizedNewtypeDeriving #-}

module Talos.Strategy.SymbolicM where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans

import Talos.SymExec.Path
import Talos.Strategy.DFST
import Talos.Strategy.Monad

import Talos.SymExec.SolverT (SolverT, push, pop)
                             
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

newtype SymbolicM a = SymbolicM { _getSymbolicM :: DFST (Maybe SelectedPath) (SolverT StrategyM) a }
  deriving (Applicative, Functor, Monad, MonadIO, LiftStrategyM)

runSymbolicM :: SymbolicM SelectedPath -> SolverT StrategyM (Maybe SelectedPath)
runSymbolicM (SymbolicM m) = runDFST m (pure . Just) (pure Nothing)

instance Alternative SymbolicM where
  (SymbolicM m1) <|> (SymbolicM m2) = SymbolicM $ bracketS m1 <|> bracketS m2
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

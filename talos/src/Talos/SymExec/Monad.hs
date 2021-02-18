{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleInstances, FlexibleContexts #-}

-- | Defines the symbolic parser API.  This wraps the SimpleSMT API

module Talos.SymExec.Monad (
  -- * Solver interaction monad
  SymExecM,
  runSymExecM,
  freshSym,
  withSolver
  ) where

import Control.Applicative (liftA2)
import Control.Monad.State

import SimpleSMT (Solver)
import qualified SimpleSMT as S

import Daedalus.GUID
import Daedalus.PP

data SymExecMState =
  SymExecMState { nextGUID :: GUID
                , solver   :: Solver
                --                , globalPrefix :: String
                }

newtype SymExecM a = SymExecM { getSymExecM :: StateT SymExecMState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance HasGUID SymExecM where
  guidState f = SymExecM $ state (mkGUIDState' nextGUID (\guid' s -> s { nextGUID = guid' }) f)

-- From the instances for IO
instance Semigroup a => Semigroup (SymExecM a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (SymExecM a) where
    mempty = pure mempty

freshSym :: String {- ^ The hint string to base the fresh name upon -} ->
            SymExecM String
freshSym hint =
  do nf <- getNextGUID
     pure (S.quoteSymbol (hint ++ "@" ++ showPP nf))

withSolver :: (Solver -> SymExecM a) -> SymExecM a
withSolver f = do
  s <- SymExecM $ gets solver
  f s

runSymExecM :: Solver -> GUID -> SymExecM a -> IO (a, GUID)
runSymExecM s nguid m = do
  (a, s') <- runStateT (getSymExecM m) (SymExecMState nguid s)
  pure (a, nextGUID s')

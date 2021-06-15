{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Daedalus.LSP.Monad where

import           Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

import           Control.Monad.STM
import           Control.Concurrent.STM.TVar

import           Control.Lens hiding (Iso)
import Data.Aeson (ToJSON, FromJSON)                           

import Daedalus.GUID
import Daedalus.Pass

import Daedalus.AST (Located(..), ModuleName, Module)
import Daedalus.Type.AST (TCModule, SourceRange)
import GHC.Generics (Generic)
import Language.LSP.Server (LanguageContextEnv, LspM, runLspT)

data ModuleSource = ClientModule Int | FileModule

data ModuleInfo =
  ModuleInfo { moduleSource :: ModuleSource
             , moduleParsed :: Maybe Module  -- parsed, scoped
             , moduleTC     :: Maybe (TCModule SourceRange)
             }

data ServerState =
  ServerState { _parsedModules :: Map ModuleName ModuleInfo
              , _nextGUID      :: GUID
              }

makeLenses ''ServerState

-- User-alterable config
data Config = Config ()
  deriving (Generic, ToJSON, FromJSON, Show)

type ServerM = ReaderT (TVar ServerState) (LspM Config)

-- This needs to be behind a IO var of some sort as we need an Iso in
-- interpretHandler (so we thread the state using mutation).

emptyServerState :: ServerState
emptyServerState = ServerState mempty firstValidGUID

runServerM :: TVar ServerState -> LanguageContextEnv Config -> ServerM a -> IO a
runServerM sst lspE m = runLspT lspE (runReaderT m sst)

liftPassM :: PassM a -> ServerM a
liftPassM m = do
  stsv        <- ask
  sts         <- liftIO $ atomically (readTVar stsv)
  (r, nguid') <- liftIO $ runPassM' (sts ^. nextGUID) m
  liftIO $ atomically (modifyTVar stsv $ set nextGUID nguid')
  pure r

-- | We are about to reparse/reTC a module, so we should remove it from the state.
forgetModule :: ModuleName -> ServerM ()
forgetModule m = do
  sstv <- ask
  liftIO $ atomically $ modifyTVar' sstv $ 
      over parsedModules (Map.delete m)

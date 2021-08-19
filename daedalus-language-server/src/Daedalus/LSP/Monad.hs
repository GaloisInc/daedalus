{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
module Daedalus.LSP.Monad where

import           Control.Concurrent.Async     (Async)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Lens                 (makeLenses, (^.))
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           GHC.Generics                 (Generic)

import           Daedalus.AST                 (Module, ModuleName)
import           Daedalus.Module              (pathToModuleName)
import           Daedalus.PP
import           Daedalus.Parser.Lexer        (Lexeme, Token)
import           Daedalus.Pass
import           Daedalus.Scope               (GlobalScope, Scope)
import           Daedalus.Type.AST            (SourceRange, TCModule, TCTyDecl,
                                               TCTyName)
import           Daedalus.Type.Monad          (RuleEnv)

import           Data.Text                    (Text)
import           Language.LSP.Server          (LanguageContextEnv, LspM,
                                               MonadLsp (..), runLspT)
import qualified Language.LSP.Types           as J

data ModuleSource = ClientModule J.NormalizedUri J.TextDocumentVersion
                  | FileModule FilePath -- FilePath here is the path to search
                  deriving (Eq, Ord, Show)

isFileModule :: ModuleSource -> Bool
isFileModule FileModule {} = True
isFileModule _ = False

-- Worker commands

data WorkerRequest =
  ChangedReq J.NormalizedUri  J.TextDocumentVersion Int Text -- ms delay
  | WakeUpReq
  deriving Show

-- 
-- Worker phases

data PassStatusClass a = NotStarted | ErrorStatus | FinishedStatus a
  deriving (Functor)

data PassStatus a =
  PassStatus { _statusChanged :: Bool
             , _passStatus    :: PassStatusClass a
             }
  deriving (Functor)

makeLenses ''PassStatus

passStatusToMaybe :: PassStatus a -> Maybe a
passStatusToMaybe (PassStatus _ (FinishedStatus a)) = Just a
passStatusToMaybe _ = Nothing


data ModuleState =
  ModuleState { _msSource        :: ModuleSource
              -- We can't fail (missing modules don't get a ModuleState)
              , _msTokens        :: PassStatus [Lexeme Token]
              , _msParsedModule  :: PassStatus Module
              -- ^ The parsed module, not scoped.  We don't really need
              -- the error, but it is nice to have.

              , _msScopeRes      :: PassStatus (Module, GlobalScope)

              , _msTCRes         :: PassStatus (TCModule SourceRange, RuleEnv, Map TCTyName TCTyDecl)
              }

makeLenses ''ModuleState

type ModuleStates = Map ModuleName ModuleState

type WatcherTag = Int

data ServerState =
  ServerState { lspEnv       :: LanguageContextEnv Config
              , passState    :: PassState -- shared between threads
              , workerChan   :: TChan WorkerRequest
              , moduleStates :: TVar ModuleStates
              , watchers     :: TVar (Map WatcherTag (Async ()))
              }

-- User-alterable config
data Config = Config ()
  deriving (Generic, ToJSON, FromJSON, Show)

newtype ServerM a = ServerM { getServerM :: ReaderT ServerState (LspM Config) a }
  deriving newtype (Applicative, Functor, Monad, MonadReader ServerState
                   , MonadIO, MonadUnliftIO, MonadLsp Config)

-- This needs to be behind a IO var of some sort as we need an Iso in
-- interpretHandler (so we thread the state using mutation).

emptyServerState :: LanguageContextEnv Config -> IO ServerState
emptyServerState lenv = do
  ServerState lenv <$> newPassState <*> atomically newTChan <*> atomically (newTVar mempty) <*> atomically (newTVar mempty) 

runServerM :: ServerState ->  ServerM a -> IO a
runServerM sst m = runLspT (lspEnv sst) (runReaderT (getServerM m) sst)

-- liftPassM :: PassM a -> ServerM a
-- liftPassM m = do
--   stsv        <- ask
--   sts         <- liftIO $ atomically (readTVar stsv)
--   (r, nguid') <- liftIO $ runPassM' (sts ^. nextGUID) m
--   liftIO $ atomically (modifyTVar stsv $ set nextGUID nguid')
--   pure r

-- ----------------------------------------------------------------------------------------
-- Helpers

uriToModuleName :: J.NormalizedUri -> Maybe ModuleName
uriToModuleName = fmap (snd . pathToModuleName) . J.uriToFilePath . J.fromNormalizedUri

uriToModuleState :: J.NormalizedUri -> ServerM (Either J.ResponseError ModuleState)
uriToModuleState uri = do
  sst <-  ask
  let Just mn = uriToModuleName uri -- FIXME
  liftIO $ atomically $ do
    mods <- readTVar (moduleStates sst)
    pure $ case Map.lookup mn mods of
      Nothing -> Left $ J.ResponseError J.InvalidParams "Missing module" Nothing
      Just ms -> Right ms


uriToTCModule :: J.NormalizedUri -> ServerM (Either J.ResponseError (Maybe (TCModule SourceRange)))
uriToTCModule uri = do
  sst <-  ask
  let Just mn = uriToModuleName uri -- FIXME
  liftIO $ atomically $ do
    mods <- readTVar (moduleStates sst)
    pure $ case Map.lookup mn mods of
      Nothing -> Left $ J.ResponseError J.InvalidParams "Missing module" Nothing
      Just ms -> Right $ (\(tcm, _, _) -> tcm) <$> passStatusToMaybe (ms ^. msTCRes)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Daedalus.LSP.Monad where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Map                     (Map)
import           GHC.Generics                 (Generic)

import           Daedalus.AST                 (ModuleName)
import           Daedalus.Module              (pathToModuleName)
import           Daedalus.PP
import           Daedalus.Parser.Lexer        (Lexeme, Token)
import           Daedalus.Pass
import           Daedalus.Scope               (Scope)
import           Daedalus.Type.AST            (SourceRange, TCModule, TCTyDecl,
                                               TCTyName)
import           Daedalus.Type.Monad          (RuleEnv)

import           Language.LSP.Server          (LanguageContextEnv, LspM, MonadLsp(..)
                                              , runLspT)
import qualified Language.LSP.Types           as J
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Map as Map
import Control.Concurrent.Async (Async)



data ModuleSource = ClientModule J.NormalizedUri J.TextDocumentVersion
                  | FileModule FilePath -- FilePath here is the path to search
                  deriving (Eq, Show)


-- =============================================================================
-- Worker phases

data ParseRequest = ParseReq ModuleSource Int -- ms delay
                  | WakeUpReq
                  deriving Show

isFileModule :: ModuleSource -> Bool
isFileModule FileModule {} = True
isFileModule _ = False


-- -----------------------------------------------------------------------------
-- Errors from cycle detection

data CycleCheckError =
  CycleDetected [ModuleName] ModuleName
  | UnavailableDep [ModuleName] ModuleName
  | MissingDep [ModuleName] ModuleName
  deriving Eq

instance PP CycleCheckError where
  pp err = case err of
    CycleDetected pfx m  -> "Import cycle detected" $$ ppPath pfx m
    UnavailableDep pfx m -> "Unavailable dependency" $$ ppPath pfx m
    MissingDep pfx m     -> "Missing dependency" $$ ppPath pfx m
    where
      ppPath pfx m = foldr (\a b -> pp a <+> "->" <+> b) (pp m) pfx
    
-- FIXME: add a reason to Unavailable
-- data InfoStatus a = Pending | Unavailable | Finished a

-- finished :: InfoStatus a -> Bool
-- finished Finished {} = True
-- finished _ = False


data ModuleInfo = 
  ModuleInfo { miScope :: Maybe Scope
             , miTypeEnv :: Maybe (RuleEnv, Map TCTyName TCTyDecl)
             -- ^ Rules for this module, can be partial (will result in reported errors)
             -- while type decls for this and imported modules, can be partial
             } deriving Eq

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo Nothing Nothing

data ModuleResults =
  ModuleResults { mrSource      :: !ModuleSource
                , mrImportScope :: !(Maybe (Map ModuleName Scope))
                , mrTokens      :: !(Maybe [Lexeme Token])
                -- ^ Scope of imported modules, including this one.
                , mrTC          :: !(Maybe (TCModule SourceRange))
                }

emptyModuleResults :: ModuleSource -> ModuleResults
emptyModuleResults ms =
  ModuleResults { mrTokens      = Nothing
                , mrSource = ms
                , mrImportScope = Nothing
                , mrTC          = Nothing
                }

data ModuleState =
  ModuleState { moduleChan :: TChan ParseRequest
              , moduleImportInfo :: TVar (Maybe [ModuleName])
              -- These maps can be partial if there are errors, so we can use some of the information
              , moduleInfo    :: TVar ModuleInfo
              -- ^ Used between workers
              , moduleResults         :: TVar ModuleResults
              -- ^ Exported for use by front-end commands (hover, rename, etc)

             -- moduleSource  :: ModuleSource
             -- , modulePending :: Maybe (Async ())
             -- , moduleParsed  :: Maybe Module  -- ^ parsed, scoped
             -- , 
             -- moduleTC      :: Maybe (TCModule SourceRange)
             }

newModuleState :: ModuleSource -> STM ModuleState
newModuleState ms =
  ModuleState <$> newTChan <*> newTVar Nothing <*> newTVar emptyModuleInfo <*> newTVar (emptyModuleResults ms)

type WatcherTag = Int

data ServerState =
  ServerState { lspEnv       :: LanguageContextEnv Config
              , passState    :: PassState -- shared between threads
              , knownModules :: TVar (Map ModuleName ModuleState)
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
  ServerState lenv <$> newPassState <*> atomically (newTVar mempty) <*> atomically (newTVar mempty) 

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

uriToModuleResults :: J.NormalizedUri -> ServerM (Either J.ResponseError ModuleResults)
uriToModuleResults uri = do
  sst <-  ask
  let Just mn = uriToModuleName uri -- FIXME
  liftIO $ atomically $ do
    mods <- readTVar (knownModules sst)
    case Map.lookup mn mods of
      Nothing -> pure $ Left $ J.ResponseError J.InvalidParams "Missing module" Nothing
      Just mi -> Right <$> readTVar (moduleResults mi)

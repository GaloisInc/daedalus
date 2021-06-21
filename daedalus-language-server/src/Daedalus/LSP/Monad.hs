{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Daedalus.LSP.Monad where

import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding (Iso)
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           GHC.Generics                (Generic)

import           Data.Aeson                  (FromJSON, ToJSON)

import Daedalus.AST ( Module, ModuleName, Located )
import           Daedalus.GUID
import           Daedalus.Module             (pathToModuleName)
import           Daedalus.Pass
import           Daedalus.Type.AST           (SourceRange, TCModule, TCTyName, TCTyDecl)

import           Language.LSP.Server         (LanguageContextEnv, LspM, runLspT)
import qualified Language.LSP.Types          as J
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TChan
import Control.Concurrent (MVar)
import Daedalus.Scope (Scope)
import Daedalus.Type.Monad (RuleEnv)
import Daedalus.PP


data ModuleSource = ClientModule J.NormalizedUri Int
                  | FileModule FilePath -- FilePath here is the path to search


-- =============================================================================
-- Worker phases

data ParseRequest = ParseReq ModuleSource Int -- ms delay
                  | WakeUpReq
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

data ModuleState =
  ModuleState { moduleChan :: TChan ParseRequest
              , moduleImportInfo :: TVar (Maybe [ModuleName])
              -- These maps can be partial if there are errors, so we can use some of the information
              , moduleResults    :: TVar ModuleInfo
              , moduleTC         :: TVar (Maybe (TCModule SourceRange))

             -- moduleSource  :: ModuleSource
             -- , modulePending :: Maybe (Async ())
             -- , moduleParsed  :: Maybe Module  -- ^ parsed, scoped
             -- , 
             -- moduleTC      :: Maybe (TCModule SourceRange)
             }

newModuleState :: STM ModuleState
newModuleState =
  ModuleState <$> newTChan <*> newTVar Nothing <*> newTVar emptyModuleInfo <*> newTVar Nothing

data ServerState =
  ServerState { knownModules :: TVar (Map ModuleName ModuleState)
              , passState    :: PassState -- shared between threads
              , lspEnv       :: LanguageContextEnv Config
              }

-- User-alterable config
data Config = Config ()
  deriving (Generic, ToJSON, FromJSON, Show)

type ServerM = ReaderT (TVar ServerState) (LspM Config)

-- This needs to be behind a IO var of some sort as we need an Iso in
-- interpretHandler (so we thread the state using mutation).

-- emptyServerState :: ServerState
-- emptyServerState = ServerState mempty firstValidGUID

runServerM :: TVar ServerState -> LanguageContextEnv Config -> ServerM a -> IO a
runServerM sst lspE m = runLspT lspE (runReaderT m sst)

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

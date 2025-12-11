{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

module Daedalus.LSP.Monad where

import           Control.Concurrent.Async     (Async)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Exception            (try)
import           Control.Lens                 (_1, makeLenses, (^.))
import           Control.Lens.Getter          (view)
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Foldable                (traverse_, msum)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           GHC.Generics                 (Generic)
import           System.FilePath              ((<.>), (</>))
import           System.IO                    (stderr)

import           Daedalus.AST                 (Module, ModuleName)
import           Daedalus.Driver              (Daedalus)
import qualified Daedalus.Driver              as D
import           Daedalus.Module              (pathToModuleName)
import           Daedalus.Parser.Lexer        (Lexeme, Token)
import           Daedalus.Pass
import           Daedalus.Scope               (GlobalScope)
import           Daedalus.Type.AST            (SourceRange, TCModule, TCTyDecl,
                                               TCTyName)
import           Daedalus.Type.Monad          (RuleEnv)

import           Language.LSP.Server          (LanguageContextEnv, LspM,
                                               MonadLsp (..), runLspT)
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Protocol.Lens as J
import qualified Language.LSP.Protocol.Message as J

data ModuleSource = ClientModule J.NormalizedUri J.Int32
                  | FileModule FilePath -- FilePath here is the path to search
                  deriving (Eq, Ord, Show)

isFileModule :: ModuleSource -> Bool
isFileModule FileModule {} = True
isFileModule _ = False

-- Worker commands

data WorkerRequest =
  ChangedReq J.NormalizedUri  J.Int32 Int Text -- ms delay
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
  ServerState lenv <$> newPassState <*> atomically newTChan <*> newTVarIO mempty <*> newTVarIO mempty

runServerM :: ServerState ->  ServerM a -> IO a
runServerM sst m = runLspT (lspEnv sst) (runReaderT (getServerM m) sst)

-- ----------------------------------------------------------------------------------------
-- Running DDL passes

liftDaedalus :: Daedalus a -> ServerM (Either D.DaedalusError a)
liftDaedalus m = do
  sst <- ask
  mst <- liftIO (readTVarIO (moduleStates sst))
  
  let (tcMs, restMs) = Map.mapEither extractTCs mst
      mkDst s = s { D.useWarning  = const False
                  , D.outHandle   = stderr
                  , D.moduleFiles   = Map.mapWithKey (\k -> mstToFilename k . view msSource) mst
                  -- Ignores all the broken modules
                  , D.loadedModules = Map.mapMaybe mstToPhase restMs
                  , D.moduleDefines = foldMap (maybe mempty snd . passStatusToMaybe . view msScopeRes) mst
                  }
  
  liftIO $ try $ runPassM' (passState sst) $ D.daedalusPass $ do
    D.ddlUpdate_ mkDst
    traverse_ D.recordTCModule tcMs
    m
  where
    mstToFilename _ (ClientModule uri _) = fromMaybe "<unknown>" (J.uriToFilePath (J.fromNormalizedUri uri))
    mstToFilename n (FileModule fp)      = fp </> Text.unpack n <.> "ddl"

    extractTCs s
      | Just (tc, _, _) <- passStatusToMaybe (s ^. msTCRes) = Left tc
      | otherwise = Right s

    -- Pick latest that worked
    mstToPhase mst = msum [ D.TypeCheckedModule . view _1 <$> passStatusToMaybe (mst ^. msTCRes)
                          , D.ResolvedModule    . view _1 <$> passStatusToMaybe (mst ^. msScopeRes)
                          , D.ParsedModule                <$> passStatusToMaybe (mst ^. msParsedModule)
                          ]

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
      Nothing -> Left $ J.ResponseError (J.InR J.ErrorCodes_InvalidParams) "Missing module" Nothing
      Just ms -> Right ms


uriToTCModule :: J.NormalizedUri -> ServerM (Either J.ResponseError (Maybe (TCModule SourceRange)))
uriToTCModule uri = do
  sst <-  ask
  let Just mn = uriToModuleName uri -- FIXME
  liftIO $ atomically $ do
    mods <- readTVar (moduleStates sst)
    pure $ case Map.lookup mn mods of
      Nothing -> Left $ J.ResponseError (J.InR J.ErrorCodes_InvalidParams) "Missing module" Nothing
      Just ms -> Right $ (\(tcm, _, _) -> tcm) <$> passStatusToMaybe (ms ^. msTCRes)

maybeToNull :: Maybe a -> a J.|? J.Null
maybeToNull = maybe (J.InR J.Null) J.InL

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Daedalus.LSP.Command (supportedCommands, executeCommand) where

import           Control.Concurrent.Async     (async, cancel)
import           Control.Concurrent.STM       (atomically, modifyTVar, readTVar)
import           Control.Concurrent.STM.TVar  (stateTVar)
import           Control.Lens
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.IO.Unlift      (withRunInIO)
import           Control.Monad.Reader         (ask)
import           Data.Aeson                   (FromJSON, Result (..), ToJSON,
                                               fromJSON, toJSON)
import qualified Data.Aeson                   as A
import           Data.Foldable                (traverse_)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

import           Language.LSP.Server          (sendNotification)
import qualified Language.LSP.Types           as J
import qualified Language.LSP.Types.Lens      as J

import qualified Daedalus.LSP.Command.Debug   as C
import qualified Daedalus.LSP.Command.Regions as C
import qualified Daedalus.LSP.Command.Run     as C
import           Daedalus.LSP.Monad
import           Daedalus.LSP.Position        (declAtPos)
import           Daedalus.PP
import           Daedalus.SourceRange
import           Daedalus.Type.AST            (TCModule, tcDeclName)


type CommandImplFun = [A.Value] -> Either String (ServerM (Either J.ResponseError A.Value))

data CommandImpl = forall a. CanInvoke a => CommandImpl a

class CanInvoke a where
  invoke :: a -> CommandImplFun

instance CanInvoke CommandImpl where
  invoke (CommandImpl impl) = invoke impl

instance ToJSON a => CanInvoke (ServerM (Either J.ResponseError a)) where
  invoke m  [] = Right (fmap toJSON <$> m)
  invoke _  _ = Left "Expecting empty args"

instance (FromJSON a, CanInvoke b) => CanInvoke (a -> b) where
  invoke f (j : js) =
    case fromJSON j of
      Success v -> invoke (f v) js
      Error msg -> Left ("Error parsing '" ++ show j ++ ": " ++ msg)
  invoke _ _ = Left "Too few arguments"

commands :: Map Text CommandImpl
commands = Map.fromList [ ("positionToRegions", CommandImpl positionToRegions)
                        , ("run"              , CommandImpl runModule)
                        , ("run/watch"        , CommandImpl watchModule)
                        , ("run/cancel"       , CommandImpl cancelWatchModule)
                        , ("debug/pass"       , CommandImpl debugPass)
                        , ("debug/supportedPasses", CommandImpl debugSupportedPasses)
                        , ("debug/dumpModuleInfo", CommandImpl debugDumpModuleInfo)
                        ]

executeCommand :: (Either J.ResponseError A.Value -> ServerM ()) -> Text -> [A.Value] -> ServerM ()
executeCommand resp nm vs
  | Just impl <- Map.lookup nm commands = do
      case invoke impl vs of
        Left err -> resp $ Left $ J.ResponseError J.InvalidParams (Text.pack err) Nothing
        Right m  -> resp =<< m
  | otherwise = resp (Left $ J.ResponseError J.InternalError ("Unknown command " <> nm) Nothing)

supportedCommands :: [Text]
supportedCommands = Map.keys commands

positionToRegions :: J.TextDocumentIdentifier -> J.Position -> ServerM (Either J.ResponseError (J.List J.Range))
positionToRegions doc pos = do
  e_mr <- uriToModuleState (J.toNormalizedUri (doc ^. J.uri))
  pure $ do
    ms <- e_mr
    case passStatusToMaybe (ms ^. msTCRes) of
      Nothing -> Left $ J.ResponseError J.ParseError "Missing module" Nothing
      Just (m, _, _)  -> Right $ C.positionToRegions pos m

runModule :: J.TextDocumentIdentifier -> J.Position -> ServerM (Either J.ResponseError (Maybe A.Value))
runModule doc pos = do
  sst <- ask
  e_ms <- uriToModuleState (J.toNormalizedUri (doc ^. J.uri))
  case e_ms of
    Left err -> pure (Left err)
    Right ms -> case passStatusToMaybe (ms ^. msTCRes) of
      Nothing -> pure $ Left $ J.ResponseError J.ParseError "Missing module" Nothing
      Just (m, _, _)  -> liftIO $ Right <$> C.runModule pos sst m

watchModule :: J.TextDocumentIdentifier -> J.Position -> A.Value -> ServerM (Either J.ResponseError WatcherTag)
watchModule doc pos clientHandle = do
  e_mr <- uriToModuleState (J.toNormalizedUri (doc ^. J.uri))
  case e_mr of
    Left err -> pure (Left err)
    Right ms -> case passStatusToMaybe (ms ^. msTCRes) of
      Nothing -> pure $ Left $ J.ResponseError J.ParseError "Missing module" Nothing
      Just (m, _, _)  -> go m
  where
    go :: TCModule SourceRange -> ServerM (Either J.ResponseError WatcherTag)
    go m | Just d <- declAtPos pos m = do
      sst <- ask
      a <- withRunInIO $ \runInBase ->
        async $ C.watchModule (runInBase . sendNotification (J.SCustomMethod "daedalus/run/watchResult"))
                              (runInBase . sendNotification J.SWindowShowMessage)
                              clientHandle sst (tcDeclName d)
      liftIO $ atomically $ do
        wmap <- readTVar (watchers sst)
        let next = maybe 0 (1 +) $ fst <$> Map.lookupMax wmap
        modifyTVar (watchers sst) (Map.insert next a)
        pure (Right next)

    go _ = pure $ Left $ J.ResponseError J.ParseError "No decl at position" Nothing

-- We don't bother telling the client if the watcher doesn't exist
cancelWatchModule :: WatcherTag -> ServerM (Either J.ResponseError ())
cancelWatchModule tag = do
  sst <- ask
  -- This means we may reuse tags
  m_a <- liftIO $ atomically $ stateTVar (watchers sst) (Map.updateLookupWithKey (\_ _ -> Nothing) tag)
  liftIO $ traverse_ cancel m_a
  pure (Right ())

-- pure $ Left $ J.ResponseError J.ParseError "Cannot determine decl" Nothing

debugSupportedPasses :: ServerM (Either J.ResponseError [A.Value])
debugSupportedPasses = pure (Right (map A.String C.passNames))

-- Debugging functions
debugPass :: J.TextDocumentIdentifier -> J.Position -> Text -> ServerM (Either J.ResponseError (Maybe A.Value))
debugPass doc pos passN = do
  e_ms <- uriToModuleState (J.toNormalizedUri (doc ^. J.uri))
  case e_ms of
    Left err -> pure (Left err)
    Right ms -> case passStatusToMaybe (ms ^. msTCRes) of
      Nothing -> pure $ Left $ J.ResponseError J.ParseError "Missing module" Nothing
      Just (m, _, _) -> Right <$> C.debugPass pos m passN

debugDumpModuleInfo :: ServerM (Either J.ResponseError A.Value)
debugDumpModuleInfo = do
  sst <- ask
  mst <- liftIO $ atomically $ readTVar (moduleStates sst)
  let msg = show $ vcat (map go (Map.toList mst))
  pure (Right (A.String (Text.pack msg)))
  where
    ppStatus :: forall a. Lens' ModuleState (PassStatus a) -> ModuleState -> Doc
    ppStatus l st =
      case st ^. l . passStatus of
        NotStarted -> "not started"
        ErrorStatus -> "error"
        FinishedStatus {} -> "finished"

    go (mn, st) =
      pp mn $$ nest 4
        (bullets [ "Parse: " <> ppStatus msParsedModule st
                 , "Scope: " <> ppStatus msScopeRes st
                 , "TC:    " <> ppStatus msTCRes st
                 ])

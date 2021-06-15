{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | This modules has the LSP specific server code, including intialising LSP
module Daedalus.LSP.Server where

import Data.Aeson ( encode, fromJSON )
import qualified Data.ByteString.Lazy.Char8 as BSL

import Daedalus.LSP.Monad
import Daedalus.LSP.Diagnostics

import qualified Control.Exception as E
import Language.LSP.Server
import Data.Aeson.Types (Result(..))
import qualified Data.Text as Text
import qualified Language.LSP.Types as J
import Control.Concurrent.STM.TChan
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger
import Control.Concurrent (forkIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Lens hiding (Iso)
import Control.Monad.Reader

newtype ReactorInput
  = ReactorAction (IO ())

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  sst <- atomically (newTVar emptyServerState)

  let
    serverDefinition = ServerDefinition
      { defaultConfig = Config ()
      , onConfigurationChange = \_old v -> do
          case fromJSON v of
            Error e -> Left (Text.pack e)
            Success cfg -> Right cfg
      , doInitialize = \env _ -> forkIO (reactor rin) >> pure (Right env)
      , staticHandlers = lspHandlers rin
      , interpretHandler = \env -> Iso (runServerM sst env) liftIO
      , options = lspOptions
      }

  flip E.finally finalProc $ do
    setupLogger Nothing ["reactor"] DEBUG
    runServer serverDefinition

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { textDocumentSync = Just syncOptions
  }


-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debugM "reactor" "Started the reactor"
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TChan ReactorInput -> Handlers ServerM
lspHandlers rin = mapHandlers goReq goNot handle
  where
    goReq :: forall (a :: J.Method 'J.FromClient 'J.Request). Handler ServerM a -> Handler ServerM a
    goReq f = \msg k -> do
      env <- getLspEnv
      sst <- ask
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runServerM sst env $ f msg k)

    goNot :: forall (a :: J.Method 'J.FromClient 'J.Notification). Handler ServerM a -> Handler ServerM a
    goNot f = \msg -> do
      env <- getLspEnv
      sst <- ask
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runServerM sst env $ f msg)

-- | Where the actual logic resides for handling requests and notifications.
handle :: Handlers ServerM
handle = mconcat
  [ notificationHandler J.SInitialized $ \_msg -> do
      caps <- getClientCapabilities
      liftIO $ debugM "reactor.handle" $ "Processing the Initialized notification" ++ BSL.unpack (encode caps)

  , notificationHandler J.STextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument
    liftIO $ debugM "reactor.handle" $ "Processing didOpen for: " ++ show doc
    handleChangedDocument (doc ^. J.uri . to J.toNormalizedUri) (Just (doc ^. J.version))

  , notificationHandler J.SWorkspaceDidChangeConfiguration $ \msg -> do
      cfg <- getConfig
      liftIO $ debugM "configuration changed: " (show (msg,cfg))

  , notificationHandler J.STextDocumentDidChange $ \msg -> do
    let doc  = msg ^. J.params
                    . J.textDocument
    liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show (doc ^. J.uri . to J.toNormalizedUri)
    handleChangedDocument (doc ^. J.uri . to J.toNormalizedUri) (doc ^. J.version)

  -- , notificationHandler J.STextDocumentDidSave $ \msg -> do
  --     let doc = msg ^. J.params . J.textDocument . J.uri
  --         fileName = J.uriToFilePath doc
  --     liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
  --     sendDiagnostics (J.toNormalizedUri doc) Nothing

  -- , requestHandler J.STextDocumentRename $ \req responder -> do
  --     liftIO $ debugM "reactor.handle" "Processing a textDocument/rename request"
  --     let params = req ^. J.params
  --         J.Position l c = params ^. J.position
  --         newName = params ^. J.newName
  --     vdoc <- getVersionedTextDoc (params ^. J.textDocument)
  --     -- Replace some text at the position with what the user entered
  --     let edit = J.InL $ J.TextEdit (J.mkRange l c l (c + T.length newName)) newName
  --         tde = J.TextDocumentEdit vdoc (J.List [edit])
  --         -- "documentChanges" field is preferred over "changes"
  --         rsp = J.WorkspaceEdit Nothing (Just (J.List [J.InL tde])) Nothing
  --     responder (Right rsp)

  -- , requestHandler J.STextDocumentHover $ \req responder -> do
  --     liftIO $ debugM "reactor.handle" "Processing a textDocument/hover request"
  --     let J.HoverParams _doc pos _workDone = req ^. J.params
  --         J.Position _l _c' = pos
  --         rsp = J.Hover ms (Just range)
  --         ms = J.HoverContents $ J.markedUpContent "lsp-daedalus" "Your type info here!"
  --         range = J.Range pos pos
  --     responder (Right $ Just rsp)

  -- , requestHandler J.STextDocumentDocumentSymbol $ \req responder -> do
  --     liftIO $ debugM "reactor.handle" "Processing a textDocument/documentSymbol request"
  --     let J.DocumentSymbolParams _ _ doc = req ^. J.params
  --         loc = J.Location (doc ^. J.uri) (J.Range (J.Position 0 0) (J.Position 0 0))
  --         sym = J.SymbolInformation "lsp-daedalus" J.SkFunction Nothing Nothing loc Nothing
  --         rsp = J.InR (J.List [sym])
  --     responder (Right rsp)

  -- , requestHandler J.STextDocumentCodeAction $ \req responder -> do
  --     liftIO $ debugM "reactor.handle" $ "Processing a textDocument/codeAction request"
  --     let params = req ^. J.params
  --         doc = params ^. J.textDocument
  --         (J.List diags) = params ^. J.context . J.diagnostics
  --         -- makeCommand only generates commands for diagnostics whose source is us
  --         makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-daedalus") _m _t _l) = [J.Command title cmd cmdparams]
  --           where
  --             title = "Apply LSP hello command:" <> head (T.lines _m)
  --             -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
  --             cmd = "lsp-daedalus-command"
  --             -- need 'file' and 'start_pos'
  --             args = J.List
  --                     [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
  --                     , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
  --                     ]
  --             cmdparams = Just args
  --         makeCommand (J.Diagnostic _r _s _c _source _m _t _l) = []
  --         rsp = J.List $ map J.InL $ concatMap makeCommand diags
  --     responder (Right rsp)

  -- , requestHandler J.SWorkspaceExecuteCommand $ \req responder -> do
  --     liftIO $ debugM "reactor.handle" "Processing a workspace/executeCommand request"
  --     let params = req ^. J.params
  --         margs = params ^. J.arguments

  --     liftIO $ debugM "reactor.handle" $ "The arguments are: " ++ show margs
  --     responder (Right (J.Object mempty)) -- respond to the request

  --     void $ withProgress "Executing some long running command" Cancellable $ \update ->
  --       forM [(0 :: Double)..10] $ \i -> do
  --         update (ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
  --         liftIO $ threadDelay (1 * 1000000)
  ]

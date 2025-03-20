{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This modules has the LSP specific server code, including intialising LSP

module Daedalus.LSP.Server where


import qualified Data.ByteString.Lazy.Char8    as BSL

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM.TChan
import qualified Control.Exception             as E
import           Control.Lens                  hiding (Fold, Iso)
import           Control.Monad                 (forever, void)
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.Text                     as Text

import           Data.Aeson                    (encode, fromJSON)
import           Data.Aeson.Types              (Result (..))
import           Data.Maybe                    (fromMaybe)

import qualified Language.LSP.Protocol.Lens    as J
import qualified Language.LSP.Protocol.Message as J
import qualified Language.LSP.Protocol.Types   as J
import           Language.LSP.Server
import           System.Log.Logger

-- import           Daedalus.PP

import           Daedalus.LSP.Command
import           Daedalus.LSP.LanguageFeatures
import           Daedalus.LSP.Monad
import           Daedalus.LSP.Worker           (requestParse, worker)


-- SI for server impl.

newtype ReactorInput
  = ReactorAction (IO ())

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  let
    serverDefinition = ServerDefinition
      { defaultConfig = Config ()
      , onConfigChange = \_old v -> do
          case fromJSON v of
            Error e -> Left (Text.pack e)
            Success cfg -> Right cfg
      , doInitialize = \env _ -> do
          sst <- emptyServerState env
          void $ forkIO (reactor rin)
          void $ forkIO (worker sst)
          pure (Right sst)
      , staticHandlers = lspHandlers rin
      , interpretHandler = \sst -> Iso (runServerM sst) liftIO
      , options = lspOptions
      }

  -- FIXME: add logging
  flip E.finally finalProc (runServer serverDefinition)

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
  , J._change            = Just J.TextDocumentSyncKind_Incremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { optTextDocumentSync = Just syncOptions
  , optExecuteCommandCommands = Just supportedCommands
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
lspHandlers :: TChan ReactorInput -> a -> Handlers ServerM
lspHandlers rin _client_caps = mapHandlers goReq goNot handle
  where
    goReq :: forall (a :: J.Method 'J.ClientToServer 'J.Request). Handler ServerM a -> Handler ServerM a
    goReq f = \msg k -> do
      sst <- ask
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runServerM sst $ f msg k)

    goNot :: forall (a :: J.Method 'J.ClientToServer 'J.Notification). Handler ServerM a -> Handler ServerM a
    goNot f = \msg -> do
      sst <- ask
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runServerM sst $ f msg)

debounceTime :: Int
debounceTime = 100 -- ms

-- | Where the actual logic resides for handling requests and notifications.
handle :: Handlers ServerM
handle = mconcat
  [ notificationHandler J.SMethod_Initialized $ \_msg -> do
      caps <- getClientCapabilities
      liftIO $ debugM "reactor.handle" $ "Processing the Initialized notification" ++ BSL.unpack (encode caps)

  , notificationHandler J.SMethod_TextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument
        uri = doc ^. J.uri . to J.toNormalizedUri

    sst <- ask
    liftIO $ debugM "reactor.handle" $ "Processing didOpen for: " ++ show doc

    liftIO $ requestParse sst uri (doc ^. J.version) debounceTime

  , notificationHandler J.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
      cfg <- getConfig
      liftIO $ debugM "configuration changed: " (show (msg,cfg))

  , notificationHandler J.SMethod_TextDocumentDidChange $ \msg -> do
    let doc  = msg ^. J.params
                    . J.textDocument

    let uri = doc ^. J.uri . to J.toNormalizedUri
    liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show uri

    sst <- ask

    liftIO $ requestParse sst uri (doc ^. J.version) debounceTime

  -- , notificationHandler J.STextDocumentDidSave $ \msg -> do
  --     let doc = msg ^. J.params . J.textDocument . J.uri
  --         fileName = J.uriToFilePath doc
  --     liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
  --     sendDiagnostics (J.toNormalizedUri doc) Nothing

  , requestHandler J.SMethod_TextDocumentDefinition $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/rename request"
      let params = req ^. J.params
          uri  = params ^. J.textDocument
                         . J.uri
                         . to J.toNormalizedUri
          pos = params ^. J.position
      definition responder uri pos

  , requestHandler J.SMethod_TextDocumentRename $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/rename request"
      let params = req ^. J.params
          uri  = params ^. J.textDocument
                         . J.uri
                         . to J.toNormalizedUri
          pos = params ^. J.position
          newName = params ^. J.newName

      -- vdoc <- getVersionedTextDoc (params ^. J.textDocument)
      rename responder uri pos newName -- vdoc
  
  , requestHandler J.SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/semanticTokens/full request"
      let params = req ^. J.params
          uri  = params ^. J.textDocument
                         . J.uri
                         . to J.toNormalizedUri

      semanticTokens responder Nothing uri

  , requestHandler J.SMethod_TextDocumentSemanticTokensRange $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/semanticTokens/range request"
      let params = req ^. J.params
          uri  = params ^. J.textDocument
                         . J.uri
                         . to J.toNormalizedUri
          r    = params ^. J.range

      semanticTokens responder (Just r) uri

      -- -- Replace some text at the position with what the user entered
      -- let edit = J.InL $ J.TextEdit (J.mkRange l c l (c + T.length newName)) newName
      --     tde = J.TextDocumentEdit vdoc (J.List [edit])
      --     -- "documentChanges" field is preferred over "changes"
      --     rsp = J.WorkspaceEdit Nothing (Just (J.List [J.InL tde])) Nothing
      -- responder (Right rsp)

  , requestHandler J.SMethod_TextDocumentHover $ \req responder -> do
      let uri  = req ^. J.params
                      . J.textDocument
                      . J.uri
                      . to J.toNormalizedUri
          pos = req ^. J.params . J.position

      liftIO $ debugM "reactor.handle" ("Processing a textDocument/hover request " ++ show uri ++ " at " ++ show pos)
      hover responder uri pos

  , requestHandler J.SMethod_TextDocumentDocumentHighlight $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/documentHighlight request"
      let uri  = req ^. J.params
                      . J.textDocument
                      . J.uri
                      . to J.toNormalizedUri
          pos = req ^. J.params . J.position
      highlight responder uri pos

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

  , requestHandler J.SMethod_WorkspaceExecuteCommand $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a workspace/executeCommand request"
      let params = req    ^. J.params
          cmd          = params ^. J.command
          args  = fromMaybe mempty (params ^. J.arguments)

      liftIO $ debugM "reactor.handle" $ Text.unpack cmd ++ ": " ++ show args
      let resp' res = do
            case res of
              Left err -> liftIO $ debugM "reactor.handle" ("Request failed: " ++ show err)
              _ -> pure ()
            responder res

      executeCommand resp' cmd args
  ]

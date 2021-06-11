{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

-- Based upon lsp/lsp/example/Reactor.hs from the lsp package

{- |
This is an example language server built with haskell-lsp using a 'Reactor'
design. With a 'Reactor' all requests are handled on a /single thread/.
A thread is spun up for it, which repeatedly reads from a 'TChan' of
'ReactorInput's.
The `lsp` handlers then simply pass on all the requests and
notifications onto the channel via 'ReactorInput's.
This way there is the option of executing requests on multiple threads, without
blocking server communication.

To try out this server, install it with
> cabal install lsp-demo-reactor-server -fdemo
and plug it into your client of choice.
-}
module Main (main) where

import qualified Data.Text as Text
import Data.Text (Text)

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                     as E
import           Control.Lens hiding (Iso)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Reader
import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import           GHC.Generics (Generic)
import           Language.LSP.Server
import           Language.LSP.Diagnostics
import qualified Language.LSP.Types            as J
import           Language.LSP.Types (Diagnostic(..))

import qualified Language.LSP.Types.Lens       as J
import           Language.LSP.VFS
import           System.Exit
import           System.Log.Logger
import           Control.Concurrent

import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Map (Map)
import qualified Data.Map as Map


import Daedalus.Parser.Monad (ParseError(..))
import Daedalus.Parser (parseFromText)

import Daedalus.Scope (resolveModule, ScopeError(..))

import Daedalus.Type.Monad (TypeError(..), runMTypeM)
import Daedalus.Module (ModuleException(..))
import Daedalus.Driver
import Daedalus.AST (Located(..), ModuleName, Module)
import Daedalus.Type.AST (TCModule)

import Daedalus.GUID
import Daedalus.Pass
import Daedalus.PP
import Daedalus.SourceRange

import CommandLine
import Data.Maybe (fromMaybe)
import Daedalus.Type (inferRules)

-- ----------------------------------------------------------------------------------------
-- Server monad

data ServerState =
  ServerState { _parsedModules :: Map ModuleName Module -- parsed, scoped
              , _tcModules     :: Map ModuleName (TCModule SourceRange)
              , _nextGUID      :: GUID
              }

makeLenses ''ServerState

type ServerM = ReaderT (TVar ServerState) (LspM Config)

-- This needs to be behind a IO var of some sort as we need an Iso in
-- interpretHandler (so we thread the state using mutation).

emptyServerState :: ServerState
emptyServerState = ServerState mempty mempty firstValidGUID

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
    . over tcModules     (Map.delete m)
  
-- ---------------------------------------------------------------------

main :: IO ()
main = do
  _ <- getOptions
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c


-- User-alterable config
data Config = Config ()
  deriving (Generic, J.ToJSON, J.FromJSON, Show)

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  sst <- atomically (newTVar emptyServerState)

  let
    serverDefinition = ServerDefinition
      { defaultConfig = Config ()
      , onConfigurationChange = \_old v -> do
          case J.fromJSON v of
            J.Error e -> Left (T.pack e)
            J.Success cfg -> Right cfg
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

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

newtype ReactorInput
  = ReactorAction (IO ())

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.NormalizedUri -> Maybe Int -> LspM Config ()
sendDiagnostics fileUri version = do
  let
    diags = [J.Diagnostic
              (J.Range (J.Position 0 1) (J.Position 0 5))
              (Just J.DsWarning)  -- severity
              Nothing  -- code
              (Just "lsp-daedalus") -- source
              "Example diagnostic message\n With another line"
              Nothing -- tags
              (Just (J.List []))
            ]
  let maxDiags = 100
            
  publishDiagnostics maxDiags fileUri version (partitionBySource diags)

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
    goReq :: forall (a :: J.Method J.FromClient J.Request). Handler ServerM a -> Handler ServerM a
    goReq f = \msg k -> do
      env <- getLspEnv
      sst <- ask
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runServerM sst env $ f msg k)

    goNot :: forall (a :: J.Method J.FromClient J.Notification). Handler ServerM a -> Handler ServerM a
    goNot f = \msg -> do
      env <- getLspEnv
      sst <- ask      
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runServerM sst env $ f msg)

-- | Where the actual logic resides for handling requests and notifications.
handle :: Handlers ServerM
handle = mconcat
  [ notificationHandler J.SInitialized $ \_msg -> do
      caps <- getClientCapabilities
      liftIO $ debugM "reactor.handle" $ "Processing the Initialized notification" ++ BSL.unpack (J.encode caps)

  , notificationHandler J.STextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument
    liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show doc
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

-- ---------------------------------------------------------------------
-- Diagnostics

handleChangedDocument :: J.NormalizedUri -> J.TextDocumentVersion -> ServerM ()
handleChangedDocument uri version = do
  mdoc <- getVirtualFile uri
  case (mdoc, version) of
    -- Can we race the server thread?  It is possible to imagine that
    -- if the reactor is a bit slow then the server might update the
    -- doc before we start, so we check here and do nothing if we are
    -- out of date.
    (Just vf, Just version') | virtualFileVersion vf == version' -> goParse (virtualFileText vf)
    (Just vf, Nothing)  -> goParse (virtualFileText vf)
    _ ->  liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show uri
  where
    mn = Text.pack $ fromMaybe "Main" (J.uriToFilePath (J.fromNormalizedUri uri))
    maxDiags = 100
    reportDiags = publishDiagnostics maxDiags uri version . partitionBySource 

    goParse txt = 
      case parseFromText (J.getUri (J.fromNormalizedUri uri))  mn txt of
        Left err -> reportDiags (toDiagnostics err)
        Right m  -> goScope m

    goScope m = do
      r <- liftPassM (resolveModule mempty m)
      case r of
        Left err -> reportDiags (toDiagnostics err)
        Right (m, _gs) -> goTC m

    goTC m = do
      r <- liftPassM (runMTypeM mempty mempty (inferRules m))
      case r of
        Left err  -> reportDiags (toDiagnostics err)
        Right _m' -> reportDiags [] -- report no diagnostics to clear diagnostics on the client

sourcePosToPosition :: SourcePos -> J.Position
sourcePosToPosition sp = J.Position (sourceLine sp - 1) (sourceColumn sp - 1)

sourcePosToRange :: SourcePos -> J.Range
sourcePosToRange sp = J.Range pos pos -- FIXME: same point? 
  where
    pos = sourcePosToPosition sp

sourceRangeToRange :: SourceRange -> J.Range
sourceRangeToRange sr = J.Range (sourcePosToPosition (sourceFrom sr))
                                (over J.character (+ 1) (sourcePosToPosition (sourceTo sr)))

-- FIXME: we should never really need these? (from haskell-language-server)
noRange :: J.Range
noRange =  J.Range (J.Position 0 0) (J.Position 1 0)

makeDiagnosticText :: J.Range -> Text -> Diagnostic
makeDiagnosticText r msg = 
  Diagnostic { _range    = r
             , _severity = Just J.DsError
             , _source   = Nothing -- ??
             , _message  = msg
             , _code     = Nothing
             , _relatedInformation = Nothing
             , _tags     = Nothing
             }

makeDiagnostic :: PP a => J.Range -> a -> Diagnostic
makeDiagnostic r msg = makeDiagnosticText r (Text.pack (showPP msg))

class ToDiagnostics a where
  toDiagnostics :: a -> [Diagnostic]

instance ToDiagnostics ParseError where
  toDiagnostics (ParseError { errorLoc = sp, errorMsg = msg }) = [ makeDiagnosticText (sourcePosToRange sp) (Text.pack msg) ]

instance ToDiagnostics ScopeError where
  toDiagnostics serr = [ makeDiagnostic noRange serr ]

instance ToDiagnostics TypeError where
  toDiagnostics (TypeError l) = [ makeDiagnosticText (sourceRangeToRange $ thingRange l) (Text.pack (show (thingValue l))) ]

-- daedalusErrorToDiagnostics :: DaedalusError -> [Diagnostic]
-- daedalusErrorToDiagnostics err =
--   case err of
--     AParseError (ParseError { errorLoc = sp, errorMsg = msg }) ->
--       [ makeDiagnosticText (sourcePosToRange sp) (Text.pack msg) ]
      
--     -- FIXME: we should add positions to these!
--     AModuleError merr -> [ makeDiagnostic noRange merr ]

--     -- FIXME: we should add positions to these!                         
--     AScopeError  serr -> [ makeDiagnostic noRange serr ]
    
--     ASpecializeError str -> [ makeDiagnosticText noRange (Text.pack str) ]
--     ADriverError     str -> [ makeDiagnosticText noRange (Text.pack str) ]


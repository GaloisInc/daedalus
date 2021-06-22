{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | This modules has the LSP specific server code, including intialising LSP

module Daedalus.LSP.Server where


import qualified Data.ByteString.Lazy.Char8   as BSL

import           Control.Lens                 hiding (Iso, Fold)
import           Data.Aeson                   (encode, fromJSON)
import           Data.Aeson.Types             (Result (..))
import qualified Data.Text                    as Text

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM.TChan
import qualified Control.Exception            as E
import           Control.Monad.Reader
import           Control.Monad.STM

import           Language.LSP.Server
import qualified Language.LSP.Types           as J
import qualified Language.LSP.Types.Lens      as J
import           System.Log.Logger

import Data.Parameterized.Some

import           Daedalus.LSP.Diagnostics (requestParse, sourceRangeToRange)
import           Daedalus.LSP.Monad
import Daedalus.PP
import qualified Data.Map as Map
import Control.Concurrent.STM (readTVar)
import Daedalus.SourceRange
import Daedalus.Type.AST
import Daedalus.Rec
import Daedalus.Type.Traverse (foldMapTC)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Foldable

newtype ReactorInput
  = ReactorAction (IO ())

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  let
    serverDefinition = ServerDefinition
      { defaultConfig = Config ()
      , onConfigurationChange = \_old v -> do
          case fromJSON v of
            Error e -> Left (Text.pack e)
            Success cfg -> Right cfg
      , doInitialize = \env _ -> do
          void $ forkIO (reactor rin)
          sst <- emptyServerState env
          pure (Right sst)
      , staticHandlers = lspHandlers rin
      , interpretHandler = \sst -> Iso (runServerM sst) liftIO
      , options = lspOptions
      }

  flip E.finally finalProc $ do
    setupLogger Nothing ["reactor", "worker"] DEBUG
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
      sst <- ask
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runServerM sst $ f msg k)

    goNot :: forall (a :: J.Method 'J.FromClient 'J.Notification). Handler ServerM a -> Handler ServerM a
    goNot f = \msg -> do
      sst <- ask
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runServerM sst $ f msg)

debounceTime :: Int
debounceTime = 350 -- ms

-- | Where the actual logic resides for handling requests and notifications.
handle :: Handlers ServerM
handle = mconcat
  [ notificationHandler J.SInitialized $ \_msg -> do
      caps <- getClientCapabilities
      liftIO $ debugM "reactor.handle" $ "Processing the Initialized notification" ++ BSL.unpack (encode caps)

  , notificationHandler J.STextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument
    sst <- ask
    let uri = doc ^. J.uri . to J.toNormalizedUri
    let Just mn = uriToModuleName uri -- FIXME
    liftIO $ debugM "reactor.handle" $ "Processing didOpen for: " ++ show doc ++ " for " ++ showPP mn

    liftIO $ requestParse sst mn (ClientModule uri (Just (doc ^. J.version))) debounceTime

  , notificationHandler J.SWorkspaceDidChangeConfiguration $ \msg -> do
      cfg <- getConfig
      liftIO $ debugM "configuration changed: " (show (msg,cfg))

  , notificationHandler J.STextDocumentDidChange $ \msg -> do
    let doc  = msg ^. J.params
                    . J.textDocument

    let uri = doc ^. J.uri . to J.toNormalizedUri
    liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show uri

    sst <- ask
    let Just mn = uriToModuleName uri -- FIXME

    liftIO $ requestParse sst mn (ClientModule uri (doc ^. J.version)) debounceTime

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

  , requestHandler J.STextDocumentHover $ \req responder -> do
      let uri  = req ^. J.params
                      . J.textDocument
                      . J.uri
                      . to J.toNormalizedUri
          pos = req ^. J.params . J.position
          
      liftIO $ debugM "reactor.handle" ("Processing a textDocument/hover request " ++ show uri ++ " at " ++ show pos)
      hover responder uri pos

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

-- FIXME: what about version ?
-- FIXME: check uri against module's URI
hover :: (Either J.ResponseError (Maybe J.Hover) -> ServerM ()) -> J.NormalizedUri -> J.Position -> ServerM ()
hover resp uri pos = do
  sst <- ask
  let Just mn = uriToModuleName uri -- FIXME
  e_tc <- liftIO $ atomically $ do
    mods <- readTVar (knownModules sst)
    case Map.lookup mn mods of
      Nothing -> pure $ Left $ J.ResponseError J.InvalidParams "Missing module" Nothing
      Just mi -> Right <$> readTVar (moduleTC mi)

  -- case e_tc of
  --   Right (Just m) -> do
  --     let Just d = declAtPos pos m
  --     liftIO $ debugM "reactor.hover" (show pos ++ "\n" ++ show (exprTree d))
      
  --   _ -> liftIO $ debugM "reactor.hover" "Didn't find a tc'd module"

  -- case e_tc of
  --   Right Nothing -> liftIO $ debugM "reactor.hover" "Module hasn't been type checked"
  --   Right (Just m) -> do
  --     liftIO $ debugM "reactor.hover" "Found a module"
  --     void $ traverse (liftIO . debugM "reactor.hover" . (show . bullets . map (viewSome pp)))
  --       $ typeAtModule pos m
        
  --   _ -> liftIO $ debugM "reactor.hover" "Didn't find a tc'd module"

  -- resp (Right Nothing)
  resp $ fmap (doHover pos =<<) e_tc

doHover :: J.Position -> TCModule SourceRange -> Maybe J.Hover
doHover pos m = do
  (ty, r) <- getAlt $ typeAtModule pos m
  let ms = J.HoverContents $ J.markedUpContent "lsp-daedalus" (Text.pack (showPP ty))
  pure $ J.Hover ms (Just (sourceRangeToRange r))

typeAtModule :: J.Position -> TCModule SourceRange -> Alt Maybe (Type, SourceRange)
typeAtModule pos m = foldMap (typeAtDecl pos) (forgetRecs (tcModuleDecls m))

declAtPos :: J.Position -> TCModule SourceRange -> Maybe (TCDecl SourceRange)
declAtPos pos m = find (positionInRange pos . tcDeclAnnot) (forgetRecs (tcModuleDecls m))
  
typeAtDecl :: J.Position -> TCDecl SourceRange -> Alt Maybe (Type, SourceRange)
typeAtDecl pos d | not (positionInRange pos (tcDeclAnnot d)) = mempty
typeAtDecl pos TCDecl { tcDeclName = n, tcDeclParams = ps, tcDeclDef = def
                      , tcDeclAnnot = r } =
  tryOne' n def <> foldMap tryOne ps <> defOne <> fallback
  where
    defOne = case def of
      ExternDecl _ -> mempty
      Defined tc   -> typeAtTC pos tc

    tryOne :: (HasRange a, TypeOf a) => a -> Alt Maybe (Type, SourceRange)
    tryOne v = tryOne' v v

    tryOne' :: (HasRange a, TypeOf b) => a -> b -> Alt Maybe (Type, SourceRange)
    tryOne' v v' = Alt $ if positionInRange pos v then Just (typeOf v', range v) else Nothing
    
    -- If nothing else, return the whole thing
    fallback = Alt (Just (typeOf def, r))

typeAtTC :: J.Position -> TC SourceRange k -> Alt Maybe (Type, SourceRange)
typeAtTC pos tc = do
  exprs <- positionToExprs pos tc
  Alt $ case reverse exprs of
    [] -> Nothing -- means the position isn't inside tc
    -- We need to 'fixup' the results of exprs at, as we may be looking at e.g. a binder
    Some tc' : _ -> case texprValue tc' of
      TCDo (Just n) _ _ | positionInRange pos n  -> Just (typeOf n, range n)
      TCCall fn _ _     | positionInRange pos fn -> Just (typeOf fn, range fn) -- FIXME: this just returns the result type, not the fn type
      TCFor Loop { loopFlav = Fold n _ } | positionInRange pos n  -> Just (typeOf n, range n)
      TCFor Loop { loopKName = Just n  } | positionInRange pos n  -> Just (typeOf n, range n)
      TCFor Loop { loopElName = n  }     | positionInRange pos n  -> Just (typeOf n, range n)
      _ -> Just (typeOf tc', range tc') -- Fallthough, at least return something.

exprTree :: TCDecl SourceRange -> Doc
exprTree TCDecl { tcDeclDef = ExternDecl _  } = "external"
exprTree TCDecl { tcDeclDef = Defined def } = bullets (go def)
  where
    go :: forall k. TC SourceRange k -> [Doc]
    go tc =
      let kids = foldMapTC go tc
      in [ hang (text (prettySourceRange (range tc)) $$ pp tc) 4 (bullets kids) ]
        
-- This assumes that a position cannot be in sibling expressions
positionToExprs :: J.Position -> TC SourceRange k -> Alt Maybe [Some (TC SourceRange)]
positionToExprs pos = go
  where
    go :: forall k. TC SourceRange k -> Alt Maybe [Some (TC SourceRange)]
    go tc | not (positionInRange pos tc) = mempty
    -- Nothing means either no matches, or no children.  We know _we_
    -- match, so either some child also matches, or no TC child
    -- matches (but some non-tc type may match, e.g. the variable in a
    -- do).
    go tc = Alt (Just (Some tc : fromMaybe [] (getAlt $ foldMapTC go tc)))

positionInRange :: HasRange a => J.Position -> a -> Bool
positionInRange (J.Position line0 col0) (range -> SourceRange start end) =
  (sourceLine start < line || (sourceLine start == line && sourceColumn start <= col))
  &&
  (sourceLine end > line || (sourceLine end == line && sourceColumn end >= col))
  where
    line = line0 + 1
    col  = col0  + 1

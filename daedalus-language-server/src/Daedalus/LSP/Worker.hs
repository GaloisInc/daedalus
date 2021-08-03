{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the worker thread, i.e., the thread which
-- does all the parsing and type checking.
{-# LANGUAGE DeriveFunctor #-}
module Daedalus.LSP.Worker (requestParse, worker) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, async, cancel)
import           Control.Concurrent.STM   (atomically, readTChan, writeTChan,
                                           writeTVar)
import           Control.Lens             hiding ((<.>))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Either              (partitionEithers)
import           Data.Foldable            (traverse_)
import           Data.Functor
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           System.FilePath

import           Daedalus.AST             (Module (..))
import           Daedalus.Module          (pathToModuleName)
import           Daedalus.PP              hiding ((<.>))
import           Daedalus.Panic
import           Daedalus.Parser          (parseFromTokens)
import           Daedalus.Parser.Lexer    (Lexeme, Token, lexer)
import           Daedalus.Pass            (PassM, PassState, runPassM')
import           Daedalus.Rec             (Rec (..), forgetRecs, topoOrder)
import           Daedalus.Scope           (GlobalScope, resolveModule)
import           Daedalus.SourceRange
import           Daedalus.Type            (inferRules)
import           Daedalus.Type.AST        (Located (..), ModuleName,
                                           TCModule (..), TCTyDecl, TCTyName,
                                           declTypeOf, tcDeclName, tctyName)
import           Daedalus.Type.Monad      (RuleEnv, runMTypeM)


import           Language.LSP.Diagnostics (partitionBySource)
import           Language.LSP.Server      (publishDiagnostics, runLspT, getVirtualFile)
import           Language.LSP.Types       (Diagnostic (..))
import qualified Language.LSP.Types       as J

import           Daedalus.LSP.Diagnostics
import           Daedalus.LSP.Monad
import Language.LSP.VFS (virtualFileText, virtualFileVersion)
import System.Log.Logger (debugM)



-- -----------------------------------------------------------------------------
-- Asking the worker thread to do something

requestParse :: ServerState -> J.NormalizedUri -> J.TextDocumentVersion -> Int -> 
                IO ()
requestParse sst uri version delay = do
  m_txt <- runLspT (lspEnv sst) $ do
    mdoc <- getVirtualFile uri
    case mdoc of
      -- Can we race the req. handler thread?  It is possible to
      -- imagine that if the reactor is a bit slow then the server
      -- might update the doc before we start, so we check here and do
      -- nothing if we are out of date.

      Just vf | version == Just (virtualFileVersion vf) ->
                pure (Just (virtualFileText vf))
      _ -> do
        liftIO $ debugM "reactor.requestParse" "Requested version is out of date"
        pure Nothing

  traverse_ sendReq m_txt
  where
    sendReq txt = atomically $
      writeTChan (workerChan sst) (ChangedReq uri version delay txt)

-- The workflow is this:
-- 1. A message comes in telling us about a change to a file
-- 2. We lex the file and the set a wakeup for later (to debounce)
-- 3. Upon a wakeup, we parse any modules that need it, then scope/tc/etc as required.

-- Notes:
-- + When a dep. changes we need to rescope at least, as Names will have changed
-- + We don't expose stale results, although this might be preferable (i.e., we forget old results)
-- + There aren't going to be that many modules, we can be a bit naive in the algorithms.
-- + We shouldn't repeat work mainly to avoid flooding the client.

-- -----------------------------------------------------------------------------
-- Pass status

resetPassStatus :: PassStatus a -> PassStatus a
resetPassStatus = set statusChanged False

-- Used for when a previous pass failed, this is called on the later
-- ones so that future invocations will send proper diagnostics
cleanPassState :: Bool -> PassStatus a -> PassStatus a
cleanPassState True  (PassStatus _ NotStarted) = PassStatus False NotStarted
cleanPassState False (PassStatus _ ErrorStatus) = PassStatus False ErrorStatus
cleanPassState ok _ = PassStatus True (if ok then NotStarted else ErrorStatus)

emptyPassStatus :: PassStatus a
emptyPassStatus = PassStatus False NotStarted

-- Clean up after a pass has finished (tokenize resets the state anyway).

parsePostCleanup,scopePostCleanup, tcPostCleanup :: Bool -> (ModuleState -> ModuleState)

parsePostCleanup ok = over msScopeRes (cleanPassState ok) . scopePostCleanup ok
scopePostCleanup ok = over msTCRes (cleanPassState ok) . tcPostCleanup ok
tcPostCleanup _ok = id -- Nothing to do

resetModulePassStatus :: ModuleState -> ModuleState
resetModulePassStatus =
  over msTokens resetPassStatus
  . over msParsedModule resetPassStatus
  . over msScopeRes resetPassStatus
  . over msTCRes resetPassStatus

-- -----------------------------------------------------------------------------
-- Worker monad

-- Private to the worker
data WorkerState =
  WorkerState { _wsModules     :: ModuleStates
              , _wsDiagnostics :: Map ModuleSource [Diagnostic]
              , _wsTimerThread :: Maybe (Async ())              
              , _wsPassState   :: PassState -- for running the passes in Daedalus
              }

makeLenses ''WorkerState

initWorkerState :: PassState -> WorkerState
initWorkerState = WorkerState mempty mempty Nothing

newtype WorkerM a = WorkerM { getWorkerM :: StateT WorkerState IO a }
  deriving (Applicative, Functor, Monad
           , MonadState WorkerState
           , MonadIO)

runWorkerM :: WorkerM a -> WorkerState -> IO (a, WorkerState)
runWorkerM m = runStateT (getWorkerM m)

liftPassM :: PassM a -> WorkerM a
liftPassM p = do
  pState <- use wsPassState
  liftIO $ runPassM' pState p

-- -----------------------------------------------------------------------------
-- Helpers

addDiagnostic :: ModuleSource -> [Diagnostic] -> WorkerM ()
addDiagnostic ms ds =
  modifying wsDiagnostics (Map.insertWith (<>) ms ds)

data StatusAtError = MissingModule ModuleName
                   | PassPost    ModuleName

instance PP StatusAtError where
  pp (MissingModule mn) = "Module '" <> pp mn <> "' could not be found"
  pp (PassPost    mn) = "Module '" <> pp mn <> "' contains errors"

statusAt :: Lens' ModuleState (PassStatus a) ->
            ModuleName ->
            WorkerM (Bool, Either StatusAtError a)
statusAt l mn = do
  m_ms <- use (wsModules . at mn)
  case m_ms of
    Just ms
      | FinishedStatus r <- ms ^. l . passStatus
        -> pure (ms ^. l . statusChanged, Right r)
      | otherwise -> pure (ms ^. l . statusChanged, Left (PassPost mn))
    -- FIXME: is False right here?
    _ -> pure (False, Left (MissingModule mn))


-- This is used to coordinate execution of passes -- we run this for
-- every module and every pass, but the pass only gets run if a
-- dep. of the pass has changed, i.e. the previous pass succeeded, or
-- some import has changed.

runIfNeeded :: forall a a'. Lens' ModuleState (PassStatus a) ->
               Lens' ModuleState (PassStatus a') ->
               (Bool -> ModuleState -> ModuleState) ->
               (a' -> [Located ModuleName]) ->
               (ModuleName -> ModuleSource -> a' -> [a] ->
                WorkerM (Either [Diagnostic] a)) ->
               ModuleName ->
               WorkerM ()
runIfNeeded l prevl cleanup deps doIt mn = do
  (cm, e_m) <- statusAt prevl mn
  m_mst <- use (wsModules . at mn)
  case (e_m, m_mst) of
    -- not much we can do if the previou pass failed.  That pass should have
    -- cleaned up the state.
    (Left _,  _) -> pure ()
    -- Probably shouldn't happen?
    (_, Nothing) -> pure ()

    (Right m, Just mst) -> do
      let ms = mst ^. msSource
          imps = deps m
          addLoc m' = over _Left (m' $>)
          getStatus m' = over _2 (addLoc m') <$> statusAt l (thingValue m')

      (depcs, e_depvs) <- unzip <$> traverse getStatus imps
      let rerun = or (cm : depcs) -- has any dep changed, or have we never run.

      case (rerun, partitionEithers e_depvs) of
        -- Nothing has changed, so do nothing
        (False, _)      -> pure ()

        -- Something has changed, and all deps are available
        (_, ([], depvs)) -> do
          e_v <- doIt mn ms m depvs
          case e_v of
            Left err -> do
              -- FIXME: should we compare errors or something?
              setStatus False $ PassStatus True ErrorStatus
              addDiagnostic ms err

            Right r -> do
              setStatus True $ PassStatus True (FinishedStatus r)
              addDiagnostic ms [] -- clear diagnostics, at least for this pass

        -- Something has changed, but a dep is missing, so we report
        -- anyway (this might send spurious updates, but it should be
        -- infrequent).
        (_, (errs, _)) -> do
          setStatus False $ PassStatus True ErrorStatus
          addDiagnostic ms (map makeDiagnosticL errs)
  where
    setStatus :: Bool -> PassStatus a -> WorkerM ()
    setStatus ok x = wsModules . ix mn %= (set l x . cleanup ok)

moduleSourceToDir :: ModuleSource -> FilePath
moduleSourceToDir (ClientModule uri _) =
  case J.uriToFilePath (J.fromNormalizedUri uri) of
    Just fsrc -> fst (pathToModuleName fsrc)
    Nothing -> panic "Couldn't get filename" []
moduleSourceToDir (FileModule dir) = dir

-- ----------------------------------------------------------------------------------------
-- Lexing

addModule :: FilePath -> ModuleSource -> Text -> WorkerM ()
addModule filename ms txt = do
  let toks = lexer (Text.pack filename) txt
      toksSt = PassStatus True (FinishedStatus toks)
      mn = snd (pathToModuleName filename)

  wsModules . at mn .=
      Just (ModuleState ms toksSt
                        emptyPassStatus emptyPassStatus emptyPassStatus)

addModuleFromFile :: FilePath -> ModuleName -> WorkerM ()
addModuleFromFile rootDir mn = do
  let filename = rootDir </> Text.unpack mn <.> "ddl"
  txt <- liftIO $ Text.readFile filename
  addModule filename (FileModule rootDir) txt

addModuleFromRequest :: J.NormalizedUri -> J.TextDocumentVersion -> Text -> WorkerM ()
addModuleFromRequest uri version txt = do
  let ms = ClientModule uri version
      filename = case J.uriToFilePath (J.fromNormalizedUri uri) of
        Just fp -> fp
        Nothing -> panic "Couldn't get filename" []
  addModule filename ms txt

-- ----------------------------------------------------------------------------------------
-- Parsing

-- Ensure all modules are parsed (or parsing has been attempted) and
-- all imports have been loaded.  Note that after this, a missing
-- import means that the file itself was missing.  

parseAll :: WorkerM ()
parseAll = do
  allMods <- use (wsModules . to Map.keysSet)
  go allMods
  where
    go :: Set ModuleName -> WorkerM ()
    go wl | Just (mn, wl') <- Set.minView wl = do
              m_ms <- use (wsModules . at mn)
              case m_ms of
                Nothing  -> go wl'
                Just mst -> goOne (mst ^. msSource) mn wl'
    -- wl is empty
    go _wl = pure ()

    goOne ms mn wl' = do
      runIfNeeded msParsedModule msTokens parsePostCleanup
                  (const []) parseModule mn
      (_, e_m) <- statusAt msParsedModule mn
      let allImps = either (const mempty) (map thingValue . moduleImports) e_m
          rootDir = moduleSourceToDir ms
      newImps <- filterM (\mn' -> use (wsModules . to (Map.notMember mn'))) allImps
      -- Tokenize the new modules and add to state
      traverse_ (addModuleFromFile rootDir) newImps
      go (Set.union wl' (Set.fromList newImps))

-- Parse the module and ensure that any deps. are at least in the process of being loaded.
parseModule :: ModuleName -> ModuleSource -> [Lexeme Token] -> a ->
               WorkerM (Either [Diagnostic] Module)
parseModule mn ms toks _ignored =
  pure $ over _Left toDiagnostics $ parseFromTokens (Text.pack fsrc) mn toks
  where
    fsrc = case ms of
             ClientModule uri _version ->
               case J.uriToFilePath (J.fromNormalizedUri uri) of
                 Just fp -> fp
                 Nothing -> panic "Couldn't get filename" []
             FileModule dir -> dir </> Text.unpack mn <.> "ddl"

-- -----------------------------------------------------------------------------
-- Cycles + Scope

recToEither :: Rec a -> Either a [a]
recToEither (NonRec a) = Left a
recToEither (MutRec b) = Right b

-- | Returns an ordering on modules, along with any SCCs.
orderModules :: ModuleStates -> [Rec ModuleName]
orderModules mods = map (fmap moduleName) $ topoOrder order parsed
  where
    order m = (moduleName m, Set.fromList (map thingValue (moduleImports m)))
    parsed  = [ m | ms <- Map.elems mods
                  , FinishedStatus m <- [ ms ^. msParsedModule . passStatus ] ]
    -- ordered = 

    -- (nonrec, recs) = partitionEithers (map recToEither ordered)
    -- allRecs = Set.fromList (map moduleName (concat recs))

    -- mkOkR okR []       = okR
    -- mkOkR okR (m : ms)
    --   | moduleName m `Set.member` allRecs       = mkOkR okR ms
    --   | all (\mn -> thingValue mn `member` okR) (moduleImports m) = mkOkR (moduleName m : okR) ms
    --   | otherwise = mkOkR okR ms

scopeAll :: WorkerM [ModuleName]
scopeAll = do
  ordered <- use (wsModules . to orderModules)
  traverse_ go ordered
  let (nonrecs, _recs) = partitionEithers (map recToEither ordered)
  pure nonrecs
  where
    -- Module is non-cyclic, we need to scope the module    
    go (NonRec m) =
      runIfNeeded msScopeRes msParsedModule scopePostCleanup
                  moduleImports scopeModule m

    -- Cyclic import in these modules, update as required.
    -- 'runIfNeeded' would also report an error, but we want an
    -- explicit cycle error.
    go (MutRec mns) = traverse_ (cycleError mns) mns

    cycleError mns mn = do
      let err = "Cyclic imports with " <> hsep (punctuate ", " (map pp mns))
          diag lmn = makeDiagnosticText (sourceRangeToRange (range lmn))
                                        (Text.pack (show err))
      m_mst <- use (wsModules . at mn)
      case m_mst of
        -- should always trigger
        Just mst | FinishedStatus m <- mst ^. msParsedModule . passStatus -> do
                     let diags = [ diag mn' | mn' <- moduleImports m
                                            , thingValue mn' `elem` mns ]
                     addDiagnostic (mst ^. msSource) diags


        _  -> pure () -- Shouldn't happen

scopeModule :: ModuleName -> ModuleSource -> Module -> [(a, GlobalScope)] ->
               WorkerM (Either [Diagnostic] (Module, GlobalScope))
scopeModule _mn _ms m deps =
  over _Left toDiagnostics <$> liftPassM (resolveModule scope m)
  where
    scope = mconcat (map snd deps)

-- -----------------------------------------------------------------------------
-- Type checking

tcAll :: [ModuleName] -> WorkerM ()
tcAll = traverse_ go
  where
    go = runIfNeeded msTCRes msScopeRes tcPostCleanup
                     (moduleImports . fst) tcModule

tcModule :: ModuleName -> ModuleSource -> (Module, a) -> [(b, RuleEnv, Map TCTyName TCTyDecl)] ->
            WorkerM (Either [Diagnostic] (TCModule SourceRange, RuleEnv, Map TCTyName TCTyDecl))
tcModule _mn _ms (m, _) deps = do
  e_r <- liftPassM (runMTypeM importDecls importRules (inferRules m))
  pure $ case e_r of
    Left err  -> Left (toDiagnostics err)
    Right tcm -> Right (tcm, mkRules tcm, mkTDecls tcm)
  where
    -- Copied from Driver, more or less.
    -- All deps.
    mkTDecls m' =
      foldr (\d -> Map.insert (tctyName d) d)
            importDecls
            (forgetRecs (tcModuleTypes m'))

    -- Just this module
    mkRules m' = Map.fromList [ (tcDeclName d, declTypeOf d)
                              | d <- forgetRecs (tcModuleDecls m')
                              ]

    importDecls = mconcat tcdecls
    importRules = mconcat rules
    
    (_, rules, tcdecls) = unzip3 deps

-- -----------------------------------------------------------------------------
-- Worker thread

cancelTimer :: WorkerM ()
cancelTimer = do
  m_a <- use wsTimerThread
  void $ liftIO $ traverse cancel m_a
  wsTimerThread .= Nothing

startTimer :: ServerState -> Int -> WorkerM ()
startTimer sst ms = do
  cancelTimer
  a <- liftIO $ async $ do
    threadDelay (ms * 1000)
    atomically $ writeTChan (workerChan sst) WakeUpReq
  wsTimerThread .= Just a

publishDiags :: ServerState -> WorkerM ()
publishDiags sst = do
  allDiags <- use wsDiagnostics
  wsDiagnostics .= mempty
  traverse_ go (Map.toList allDiags)
  where
    go :: (ModuleSource, [Diagnostic]) -> WorkerM ()
    go (ms, diags) = 
      -- We don't send diags for non-client files (maybe we should?)
      case ms of
        FileModule {} -> pure ()
        ClientModule uri ver -> do
          let maxNDiags = 100
          void $ liftIO $ runLspT (lspEnv sst) $
            publishDiagnostics maxNDiags uri ver (partitionBySource diags)

worker :: ServerState -> IO ()
worker sst = void $ runWorkerM go (initWorkerState (passState sst))
  where
    go = forever $ do
      req <- liftIO $ atomically $ readTChan (workerChan sst)
      case req of
        ChangedReq uri ver delay txt -> do
          addModuleFromRequest uri ver txt
          publishModules
          startTimer sst delay
          
        WakeUpReq -> do
          cancelTimer
          runPasses
          publishModules
          publishDiags sst
          -- reset the changed status of all pass state.
          modifying wsModules (fmap resetModulePassStatus)

    runPasses = do
      parseAll
      mns <- scopeAll
      tcAll mns
    
    -- Make, e.g., tokens available to other parts of the server
    publishModules = do
      mods <- use wsModules
      liftIO $ atomically $ writeTVar (moduleStates sst) mods
      

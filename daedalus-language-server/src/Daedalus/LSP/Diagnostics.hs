{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Implements core diagnostics (parse errors, type errors, etc.)


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Daedalus.LSP.Diagnostics (requestParse, sourceRangeToRange) where

import           Control.Applicative          (Alternative)
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Control.Monad.State
import           Data.Foldable                (fold)
import           Data.Functor
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, isJust)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           System.FilePath

import           Control.Lens                 hiding (Iso, (<.>))

import           Language.LSP.Diagnostics     (partitionBySource)
import           Language.LSP.Server
import           Language.LSP.Types           (Diagnostic (..))
import qualified Language.LSP.Types           as J
import qualified Language.LSP.Types.Lens      as J
import           Language.LSP.VFS

import           Daedalus.PP                  hiding ((<.>))
import           Daedalus.Panic
import           Daedalus.Pass
import           Daedalus.Rec                 (forgetRecs)
import           Daedalus.SourceRange         (SourcePos (..), SourceRange (..))

import           Daedalus.AST                 (Module (..))
import           Daedalus.Module              (pathToModuleName)
import           Daedalus.Parser              (ParseError (..), parseFromText)
import           Daedalus.Scope               (Scope(..), ScopeError (..),
                                               resolveModule)
import           Daedalus.Type.AST            (Located (..), ModuleName,
                                               TCModule (..), TCTyDecl, TCDecl(..),
                                               TCTyName, tctyName, declTypeOf)
import           Daedalus.Type.Monad          (RuleEnv, TypeError (..), runMTypeM)

import           Daedalus.LSP.Monad
import Daedalus.Type (inferRules)
import System.Log.Logger


-- This module handles parsing and type checking of modules.  Note
-- that parsing can be done independently of other modules, but
-- scoping and type checking need dependencies to be processed.
--
-- We do this asynchronously as we want to debounce and also not block
-- the main thread.  As a consequence, we may have multiple parsing
-- requests outstanding for different modules (re-parsing of the same
-- module will cancel any parsing operations for previous versions of the doc.)
--
-- We thus split this into two phases: local (parsing) and global (type checking).

-- -----------------------------------------------------------------------------
-- Snapshots

-- The location here points to the import statement
type Snapshot = Map ModuleName (Located ModuleInfo)

data SnapshotDelta =
  SnapshotDelta { scopeChanged :: Bool, typeInfoChanged :: Bool }
  deriving (Eq, Show)

instance Monoid SnapshotDelta where
  mempty = SnapshotDelta False False

instance Semigroup SnapshotDelta where
  s1 <> s2 = SnapshotDelta (scopeChanged s1 || scopeChanged s2)
                           (typeInfoChanged s1 || typeInfoChanged s2)

snapshotDelta :: Snapshot -> Snapshot -> SnapshotDelta
snapshotDelta s1 s2 = fold (Map.intersectionWith comb s1 s2)
  where
    comb (thingValue -> mi1) (thingValue -> mi2) =
      SnapshotDelta (miScope mi1 /= miScope mi2)
                    (miTypeEnv mi1 /= miTypeEnv mi2)

snapshotDeps :: Module -> WorkerT STM Snapshot
snapshotDeps m = do
  modsv  <- asks (knownModules . weServerState)
  mods   <- lift $ readTVar modsv
  let imps = moduleImports m
  Map.fromList <$> mapM (snapshotOne mods) imps
  where
    snapshotOne mods lmn =
      case Map.lookup (thingValue lmn) mods of
        Just mst -> (,) (thingValue lmn) . ($>) lmn <$> lift (readTVar (moduleInfo mst))
        Nothing -> panic "Missing dep" [showPP lmn]

snapshotToScope :: Snapshot -> Map ModuleName Scope
snapshotToScope = Map.mapMaybe (miScope . thingValue)

snapshotToTypeInfo :: Snapshot -> (RuleEnv, Map TCTyName TCTyDecl)
snapshotToTypeInfo = fold . Map.mapMaybe (miTypeEnv . thingValue)

-- ---------------------------------------------------------------------
-- Worker

-- What triggers us to do work:
-- 1. Request from client (file changed, incl. startup)
--    reparse, start missing imports, check cycles, check scope, retype
-- 2. (Transitive) import changed, need to check cycles
--    check cycles, check scope, retype
-- 3. Import changed, need to check scope, types
--    check scope, retype
--
-- Whenever an import changes, we will need to reperform everything
-- after cycle checks, although ideally we wouldn't update the module
-- info unless something has changed.

data WorkerAction =
  RequestAct ParseRequest
  | SendDiagnosticAct [Diagnostic]
  | ScopeAct
  | SnapshotChangedAct SnapshotDelta
  deriving Show

data WorkerEnv =
  WorkerEnv { weServerState :: ServerState
            , weModuleName  :: ModuleName
            , weModuleState :: ModuleState
            }

newtype WorkerT m a = WorkerT { getWorkerT :: ReaderT WorkerEnv (StateT WorkerState m) a }
  deriving (Applicative, Functor, Monad
           , MonadState WorkerState, MonadReader WorkerEnv
           , Alternative, MonadPlus, MonadIO)

instance MonadTrans WorkerT where
  lift m = WorkerT (lift (lift m))

runWorkerT :: WorkerT m a -> WorkerEnv -> WorkerState -> m (a, WorkerState)
runWorkerT m e s = runStateT (runReaderT (getWorkerT m) e) s

wAtomically :: MonadIO m => WorkerT STM a -> WorkerT m a
wAtomically m = do
  e <- ask
  s <- get
  (r, s') <- liftIO $ atomically $ runWorkerT m e s
  put s'
  pure r

wPassM :: MonadIO m => PassM a -> WorkerT m a
wPassM p = do
  pState <- asks (passState . weServerState)
  liftIO $ runPassM' pState p

data PassStatus e a = NotStarted | ErrorStatus e | FinishedStatus a

newtype CycleCheckErrors = CycleCheckErrors [Located CycleCheckError]

-- We care about locations, so we can't derive as Ea for Located ignores locations
instance Eq CycleCheckErrors where
  CycleCheckErrors es1 == CycleCheckErrors es2 = all same (zip es1 es2)
    where
      same (le1, le2) = thingRange le1 == thingRange le2 && thingValue le1 == thingValue le2

data WorkerState =
  WorkerState { _wsParsedModule  :: PassStatus ParseError Module
              -- ^ The parsd module, not scoped.  We don't really need
              -- the error, but it is nice to have.

              , _wsCycleCheck    :: PassStatus CycleCheckErrors ()
              -- ^ This is a little odd -- we need to record whether
              -- we have started parsing, and if so, what the results
              -- were.
              , _wsScopeRes      :: PassStatus ScopeError (Module, Map ModuleName Scope)
              -- FIXME: this is also in the module state, maybe remove from here and use the tvar instead?
              , _wsTCRes         :: PassStatus TypeError (TCModule SourceRange, Map TCTyName TCTyDecl)

              , _wsSnapshot      :: PassStatus () Snapshot -- This can't throw an error, but this is simpler to use 
              , _wsCurrentSource :: ModuleSource
              , _wsTimerThread   :: Maybe (Async ())
              }

debugW :: (MonadIO m) => Doc -> WorkerT m ()
debugW msg = do
  mn <- asks weModuleName
  liftIO $ debugM ("worker." ++ showPP mn) (show msg)
      
makeLenses ''WorkerState

debugState :: MonadIO m => String -> WorkerT m ()
debugState m = do
  s <- get
  ts <- liftIO $ timerStatus s
  debugW $ text m <> hcat (punctuate "," [
    dbgPS s wsParsedModule "ParsedModule"
    , dbgPS s wsCycleCheck "CycleCheck"
    , dbgPS s wsScopeRes   "ScopeRes"
    , dbgPS s wsTCRes       "TCRes"
    , dbgPS s wsSnapshot    "Snapshot"
    , "Timer (" <> ts <> ")"
    ])
  where
    timerStatus s = do
      case s ^. wsTimerThread of
        Nothing -> pure "N"
        Just a  -> do
          status <- poll a
          pure $ case status of
            Nothing -> "R"
            Just _  -> "F"
        
    dbgPS s l n =
      let msg = case s ^. l of
            NotStarted -> "N"
            ErrorStatus _e -> "E"
            FinishedStatus _r -> "D"
      in n <> " (" <> msg <> ")"

emptyWorkerState :: ModuleSource -> WorkerState
emptyWorkerState ms =
  WorkerState { _wsParsedModule  = NotStarted
              , _wsCycleCheck    = NotStarted
              , _wsScopeRes      = NotStarted
              , _wsTCRes         = NotStarted
              , _wsSnapshot      = NotStarted
              , _wsCurrentSource = ms
              , _wsTimerThread   = Nothing
              }

-- Timers

-- Cancels the timer, although there is a race where it might be
-- sending the wakeup now, so we have to treat wakeups as a
-- suggestion.
cancelTimer :: MonadIO m => WorkerT m ()
cancelTimer = do
  m_a <- use wsTimerThread
  void $ liftIO $ traverse cancel m_a
  wsTimerThread .= Nothing

startTimer :: MonadIO m => Int -> WorkerT m ()
startTimer ms = do
  cancelTimer
  chan <- asks (moduleChan . weModuleState)
  a <- liftIO $ async $ do
    threadDelay (ms * 1000)
    atomically $ writeTChan chan WakeUpReq
  wsTimerThread .= Just a

finished :: Getting (PassStatus e a) WorkerState (PassStatus e a)
         -> (a -> WorkerT STM b) -> WorkerT STM b
finished getter f = do
  m_a <- use getter
  case m_a of
    FinishedStatus a -> f a
    _                -> lift retry

-- Blocks until there is something to do.  This is a long transaction,
-- but it should be OK as contention should be low.
getNextWorkerAction :: WorkerT STM WorkerAction
getNextWorkerAction = do
  msum [ nextReq
       -- If we parsed a module, we need to check that nothing has
       -- changed wrt scoping.
       , finished wsParsedModule doCycleCheck
       , finished wsParsedModule doSnapshot
       ]
  where
    nextReq = do
      chan <- asks (moduleChan . weModuleState)
      lift (RequestAct <$> readTChan chan)

    doSnapshot m = do
      m_snap <- use wsSnapshot
      case m_snap of
        FinishedStatus snap -> do
          snap' <- snapshotDeps m
          let delta = snapshotDelta snap snap'
          if delta == mempty
            then lift retry
            else do wsSnapshot .= FinishedStatus snap'
                    pure (SnapshotChangedAct delta)
        _ -> lift retry

    -- Check for cycles, and if there are errors we only report them
    -- if they are different than the previous time we checked.  If we
    -- got from an error state to a normal state, we make a snapshot
    -- and will proceed from the scoping check.
    doCycleCheck m = do
      ccr  <- use wsCycleCheck
      sst  <- asks weServerState

      ccr' <- lift $ cycleCheck sst m
      case (ccr', ccr) of
        -- Nothing to do
        (Right _  , FinishedStatus _) -> lift retry
        -- We either didn't get here last time, or this is the first time
        (Right _, _) -> do
          snap <- snapshotDeps m

          wsCycleCheck .= FinishedStatus ()
          -- Everything else should be NotStarted

          wsSnapshot   .= FinishedStatus snap

          pure ScopeAct

        -- Nothing new
        (Left errs, ErrorStatus errs') | errs == errs' -> lift retry

        (Left err, _)      -> do
          -- We have discovered some new error, we need to update
          -- the shared state and let the client know.
          wsCycleCheck .= ErrorStatus err
          wsScopeRes   .= NotStarted
          wsTCRes      .= NotStarted

          pure (SendDiagnosticAct (toDiagnostics err))

sendDiagnostics :: [Diagnostic] -> WorkerT (LspM Config) ()
sendDiagnostics diags = do
  ms <- use wsCurrentSource
  -- We don't send diags for non-client files (maybe we should?)
  case ms of
    FileModule {} -> pure ()
    ClientModule uri ver -> do
      let maxNDiags = 100
      lift $ publishDiagnostics maxNDiags uri ver (partitionBySource diags)

runIfNeeded :: Lens' WorkerState (PassStatus e a) ->
               (e -> [Diagnostic]) ->
               WorkerT (LspM Config) (Either e a) ->
               ExceptT [Diagnostic] (WorkerT (LspM Config)) a
runIfNeeded l mkD doIt = do
  p <- use l
  case p of
    FinishedStatus r -> pure r
    _ -> do
      e_v <- lift doIt
      case e_v of
        Left err -> do
          l .= ErrorStatus err
          throwError (mkD err)
        Right r -> do
          l .= FinishedStatus r
          pure r

writeIfChanged :: Eq v => TVar v -> v -> STM ()
writeIfChanged var v = do
  v' <- readTVar var
  when (v /= v') (writeTVar var v)

doNextThing :: WorkerT (LspM Config) ()
doNextThing = do
  e_r <- runExceptT go
  case e_r of
    Left d  -> sendDiagnostics d
    Right _ -> sendDiagnostics [] -- clear any errors
  wAtomically publishInfo
  where
    go = do
      m <- runIfNeeded wsParsedModule toDiagnostics parseModule
      runIfNeeded wsCycleCheck toDiagnostics (doCycleCheck m)
      snap <- runIfNeeded wsSnapshot (const []) (doSnapshot m)
      (m', _scope) <- runIfNeeded wsScopeRes toDiagnostics (doScopeCheck m snap)
      void $ runIfNeeded wsTCRes toDiagnostics (doTypeCheck m' snap)

    doCycleCheck :: Module -> WorkerT (LspM Config) (Either CycleCheckErrors ())
    doCycleCheck m = do
      sst  <- asks weServerState
      liftIO $ atomically $ cycleCheck sst m

    doSnapshot m = Right <$> wAtomically (snapshotDeps m)

    doScopeCheck m snap = do
      let scope = repairScope m (snapshotToScope snap)
      wPassM (resolveModule scope m)

    doTypeCheck m snap = do
      let (renv, tdecls) = snapshotToTypeInfo snap
      e_r <- wPassM (runMTypeM tdecls renv (inferRules m))
      let mk tcm = (tcm, tdecls)
      pure (mk <$> e_r)

    -- The scope pass expects all modules to be mapped, so we add in an
    -- empty scope for all the missing imports.  Basically a hack ...
    repairScope m scope =
      let impMap = Map.fromList [ (mn, Scope mempty) | mn <- map thingValue (moduleImports m) ]
      in scope `Map.union` impMap


-- Exports the results to other threads (based upon our state).  This
-- way means we don't have to worry to much about setting the state in
-- doNextThing.  
publishInfo :: WorkerT STM ()
publishInfo = do
  -- Imports
  iiv <- asks (moduleImportInfo . weModuleState)
  parseRes <- use wsParsedModule
  let newImps = case parseRes of
        FinishedStatus m -> Just (map thingValue $ moduleImports m)
        _ -> Nothing
  lift $ writeIfChanged iiv newImps
  
  -- ModuleInfo
  mn       <- asks weModuleName
  scopeRes <- use wsScopeRes
  tcRes    <- use wsTCRes
  let (mi0, scope) = case scopeRes of
        FinishedStatus (_, scope) -> (emptyModuleInfo { miScope = Map.lookup mn scope }, Just scope)
        _    -> (emptyModuleInfo, Nothing)

  let (m_tcm, newInfo) = case tcRes of
        FinishedStatus (tcm, decls) ->
          (Just tcm, mi0 { miTypeEnv = Just (mkRules tcm, mkTDecls decls tcm) })
        _ -> (Nothing, mi0)

  miv <- asks (moduleInfo . weModuleState)
  lift $ writeIfChanged miv newInfo

  -- ModuleResults
  ms <- use wsCurrentSource

  let mr = ModuleResults { mrSource = ms, mrImportScope = scope, mrTC = m_tcm }

  -- FIXME: always?  maybe we should guard on if it has changed (we would need eq)
  mrv <- asks (moduleResults . weModuleState)
  lift (writeTVar mrv mr)
  where
    -- Copied from Driver, more or less.
    mkTDecls importDecls m =
      foldr (\d -> Map.insert (tctyName d) d)
            importDecls
            (forgetRecs (tcModuleTypes m))

    mkRules m = Map.fromList [ (tcDeclName d, declTypeOf d)
                           | d <- forgetRecs (tcModuleDecls m)
                           ]

worker :: WorkerT (LspM Config) ()
worker = forever $ do
  a <- wAtomically getNextWorkerAction
  debugW $ "Got an action " <> text (show a)
  act a
  where
    act next =
      case next of
        -- A timer has expired, we need to do something
        RequestAct WakeUpReq -> do
          cancelTimer
          doNextThing

        RequestAct (ParseReq ms delta) -> do
          cancelTimer

          wsCurrentSource .= ms
          wsParsedModule  .= NotStarted
          wsCycleCheck    .= NotStarted
          wsScopeRes      .= NotStarted
          wsTCRes         .= NotStarted
          wsSnapshot      .= NotStarted

          -- We don't change the module info until we do something.
          if delta == 0 then doNextThing else startTimer delta

        SendDiagnosticAct diags -> do
          wAtomically publishInfo
          sendDiagnostics diags

        ScopeAct -> do
          wsScopeRes      .= NotStarted
          wsTCRes         .= NotStarted
          wsSnapshot      .= NotStarted

          doNextThing

        -- delta is guaranteed to be non-mempty
        SnapshotChangedAct delta -> do
          when (scopeChanged delta) (wsScopeRes .= NotStarted)
          wsTCRes .= NotStarted

          doNextThing

-- ---------------------------------------------------------------------
-- The worker thread and helpers

parserThread :: ServerState -> ModuleName -> ModuleState -> ModuleSource -> IO ()
parserThread sst mn mst ms = do
  debugM "worker" ("Starting worker for module " ++ showPP mn)
  void $ runLspT (lspEnv sst) $ runWorkerT worker (WorkerEnv sst mn mst) (emptyWorkerState ms)

checkStartParserThread :: ServerState -> FilePath -> ModuleName
                       -> STM (Maybe (IO ()))
checkStartParserThread sst path mn = do
  mods <- readTVar (knownModules sst)
  case Map.lookup mn mods of
    Just {} -> pure Nothing -- nothing to do, we could wake?
    Nothing -> do
      let ms = FileModule path
      mst <- newModuleState ms
      writeTVar (knownModules sst) (Map.insert mn mst mods)
      writeTChan (moduleChan mst) WakeUpReq
      pure (Just (void $ forkIO (parserThread sst mn mst ms)))

-- | Wake/start a parser.  Should be callable in the main thread
-- (shouldn't block etc.)
requestParse :: ServerState -> ModuleName -> ModuleSource -> Int -> 
                IO ()
requestParse sst mn ms delay = do  
  join $ atomically $ do
    mods <- readTVar (knownModules sst)
    case Map.lookup mn mods of
      Just mst -> do
        writeTChan (moduleChan mst) req
        pure (pure ())
      Nothing -> do -- FIXME: duped from above
        mst <- newModuleState ms
        writeTVar (knownModules sst) (Map.insert mn mst mods)        
        writeTChan (moduleChan mst) req
        pure (start mst)
  where
    start mst = void $ forkIO (parserThread sst mn mst ms)
      
    req = ParseReq ms delay

-- ---------------------------------------------------------------------
-- Parsing Operations
--
-- Each module is owned by a thread, which is responsible for parsing
-- the corresponding module.  Before any parsing results are updated
-- in the system state, any deps. must be started (so we don't have to
-- worry about recursively starting parsers)

-- To perform scoping we need to get the modules for all our deps.,
-- including their imports (to check for cycles).  We may have to
-- start the parser thread for that module if, for example, it is not
-- in the client (i.e., it is a file)

-- Parse the module and ensure that any deps. are at least in the process of being loaded.
parseModule :: WorkerT (LspM Config) (Either ParseError Module)
parseModule = do
  ms <- use wsCurrentSource
  mn <- asks weModuleName
  (fsrc, txt) <- case ms of
    ClientModule uri version -> do
      mdoc <- lift (getVirtualFile uri)
      case mdoc of
        -- Can we race the req. handler thread?  It is possible to
        -- imagine that if the reactor is a bit slow then the server
        -- might update the doc before we start, so we check here and
        -- do nothing if we are out of date.

        Just vf | version == Just (virtualFileVersion vf) -> 
                  case J.uriToFilePath (J.fromNormalizedUri uri) of
                    Just fp -> pure (fp, virtualFileText vf)
                    Nothing -> panic "Couldn't get filename" []
        -- FIXME: this seems dangerous                    
        _ -> panic "Version mismatch" []
    FileModule dir -> do
      let filename = dir </> Text.unpack mn <.> "ddl"
      txt <- liftIO $ Text.readFile filename
      pure (filename, txt)
  case parseFromText (Text.pack fsrc) mn txt of
    Left err -> pure (Left err)
    Right m  -> do
      -- FIXME: maybe move starting threads out of this function?
      sst <- asks weServerState
      let imps = map thingValue (moduleImports m)
      newThreads <- liftIO $ catMaybes <$>
        atomically (mapM (checkStartParserThread sst (fst (pathToModuleName fsrc))) imps)

      -- If this is not empty, we will block in e.g. cycle check
      -- FIXME: probably
      liftIO $ sequence_ newThreads
      -- Announce that we are at least parsed, and that others can
      -- then check for cycles etc.
      --
      -- We do this here in case we block after this.
      miv <- asks (moduleImportInfo . weModuleState)
      liftIO $ atomically $ writeIfChanged miv (Just imps)
      
      --
      -- FIXME: should we update everything else to panding?  It will
      -- probably just wake things up, when we don't need to until we
      -- have a result to report.
      pure (Right m)

-- Assumes we have already started any required threads,
-- c.f. checkStartParserThread.  This will retry if any dep. is not at
-- least parsed.  Note that we only really care if we can be reached
-- recursively, not that an imported module may have cycles (it can
-- figure that out itself, and will be detected in by that module).

-- This is tricky, as it is non-local: to figure out if we have a
-- cycle, we need to examine our imports, and theirs, etc.  Also, we
-- want to report a problem for an import, but we also want to wait
-- for a change of status of any dep., so we only retry when we
-- nothing has changed.  The behaviour will then be something like:
--
-- 1. scan all imports, transitively, collecting any errors
-- 2. if there are no errors, check for cycles involving the current module
-- 3. if there were errors, report them if they are new, otherwise
--    retry (until some dep. has changed).
--
-- It is a little gross to do this inside STM, but it shouldn't be
-- too expensive (not many modules)


cycleCheck :: ServerState -> Module -> STM (Either CycleCheckErrors ())
cycleCheck sst m = do
  mods <- readTVar (knownModules sst)
  let imps = moduleImports m
  errs <- traverse (errors mods [moduleName m] . thingValue) imps
  let allErrs = [ imp $> e | (imp, es) <- zip imps errs, e <- es ]
  if null allErrs
    then pure (Right ())
    else pure (Left (CycleCheckErrors allErrs))
  where
    -- This could visit module multiple times, if they are imported by
    -- modules we import, but this is much simpler.
    errors mods revpfx mn
      | mn `elem` revpfx = pure [CycleDetected (reverse revpfx) mn]
      | otherwise = case Map.lookup mn mods of
          Just mst -> do
            mmi <- readTVar (moduleImportInfo mst)
            case mmi of
              Just imps -> concat <$> mapM (errors mods (mn : revpfx)) imps
              -- The module didn't parse, so we can't do much until it does.
              Nothing   -> pure [UnavailableDep (reverse revpfx) mn]
          -- Module isn't yet loaded, this is a bug in the server
          -- (the parser should ensure deps. exist first)
          Nothing -> pure [MissingDep (reverse revpfx) mn]


-- -----------------------------------------------------------------------------
-- Diagnostics

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


makeDiagnosticL :: PP a => Located a -> Diagnostic
makeDiagnosticL la = makeDiagnosticText (sourceRangeToRange (thingRange la)) (Text.pack (showPP (thingValue la)))

class ToDiagnostics a where
  toDiagnostics :: a -> [Diagnostic]

instance ToDiagnostics CycleCheckErrors where
  toDiagnostics (CycleCheckErrors errs) = map makeDiagnosticL errs

instance ToDiagnostics ParseError where
  toDiagnostics (ParseError { errorLoc = sp, errorMsg = msg }) = [ makeDiagnosticText (sourcePosToRange sp) (Text.pack msg) ]

instance ToDiagnostics ScopeError where
  toDiagnostics serr = [ makeDiagnostic noRange serr ]

instance ToDiagnostics TypeError where
  toDiagnostics (TypeError l) = [ makeDiagnosticText (sourceRangeToRange $ thingRange l) (Text.pack (show (thingValue l))) ]

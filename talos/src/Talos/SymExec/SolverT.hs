{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Defines the symbolic parser API.  This wraps the SimpleSMT API

module Talos.SymExec.SolverT (
  -- * Solver interaction monad
  SolverT, runSolverT, mapSolverT, emptySolverState,
  nameToSMTName, fnameToSMTName, tnameToSMTName,
  getName,
  SolverState,
  withSolver,
  -- assert, declare, check,
  solverOp, solverState,
  getValue,
  -- * Context management
  SolverContext, getContext, restoreContext, scoped,
  -- * Functions
  SMTFunDef(..), defineSMTFunDefs,
  -- * SMT Polymorphic functions
  PolyFun (..), defineSMTPolyFun, getPolyFun,
  -- * Types
  SMTTypeDef(..), defineSMTTypeDefs,
  typeNameToDefault,
  -- * Context management
  modifyCurrentFrame, bindName, -- FIXME: probably should be hidden
  freshName, freshSymbol, defineName, declareName, declareSymbol, knownFNames,
  reset, assert, check

  ) where

import Control.Lens
import           Control.Applicative
import           Control.Monad.State
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           SimpleSMT             (SExpr, Solver)
import qualified SimpleSMT             as S

import           Daedalus.Core  hiding (freshName)
import qualified Daedalus.Core  as C
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic

import           Talos.SymExec.StdLib
import GHC.Generics (Generic)
import Data.Foldable (for_)
import Data.Function (on)
-- import Text.Printf (printf)

-- The name of a polymorphic function (map lookup, map insertion, etc)
data PolyFun = PMapLookup SExpr SExpr -- kt vt
             | PMapInsert SExpr SExpr -- kt vt
             | PMapMember SExpr SExpr -- kt vt
             deriving (Eq, Ord, Show)

data QueuedCommand =
  QCAssert SExpr
  | QCDeclare String SExpr
  | QCDefine  String SExpr SExpr

-- We manage this explicitly to make sure we are in synch with the
-- solver as push/pop are effectful.
data SolverFrame = SolverFrame
  { frId        :: !Int -- ^ The index of the choice which led to this frame
  , frCommands   :: ![QueuedCommand]
  , frBoundNames :: !(Map Name String)
  -- ^ May include names bound in closed scopes, to allow for
  --
  -- def P = { x = { $$ = UInt8; $$ > 10 }, ...}
  --
  -- where the execution of the body of x will return
  -- '$$' as it's symbolc result
  } deriving (Generic)

emptySolverFrame :: Int -> SolverFrame
emptySolverFrame i = SolverFrame
  { frId = i
  , frCommands = mempty
  , frBoundNames = mempty
  }

nullSolverFrame :: SolverFrame -> Bool
nullSolverFrame fr = Map.null (frBoundNames fr) && null (frCommands fr)

newtype SolverContext = SolverContext { _getSolverContext :: [SolverFrame] }

data SolverState = SolverState
  { solver       :: !Solver
    -- These have to be defined at the top level (before we start pushing etc.)
  , ssKnownTypes :: !(Set TName)
  , ssKnownFuns  :: !(Set FName)
  , ssKnownPolys :: !(Map PolyFun String)

  , ssFrames          :: ![SolverFrame]
  , ssNFlushedFrames  :: !Int

  , ssCurrentFrame    :: !SolverFrame
  , ssNCurrentFlushed :: !Int

  , ssNextFrameID     :: !Int
  } deriving (Generic)

emptySolverState :: Solver -> SolverState
emptySolverState s = SolverState
  { solver = s
  , ssKnownTypes = mempty
  , ssKnownFuns  = mempty
  , ssKnownPolys = mempty

  , ssFrames          = mempty
  , ssNFlushedFrames  = 0
  , ssCurrentFrame    = emptySolverFrame 0
  , ssNCurrentFlushed = 0
  , ssNextFrameID     = 1
  }

inCurrentFrame :: Monad m => (SolverFrame -> SolverT m a) -> SolverT m a
inCurrentFrame f = SolverT (gets ssCurrentFrame) >>= f

overCurrentFrame :: Monad m => (SolverFrame -> SolverT m (a, SolverFrame)) -> SolverT m a
overCurrentFrame f = do
  (r, frame') <- inCurrentFrame f
  SolverT (modify (\s -> s { ssCurrentFrame = frame' }))
  pure r

modifyCurrentFrame :: Monad m => (SolverFrame -> SolverFrame) -> SolverT m ()
modifyCurrentFrame f = overCurrentFrame (\s -> pure ((), f s))

-- push :: MonadIO m => SolverT m ()
-- push = do
--   -- We push lazily
--   SolverT (modify (\s -> s { pendingPushes = pendingPushes s + 1 }))
--   -- solverOp S.push

-- pop :: MonadIO m => SolverT m ()
-- pop = do
--   n <- SolverT $ gets pendingPushes
--   if n > 0
--     then SolverT (modify (\s -> s { pendingPushes = pendingPushes s - 1 }))
--     else do
--     fs <- SolverT (gets frames)
--     case fs of
--       [] -> panic "Attempted to pop past top of solver stack" []
--       (f : fs') -> do
--         SolverT (modify (\s -> s { currentFrame = f, frames = fs' }))
--         solverOp S.pop

-- popAll :: MonadIO m => SolverT m ()
-- popAll = do
--   fs <- SolverT (gets frames)
--   case reverse fs of
--     [] -> pure () -- do nothing
--     (topF : _rest) -> do
--       solverOp (\s -> S.popMany s (fromIntegral $ length fs))
--       SolverT (modify (\s -> s { frames = [], currentFrame = topF, pendingPushes = 0 }))


execQueuedCommand :: MonadIO m => QueuedCommand -> SolverT m ()
execQueuedCommand qc =
  solverOp $ \s ->
    case qc of
      QCAssert se      -> S.assert s se
      QCDeclare v ty   -> void $ S.declare s v ty
      QCDefine  v ty e -> void $ S.define s v ty e

-- | Flushes all pending commands to the solver
flush :: MonadIO m => SolverT m ()
flush = do
  pending <- SolverT $ gets (\s -> drop (ssNFlushedFrames s) (ssFrames s))
    
  for_ pending $ \frame -> do
    mapM_ execQueuedCommand (frCommands frame)
    solverOp S.push

  currentPending <- SolverT $ gets $ \s ->
    drop (ssNCurrentFlushed s) (frCommands (ssCurrentFrame s))

  mapM_ execQueuedCommand currentPending
  SolverT . modify $ \s -> do
    s { ssNFlushedFrames  = length (ssFrames s)
      , ssNCurrentFlushed = length (frCommands (ssCurrentFrame s))
      }

pushFrame :: MonadIO m => Bool -> SolverT m ()
pushFrame force = do
  s <- SolverT get
  let cf = ssCurrentFrame s
  -- If we have partially executed some commands, execute the rest
  when (ssNCurrentFlushed s > 0) $ do
    flush
    solverOp S.push -- FIXME: This feels wrong here.
    SolverT $ field @"ssNFlushedFrames" += 1
      
  when (force || not (nullSolverFrame cf)) $ do
    SolverT $ field @"ssFrames" <>= [cf]
    resetCurrentFrame
  
getContext :: MonadIO m => SolverT m SolverContext
getContext = do
  -- This is a bit odd, but it is always OK to push additional frames
  -- (from a correctness POV) as we disassociate push/pop with
  -- backtracking
  pushFrame False
  SolverContext <$> SolverT (gets ssFrames)

-- We always have a common frame representing the global frame.  It is
-- common to all contexts, so should never be popped.
scoped :: MonadIO m => SolverT m a -> SolverT m a
scoped m = do
  pushFrame True
  flush -- This ensures we always have at least 1 frame which is
        -- actually pushed, so we can safely pop in restoreContext.
  m

resetCurrentFrame :: Monad m => SolverT m ()
resetCurrentFrame = SolverT $ do
  fid <- field @"ssNextFrameID" <<+= 1  
  field @"ssCurrentFrame"     .= emptySolverFrame fid
  field @"ssNCurrentFlushed"  .= 0

restoreContext :: MonadIO m => SolverContext -> SolverT m ()
restoreContext (SolverContext fs) = do
  -- Discard current frame.
  solverOp S.pop
  
  fs' <- SolverT (gets ssFrames)
  let commonPfx = takeWhile (uncurry ((==) `on` frId)) (zip fs fs')
      nCommon   = length commonPfx

  nFlushed <- SolverT (gets ssNFlushedFrames)
    
  when (nFlushed > nCommon) $
    solverOp (\s -> S.popMany s (fromIntegral $ nFlushed - nCommon))
  SolverT $ do
    field @"ssFrames"         .= fs
    field @"ssNFlushedFrames" .= min nCommon nFlushed
    
  solverOp S.push -- FIXME: This feels wrong here.
  resetCurrentFrame
  
reset :: MonadIO m => SolverT m ()
reset = do
  SolverT (modify (emptySolverState . solver))
  solverOp (\s -> S.ackCommand s (S.app (S.const "reset") []))
  solverOp makeStdLib

bindName :: Name -> String -> SolverFrame -> SolverFrame
bindName k v = field @"frBoundNames" %~ Map.insert k v

lookupName :: Name -> SolverFrame -> Maybe SExpr
lookupName k = fmap S.const . Map.lookup k . frBoundNames

newtype SolverT m a = SolverT { _getSolverT :: StateT SolverState m a }
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

withSolver :: (MonadIO m, Monad m) => (Solver -> SolverT m a) -> SolverT m a
withSolver f = do
  s <- SolverT $ gets solver
  f s

-- -----------------------------------------------------------------------------
-- Solver operations, over SExprs 

solverOp :: MonadIO m => (Solver -> IO a) -> SolverT m a
solverOp f = withSolver (liftIO . f)

queueSolverOp :: MonadIO m => QueuedCommand -> SolverT m ()
queueSolverOp qc = 
  modifyCurrentFrame (field @"frCommands" <>~ [qc])

-- MonadIO would be enough here.
assert :: MonadIO m => SExpr -> SolverT m ()
assert = queueSolverOp . QCAssert

check :: MonadIO m => SolverT m S.Result
check = flush >> solverOp S.check

getValue :: MonadIO m => SExpr -> SolverT m SExpr
getValue v = do
  res <- solverOp (\s -> S.command s $ S.fun "get-value" [S.List [v]])
  case res of
    S.List [S.List [_, v']] -> pure v'
    _ -> panic (unlines
                 [ "Unexpected response from the SMT solver:"
                 , "  Exptected: a value"
                 , "  Result: " ++ S.showsSExpr res ""
                 ]) []

-- -----------------------------------------------------------------------------
-- Names

stringToSMTName :: Text -> GUID -> String
stringToSMTName n g = show (pp n <> "@" <> pp g)

nameToSMTName :: Name -> String
nameToSMTName n = stringToSMTName (maybe "_N" id (nameText n)) (nameId n)

fnameToSMTName :: FName -> String
fnameToSMTName n = stringToSMTName (fnameText n) (fnameId n)

tnameToSMTName :: TName -> String
tnameToSMTName n = stringToSMTName (tnameText n) (tnameId n)

-- symExecName :: Name -> SExpr
-- symExecName =  S.const . nameToSMTName

-- symExecFName :: FName -> SExpr
-- symExecFName =  S.const . fnameToSMTName

knownFNames :: Monad m => SolverT m (Set FName)
knownFNames = SolverT $ gets ssKnownFuns

getName :: Monad m => Name -> SolverT m SExpr
getName n = do
  m_s <- inCurrentFrame (pure . lookupName n)
  case m_s of
    Just s -> pure s
    Nothing -> panic "Missing name" [showPP n]

getPolyFun :: Monad m => PolyFun -> SolverT m SExpr
getPolyFun pf = do
  m_s <- SolverT $ gets (Map.lookup pf . ssKnownPolys)
  case m_s of
    Just s  -> pure (S.const s)
    Nothing -> panic "Missing polyfun" [show pf]

freshSymbol :: (Monad m, HasGUID m) => Text -> SolverT m String
freshSymbol pfx = do
  guid <- lift getNextGUID
  pure (stringToSMTName pfx guid)

declareSymbol :: (MonadIO m, HasGUID m) => Text -> SExpr -> SolverT m SExpr
declareSymbol pfx ty = do
  sym <- freshSymbol pfx
  queueSolverOp (QCDeclare sym ty)
  pure (S.const sym)

freshName :: (Monad m, HasGUID m) => Name -> SolverT m String
freshName n = do
  n' <- lift (C.freshName n)
  let ns = nameToSMTName n'
  modifyCurrentFrame (bindName n ns)
  pure ns

-- FIXME: we could convert the type here
-- gives a name a value, returns the fresh name
defineName :: (MonadIO m, HasGUID m) => Name -> SExpr -> SExpr -> SolverT m SExpr
defineName n ty v = do
  n' <- freshName n
  queueSolverOp (QCDefine n' ty v)
  pure (S.const n')

declareName :: (MonadIO m, HasGUID m) => Name -> SExpr -> SolverT m SExpr
declareName n ty = do
  n' <- freshName n
  queueSolverOp (QCDeclare n' ty)
  pure (S.const n')

solverState :: Monad m => (SolverState -> m (a, SolverState)) -> SolverT m a
solverState f = do
  s <- SolverT get
  (a, s') <- lift (f s)
  SolverT (put s')
  pure a

-- -- Execute the monadic action and clean up the scope and state when it
-- -- completes.
-- scoped :: MonadIO m => SolverT m a -> SolverT m a
-- scoped m = do
--   st0  <- SolverT get
--   SolverT (put (st0 { frames = [] }))
--   push
--   r <- m
--   popAll
--   -- We could just reuse st0, but this avoids issues if we change the state type.
--   SolverT (modify (\s -> s { frames = frames st0 })) 
--   pure r

runSolverT :: SolverT m a -> SolverState -> m (a, SolverState)
runSolverT (SolverT m) s = runStateT m s

mapSolverT :: (m (a, SolverState) -> n (b, SolverState)) -> SolverT m a -> SolverT n b
mapSolverT f (SolverT m) = SolverT (mapStateT f m)

onState :: Monad m => Lens' SolverState a -> (a -> SolverT m a) -> SolverT m ()
onState l f = do
  v <- SolverT $ use l
  r <- f v
  SolverT $ l .= r

--------------------------------------------------------------------------------
-- Types

data SMTTypeDef =
  SMTTypeDef { stdName :: TName
             , stdBody :: [(String, [(String, SExpr)])]
             }

typeNameToDefault :: TName -> String
typeNameToDefault n = "default-" ++ tnameToSMTName n

-- FIXME: merge into simple-smt

-- Arguments are as for declareDatatype: (name, tparams, constructors)
declareDatatypes :: Solver -> [ (String, [String], [(String, [(String, SExpr)])]) ] -> IO ()
declareDatatypes s ts = S.ackCommand s (S.fun "declare-datatypes" [namesArities, decls])
  where
    namesArities = S.List (map nameArity ts)
    nameArity (n, tparams, _) = S.List [ S.Atom n, S.int (fromIntegral $ length tparams) ]
    decls        = S.List (map decl ts)
    decl (_, [], ctors) = mkbody ctors
    decl (_, tparams, ctors) = S.fun "par" [ S.List (map S.Atom tparams), mkbody ctors ]
    mkbody ctors = S.List [ S.fun ctor [ S.List [ S.Atom fld, ty ]
                                       | (fld, ty) <- flds ]
                          | (ctor, flds) <- ctors
                          ]

defineDefaultType :: MonadIO m => SMTTypeDef -> SolverT m ()
defineDefaultType std = void $ solverOp (\s -> S.declare s n ty)
  where
    ty = S.const (tnameToSMTName (stdName std))
    n  = typeNameToDefault (stdName std)

-- FIXME: merge with defineSMTFunDefs ?
-- FIXME: add defaults
defineSMTTypeDefs :: MonadIO m => Rec SMTTypeDef -> SolverT m ()
defineSMTTypeDefs (NonRec std) = onState (field @"ssKnownTypes") $ \known -> do
  if stdName std `Set.member` known
  then pure known
  else do solverOp doDef
          defineDefaultType std
          pure (Set.insert (stdName std) known)
  where
    doDef s = void $ S.declareDatatype s n' [] (stdBody std)
    n' = tnameToSMTName (stdName std)

defineSMTTypeDefs (MutRec stds) = onState (field @"ssKnownTypes") $ \known -> do
  if not (Set.disjoint allNames known) -- if we define one we should have defined all
  then pure known
  else do solverOp (flip declareDatatypes defs)
          mapM_ defineDefaultType stds
          pure (Set.union allNames known)
  where
    allNames = Set.fromList (map stdName stds)
    defs = map (\std -> (tnameToSMTName (stdName std), [], stdBody std)) stds

-- defineTName :: (MonadIO m) => TName ->  -> SolverT m SExpr
-- defineTName n flds = overCurrentFrame $ \f ->
--   if n `Set.member` frKnownTypes f
--   then pure (sn, f)
--   else do
--     solverOp (\s -> S.declareDatatype s n' [] flds)
--     -- Add a 'default' constant for this type, which is used to
--     -- init. arrays etc.  We never care what it actually is.
--     solverOp (\s -> void $ S.declare s (typeNameToDefault n) sn)

--     pure (sn, f { frKnownTypes = Set.insert n (frKnownTypes f) })

--   where
--     n' = tnameToSMTName n
--     sn = S.const n'



-- -----------------------------------------------------------------------------
-- Functions

data SMTFunDef = SMTFunDef { sfdName :: FName
                           , sfdArgs :: [(String, SExpr)]
                           , sfdRet  :: SExpr
                           , sfdBody :: SExpr
                           , sfdPureDeps :: Set FName
                           , sfdTyDeps   :: Set TName
                           }

defineSMTFunDefs :: MonadIO m => Rec SMTFunDef -> SolverT m ()
defineSMTFunDefs (NonRec sfd) = onState (field @"ssKnownFuns") $ \funs ->
  if sfdName sfd `Set.member` funs
  then pure funs
  else solverOp doDef $> Set.insert (sfdName sfd) funs
  where
    doDef s = void $ S.defineFun s (fnameToSMTName (sfdName sfd)) (sfdArgs sfd) (sfdRet sfd) (sfdBody sfd)

defineSMTFunDefs (MutRec sfds) = onState (field @"ssKnownFuns") $ \funs ->
  if not (Set.disjoint allNames funs) -- if we define one we should have defined all
  then pure funs
  else solverOp (flip S.defineFunsRec defs) $> Set.union allNames funs
  where
    allNames = Set.fromList (map sfdName sfds)
    defs = map (\sfd -> (fnameToSMTName (sfdName sfd), sfdArgs sfd, sfdRet sfd, sfdBody sfd)) sfds

-- -----------------------------------------------------------------------------
-- Poly functions

defineSMTPolyFun :: (HasGUID m, MonadIO m) => PolyFun -> SolverT m ()
defineSMTPolyFun pf = onState (field @"ssKnownPolys") $ \polys -> do
  if pf `Map.member` polys
    then pure polys
    else case pf of
           PMapLookup kt vt -> do
             fnm <- freshSymbol "mapLookup"
             solverOp (\s' -> mkMapLookup s' fnm kt vt) $> Map.insert pf fnm polys
           PMapMember kt vt -> do
             fnm <- freshSymbol "mapMember"
             solverOp (\s' -> mkMapMember s' fnm kt vt) $> Map.insert pf fnm polys
           PMapInsert kt vt -> do
             fnm <- freshSymbol "mapInsert"
             solverOp (\s' -> mkMapInsert s' fnm kt vt) $> Map.insert pf fnm polys

-- -----------------------------------------------------------------------------
-- instances

instance MonadTrans SolverT where
  lift m = SolverT (lift m)

instance (Monad m, HasGUID m) => HasGUID (SolverT m) where
  guidState f = lift (guidState f)


-- instance Alternative m => Alternative (SolverT m) where


-- instance MonadPlus m => MonadPlus (SolverT m) where

instance PP QueuedCommand where
  pp qc =
    case qc of
      QCAssert e      -> hang "assert" (length ("assert " :: String)) (vcat (map text (lines (S.ppSExpr e ""))))
      QCDeclare v ty  -> "declare" <+> text v <+> text (S.ppSExpr ty "")
      QCDefine v ty e -> "define" <+> text v <+> text (S.ppSExpr ty "") <+> text (S.ppSExpr e "")

instance PP SolverFrame where
  pp sf =
    hang ("index" <> pp (frId sf)) 2 (bullets (map pp (frCommands sf))) 
      
  

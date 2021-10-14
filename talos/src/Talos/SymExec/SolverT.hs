{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving #-}
{-# Language OverloadedStrings #-}

-- | Defines the symbolic parser API.  This wraps the SimpleSMT API

module Talos.SymExec.SolverT (
  -- * Solver interaction monad
  SolverT, runSolverT, mapSolverT, emptySolverState,
  nameToSMTName, fnameToSMTName, tnameToSMTName,
  getName,
  SolverState,
  withSolver, scoped,
  -- assert, declare, check,
  solverOp, solverState,
  getValue,

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
  push, pop, popAll, reset,
  assert, check
  
  ) where

import Control.Applicative
import Data.Functor ( ($>) )
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text(Text)

import Data.Set (Set)
import qualified Data.Set as Set

import SimpleSMT (Solver, SExpr)
import qualified SimpleSMT as S

import Daedalus.GUID
import Daedalus.Panic
import Daedalus.PP

import Daedalus.Core hiding (freshName)
import qualified Daedalus.Core as C

import Talos.SymExec.StdLib

-- The name of a polymorphic function (map lookup, map insertion, etc)
data PolyFun = PMapLookup SExpr SExpr -- kt vt
             | PMapInsert SExpr SExpr -- kt vt
             | PMapMember SExpr SExpr -- kt vt
             deriving (Eq, Ord, Show)

-- We manage this explicitly to make sure we are in synch with the
-- solver as push/pop are effectful.
data SolverFrame =
  SolverFrame { frKnownTypes :: Set TName -- ^ We lazily define types, but their names are mapped directly
              , frKnownFuns  :: Set FName -- ^ Similarly for (pure) functions (incl. bytesets)
              , frBoundNames :: Map Name String
              -- ^ May include names bound in closed scopes, to allow for
              --
              -- def P = { x = { $$ = UInt8; $$ > 10 }, ...}
              --
              -- where the execution of the body of x will return
              -- '$$' as it's symbolc result
              , frKnownPolys :: Map PolyFun String -- Maps from a defined poly fun to its SMT name
              }
  
emptySolverFrame :: SolverFrame
emptySolverFrame = SolverFrame mempty mempty mempty mempty

data SolverState =
  SolverState { solver       :: Solver
              , frames       :: [SolverFrame]
              , currentFrame :: SolverFrame
              -- This is the number of pushes we have delayed, as
              -- frequently we have push/pop with nothing in between.
              , pendingPushes :: Int
              }

emptySolverState :: Solver -> SolverState
emptySolverState s = SolverState s mempty emptySolverFrame 0

inCurrentFrame :: Monad m => (SolverFrame -> SolverT m a) -> SolverT m a
inCurrentFrame f = SolverT (gets currentFrame) >>= f

overCurrentFrame :: Monad m => (SolverFrame -> SolverT m (a, SolverFrame)) -> SolverT m a
overCurrentFrame f = do
  (r, frame') <- inCurrentFrame f
  SolverT (modify (\s -> s { currentFrame = frame' }))
  pure r

modifyCurrentFrame :: Monad m => (SolverFrame -> SolverFrame) -> SolverT m ()
modifyCurrentFrame f = overCurrentFrame (\s -> pure ((), f s))

-- If we have pending pushes we do them now.  We only need to do this
-- before talking to the solver, so it is done in withSolver.
flushPushes :: MonadIO m => SolverT m ()
flushPushes = do
  n <- SolverT $ gets pendingPushes
  when (n > 0) $ do
    SolverT $ modify (\s -> s { frames = replicate n (currentFrame s) ++ frames s
                              , pendingPushes = 0
                              })
    solverOp (flip S.pushMany (fromIntegral n))

push :: MonadIO m => SolverT m ()
push = do
  -- We push lazily
  SolverT (modify (\s -> s { pendingPushes = pendingPushes s + 1 }))
  -- solverOp S.push

pop :: MonadIO m => SolverT m ()
pop = do
  n <- SolverT $ gets pendingPushes
  if n > 0
    then SolverT (modify (\s -> s { pendingPushes = pendingPushes s - 1 }))
    else do
    fs <- SolverT (gets frames)
    case fs of
      [] -> panic "Attempted to pop past top of solver stack" []
      (f : fs') -> do
        SolverT (modify (\s -> s { currentFrame = f, frames = fs' }))
        solverOp S.pop

popAll :: MonadIO m => SolverT m ()
popAll = do
  fs <- SolverT (gets frames)
  case reverse fs of
    [] -> pure () -- do nothing
    (topF : _rest) -> do
      solverOp (\s -> S.popMany s (fromIntegral $ length fs))
      SolverT (modify (\s -> s { frames = [], currentFrame = topF, pendingPushes = 0 }))

reset :: MonadIO m => SolverT m ()
reset = do
  SolverT (modify (emptySolverState . solver))
  solverOp (\s -> S.ackCommand s (S.app (S.const "reset") []))
  solverOp makeStdLib

bindName :: Name -> String -> SolverFrame -> SolverFrame
bindName k v f = f { frBoundNames = Map.insert k v (frBoundNames f) }

lookupName :: Name -> SolverFrame -> Maybe SExpr
lookupName k = fmap S.const . Map.lookup k . frBoundNames

lookupPolyFun :: PolyFun -> SolverFrame -> Maybe SExpr
lookupPolyFun pf = fmap S.const . Map.lookup pf . frKnownPolys

newtype SolverT m a = SolverT { _getSolverT :: StateT SolverState m a }
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

withSolver :: (MonadIO m, Monad m) => (Solver -> SolverT m a) -> SolverT m a
withSolver f = do
  flushPushes
  s <- SolverT $ gets solver
  f s

-- -----------------------------------------------------------------------------
-- Solver operations, over SExprs 

solverOp :: MonadIO m => (Solver -> IO a) -> SolverT m a
solverOp f = withSolver (liftIO . f)
  
-- MonadIO would be enough here.
assert :: MonadIO m => SExpr -> SolverT m ()
assert assn = solverOp (\s -> S.assert s assn)

check :: MonadIO m => SolverT m S.Result
check = solverOp S.check

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
fnameToSMTName n = stringToSMTName (maybe "_F" id (fnameText n)) (fnameId n)

tnameToSMTName :: TName -> String
tnameToSMTName n = stringToSMTName (tnameText n) (tnameId n)

-- symExecName :: Name -> SExpr
-- symExecName =  S.const . nameToSMTName

-- symExecFName :: FName -> SExpr
-- symExecFName =  S.const . fnameToSMTName

knownFNames :: Monad m => SolverT m (Set FName)
knownFNames = inCurrentFrame (pure . frKnownFuns)

getName :: Monad m => Name -> SolverT m SExpr
getName n = do
  m_s <- inCurrentFrame (pure . lookupName n)
  case m_s of
    Just s -> pure s
    Nothing -> panic "Missing name" [showPP n]

getPolyFun :: Monad m => PolyFun -> SolverT m SExpr
getPolyFun pf = do
  m_s <- inCurrentFrame (pure . lookupPolyFun pf)
  case m_s of
    Just s -> pure s
    Nothing -> panic "Missing polyfun" [show pf]


freshSymbol :: (Monad m, HasGUID m) => Text -> SolverT m String
freshSymbol pfx = do
  guid <- lift getNextGUID
  pure (stringToSMTName pfx guid)

declareSymbol :: (MonadIO m, HasGUID m) => Text -> SExpr -> SolverT m SExpr
declareSymbol pfx ty = do
  sym <- freshSymbol pfx
  solverOp (\s -> S.declare s sym ty)

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
  solverOp (\s -> S.define s n' ty v)

declareName :: (MonadIO m, HasGUID m) => Name -> SExpr -> SolverT m SExpr
declareName n ty = do
  n' <- freshName n
  solverOp (\s -> S.declare s n' ty)

solverState :: Monad m => (SolverState -> m (a, SolverState)) -> SolverT m a
solverState f = do
  s <- SolverT get
  (a, s') <- lift (f s)
  SolverT (put s')
  pure a

-- Execute the monadic action and clean up the scope and state when it
-- completes.
scoped :: MonadIO m => SolverT m a -> SolverT m a
scoped m = do
  st0  <- SolverT get
  SolverT (put (st0 { frames = [] }))
  push
  r <- m
  popAll
  -- We could just reuse st0, but this avoids issues if we change the state type.
  SolverT (modify (\s -> s { frames = frames st0 })) 
  pure r

runSolverT :: SolverT m a -> SolverState -> m (a, SolverState)
runSolverT (SolverT m) s = runStateT m s

mapSolverT :: (m (a, SolverState) -> n (b, SolverState)) -> SolverT m a -> SolverT n b
mapSolverT f (SolverT m) = SolverT (mapStateT f m)

-- ----------------------------------------------------------------------------------------
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
defineSMTTypeDefs (NonRec std) = overCurrentFrame $ \f -> 
  if stdName std `Set.member` frKnownTypes f
  then pure ((), f)
  else do solverOp doDef
          defineDefaultType std
          pure ((), f { frKnownTypes = Set.insert (stdName std) (frKnownTypes f) })
  where
    doDef s = void $ S.declareDatatype s n' [] (stdBody std)
    n' = tnameToSMTName (stdName std)
      
defineSMTTypeDefs (MutRec stds) = overCurrentFrame $ \f -> 
  if not (Set.disjoint allNames (frKnownTypes f)) -- if we define one we should have defined all
  then pure ((), f)
  else do solverOp (flip declareDatatypes defs)
          mapM_ defineDefaultType stds
          pure ((), f { frKnownTypes = Set.union allNames (frKnownTypes f) })
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
defineSMTFunDefs (NonRec sfd) = overCurrentFrame $ \f -> 
  if sfdName sfd `Set.member` frKnownFuns f
  then pure ((), f)
  else solverOp doDef $> ((), f { frKnownFuns = Set.insert (sfdName sfd) (frKnownFuns f) })
  where
    doDef s = void $ S.defineFun s (fnameToSMTName (sfdName sfd)) (sfdArgs sfd) (sfdRet sfd) (sfdBody sfd)

defineSMTFunDefs (MutRec sfds) = overCurrentFrame $ \f -> 
  if not (Set.disjoint allNames (frKnownFuns f)) -- if we define one we should have defined all
  then pure ((), f)
  else solverOp (flip S.defineFunsRec defs) $> ((), f { frKnownFuns = Set.union allNames (frKnownFuns f) })
  where
    allNames = Set.fromList (map sfdName sfds)
    defs = map (\sfd -> (fnameToSMTName (sfdName sfd), sfdArgs sfd, sfdRet sfd, sfdBody sfd)) sfds

-- -----------------------------------------------------------------------------
-- Poly functions

defineSMTPolyFun :: (HasGUID m, MonadIO m) => PolyFun -> SolverT m ()
defineSMTPolyFun pf = overCurrentFrame $ \f ->
  if pf `Map.member` frKnownPolys f
  then pure ((), f)
  else do f' <- go f
          pure ((), f')
  where
    go f = do
      fnm <- case pf of
        PMapLookup kt vt -> do
          fnm <- freshSymbol "mapLookup"
          solverOp (\s -> mkMapLookup s fnm kt vt)
          pure fnm
        PMapMember kt vt -> do
          fnm <- freshSymbol "mapMember"
          solverOp (\s -> mkMapMember s fnm kt vt)
          pure fnm
        PMapInsert kt vt -> do
          fnm <- freshSymbol "mapInsert"
          solverOp (\s -> mkMapInsert s fnm kt vt)
          pure fnm
          
      pure (f { frKnownPolys = Map.insert pf fnm (frKnownPolys f) })

  
-- -----------------------------------------------------------------------------
-- instances

instance MonadTrans SolverT where
  lift m = SolverT (lift m)

instance (Monad m, HasGUID m) => HasGUID (SolverT m) where
  guidState f = lift (guidState f)


-- instance Alternative m => Alternative (SolverT m) where
  

-- instance MonadPlus m => MonadPlus (SolverT m) where
  

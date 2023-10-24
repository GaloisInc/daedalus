{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedLabels #-}

module Talos.Strategy.PathSymbolic.Monad where

import           Control.Lens                          (at, locally, view, (%~),
                                                        (.~), listening, alongside, imap)
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Reader                  (MonadReader, ReaderT,
                                                        ask, asks, local,
                                                        runReaderT)
import           Data.Functor                          (($>))
import           Data.Generics.Product                 (field)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           GHC.Generics                          (Generic)
import qualified SimpleSMT                             as SMT

import           Control.Monad.Except                  (ExceptT, MonadError,
                                                        catchError, runExceptT,
                                                        throwError)
-- FIXME: use .CPS?
import           Control.Monad.Writer                  (MonadWriter, WriterT,
                                                        censor, listens,
                                                        runWriterT, tell)
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Text                             (Text)

import           Daedalus.Core                         (Expr, Name, Type,
                                                        Typed (..))
import           Daedalus.GUID                         (getNextGUID,
                                                        invalidGUID, GUID)
import           Daedalus.Panic                        (panic)
import           Daedalus.PP
import           Daedalus.Rec                          (Rec)

import           Talos.Analysis.Exported               (ExpSlice, SliceId)
import           Talos.Strategy.Monad
import           Talos.Strategy.PathSymbolic.MuxValue  (MuxValue, SemiSolverM,
                                                        runSemiSolverM, SymbolicStream)
                                                            -- SequenceTag,
                                                            -- )
import           Talos.Monad                           (LiftTalosM, LogKey)
import           Talos.Path                            (ProvenanceTag,
                                                        SelectedPathF)
import qualified Talos.Solver.SolverT                  as Solv
import           Talos.Solver.SolverT                  (MonadSolver, SMTVar,
                                                        SolverT, liftSolver)
import qualified Talos.Strategy.PathSymbolic.Assertion as A
import           Talos.Strategy.PathSymbolic.Assertion (Assertion)
import qualified Talos.Strategy.PathSymbolic.Branching as B
import           Talos.Strategy.PathSymbolic.Branching (Branching, IndexedBranching)
import qualified Talos.Strategy.PathSymbolic.MuxValue  as MV
import           Talos.Strategy.PathSymbolic.PathSet   (LoopCountVar (..),
                                                        PathSet, PathVar (..),
                                                        loopCountToSExpr,
                                                        loopCountVarSort)
import qualified Talos.Strategy.PathSymbolic.Streams as S
import           Talos.Strategy.PathSymbolic.Streams (StreamTreeInfo, StreamTreeNode(..))
import Data.Sequence (Seq)
import Control.Arrow ((&&&))
import qualified Talos.Strategy.PathSymbolic.PathSet     as PS
import Daedalus.Core.Type (sizeType)

-- =============================================================================
-- (Path) Symbolic monad

pathKey :: LogKey
pathKey = "pathsymb"


-- -----------------------------------------------------------------------------
-- Streams

-- class IsSymbolicStream a where
--   type SymbolicSegments a
--   muxSymbolicStreams :: Branching a -> a

-- data StreamSegment = StreamSegment
--   { stsOffset :: SMTVar
--   , stsSize   :: SMTVar
--   } deriving (Generic)

-- type StreamSegments = Seq (Branching (Maybe StreamSegment))

-- Only used in the simulator to record the current stream usage.
data StreamUsage = StreamUsage
  { suBase    :: MuxValue
  -- , suBaseChanged :: Bool
  --  ^ Set when a Set/GetStream operation is performed, and used to
  -- avoid spurious merges of the suUsage.  Does NOT track suUsage.
  , suUsage   :: MuxValue
  } deriving Generic

data Result = Result
  { rValue          :: MuxValue
  -- | The current stream, if any.  This accumulates the size
  -- constraints, and holds the remaining bytes in the stream.
  , rStream         :: Maybe StreamUsage
  , rBuilder        :: PathBuilder
  } deriving (Generic)

type SymVarEnv = Map Name MuxValue

data SymbolicEnv = SymbolicEnv
  { sVarEnv        :: SymVarEnv
  , sCurrentSCC    :: Maybe (Set SliceId)
  -- ^ The set of slices in the current SCC, if any
  , sBackEdges     :: Map SliceId (Set SliceId)
  -- ^ All back edges from the current SCC
  , sSliceId       :: Maybe SliceId
  -- ^ Current slice

  , sRecDepth      :: Int
  , sMaxRecDepth   :: Int
  , sNLoopElements :: Int

  -- ^ Current recursive depth (basically the sum of all back edge
  -- calls, irrespective of source/target).

  , sProvenance    :: ProvenanceTag

  -- Used for getting prettier solver variables.
  , sCurrentName   :: Text
  -- The path isn't required as we add it on post-facto
  -- , sPath    :: ValueGuard -- ^ Current path.
  } deriving (Generic)

-- ppSymbolicEnv :: SymbolicEnv -> Doc
-- ppSymbolicEnv e =
--   block "{" ", " "}" [ ppN k <+> "->" <+> ppPrec 1 (text . flip ppSExpr ""  . typedThing <$> v)
--                      | (k,v) <- Map.toList (sVarEnv e) ]
--   where
--     ppN n = ppPrec 1 n <> parens (pp (nameId n))

data SolverResult =
  ByteResult SMTVar
  | InverseResult (Map Name MuxValue) Expr -- The env. includes the result var.

-- | When we see a loop while parsing a model we either suspend the
-- loop (for pooling) and just record the loop's tag, or we have
-- elements we just inline (for non-pooling loops).
data PathLoopBuilder a =
  PathLoopUnrolled (Maybe LoopCountVar) [a]
  | PathLoopGenerator SymbolicLoopTag
                      (Maybe LoopCountVar) -- 0 or 1
                      a
  | PathLoopMorphism SymbolicLoopTag (Branching (MV.VSequenceMeta, [a]))
    deriving Functor

type PathBuilder = SelectedPathF (IndexedBranching Int) (IndexedBranching Int) PathLoopBuilder SolverResult

emptySymbolicEnv :: Int -> Int -> ProvenanceTag -> SymbolicEnv
emptySymbolicEnv maxRecDepth nLoopElements ptag = SymbolicEnv
  { sVarEnv     = mempty
  , sCurrentSCC = mempty
  , sBackEdges  = mempty
  , sSliceId    = Nothing
  , sRecDepth   = 0
  , sMaxRecDepth = maxRecDepth
  , sNLoopElements = nLoopElements
  , sProvenance    = ptag
  , sCurrentName   = "result"
  }

-- A reference to a loop, used to collect loop elements and their
-- children.  Using a GUID here is a bit lazy as any uniquely
-- generated Int etc. would do.
type SymbolicLoopTag = MV.SequenceTag

-- | A hack so we can use a tag which will never actually occur.
invalidSymbolicLoopTag :: SymbolicLoopTag
invalidSymbolicLoopTag = invalidGUID

-- -----------------------------------------------------------------------------
-- Symbolic models
                        
-- We could figure this out from the generated parse tree, but this is
-- (maybe?) clearer.
data SymbolicModel = SymbolicModel
  { smGlobalAsserts :: [SMT.SExpr]
    -- ^ These are not predicated, e.g. range limits on choices.
  , smGuardedAsserts :: Assertion
  , smStreamTreeInfo :: StreamTreeInfo
  , smChoices :: Map PathVar ( [SMT.SExpr] -- Symbolic path (all true to be enabled)
                             , [Int] -- Allowed choice indicies
                             )
  , smNamedValues :: Map SMTVar Type
  , smLoopVars    :: Set LoopCountVar
  } deriving Generic

smAsserts :: SymbolicModel -> [SMT.SExpr]
smAsserts sm = smGlobalAsserts sm ++
               [ A.toSExpr (smGuardedAsserts sm)
               | not (A.trivial (smGuardedAsserts sm)) ]

-- We should only ever combine disjoint sets, so we cheat here.
instance Semigroup SymbolicModel where
  sm1 <> sm2 = SymbolicModel
    (smGlobalAsserts sm1 <> smGlobalAsserts sm2)
    (smGuardedAsserts sm1 <> smGuardedAsserts sm2)
    (smStreamTreeInfo sm1 <> smStreamTreeInfo sm2)
    (smChoices sm1 <> smChoices sm2)
    (smNamedValues sm1 <> smNamedValues sm2)
    (smLoopVars sm1 <> smLoopVars sm2)

instance Monoid SymbolicModel where
  mempty = SymbolicModel
           { smGlobalAsserts  = mempty
           , smGuardedAsserts = mempty
           , smStreamTreeInfo = mempty
           , smChoices        = mempty
           , smNamedValues    = mempty
           , smLoopVars       = mempty
           }

-- Writer outside of Except as writer state should be discarded on exception.
newtype SymbolicM a =
  SymbolicM { getSymbolicM :: WriterT SymbolicModel (ExceptT () (ReaderT SymbolicEnv (SolverT StrategyM))) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadError ()
           , MonadReader SymbolicEnv, MonadWriter SymbolicModel, MonadSolver, LiftTalosM)

instance LiftStrategyM SymbolicM where
  liftStrategy m = SymbolicM (liftStrategy m)

runSymbolicM :: -- | Slices for pre-run analysis
                (ExpSlice, [ Rec (SliceId, ExpSlice) ]) ->
                Int ->
                Int ->
                ProvenanceTag ->
                SymbolicM Result ->
                SolverT StrategyM (Either () (Result, SymbolicModel))
runSymbolicM _sls maxRecDepth nLoopEls ptag (SymbolicM m) =
  runReaderT (runExceptT (runWriterT m)) (emptySymbolicEnv maxRecDepth nLoopEls ptag)

--------------------------------------------------------------------------------
-- Names

bindNameIn :: Name -> SymbolicM Result -> 
              (Result -> SymbolicM a) -> SymbolicM a
bindNameIn n lhs rhs = lhs >>= \r -> primBindName n (rValue r) (rhs r)

bindNameInMaybe :: Maybe Name -> SymbolicM Result -> 
                   (Result -> SymbolicM a) -> SymbolicM a
bindNameInMaybe Nothing lhs rhs = lhs >>= rhs
bindNameInMaybe (Just n) lhs rhs = bindNameIn n lhs rhs

primBindName :: Name -> MuxValue -> SymbolicM a -> SymbolicM a
primBindName n v = locally (field @"sVarEnv"  . at n) (const (Just v))

getName :: Name -> SymbolicM MuxValue
getName n = SymbolicM $ do
  m_local <- asks (view (field @"sVarEnv" . at n))
  case m_local of
    Nothing -> panic "Missing variable" [showPP n]
    Just r  -> pure r

noteCurrentName :: Text -> SymbolicM a -> SymbolicM a
noteCurrentName n =
  local (field @"sCurrentName" .~ n)

makeNicerSym :: Text -> SymbolicM Text
makeNicerSym pfx = do
  n <- asks sCurrentName
  pure (pfx <> "." <> n)

pathVarSort :: SMT.SExpr
pathVarSort = SMT.tInt

constrainPathVar :: PathVar -> Int -> SymbolicM ()
constrainPathVar (PathVar sym) bnd = do
  -- The symbol only makes sense with these bounds, hence the global
  -- assertion (without this we can get e.g. negative indicies)
  assertGlobalSExpr $ SMT.and (SMT.leq (SMT.int 0) (SMT.const sym))
                              (SMT.lt (SMT.const sym) (SMT.int (fromIntegral bnd)))

-- Here because we do low-level pathvar stuff.
-- TODO: use this where necessary (unzip and friends)
namePathSets :: Branching a -> SymbolicM (Branching a)
namePathSets b = do
  n <- makeNicerSym "pvN"
  pv <- PathVar <$> liftSolver (Solv.declareSymbol n pathVarSort)
  -- FIXME: check if the branching pathsets are sufficiently
  -- complicated to make this worthwhile.  
  let (bnd, assn, r) = B.namePathSets pv b
  constrainPathVar pv bnd
  recordChoice pv [0 .. bnd - 1]
  assert (A.BAssert (A.PSAssert <$> assn))
  pure r

pathVariants :: [a] -> SymbolicM (PathVar, Branching a)
pathVariants vs = do
  pv <- freshPathVar (length vs)
  let b = B.branching True $ imap (\i v -> (PS.choiceConstraint pv i, v)) vs
  pure (pv, b)

freshPathVar :: Int -> SymbolicM PathVar
freshPathVar bnd = do
  n <- makeNicerSym "c"
  pv <- PathVar <$> liftSolver (Solv.declareSymbol n pathVarSort)
  constrainPathVar pv bnd
  pure pv

freshLoopCountVar :: Int -> Int -> SymbolicM LoopCountVar
freshLoopCountVar lb ub = do
  n <- makeNicerSym "lc"
  sym <- liftSolver $ Solv.declareSymbol n loopCountVarSort
  -- Hard (non-symbolic) limits on loop bounds.
  assertGlobalSExpr $ SMT.and (SMT.bvULeq (loopCountToSExpr lb) (SMT.const sym))
                              (SMT.bvULeq  (SMT.const sym) (loopCountToSExpr ub))
  let lv = LoopCountVar sym
  tell (mempty { smLoopVars = Set.singleton lv })
  pure lv

freshSymbolicLoopTag :: SymbolicM SymbolicLoopTag
freshSymbolicLoopTag = liftStrategy getNextGUID
                       
--------------------------------------------------------------------------------
-- Assertions

-- asserts :: [Assertion] -> SymbolicM ()
-- asserts assns | all isTrivialAssertion assns = pure ()
-- asserts assns = tell (mempty { smGuardedAsserts = assns })

assert :: Assertion -> SymbolicM ()
assert a = tell (mempty { smGuardedAsserts = a })

emitStreamTreeInfo :: StreamTreeInfo -> SymbolicM ()
emitStreamTreeInfo sti = tell (mempty { smStreamTreeInfo = sti })

-- emitStreamTreeNode :: StreamTreeNode -> SymbolicM ()
-- emitStreamTreeNode = emitStreamTreeInfo . S.STNode

assertSExpr :: SMT.SExpr -> SymbolicM ()
assertSExpr = assert . A.SExprAssert

assertGlobalSExpr :: SMT.SExpr -> SymbolicM ()
assertGlobalSExpr p | p == SMT.bool True = pure ()
assertGlobalSExpr p = tell (mempty { smGlobalAsserts = [p] })

recordChoice :: PathVar -> [Int] -> SymbolicM ()
recordChoice pv ixs =
  tell (mempty { smChoices = Map.singleton pv (mempty, ixs) })

recordValue :: Type -> SMTVar -> SymbolicM ()
recordValue ty sym = tell (mempty { smNamedValues = Map.singleton sym ty })

recordValues :: Set (Typed SMTVar) -> SymbolicM ()
recordValues newvars = tell (mempty { smNamedValues = m })
  where
    m = Map.fromList [ (var, ty) | Typed ty var <- Set.toList newvars ]

handleUnreachable :: SymbolicM a -> SymbolicM (Maybe a)
handleUnreachable m =
  (Just <$> m) `catchError` hdl
  where
    hdl () = assert (A.BoolAssert False) $> Nothing
  
-- Handles assertions and failure as well.
branching :: Branching (SymbolicM a) -> SymbolicM (Branching a)
branching b = do
  (vs, siAndassn) <- B.unzip <$> traverse go b
  let vs' = B.catMaybes vs
  if B.null vs'
    then unreachable
    else do let (stiB, assn) = B.unzip siAndassn
                (stiAssn, sti) = S.branching stiB
            assert stiAssn
            emitStreamTreeInfo sti
            assert (A.BAssert assn) $> vs'
  where
    go m = censor forgetGuarded (runm m `catchError` hdl)
    
    runm :: SymbolicM a ->
            SymbolicM (Maybe a, (StreamTreeInfo, Assertion))
    runm m = listens (smStreamTreeInfo &&& smGuardedAsserts) (Just <$> m)
    hdl () = pure (Nothing, (mempty, A.BoolAssert False))

    forgetGuarded = (#smGuardedAsserts .~ mempty)
                    . (#smStreamTreeInfo .~ mempty)

guardAssertions :: PathSet -> SymbolicM a -> SymbolicM a
guardAssertions ps = censor (#smGuardedAsserts %~ A.entail ps)

finishStreamSegment :: StreamUsage -> SymbolicM (StreamUsage, MuxValue)
finishStreamSegment su = do
  (assn, sti, strmV) <- liftSemiSolverM (MV.finishStreamSegment (suBase su) (suUsage su))
  assert assn
  emitStreamTreeInfo sti

  let su' = StreamUsage { suBase = strmV, suUsage = MV.vInteger sizeType 0 }
  pure (su', strmV)

--------------------------------------------------------------------------------
-- Search operaations

-- We have a number of cases here
-- 1. Normal function call to non-rec. function
-- 2. Normal function to recursive function
-- 3. Non back-edge call in current SCC
-- 4. Back-edge calls for which we might need to fail (if the depth is too great)

enterFunction :: SliceId -> Map Name Name -> 
                 SymbolicM a -> SymbolicM a
enterFunction tgt argMap m = do
  env <- ask
  let m_myId = sSliceId env
      isBackEdge = maybe False (Set.member tgt) (flip Map.lookup (sBackEdges env) =<< m_myId)
      belowMaxDepth = sRecDepth env < sMaxRecDepth env
      tgtInSCC = maybe False (Set.member tgt) (sCurrentSCC env)
  
  if tgtInSCC
    then sameSCC isBackEdge belowMaxDepth
    else outsideSCC
  where
    -- Cases 1 and 2 above
    outsideSCC = do
      m_sccs <- sccsFor tgt
      backEdges <- backEdgesFor tgt
      let upd e = e { sCurrentSCC = m_sccs
                    , sRecDepth   = 0
                    , sBackEdges  = backEdges
                    }
      local upd m_with_args

    -- Case 3
    sameSCC False _ = m_with_args    
    -- Case 4 back edge, beyond the max
    sameSCC True False = unreachable
    -- Case 4 back edge, allowed, so just increase depth
    sameSCC True True = 
      locally (field @"sRecDepth") (+ 1) m_with_args

    m_with_args =
      locally (field @"sSliceId") (const (Just tgt)) .
      locally (field @"sVarEnv") (flip Map.compose argMap) $ m

--------------------------------------------------------------------------------
-- Utilities

liftSemiSolverM :: SemiSolverM StrategyM a -> SymbolicM a
liftSemiSolverM m = do
  lenv <- asks sVarEnv
  n    <- asks sCurrentName
  (m_res, newvars) <- liftSolver (runSemiSolverM lenv n m)
  recordValues newvars
  either (const unreachable) pure m_res

unreachable :: SymbolicM a
unreachable = SymbolicM $ throwError ()

sImplies :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
sImplies l r | l == SMT.bool True = r
sImplies l _r | l == SMT.bool False = SMT.bool True
sImplies l r = l `SMT.implies` r

-- -----------------------------------------------------------------------------
-- Statistics

newtype AssertionStats = AssertionStats { getAssertionStats :: Map SMTVar Int }

instance Semigroup AssertionStats where
  AssertionStats m1 <> AssertionStats m2 = AssertionStats (Map.unionWith (+) m1 m2)

instance Monoid AssertionStats where
  mempty = AssertionStats mempty

instance PP AssertionStats where
  pp (AssertionStats m) =
    bullets [ text k <> ": " <> pp i | (k, i) <- Map.toList m ]
    
-- -----------------------------------------------------------------------------
-- Instances

instance PP SolverResult where
  pp (ByteResult v) = text v
  pp (InverseResult _e _v) = "inv"

instance PP a => PP (PathLoopBuilder a) where
  pp _ = "FIXME: PathLoopBuilder"
  

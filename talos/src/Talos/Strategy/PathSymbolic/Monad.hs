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

import           Control.Lens                          (_1, _2, at, locally,
                                                        mapped, over, view,
                                                        (.~), (%~))
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Reader                  (ReaderT, ask,
                                                        MonadReader,
                                                        asks, local, runReaderT)
import           Data.Generics.Product                 (field)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           GHC.Generics                          (Generic)
import qualified SimpleSMT                             as SMT
-- FIXME: use .CPS
import           Control.Monad.Except                  (ExceptT, runExceptT,
                                                        throwError, MonadError, catchError)
import           Control.Monad.Writer                  (MonadWriter, WriterT,
                                                        runWriterT, tell,
                                                        listens, pass, censor, listen)
import           Data.List.NonEmpty                    (NonEmpty(..), nonEmpty)
import qualified Data.List as List
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Text                             (Text)

import           Daedalus.Core                         (Expr, Name, Pattern,
                                                        Type, Typed (..))
import           Daedalus.GUID                         (GUID, getNextGUID,
                                                        invalidGUID)
import           Daedalus.Panic                        (panic)
import           Daedalus.PP
import           Daedalus.Rec                          (Rec)

import           Talos.Analysis.Exported               (ExpSlice, SliceId)
import           Talos.Strategy.Monad
import           Talos.Strategy.PathSymbolic.MuxValue  (MuxValue, SemiSolverM,
                                                        runSemiSolverM)
                                                            -- SequenceTag,
                                                            -- )
import           Talos.Lib                             (andMany)
import           Talos.Monad                           (LiftTalosM, LogKey)
import           Talos.Path
import qualified Talos.Solver.SolverT                  as Solv
import           Talos.Solver.SolverT                  (MonadSolver, SMTVar,
                                                        SolverT, liftSolver)
import           Talos.Strategy.PathSymbolic.Branching (Branching)
import qualified Talos.Strategy.PathSymbolic.Branching as B
import qualified Talos.Strategy.PathSymbolic.MuxValue  as MV
import           Talos.Strategy.PathSymbolic.PathSet   (LoopCountVar (..),
                                                        PathSet, PathVar (..),
                                                        loopCountToSExpr,
                                                        loopCountVarSort)
import qualified Talos.Strategy.PathSymbolic.PathSet as PS
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Maybe (maybeToList)

-- =============================================================================
-- (Path) Symbolic monad

pathKey :: LogKey
pathKey = "pathsymb"

type Result = (MuxValue, PathBuilder)

type SymVarEnv = Map Name MuxValue

data SymbolicEnv = SymbolicEnv
  { sVarEnv     :: SymVarEnv
  , sCurrentSCC :: Maybe (Set SliceId)
  -- ^ The set of slices in the current SCC, if any
  , sBackEdges :: Map SliceId (Set SliceId)
  -- ^ All back edges from the current SCC
  , sSliceId :: Maybe SliceId
  -- ^ Current slice

  , sRecDepth :: Int
  , sMaxRecDepth :: Int
  , sNLoopElements :: Int
    
  -- ^ Current recursive depth (basically the sum of all back edge
  -- calls, irrespective of source/target).

  , sProvenance :: ProvenanceTag

  -- Used for getting prettier solver variables.
  , sCurrentName :: Text
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

-- Not all paths are necessarily feasible.
data PathChoiceBuilder a =
  SymbolicChoice   PathVar           [(Int, a)]
  | ConcreteChoice Int               a
  deriving (Functor)

-- Used to tag cases so we can iterate through models
type SymbolicCaseTag = GUID

-- | When we see a loop while parsing a model we either suspend the
-- loop (for pooling) and just record the loop's tag, or we have
-- elements we just inline (for non-pooling loops).
data PathLoopBuilder a =
  PathLoopUnrolled (Maybe LoopCountVar) [a]
  | PathLoopGenerator SymbolicLoopTag
                      (Maybe LoopCountVar) -- 0 or 1
                      a
  | PathLoopMorphism SymbolicLoopTag (Branching (MV.VSequenceMeta, [a]))
                     
type PathBuilder = SelectedPathF PathChoiceBuilder PathChoiceBuilder PathLoopBuilder SolverResult

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
-- Assertions

-- Should we do something smarter when we have entailment under a
-- Branching? We could split Entail into Conj and Implies and then
-- simplify Implies under Branching.
data Assertion =
  SExprAssert SMT.SExpr -- Try not to use as it is not that informative.
  | PSAssert PathSet
  | BoolAssert Bool
  | BAssert (Branching Assertion)
  | EntailAssert PathSet (NonEmpty Assertion) -- P |= AND assns
  deriving (Ord, Eq, Show, Generic)

assertionToSExpr :: Assertion -> SMT.SExpr
assertionToSExpr assn =
  case assn of
    SExprAssert s -> s
    PSAssert ps -> PS.toSExpr ps
    BoolAssert b -> SMT.bool b
    BAssert  b  -> B.toSExpr (assertionToSExpr <$> b)
    EntailAssert ps assns ->
      PS.toSExpr ps `SMT.implies` andMany (map assertionToSExpr (toList assns))

isTrivialAssertion :: Assertion -> Bool
isTrivialAssertion assn =
  case assn of
    SExprAssert s -> s == SMT.bool True
    PSAssert ps -> PS.isTrivial ps
    BoolAssert b -> b
    BAssert  b  -> all isTrivialAssertion b
    EntailAssert _ assns -> all isTrivialAssertion assns

instance Semigroup Assertion where
  -- Unit
  BoolAssert True <> assn = assn
  assn <> BoolAssert True = assn
  -- Absorb
  assn@(BoolAssert False) <> _ = assn
  _ <> assn@(BoolAssert False) = assn

  -- Entailment
  EntailAssert ps1 assns1 <> EntailAssert ps2 assns2
    | PS.isTrivial ps1, PS.isTrivial ps2 = EntailAssert PS.trivialPathSet (assns1 <> assns2)
  EntailAssert ps1 assns1 <> assn2
    | PS.isTrivial ps1 = EntailAssert PS.trivialPathSet (assn2 :| toList assns1)
  assn1 <> EntailAssert ps2 assns2
    | PS.isTrivial ps2 = EntailAssert PS.trivialPathSet (assn1 :| toList assns2)
  assn1 <> assn2 = EntailAssert PS.trivialPathSet (assn1 :| [assn2])

instance Monoid Assertion where mempty = BoolAssert True

entailAssert :: PathSet -> Assertion -> Assertion
entailAssert ps a
  | PS.isTrivial ps      = a
  | isTrivialAssertion a = mempty
  | EntailAssert ps' assns <- a =
      maybe mempty (`EntailAssert` assns) (PS.conjPathSet ps ps')
  | otherwise            = EntailAssert ps (a :| [])

entailAsserts :: PathSet -> NonEmpty Assertion -> Assertion
entailAsserts ps (a :| []) = entailAssert ps a
entailAsserts ps assns     = EntailAssert ps assns

-- -----------------------------------------------------------------------------
-- Symbolic models
    
-- We could figure this out from the generated parse tree, but this is
-- (maybe?) clearer.
data SymbolicModel = SymbolicModel
  { smGlobalAsserts :: [SMT.SExpr]
    -- ^ These are not predicated, e.g. range limits on choices.
  , smGuardedAsserts :: Assertion
  , smChoices :: Map PathVar ( [SMT.SExpr] -- Symbolic path (all true to be enabled)
                             , [Int] -- Allowed choice indicies
                             )
  , smCases   :: Map SymbolicCaseTag (Name -- Cased-on variable
                                     , [SMT.SExpr] -- Symbolic path (all true to be enabled)
                                     , [(NonEmpty PathSet, Pattern)] -- Pattern guards
                                     )
  , smNamedValues :: Map SMTVar Type
  , smLoopVars    :: Set LoopCountVar
  } deriving Generic

smAsserts :: SymbolicModel -> [SMT.SExpr]
smAsserts sm = smGlobalAsserts sm ++
               [ assertionToSExpr (smGuardedAsserts sm)
               | not (isTrivialAssertion (smGuardedAsserts sm)) ]

-- We should only ever combine disjoint sets, so we cheat here.
instance Semigroup SymbolicModel where
  sm1 <> sm2 = SymbolicModel
    (smGlobalAsserts sm1 <> smGlobalAsserts sm2)
    (smGuardedAsserts sm1 <> smGuardedAsserts sm2)
    (smChoices sm1 <> smChoices sm2)
    (smCases   sm1 <> smCases   sm2)
    (smNamedValues sm1 <> smNamedValues sm2)
    (smLoopVars sm1 <> smLoopVars sm2)

instance Monoid SymbolicModel where
  mempty = SymbolicModel
           { smGlobalAsserts  = mempty
           , smGuardedAsserts = mempty
           , smChoices        = mempty
           , smCases          = mempty
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

bindNameIn :: Name -> SymbolicM Result
           -> (PathBuilder -> SymbolicM a) -> SymbolicM a
bindNameIn n lhs rhs = lhs >>= \(v, p) -> primBindName n v (rhs p)

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

freshPathVar :: Int -> SymbolicM PathVar
freshPathVar bnd = do
  n <- makeNicerSym "c"
  sym <- liftSolver $ Solv.declareSymbol n pathVarSort
  -- The symbol only makes sense with these bounds, hence the global
  -- assertion (without this we can get e.g. negative indicies)
  assertGlobalSExpr $ SMT.and (SMT.leq (SMT.int 0) (SMT.const sym))
                              (SMT.lt (SMT.const sym) (SMT.int (fromIntegral bnd)))
  pure (PathVar sym)

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

freshSymbolicCaseTag :: SymbolicM SymbolicCaseTag
freshSymbolicCaseTag = liftStrategy getNextGUID

freshSymbolicLoopTag :: SymbolicM SymbolicLoopTag
freshSymbolicLoopTag = liftStrategy getNextGUID
                       
--------------------------------------------------------------------------------
-- Assertions

-- asserts :: [Assertion] -> SymbolicM ()
-- asserts assns | all isTrivialAssertion assns = pure ()
-- asserts assns = tell (mempty { smGuardedAsserts = assns })

assert :: Assertion -> SymbolicM ()
assert a = tell (mempty { smGuardedAsserts = a })

assertSExpr :: SMT.SExpr -> SymbolicM ()
assertSExpr = assert . SExprAssert

assertGlobalSExpr :: SMT.SExpr -> SymbolicM ()
assertGlobalSExpr p | p == SMT.bool True = pure ()
assertGlobalSExpr p = tell (mempty { smGlobalAsserts = [p] })

recordChoice :: PathVar -> [Int] -> SymbolicM ()
recordChoice pv ixs =
  tell (mempty { smChoices = Map.singleton pv (mempty, ixs) })

recordCase :: SymbolicCaseTag -> Name -> [(NonEmpty PathSet, Pattern)] -> SymbolicM ()
-- If we have a _single_ rhs then we ignore the case (this happens with predicates)
recordCase _stag _n [_rhs] = pure ()
recordCase stag n rhss =
  tell (mempty { smCases = Map.singleton stag (n, mempty, rhss) })

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
    hdl () = assert (BoolAssert False) $> Nothing

-- Handles assertions and failure as well.
branching :: Branching (SymbolicM a) -> SymbolicM (Branching a)
branching b = do
  (vs, assn) <- B.unzip <$> traverse go b
  let vs' = B.catMaybes vs
  if B.null vs'
    then unreachable
    else assert (BAssert assn) $> vs'
  where
    go m = censor forgetAssns (catchError (runm m) hdl)
    
    runm :: SymbolicM a -> SymbolicM (Maybe a, Assertion)
    runm m = listens smGuardedAsserts (Just <$> m)
    hdl () = pure (Nothing, BoolAssert False)

    forgetAssns = #smGuardedAsserts .~ mempty

guardAssertions :: PathSet -> SymbolicM a -> SymbolicM a
guardAssertions ps = censor (#smGuardedAsserts %~ entailAssert ps)
                  
-- -- Used for case/choice slice alternatives
-- extendPath :: SMT.SExpr -> SymbolicModel -> SymbolicModel
-- extendPath g | g == SMT.bool True = id
-- extendPath g = addGuard . addImpl
--   where
--     addImpl sm
--       | []      <- smGuardedAsserts sm = sm
--       | otherwise =
--         over (field @"smGuardedAsserts") (\ss -> [SMT.implies g (andMany ss) ]) sm

--     -- Merge in the guard g with the path for the known choices/cases
--     addGuard = 
--       over (field @"smChoices" . mapped . _1) (g :)
--       . over (field @"smCases" . mapped . _2) (g :)

-- assert :: GuardedSemiSExprs -> SymbolicM ()
-- assert sv = do
--   pe <- asks (pathToSExpr . sPath)
--   case sv of
--     VOther p -> inSolver (Solv.assert (SMT.implies pe (typedThing p)))
--     VValue (V.VBool True) -> pure ()
--     -- If we assert false, we can't get here (negate path cond)
--     VValue (V.VBool False) -> inSolver (Solv.assert (SMT.not pe))
--     _ -> panic "Malformed boolean" [show sv]

--------------------------------------------------------------------------------
-- Search operaations

-- choose :: [a] -> SymbolicM (ChoiceId, a)
-- choose bs = do
--   pathToHere <- getPathDeps
--   cid <- freshChoiceId
--   SymbolicM $ (,) cid <$> lift (chooseST cid pathToHere bs)

-- backtrack :: BacktrackReason -> SymbolicM a
-- backtrack reason = do
--   pathToFailure <- getPathDeps
--   -- liftIO $ putStrLn "Backtracking ..."
--   SymbolicM (lift (backtrackST reason pathToFailure))

-- enterPathNode :: Set ChoiceId -> SymbolicM a -> SymbolicM a
-- enterPathNode deps = local (over (field @"sPath") (deps :))


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

-- FIXME: copied from MuxValue
-- getMaybe :: SymbolicM a -> SymbolicM (Maybe a)
-- getMaybe = SymbolicM . lift . runMaybeT . getSymbolicM

-- putMaybe :: SymbolicM (Maybe a) -> SymbolicM a
-- putMaybe m = hoistMaybe =<< m

-- hoistMaybe :: Maybe a -> SymbolicM a
-- hoistMaybe r = 
--   case r of
--     Nothing -> SymbolicM $ fail "Ignored"
--     Just v  -> pure v

-- collectMaybes :: [SymbolicM a] -> SymbolicM [a]
-- collectMaybes = fmap catMaybes . mapM getMaybe

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

instance PP (PathChoiceBuilder Doc) where
  pp (SymbolicChoice pv ps) = pp pv <> ": " <> block "{" "," "}" (map pp1 ps)
    where
      pp1 (i, p) = pp i <+> "=" <+> p
  pp (ConcreteChoice i p) = pp i <+> "=" <+> p


-- instance PP (PathCaseBuilder Doc) where
--   pp (SymbolicCase stag gses ps) =
--     pp stag <> ": " <>
--     block "[[" "," "]]" [ pp (text . typedThing <$> gses)
--                         , block "{" "," "}" (map pp1 ps) ]
--     where
--       pp1 (pat, p) = pp pat <+> "=" <+> p
      
--   pp (ConcreteCase p) = p

instance PP SolverResult where
  pp (ByteResult v) = text v
  pp (InverseResult _e _v) = "inv"

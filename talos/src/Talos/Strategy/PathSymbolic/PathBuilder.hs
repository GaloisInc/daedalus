{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-} -- Just for PathSetModelMonad
{-# LANGUAGE PatternSynonyms #-} 

-- Symbolic but the only non-symbolic path choices are those in
-- recursive functions (i.e., we only unroll loops).

-- FIXME: factor out commonalities with Symbolic.hs
module Talos.Strategy.PathSymbolic.PathBuilder (buildPaths) where

import           Control.Lens                          (Lens', Setter', _1, _2,
                                                        _3, at, each, ifoldlM,
                                                        locally, mapped, scribe,
                                                        view, views, (%~), (&),
                                                        (<>~))
import           Control.Monad                         (unless, zipWithM)
import           Control.Monad.Reader
import           Control.Monad.RWS.CPS                 (RWST, censor, mapRWST,
                                                        runRWST)
import qualified Data.ByteString                       as BS
import           Data.Functor                          (($>))
import           Data.Generics.Labels                  ()
import           Data.List.NonEmpty                    (NonEmpty ((:|)))
-- Lazy maps seems faster here.
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import qualified Data.Map.Merge.Strict                 as Map
import           Data.Maybe                            (isNothing)
import qualified Data.Set                              as Set
import qualified Data.Text                             as Text
import           Data.Word                             (Word8)
import           GHC.Generics                          (Generic)
import qualified SimpleSMT                             as S
import           Text.Printf                           (printf)

import           Daedalus.Core                         (Typed (..),
                                                        pattern TByte)
import qualified Daedalus.Core.Semantics.Env           as I
import qualified Daedalus.Core.Semantics.Expr          as I
import           Daedalus.Panic
import           Daedalus.PP                           (PP, braces, bullets,
                                                        commaSep, pp, showPP,
                                                        text, Doc, vcat)
import           Daedalus.Time                         (timeIt)
import qualified Daedalus.Value                        as I

import qualified Talos.Monad                           as T
import           Talos.Monad                           (LogKey, getIEnv,
                                                        logProblem)

import           Talos.Analysis.Exported               (SliceId)
import           Talos.Lib                             (andMany)
import           Talos.Path
import           Talos.Solver.ModelParser              (evalModelP, pExact,
                                                        pNumber, pSExpr, pValue)
import qualified Talos.Solver.SolverT                  as Solv
import           Talos.Solver.SolverT                  (SMTVar, SolverT)
import           Talos.Strategy.Monad
import qualified Talos.Strategy.PathSymbolic.Branching as B
import           Talos.Strategy.PathSymbolic.Monad
import qualified Talos.Strategy.PathSymbolic.MuxValue  as MV
import           Talos.Strategy.PathSymbolic.MuxValue  (VSequenceMeta (..))
import qualified Talos.Strategy.PathSymbolic.PathSet   as PS
import           Talos.Strategy.PathSymbolic.PathSet   (LoopCountVar,
                                                        PathSetModelMonad (..),
                                                        PathVar,
                                                        loopCountToSExpr,
                                                        loopCountVarToSExpr,
                                                        pathVarToSExpr)
import qualified Talos.Strategy.PathSymbolic.SymExec   as SE
import GHC.Stack (HasCallStack)

-- ----------------------------------------------------------------------------------------
-- Model parsing and enumeration.
--
-- This code uses the Monad instance for Maybe pretty heavily, sorry :/

data ModelState = ModelState
  { msPathVars :: !(Map PathVar Int)
  , msValues   :: !(Map SMTVar (Typed I.Value))
  , msLoopCounts :: !(Map LoopCountVar Int)
  } deriving Generic

-- Only works if we merge disjoint/agreeing models  
instance Semigroup ModelState where
  ms <> ms' = ModelState
    { msPathVars   = msPathVars ms <> msPathVars ms'
    , msValues     = msValues ms <> msValues ms'
    , msLoopCounts = msLoopCounts ms <> msLoopCounts ms'
    }

instance Monoid ModelState where
  mempty = ModelState mempty mempty mempty

data SymbolicLoopPoolElement = SymbolicLoopPoolElement
  { slpePathCursor :: PathCursor
  , slpeBuilder    :: PathBuilder
  , slpeNullable   :: Bool -- ^ Only applies to generators
  } deriving Generic

-- Placeholder if we want to do finer-grained selection of paths.
type ModelParserState = ()

-- data ModelParserState = ModelParserState
--   { mpsMMS      :: MultiModelState
--   -- ^ The stats across multiple models
--   -- ^ This is _all_ the values from the solver, including those not
--   -- on the path etc.  Having this here means we don't have to worry
--   -- about preserving e.g. the solver stack when traversing the
--   -- pathbuilder.  It might also be faster to do in a single query(?)
--   } deriving Generic

emptyModelParserState :: ModelParserState
emptyModelParserState = ()

-- FIXME: clag
getByteVar :: SMTVar -> ModelParserM Word8
getByteVar symB = I.valueToByte <$> getValueVar (Typed TByte symB)

ppModelVars :: (PP a, PP b, Ord a) => Lens' ModelState (Map a b) -> ModelParserM Doc
ppModelVars msLens = do
  -- Look for a pv in the context
  ctxt   <- view (#mpeModelContext . msLens)
  solv   <- view (#mpeSolverModel . msLens)

  let m = Map.union ctxt solv
  pure (vcat [ pp k <> " -> " <> pp v | (k, v) <- Map.toList m ])
    
getModelVar :: Ord a => Lens' ModelState (Map a b) -> a -> ModelParserM b
getModelVar msLens pv = do
  -- Look for a pv in the context
  m_ctxt   <- view (#mpeModelContext . msLens . at pv)
  m_solv   <- view (#mpeSolverModel . msLens . at pv)
  
  case (m_ctxt, m_solv) of
    (Just i, _) -> pure i
    -- Note that we saw the variable
    (_, Just i) -> scribe (mpoModelState . msLens) (Map.singleton pv i) $> i
    _           -> panic "Missing variable in solver model" []
          
getPathVar :: PathVar -> ModelParserM Int
getPathVar = getModelVar #msPathVars
  -- where
  --   -- bit gross doing this here ...
  --   updateStats pv n mms
  --     | Just chi <- Map.lookup pv (mmsChoices mms)
  --     , n `notElem` mmschiSeen chi =
  --       -- If we have seen all the choices then exhaust.
  --       let res | length (mmschiSeen chi) + 1 == length (mmschiAllChoices chi) = Nothing
  --               | otherwise  = Just (chi & #mmschiSeen %~ (n :))
  --       in mms & #mmsChoices . at pv .~ res
  --              & #mmsNovel +~ 1
  --     | otherwise = mms & #mmsSeen +~ 1

-- Maybe not worth caching?
getLoopVar :: LoopCountVar -> ModelParserM Int
getLoopVar = getModelVar #msLoopCounts

getValueVar :: Typed SMTVar -> ModelParserM I.Value
getValueVar (Typed _ty x) = typedThing <$> getModelVar #msValues x

instance PathSetModelMonad ModelParserM where
  psmmPathVar = getPathVar
  psmmSMTVar  = getValueVar
  psmmLoopVar = getLoopVar

-- -- | Gets any dependencies for a given tag, forgetting about the tag as a side-effect
-- getLoopDep :: SymbolicLoopTag -> ModelParserM [SymbolicLoopPoolElement]
-- getLoopDep ltag = uses (#mpsLoopDeps . at ltag) (fromMaybe [])

-- ---------- Reader ----------

data ModelParserEnv = ModelParserEnv
  { mpeSolverModel  :: ModelState
  -- ^ The entire model from the solver.
  , mpeModelContext :: ModelState
  -- ^ Used to figure out if a value is owned by the current code or
  -- by the context.
  , mpeGetModel :: SolverT StrategyM ModelState
  -- ^ Reads the model from the solver (should always succeed, needs
  -- to be in a solver state where we have a model).  Having it here
  -- means we can override it to ask the solver for fewer things
  -- (e.g. when getting loop elements).
  , mpeModelCount :: Int
  } deriving Generic

makeModelParserEnv :: Int -> SymbolicModel -> SolverT StrategyM ModelParserEnv
makeModelParserEnv ntotal sm = do
  let mkModel = symbolicModelToModelStateP sm
  initModel <- mkModel
  
  pure (ModelParserEnv { mpeSolverModel = initModel
                       , mpeModelContext = mempty
                       , mpeGetModel = mkModel
                       , mpeModelCount = ntotal
                       })

symbolicModelToModelStateP :: SymbolicModel -> SolverT StrategyM ModelState
symbolicModelToModelStateP sm = do
  -- We assume the query/result order is the same
  let (pes, pPathVars)   = mapToP PS.pathVarToSMTVar pInt (smChoices sm)
      (ves, pValues)     = mapToP id (\ty -> Typed ty <$> pValue ty) (smNamedValues sm)
      (les, pLoopCounts) = mapToP PS.loopCountVarToSMTVar pInt (Map.fromSet (const ()) (smLoopVars sm))
  let vals = pes ++ ves ++ les
  sexp <- timed "getmodel.time" $ Solv.getValues vals
  T.statS (pathKey <> "getmodel" <> "size") (length vals)
  -- Getting the model again seems to take about the same amount ot time.
  -- _ <- timed "getmodel.again" $ Solv.getValues (pes ++ ves ++ les)
  
  let modelp = ModelState <$> pPathVars <*> pValues <*> pLoopCounts  
  case evalModelP (pSExpr modelp) sexp of
    []    -> panic "No parse" []
    b : _ -> pure b
  where
   pInt = \_ -> fromIntegral <$> pNumber
   mapToP toSMTVar pVal m =
     let pOne k v = pSExpr ( pExact (toSMTVar k) *> pVal v )
     in ( S.const . toSMTVar <$> Map.keys m
        , Map.traverseWithKey pOne m)

-- ---------- Writer ----------

-- So we can have a better monoid instance (i.e., concat the values)
newtype MPLoopDeps = MPLoopDeps { mpLoopDeps :: Map SymbolicLoopTag [SymbolicLoopPoolElement] }
  deriving Generic

instance Semigroup MPLoopDeps where
  MPLoopDeps m1 <> MPLoopDeps m2 = MPLoopDeps $ Map.unionWith (<>) m1 m2
instance Monoid MPLoopDeps where mempty = MPLoopDeps mempty

type MPLoopGenerators = Map SymbolicLoopTag SymbolicLoopPoolElement

-- data ModelParserOutput = ModelParserOutput
--   { mpoLoopDeps       :: Map SymbolicLoopTag [SymbolicLoopPoolElement]
--   -- ^ This accumulates the usages of a sequence until the generator is reached.
--   , mpoLoopGenerators :: Map SymbolicLoopTag SymbolicLoopPoolElement
--   -- ^ This contains the generator and users.
--   } deriving Generic

-- instance Semigroup ModelParserOutput where
--   mpo1 <> mpo2 = ModelParserOutput
--     { mpoLoopDeps       = Map.unionWith (<>) (mpoLoopDeps mpo1) (mpoLoopDeps mpo2)
--     -- These shouldn't overlap, so <> is OK
--     , mpoLoopGenerators = mpoLoopGenerators mpo1 <> mpoLoopGenerators mpo2
--     }

-- instance Monoid ModelParserOutput where
--   mempty = ModelParserOutput mempty mempty

recordLoopGenerator :: Bool -> SymbolicLoopTag -> PathBuilder -> ModelParserM ()
recordLoopGenerator nullable ltag pb = scribe mpoLoopGenerators (Map.singleton ltag slpe)
  where
    slpe = SymbolicLoopPoolElement
           { slpePathCursor = mempty
           , slpeBuilder = pb
           , slpeNullable = nullable
           }

recordLoopDep ::  SymbolicLoopTag -> PathBuilder -> ModelParserM ()
recordLoopDep ltag pb =
  scribe mpoLoopDeps (MPLoopDeps $ Map.singleton ltag [slpe])  
  where
    slpe = SymbolicLoopPoolElement
           { slpePathCursor = mempty
           , slpeBuilder = pb
           , slpeNullable = True -- Ignored.
           }

loopDepsPathL :: Setter' MPLoopDeps PathCursor
loopDepsPathL = #mpLoopDeps . mapped . each . #slpePathCursor

extendCursorIn :: PathCursorElement -> ModelParserM a -> ModelParserM a
extendCursorIn el = censor go
  where
    go = (mpoLoopDeps . loopDepsPathL %~ (el :))
         . (mpoLoopGenerators . mapped . #slpePathCursor %~ (:) el)

-- ---------- Monad ----------

type ModelParserM' w = RWST ModelParserEnv w ModelParserState (SolverT StrategyM)

-- We use a tuple so we can use different types at different phases.
type ModelParserM = ModelParserM' (MPLoopDeps, MPLoopGenerators, ModelState)

inSolver :: Monoid w => SolverT StrategyM a -> ModelParserM' w a
inSolver = lift

-- convenience lenses
mpoLoopDeps :: Lens' (MPLoopDeps, MPLoopGenerators, ModelState) MPLoopDeps
mpoLoopDeps = _1

mpoLoopGenerators :: Lens' (MPLoopDeps, MPLoopGenerators, ModelState) MPLoopGenerators
mpoLoopGenerators = _2

mpoModelState :: Lens' (MPLoopDeps, MPLoopGenerators, ModelState) ModelState
mpoModelState = _3

runModelParserM :: Monoid w => Int -> SymbolicModel -> ModelParserM' w a -> SolverT StrategyM (a, ModelParserState)
runModelParserM ntotal sm mp = do
  env0 <- makeModelParserEnv ntotal sm
  (a, st, _) <- runRWST mp env0 emptyModelParserState 
  pure (a, st)

-- ------------------------------------------------------------------------------
-- Multi-pass state

-- data MMSCaseInfo = MMSCaseInfo
--   { -- Read only
--     mmsciName    :: Name      -- ^ Name of the case var, for debugging
--   , mmsciAllPats :: [(S.SExpr, Pattern)] -- ^ All patterns (read only)
--   , mmsciPath    :: S.SExpr -- ^ Path to the case
--   -- Updated
--   , mmsciSeen    :: [Pattern] -- ^ Case alts we have already seen
--   } deriving Generic
  
-- data MMSChoiceInfo = MMSChoiceInfo
--   { -- Read only
--     mmschiAllChoices :: [Int] -- ^ All choices (read only)
--   , mmschiPath       :: S.SExpr -- ^ Path to the choice
--   -- Updated  
--   , mmschiSeen       :: [Int]     -- ^ Choice alts we have already seen
--   } deriving Generic
  
-- This contains all the choices we haven't yet seen.  It might be
-- better to just negate the seen choices, but this is a bit simpler
-- for now.
-- data MultiModelState = MultiModelState
--   { mmsChoices :: Map PathVar MMSChoiceInfo
--   , mmsCases   :: Map SymbolicCaseTag MMSCaseInfo
--   , mmsNovel   :: Int -- ^ For a run, how many novel choices/cases we saw.
--   , mmsSeen    :: Int -- ^ For a run, how many seen choices/cases we saw.
--   } deriving Generic

-- This just picks one at a time, we could also focus on a particular (e.g.) choice until exhausted.
-- nextChoice :: MultiModelState -> SolverT StrategyM (Maybe (S.SExpr, MultiModelState -> MultiModelState, Doc))
-- nextChoice mms
--   -- FIXME: just picks the first, we could be e.g. random
--   | Just ((pv, chi), _mmsC) <- Map.minViewWithKey (mmsChoices mms) = do
--       let notSeen = S.distinct (pathVarToSExpr pv : map (S.int . fromIntegral) (mmschiSeen chi))
--           exhaust = over (#mmsChoices) (Map.delete pv)
--       pure (Just (MV.andMany [notSeen, mmschiPath chi]
--                  , exhaust
--                  , pp pv <> " in " <> brackets (commaSep [ pp i | i <- mmschiAllChoices chi, i `notElem` mmschiSeen chi])))
--   | Just ((stag, ci), _mmsC) <- Map.minViewWithKey (mmsCases mms) = do
--       let notSeen = S.not (MV.orMany [ p | (p, pat) <- mmsciAllPats ci, pat `elem` mmsciSeen ci ])
--           exhaust = over (#mmsCases) (Map.delete stag)
--           -- p = MV.orMany (map PC.toSExpr (NE.toList vcond))
--       pure (Just ( MV.andMany [notSeen, mmsciPath ci]
--                  , exhaust
--                  , pp (mmsciName ci) <> "." <> pp stag <> " in "
--                    <> brackets (commaSep [ pp pat | (_, pat) <- mmsciAllPats ci, pat `notElem` mmsciSeen ci])))  -- <> commaSep (map pp (mmsciSeen ci))))
--   | otherwise = pure Nothing

-- modelChoiceCount :: MultiModelState -> Int
-- modelChoiceCount mms = choices + cases
--   where
--     choices = sum $ [ length (mmschiAllChoices chi) - length (mmschiSeen chi)
--                     | chi <- Map.elems (mmsChoices mms)
--                     ]
--     cases   = sum $ [ length (mmsciAllPats ci) - length (mmsciSeen ci)
--                     | ci <- Map.elems (mmsCases mms)
--                     ]

-- symbolicModelToMMS :: SymbolicModel -> MultiModelState
-- symbolicModelToMMS sm = MultiModelState
--   { mmsChoices = fmap mkCh (smChoices sm)
--   , mmsCases   = fmap mkCi (smCases   sm)
--   , mmsNovel   = 0
--   , mmsSeen    = 0
--   }
--   where
--     mkCh (paths, idxs) = MMSChoiceInfo
--       { mmschiAllChoices = idxs
--       , mmschiPath = MV.andMany paths
--       , mmschiSeen = []
--       }
--     mkCi (name, paths, pats) = MMSCaseInfo
--       { mmsciName    = name
--       , mmsciAllPats = over (each . _1) (MV.orMany . map PC.toSExpr . NE.toList) pats
--       , mmsciPath    = MV.andMany paths
--       , mmsciSeen    = []
--       }

-- nullMMS :: MultiModelState -> Bool
-- nullMMS ms = Map.null (mmsCases ms) && Map.null (mmsChoices ms)

timed :: LogKey -> SolverT StrategyM a -> SolverT StrategyM a
timed key m = do
  (r, t) <- timeIt m
  T.statistic (pathKey <> key) (Text.pack $ printf "%.3fms" (fromInteger t / 1000000 :: Double))
  pure r

buildPaths :: Int -> Maybe Int -> SliceId -> SymbolicModel -> PathBuilder ->
              SolverT StrategyM [SelectedPath]
buildPaths ntotal _nfails sid sm pb = do
  timed "assert" $ do
    mapM_ Solv.assert (smAsserts sm)
    Solv.flush

  ctxt <- Solv.getContext
  logProblem pathKey (showPP sid) (concatMap (flip S.ppSExpr "\n") (Solv.contextToSExprs ctxt))

  r <- timed "check" Solv.check
  case r of
    S.Unsat   -> pure []
    S.Unknown -> pure []
    S.Sat     -> do
      -- Have at least 1 model, lets pretend to be a loop.
      (pool, _) <- runModelParserM ntotal sm (build invalidSymbolicLoopTag fakesle [])
      pure (map fst (smPaths pool))
  where
    fakesle = SymbolicLoopPoolElement
              { slpePathCursor = mempty
              , slpeBuilder    = pb
              , slpeNullable   = False -- doesn't matter
              }
             
-- buildPaths :: Int -> Maybe Int -> SymbolicModel -> PathBuilder -> SolverT StrategyM [SelectedPath]
-- buildPaths ntotal nfails sm pb = do
--   liftIO $ printf "\n\t%d choices and cases" (modelChoiceCount st0)
--   (_, tassert) <- timeIt $ do
--     mapM_ Solv.assert (smAsserts sm)
--     Solv.flush
--   liftIO $ printf "; initial model time: %.3fms\n" (fromInteger tassert / 1000000 :: Double)

--   _ <- panic "Lets go no further" []

--   if not (nullMMS st0)
--     then Solv.getContext >>= go 0 0 st0 []
--     else do -- Only 1 choice, so take it.      
--       (r, tcheck) <- timeIt Solv.check
--       liftIO $ printf "\t(single): solve time: %.3fms" (fromInteger tcheck / 1000000 :: Double)
--       check st0 r >>= \case
--         Left errReason -> do
--           liftIO $ printf " (%s)\n" errReason
--           pure []
--         Right (p, _st', tbuild) -> do
--           liftIO $ printf ", build time: %.3fms\n" (fromInteger tbuild / 1000000 :: Double)
--           pure [p]

--   where
--     st0 = symbolicModelToMMS sm
--     go :: Int -> Int -> MultiModelState -> [SelectedPath] -> Solv.SolverContext ->
--           SolverT StrategyM [SelectedPath]
--     go _na _nf _st acc ctxt | length acc == ntotal = acc <$ Solv.restoreContext ctxt
--     go _na nf  _st acc ctxt | Just nf == nfails    = acc <$ Solv.restoreContext ctxt
--     go na nf st acc ctxt = do
--       Solv.restoreContext ctxt
--       m_next <- nextChoice st
--       case m_next of
--         Nothing -> pure acc -- Done, although we could try for different byte values?
--         Just (assn, exhaust, descr) -> do
--           (_, tassert) <- timeIt $ do
--             Solv.assert assn
--             Solv.flush
--           (r, tcheck) <- timeIt Solv.check
          
--           liftIO $ printf "\t%d: (%s) %d choices and cases, assert time: %.3fms, solve time: %.3fms"
--                           na
--                           (show descr) (modelChoiceCount st)
--                           (fromInteger tassert / 1000000 :: Double)
--                           (fromInteger tcheck / 1000000 :: Double)
          
--           check st r >>= \case
--             Left errReason -> do
--               liftIO $ printf " (%s)\n" errReason
--               go (na + 1) (nf + 1) (exhaust st) acc ctxt
--             Right (p, st', tbuild) -> do
--               liftIO $ printf ", build time: %.3fms, novel: %d, reused: %d\n"
--                 (fromInteger tbuild / 1000000 :: Double)
--                 (mmsNovel st') (mmsSeen st')
--               go (na + 1) nf st' (p : acc) ctxt
--     check :: MultiModelState -> S.Result ->
--              SolverT StrategyM (Either String
--                                  (SelectedPath, MultiModelState, Integer))
             
--     check st r = undefined -- do
      -- case r of
      --   S.Unsat   -> pure (Left "unsat")
      --   S.Unknown -> pure (Left "unknown")
      --   S.Sat     -> do
      --     let mms = st { mmsNovel = 0, mmsSeen = 0 }
      --     ((p, st'), tbuild) <- timeIt $ runModelParserM mms (buildPath pb)
      --     pure (Right (p, st', tbuild))

-- -----------------------------------------------------------------------------
-- Building a single context

buildPath :: PathBuilder -> ModelParserM SelectedPath
buildPath = go
  where
    go pbuilder = case pbuilder of
      SelectedHole -> pure SelectedHole
      SelectedBytes ptag r -> SelectedBytes ptag <$> resolveResult r
      SelectedDo l r -> do
        SelectedDo <$> extendCursorIn PCDoLeft (go l)
                   <*> extendCursorIn PCDoRight (go r)
      SelectedChoice pib -> SelectedChoice <$> buildPathChoice pib
      SelectedCall i p   -> SelectedCall i <$> go p
      SelectedCase pib   -> SelectedCase <$> buildPathChoice pib
      SelectedLoop lp    -> buildLoop lp

    resolveResult :: SolverResult -> ModelParserM BS.ByteString
    resolveResult (ByteResult b) = BS.singleton <$> getByteVar b
    resolveResult (InverseResult env ifn) = do
      -- FIXME: we should maybe make this lazy
      venv <- traverse MV.fromModel env
      ienv <- liftStrategy getIEnv -- for fun defns
      let ienv' = ienv { I.vEnv = venv }
      pure (I.valueToByteString (I.eval ifn ienv'))

buildLoop :: PathLoopBuilder PathBuilder -> ModelParserM SelectedPath

-- In this case we just generate the results, nothing further to do.
buildLoop (PathLoopUnrolled m_lv els) = do
  len <- maybe (pure (length els)) getLoopVar m_lv
  -- We don't need to worry about order too much here (wrt def/use of
  -- loops) as we deal with that in the post-phase.
  ps <- zipWithM (\i -> extendCursorIn (PCSequence i) . buildPath) [0..] (take len els)
  pure (SelectedLoop (SelectedLoopElements Nothing ps))

-- In this case we might need to pool.  Note that els should be either
-- a singleton or the empty list.
buildLoop (PathLoopGenerator ltag m_lv el) =
  loopNonEmpty m_lv $ do
    recordLoopGenerator (isNothing m_lv) ltag el
    -- We will fill this in when we have all the models
    pure SelectedHole

buildLoop (PathLoopMorphism _ltag bvs) = go =<< B.resolve bvs
  where
    go Nothing = panic "Missing branch" []
    go (Just (vsm, els))
      -- This is the unrolled case, so we just emit the path. We re-use the Unrolled case above.
      | isNothing (vsmGeneratorTag vsm) = buildLoop (PathLoopUnrolled (vsmLoopCountVar vsm) els)
      -- We always have an element, although it might not be well-defined if the loopvar is 0
      | Just genltag <- vsmGeneratorTag vsm, [el] <- els =
        loopNonEmpty (vsmLoopCountVar vsm) $ do
          recordLoopDep genltag el
          -- We will fill this in at synthesis time.
          pure SelectedHole
        
      | otherwise = panic "Non-singleton sequence" []

loopNonEmpty :: Maybe LoopCountVar -> ModelParserM SelectedPath -> ModelParserM SelectedPath
loopNonEmpty m_lv m = do
  isNull <- maybe (pure False) (fmap (== 0) . getLoopVar) m_lv
  if isNull
     -- null case, we shouldn't have any users, so we don't
     -- generate a pool.
    then pure (SelectedLoop (SelectedLoopElements Nothing []))
    -- singleton case, we need to record and move on.
    else m
    
buildPathChoice :: HasCallStack =>
                   PathChoiceBuilder PathBuilder ->
                   ModelParserM (PathIndex SelectedPath)
buildPathChoice (ConcreteChoice i p) = PathIndex i <$> buildPath p
buildPathChoice (SymbolicChoice pv ps) = do
  i <- getPathVar pv
  p <- case lookup i ps of
         Nothing  -> do
           mP <- ppModelVars #msPathVars
           mL <- ppModelVars #msLoopCounts
           
           panic "Missing choice" [showPP pv <> " = " <> show i, show mP, show mL]
         Just p'  -> pure p'
  PathIndex i <$> buildPath p

-- -----------------------------------------------------------------------------
-- Loops
  
-- symbolicLoopUsageToPool :: PathCursor -> PathBuilder -> [SymbolicLoopPoolElement] -> SelectedLoopPoolF PathBuilder
-- symbolicLoopUsageToPool hereCursor pb slus = SelectedLoopPoolF
--   { smPaths   = paths
--   , smCursors = cursors
--   }
--   where
--     -- Only a single path, we will create a bunch when we iterate over the models
--     paths   = [ (pb, map sluBuilder slus) ]
--     cursors = map (relitiviseCursors hereCursor . sluPathCursor) slus

-- We run the builder, then collect all the sequences, get a pool of
-- models for them, and up the generators.

-- This is run after a successful check sat.
-- Invariants:
--   - We leave the solver in the context in which we found it.
build :: SymbolicLoopTag -> SymbolicLoopPoolElement -> [SymbolicLoopPoolElement] ->
         ModelParserM' () SelectedLoopPool
build gentag genpe deppes = do
  ntotal <- view #mpeModelCount
  withScopedContext (SelectedLoopPoolF <$> go 0 ntotal [] <*> pure cursors)
  where
    go :: Int -> Int -> [(SelectedPath, [SelectedPath])] ->
          ModelParserM' () [(SelectedPath, [SelectedPath])]
    go n ntotal acc = do
      (ms, r) <- makePoolElement gentag (genpe :| deppes)
      let acc' = r : acc
      if n >= ntotal
        then pure acc'
        else tryAgain ms acc' (go (n + 1) ntotal acc')
        
    tryAgain ms acc m = do
      assertDifferentModel ms
      r <- inSolver (timed ("modelcheck" <> "repeated") Solv.check)
      case r of
        S.Unsat   -> pure acc
        S.Unknown -> pure acc
        S.Sat     -> do
          modelMaker <- asks mpeGetModel
          model <- inSolver modelMaker
          locally #mpeSolverModel (const model) m

    assertDifferentModel :: ModelState -> ModelParserM' () ()
    assertDifferentModel ms = do
      -- FIXME: we should name this so we don't send it twice.
      let sexp = modelStateToSExpr ms
      inSolver $ timed ("assert" <> "repeated") $ do
        Solv.assert (S.not sexp)
        Solv.flush

    cursors = map (relitiviseCursors (slpePathCursor genpe) . slpePathCursor) deppes

-- We do this in two phases to reduce order dependence:
--  1. Get models for the usages along with any generators for each
--     usage, and similarly for the generator
--  2. After getting all the models we can associate them with their
--     generators and recurse.    
makePoolElement :: SymbolicLoopTag -> NonEmpty SymbolicLoopPoolElement ->
                   ModelParserM' () (ModelState, (SelectedPath, [SelectedPath]))
makePoolElement gentag slpes = do
  -- Build each loop element in gen/deps, collecting any sub-gens/deps
  (pgs, (deps, ms)) <-
    listenCensor (\w r -> ((), (r, w))) (traverse buildOne slpes)

  let mkret (g :| ds) = (ms, (g, ds))

  -- If there are no generators we have a fast-path
  mkret <$> if all (Map.null . view _3) pgs
            then pure $ view _1 <$> pgs
            else fixModelIn ms $ recursiveSteps deps pgs
  where
    -- otherwise assert the current model and recurse    
    recursiveSteps deps pgs = do 
      -- Sanity check that we consume all the deps
      let allTags = foldMap (views _3 Map.keysSet) pgs
      unless (Map.keysSet (mpLoopDeps deps) `Set.isSubsetOf` allTags) $
        panic "Leftover loop deps" []
      traverse (resolveGenerators deps) pgs
    
    -- Gets the path and any generators.
    buildOne slpe = do
      let pc = slpePathCursor slpe ++ [PCLoopPool gentag]
          -- We peel off the generators and add the path prefix to any
          -- deps.
          lcfun (deps, gens, ms) sp = ( (deps & loopDepsPathL %~ (pc ++), ms)
                                      , (sp, pc, gens))
      listenCensor lcfun (buildPath (slpeBuilder slpe))

resolveGenerators :: MPLoopDeps -> (SelectedPath, PathCursor, MPLoopGenerators) ->
                     ModelParserM' () SelectedPath
resolveGenerators allDeps (sp, pc, gens) = ifoldlM resolveOneGenerator sp gens'
  where
    gens' :: Map SymbolicLoopTag (SymbolicLoopPoolElement, [SymbolicLoopPoolElement])
    gens' = Map.merge (Map.mapMissing $ const (, []))
                      Map.dropMissing
                      (Map.zipWithMatched (const (,)))
                      gens (mpLoopDeps allDeps)

    resolveOneGenerator ltag sp' (gen, deps) = do
      let gen'  = gen & #slpePathCursor %~ (pc ++)
      pool <- build ltag gen' deps
      
      let node = SelectedLoop $ SelectedLoopPool ltag (slpeNullable gen) [pool]
      pure (fillCursorTarget node (slpePathCursor gen) sp')
      
listenCensor :: (Monoid w, Monoid w') => (w -> a -> (w', b)) -> ModelParserM' w a -> ModelParserM' w' b
listenCensor f = mapRWST $ \m -> do
  (a, s, w) <- m
  let (w', b) = f w a
  pure (b, s, w')
  
-- -----------------------------------------------------------------------------
-- Solver context/model state management

withScopedContext :: Monoid w => ModelParserM' w a -> ModelParserM' w a
withScopedContext m = do
  ctxt <- inSolver Solv.getContext
  r <- m
  inSolver (Solv.restoreContext ctxt)
  pure r

fixModelIn :: Monoid w => ModelState -> ModelParserM' w a -> ModelParserM' w a
fixModelIn ms m = withScopedContext $ do
  let sexp = modelStateToSExpr ms
  inSolver (timed "nestedloop" (Solv.assert sexp))
  -- We also need to update the model in the context.
  local (#mpeModelContext <>~ ms) m

modelStateToSExpr :: ModelState -> S.SExpr
modelStateToSExpr ms =
  andMany (values ++ choices ++ loops)
  where
    values = map valuePred (Map.toList (msValues ms))
    valuePred = \(n, Typed _ty v) -> S.eq (S.const n) (SE.symExecValue v)
    
    choices = map choicePred (Map.toList (msPathVars ms))
    choicePred (pv, i) = S.eq (pathVarToSExpr pv) (S.int (fromIntegral i))

    loops = map lccPred (Map.toList (msLoopCounts ms))
    lccPred (lc, i) = S.eq (loopCountVarToSExpr lc) (loopCountToSExpr i)

-- -----------------------------------------------------------------------------
-- PP Instances

instance PP ModelState where
  pp ms =
    bullets [ ppM pp pp msPathVars
            , ppM text (pp . typedThing) msValues
            , ppM pp pp msLoopCounts
            ]
    where
      ppM ppK ppV f =
        braces (commaSep [ ppK k <> ": " <> ppV v | (k, v) <- Map.toList (f ms) ])








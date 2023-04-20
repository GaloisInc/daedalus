{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# Language OverloadedStrings #-}

-- Symbolic but the only non-symbolic path choices are those in
-- recursive functions (i.e., we only unroll loops).

-- FIXME: factor out commonalities with Symbolic.hs
module Talos.Strategy.PathSymbolic.PathBuilder (buildPaths) where

import           Control.Lens                              (Lens', Setter', _1,
                                                            _2, _3, at,
                                                            each, ifoldlM,
                                                            locally, mapped,
                                                            over, scribe, view,
                                                            views, (%=), (%~),
                                                            (&), (+~), (.~),
                                                            (<>~))
import           Control.Monad.Reader
import           Control.Monad.RWS               (RWST, censor,
                                                            mapRWST, runRWST)
import qualified Data.ByteString                           as BS
import           Data.Foldable                             (find)
import           Data.Functor                              (($>))
import           Data.Functor.Identity                     (Identity (Identity))
import           Data.Generics.Product                     (field)
import           Data.List.NonEmpty                        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                        as NE
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import qualified Data.Map.Merge.Strict                     as Map
import           Data.Maybe                                (isNothing)
import qualified Data.Set                                  as Set
import qualified Data.Vector                               as Vector
import           Data.Word                                 (Word8)
import           GHC.Generics                              (Generic)
import qualified SimpleSMT                                 as S
import           Text.Printf                               (printf)

import           Daedalus.Core                             hiding (streamOffset,
                                                            tByte)
import qualified Daedalus.Core.Semantics.Env               as I
import qualified Daedalus.Core.Semantics.Expr              as I
import           Daedalus.PP                               (Doc, brackets,
                                                            commaSep, pp,
                                                            showPP, text)
import           Daedalus.Panic
import           Daedalus.Time                             (timeIt)
import qualified Daedalus.Value                            as I

import           Talos.Strategy.Monad
import           Talos.Strategy.PathSymbolic.Monad
import           Talos.Strategy.PathSymbolic.MuxValue      (GuardedSemiSExpr,
                                                            GuardedSemiSExprs,
                                                            MuxValue (..),
                                                            VSequenceMeta (..))
import qualified Talos.Strategy.PathSymbolic.MuxValue      as MV
import           Talos.Strategy.PathSymbolic.PathCondition (LoopCountVar,
                                                            PathCondition,
                                                            PathVar,
                                                            ValuePathConstraint,
                                                            loopCountVarToSExpr,
                                                            pathVarToSExpr, loopCountToSExpr)
import qualified Talos.Strategy.PathSymbolic.PathCondition as PC
import           Talos.SymExec.ModelParser                 (evalModelP, pExact,
                                                            pNumber, pSExpr,
                                                            pValue)
import           Talos.SymExec.Path
import           Talos.SymExec.SolverT                     (SMTVar, SolverT)
import qualified Talos.SymExec.SolverT                     as Solv

-- ----------------------------------------------------------------------------------------
-- Model parsing and enumeration.
--
-- This code uses the Monad instance for Maybe pretty heavily, sorry :/

data ModelState = ModelState
  { msPathVars :: Map PathVar Int
  , msValues   :: Map SMTVar (Typed I.Value)
  , msLoopCounts :: Map LoopCountVar Int
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

data ModelParserState = ModelParserState
  { mpsMMS      :: MultiModelState
  -- ^ The stats across multiple models
  -- ^ This is _all_ the values from the solver, including those not
  -- on the path etc.  Having this here means we don't have to worry
  -- about preserving e.g. the solver stack when traversing the
  -- pathbuilder.  It might also be faster to do in a single query(?)
  } deriving Generic

emptyModelParserState :: MultiModelState -> ModelParserState
emptyModelParserState mms = ModelParserState
  { 
   mpsMMS   = mms
--  , mpsHaveSolverModel = False
  }

-- FIXME: clag
getByteVar :: SMTVar -> ModelParserM Word8
getByteVar symB = I.valueToByte <$> getValueVar (Typed TByte symB)

getModelVar :: Ord a => Lens' ModelState (Map a b) -> a -> ModelParserM b
getModelVar msLens pv = do
  -- Look for a pv in the context
  m_ctxt   <- view (field @"mpeModelContext" . msLens . at pv)
  m_solv   <- view (field @"mpeSolverModel" . msLens . at pv)
  
  case (m_ctxt, m_solv) of
    (Just i, _) -> pure i
    -- Note that we saw the variable
    (_, Just i) -> scribe (mpoModelState . msLens) (Map.singleton pv i) $> i
    _           -> panic "Missing variable in solver model" []
          
getPathVar :: PathVar -> ModelParserM Int
getPathVar = getModelVar (field @"msPathVars") 
  -- where
  --   -- bit gross doing this here ...
  --   updateStats pv n mms
  --     | Just chi <- Map.lookup pv (mmsChoices mms)
  --     , n `notElem` mmschiSeen chi =
  --       -- If we have seen all the choices then exhaust.
  --       let res | length (mmschiSeen chi) + 1 == length (mmschiAllChoices chi) = Nothing
  --               | otherwise  = Just (chi & field @"mmschiSeen" %~ (n :))
  --       in mms & field @"mmsChoices" . at pv .~ res
  --              & field @"mmsNovel" +~ 1
  --     | otherwise = mms & field @"mmsSeen" +~ 1

-- Maybe not worth caching?
getLoopVar :: LoopCountVar -> ModelParserM Int
getLoopVar = getModelVar (field @"msLoopCounts")

getValueVar :: Typed SMTVar -> ModelParserM I.Value
getValueVar (Typed _ty x) = typedThing <$> getModelVar (field @"msValues") x

-- -- | Gets any dependencies for a given tag, forgetting about the tag as a side-effect
-- getLoopDep :: SymbolicLoopTag -> ModelParserM [SymbolicLoopPoolElement]
-- getLoopDep ltag = uses (field @"mpsLoopDeps" . at ltag) (fromMaybe [])

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
  } deriving Generic

makeModelParserEnv :: SymbolicModel -> SolverT StrategyM ModelParserEnv
makeModelParserEnv sm = do
  let mkModel = symbolicModelToModelStateP sm
  initModel <- mkModel
  
  pure (ModelParserEnv { mpeSolverModel = initModel
                       , mpeModelContext = mempty
                       , mpeGetModel = mkModel
                       })

symbolicModelToModelStateP :: SymbolicModel -> SolverT StrategyM ModelState
symbolicModelToModelStateP sm = do
  -- We assume the query/result order is the same
  let (pes, pPathVars)   = mapToP PC.pathVarToSMTVar pInt (smChoices sm)
      (ves, pValues)     = mapToP id (\ty -> Typed ty <$> pValue ty) (smNamedValues sm)
      (les, pLoopCounts) = mapToP PC.loopCountVarToSMTVar pInt (Map.fromSet (const ()) (smLoopVars sm))
  sexp <- Solv.getValues (pes ++ ves ++ les)
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
loopDepsPathL = field @"mpLoopDeps" . mapped . each . field @"slpePathCursor"

extendCursorIn :: PathCursorElement -> ModelParserM a -> ModelParserM a
extendCursorIn el = censor go
  where
    go = (mpoLoopDeps . loopDepsPathL %~ (el :))
         . (mpoLoopGenerators . mapped . field @"slpePathCursor" %~ (:) el)

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

runModelParserM :: SymbolicModel -> ModelParserM' w a -> SolverT StrategyM (a, MultiModelState)
runModelParserM sm mp = do
  env0 <- makeModelParserEnv sm
  let mms = symbolicModelToMMS sm -- FIXME
  (a, st, _) <- runRWST mp env0 (emptyModelParserState mms)
  pure (a, mpsMMS st)

-- ------------------------------------------------------------------------------
-- Multi-pass state

data MMSCaseInfo = MMSCaseInfo
  { -- Read only
    mmsciName    :: Name      -- ^ Name of the case var, for debugging
  , mmsciAllPats :: [(S.SExpr, Pattern)] -- ^ All patterns (read only)
  , mmsciPath    :: S.SExpr -- ^ Path to the case
  -- Updated
  , mmsciSeen    :: [Pattern] -- ^ Case alts we have already seen
  } deriving Generic
  
data MMSChoiceInfo = MMSChoiceInfo
  { -- Read only
    mmschiAllChoices :: [Int] -- ^ All choices (read only)
  , mmschiPath       :: S.SExpr -- ^ Path to the choice
  -- Updated  
  , mmschiSeen       :: [Int]     -- ^ Choice alts we have already seen
  } deriving Generic
  
-- This contains all the choices we haven't yet seen.  It might be
-- better to just negate the seen choices, but this is a bit simpler
-- for now.
data MultiModelState = MultiModelState
  { mmsChoices :: Map PathVar MMSChoiceInfo
  , mmsCases   :: Map SymbolicCaseTag MMSCaseInfo
  , mmsNovel   :: Int -- ^ For a run, how many novel choices/cases we saw.
  , mmsSeen    :: Int -- ^ For a run, how many seen choices/cases we saw.
  } deriving Generic

-- This just picks one at a time, we could also focus on a particular (e.g.) choice until exhausted.
nextChoice :: MultiModelState -> SolverT StrategyM (Maybe (S.SExpr, MultiModelState -> MultiModelState, Doc))
nextChoice mms
  -- FIXME: just picks the first, we could be e.g. random
  | Just ((pv, chi), _mmsC) <- Map.minViewWithKey (mmsChoices mms) = do
      let notSeen = S.distinct (pathVarToSExpr pv : map (S.int . fromIntegral) (mmschiSeen chi))
          exhaust = over (field @"mmsChoices") (Map.delete pv)
      pure (Just (MV.andMany [notSeen, mmschiPath chi]
                 , exhaust
                 , pp pv <> " in " <> brackets (commaSep [ pp i | i <- mmschiAllChoices chi, i `notElem` mmschiSeen chi])))
  | Just ((stag, ci), _mmsC) <- Map.minViewWithKey (mmsCases mms) = do
      let notSeen = S.not (MV.orMany [ p | (p, pat) <- mmsciAllPats ci, pat `elem` mmsciSeen ci ])
          exhaust = over (field @"mmsCases") (Map.delete stag)
          -- p = MV.orMany (map PC.toSExpr (NE.toList vcond))
      pure (Just ( MV.andMany [notSeen, mmsciPath ci]
                 , exhaust
                 , pp (mmsciName ci) <> "." <> pp stag <> " in "
                   <> brackets (commaSep [ pp pat | (_, pat) <- mmsciAllPats ci, pat `notElem` mmsciSeen ci])))  -- <> commaSep (map pp (mmsciSeen ci))))
  | otherwise = pure Nothing

modelChoiceCount :: MultiModelState -> Int
modelChoiceCount mms = choices + cases
  where
    choices = sum $ [ length (mmschiAllChoices chi) - length (mmschiSeen chi)
                    | chi <- Map.elems (mmsChoices mms)
                    ]
    cases   = sum $ [ length (mmsciAllPats ci) - length (mmsciSeen ci)
                    | ci <- Map.elems (mmsCases mms)
                    ]

symbolicModelToMMS :: SymbolicModel -> MultiModelState
symbolicModelToMMS sm = MultiModelState
  { mmsChoices = fmap mkCh (smChoices sm)
  , mmsCases   = fmap mkCi (smCases   sm)
  , mmsNovel   = 0
  , mmsSeen    = 0
  }
  where
    mkCh (paths, idxs) = MMSChoiceInfo
      { mmschiAllChoices = idxs
      , mmschiPath = MV.andMany paths
      , mmschiSeen = []
      }
    mkCi (name, paths, pats) = MMSCaseInfo
      { mmsciName    = name
      , mmsciAllPats = over (each . _1) (MV.orMany . map PC.toSExpr . NE.toList) pats
      , mmsciPath    = MV.andMany paths
      , mmsciSeen    = []
      }

nullMMS :: MultiModelState -> Bool
nullMMS ms = Map.null (mmsCases ms) && Map.null (mmsChoices ms)

buildPaths :: Int -> Maybe Int -> SymbolicModel -> PathBuilder ->
              SolverT StrategyM [SelectedPath]
buildPaths _ntotal _nfails sm pb = do
  (_, tassert) <- timeIt $ do
    mapM_ Solv.assert (smAsserts sm)
    Solv.flush
  liftIO $ printf "; initial model time: %.3fms\n" (fromInteger tassert / 1000000 :: Double)
  (r, tcheck) <- timeIt Solv.check
  liftIO $ printf "\tInitial solve time: %.3fms" (fromInteger tcheck / 1000000 :: Double)
  case r of
    S.Unsat   -> pure []
    S.Unknown -> pure []
    S.Sat     -> do
      -- Have at least 1 model, lets pretend to be a loop.
      (pool, _) <- runModelParserM sm (build invalidSymbolicLoopTag fakesle [])
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

-- FIXME: we could get all the sexps from the solver in a single query.
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
      SelectedCase pib   -> SelectedCase <$> buildPathCase pib
      SelectedLoop lp    -> buildLoop lp

    resolveResult :: SolverResult -> ModelParserM BS.ByteString
    resolveResult (ByteResult b) = BS.singleton <$> getByteVar b
    resolveResult (InverseResult env ifn) = do
      -- FIXME: we should maybe make this lazy
      m_venv <- sequence <$> traverse gsesModel env
      case m_venv of
        Nothing -> panic "Couldn't construct environment" []
        Just venv -> do
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

buildLoop (PathLoopMorphism _ltag guardedEls) = do
  m_el <- findM (pathConditionModel . view _1) guardedEls
  case m_el of
    Nothing -> panic "No input collection was reachable" []
    Just (_, vsm, els)
      -- This is the unrolled case, so we just emit the path. We re-use the Unrolled case above.
      | isNothing (vsmGeneratorTag vsm) -> buildLoop (PathLoopUnrolled (vsmLoopCountVar vsm) els)
      -- We always have an element, although it might not be well-defined if the loopvar is 0
      | Just genltag <- vsmGeneratorTag vsm, [el] <- els ->
        loopNonEmpty (vsmLoopCountVar vsm) $ do
          recordLoopDep genltag el
          -- We will fill this in at synthesis time.
          pure SelectedHole
        
      | otherwise -> panic "Non-singleton sequence" []

loopNonEmpty :: Maybe LoopCountVar -> ModelParserM SelectedPath -> ModelParserM SelectedPath
loopNonEmpty m_lv m = do
  isNull <- maybe (pure False) (fmap (== 0) . getLoopVar) m_lv
  if isNull
     -- null case, we shouldn't have any users, so we don't
     -- generate a pool.
    then pure (SelectedLoop (SelectedLoopElements Nothing []))
    -- singleton case, we need to record and move on.
    else m
    
buildPathChoice :: PathChoiceBuilder PathBuilder ->
                   ModelParserM (PathIndex SelectedPath)
buildPathChoice (ConcreteChoice i p) = PathIndex i <$> buildPath p
buildPathChoice (SymbolicChoice pv ps) = do
  i <- getPathVar pv
  let p = case lookup i ps of
            Nothing  -> panic "Missing choice" []
            Just p'  -> p'
  PathIndex i <$> buildPath p

buildPathCase :: PathCaseBuilder PathBuilder ->
                 ModelParserM (Identity SelectedPath)
buildPathCase (ConcreteCase p) = Identity <$> buildPath p
buildPathCase (SymbolicCase stag gses ps) = do
  m_v <- gsesModel gses
  v <- case m_v of
         Nothing  -> do
           panic "Missing case value" [showPP (text . typedThing <$> gses)]
         Just v'  -> pure v'
  let (pat, p) = case find (flip I.matches v . fst) ps of
        Nothing ->
          panic "Missing case alt" [showPP v
                                   , showPP (text . typedThing <$> gses)
                                   , showPP (commaSep (map (pp . fst) ps))
                                   ]
        Just r -> r

  field @"mpsMMS" %= updateCaseStats pat

  Identity <$> buildPath p
  where
    -- c.f. getPathVar
    updateCaseStats pat mms'
      | Just ci <- Map.lookup stag (mmsCases mms')
      , pat `notElem` mmsciSeen ci =
        let res | length (mmsciSeen ci) + 1 == length (mmsciAllPats ci) = Nothing
                | otherwise  = Just (ci & field @"mmsciSeen" %~ (pat :))
        in mms' & field @"mmsCases" . at stag .~ res
                & field @"mmsNovel" +~ 1
      | otherwise = mms' & field @"mmsSeen" +~ 1

-- -----------------------------------------------------------------------------
-- Helpers

-- short-circuiting
andM :: Monad m => [m Bool] -> m Bool
andM [] = pure True
andM (m : ms) = do
  b <- m
  if b then andM ms else pure False

-- short-circuiting
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _f [] = pure Nothing
findM f (x : xs) = do
  b <- f x
  if b then pure (Just x) else findM f xs

-- -----------------------------------------------------------------------------
-- Path conditions and guarded values

pathConditionModel :: PathCondition -> ModelParserM Bool
pathConditionModel PC.Infeasible = pure False
pathConditionModel (PC.FeasibleMaybe pci) = do
  -- We try choices first as they are more likely to fail (?)  
  choiceb <- andM [ (==) i <$> getPathVar pv
                  | (pv, i) <- Map.toList (PC.pcChoices pci) ]
  if choiceb
    then andM (map vpcModel (Map.toList (PC.pcValues pci)))
    else pure False
  where
    vpcModel :: (SMTVar, Typed ValuePathConstraint) -> ModelParserM Bool
    vpcModel (x, vpcT) =
      PC.vpcSatisfied vpcT <$> getValueVar (vpcT $> x)

gseModel :: GuardedSemiSExpr -> ModelParserM (Maybe I.Value)
gseModel gse =
  case gse of
    VValue v -> pure (Just v)
    VOther x -> Just <$> getValueVar x
    VUnionElem l gses -> fmap (I.VUnionElem l) <$> gsesModel gses
    VStruct flds -> do
      let (ls, gsess) = unzip flds
      m_gsess <- sequence <$> mapM gsesModel gsess
      pure (I.VStruct . zip ls <$> m_gsess)

    VSequence vsm gsess
      | vsmIsBuilder vsm ->
        fmap (I.VBuilder . reverse) . sequence <$> mapM gsesModel gsess
      | otherwise -> 
        fmap (I.VArray . Vector.fromList) . sequence <$> mapM gsesModel gsess

    VJust gses -> fmap (I.VMaybe . Just) <$> gsesModel gses
    VMap els -> do
      let (ks, vs) = unzip els
      m_kvs <- sequence <$> mapM gsesModel ks
      m_vvs <- sequence <$> mapM gsesModel vs
      pure (I.VMap . Map.fromList <$> (zip <$> m_kvs <*> m_vvs))

  -- We support symbolic keys, so we can't use Map here
    VIterator els -> do
      let (ks, vs) = unzip els
      m_kvs <- sequence <$> mapM gsesModel ks
      m_vvs <- sequence <$> mapM gsesModel vs
      pure (I.VIterator <$> (zip <$> m_kvs <*> m_vvs))

gsesModel :: GuardedSemiSExprs -> ModelParserM (Maybe I.Value)
gsesModel gses = join <$> (traverse (gseModel . snd) =<< findM go els)
  where
    go  = pathConditionModel . fst
    els = MV.guardedValues gses

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
build gentag genpe deppes =
  withScopedContext $ SelectedLoopPoolF <$> go 0 10 [] <*> pure cursors   
  where 
    go :: Int -> Int -> [(SelectedPath, [SelectedPath])] ->
          ModelParserM' () [(SelectedPath, [SelectedPath])]
    go n ntotal acc = do
      (ms, r) <- makePoolElement gentag (genpe :| deppes)
      let acc' = r : acc
      if n >= ntotal
        then pure acc'
        else tryAgain ms acc (go (n + 1) ntotal acc')
        
    tryAgain ms acc m = do
      assertDifferentModel ms
      r <- inSolver Solv.check
      case r of
        S.Unsat   -> pure acc
        S.Unknown -> pure acc
        S.Sat     -> do
          modelMaker <- asks mpeGetModel
          model <- inSolver modelMaker
          locally (field @"mpeSolverModel") (const model) m

    assertDifferentModel :: ModelState -> ModelParserM' () ()
    assertDifferentModel ms = do
      -- FIXME: we should name this so we don't send it twice.
      sexp <- modelStateToSExpr ms
      inSolver (Solv.assert (S.not sexp))

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
      let gen'  = gen & field @"slpePathCursor" %~ (pc ++)
      pool <- build ltag gen' deps
      
      let node = SelectedLoop $ SelectedLoopPool ltag (slpeNullable gen) [pool]
      pure (fillCursorTarget node (slpePathCursor gen) sp')
      
listenCensor :: (w -> a -> (w', b)) -> ModelParserM' w a -> ModelParserM' w' b
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
  sexp <- modelStateToSExpr ms
  inSolver (Solv.assert sexp)
  -- We also need to update the model in the context.
  local (field @"mpeModelContext" <>~ ms) m

modelStateToSExpr :: LiftStrategyM m => ModelState -> m S.SExpr
modelStateToSExpr ms = do
  tdefs <- getTypeDefs
  pure (S.andMany (values tdefs ++ choices ++ loops))
  where
    values tdefs = map (valuePred tdefs) (Map.toList (msValues ms))
    valuePred tdefs = \(n, Typed ty v) -> S.eq (S.const n) (MV.valueToSExpr tdefs ty v)
    
    choices = map choicePred (Map.toList (msPathVars ms))
    choicePred (pv, i) = S.eq (pathVarToSExpr pv) (S.int (fromIntegral i))

    loops = map lccPred (Map.toList (msLoopCounts ms))
    lccPred (lc, i) = S.eq (loopCountVarToSExpr lc) (loopCountToSExpr i)










{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language ExistentialQuantification #-}


module Talos.Synthesis (synthesise) where

import           Control.Lens                    (at, (%=), (?~))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as BS
import           Data.Foldable                   (find, foldlM, toList)
import           Data.Functor.Identity           (Identity (Identity))
import           Data.Generics.Product           (field)
import           Data.List                       (foldl')
import qualified Data.List.NonEmpty              as NE
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import qualified Data.Set                        as Set
import           Data.Word
import           GHC.Generics                    (Generic)
import           SimpleSMT                       (Solver)
import qualified Streaming as S
import           System.Random
import System.IO               (hPutStrLn, stderr)
import System.Exit (exitFailure)
import           Text.Printf                               (printf)
import           System.IO (hFlush, stdout)


import           Daedalus.Core                   hiding (streamOffset)
import           Daedalus.Core.Free
import qualified Daedalus.Core.Semantics.Env     as I
import qualified Daedalus.Core.Semantics.Expr    as I
import qualified Daedalus.Core.Semantics.Grammar as I
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic
import           Daedalus.RTS.Input              (newInput)
import qualified Daedalus.Value                  as I
import           RTS.ParseError                  (ErrorStyle (SingleError),
                                                  ppParseError)
import           RTS.Parser                      (runParser)
import           RTS.ParserAPI                   (ResultG (..))

import           Talos.Analysis                  (summarise)
import           Talos.Analysis.Exported         (SliceId, esRootSlices)
import           Talos.Analysis.Merge            (merge)
import           Talos.Analysis.Slice
-- import Talos.SymExec
import           Talos.SymExec.Path
import           Talos.SymExec.SolverT           (emptySolverState)
import           Talos.SymExec.StdLib

import           Talos.Analysis.AbsEnv           (AbsEnvTy (AbsEnvTy))
import           Talos.Strategy
import           Talos.Strategy.Monad
import Data.Functor.Of (Of ( (:>) ))
import Talos.Monad (TalosM, getIEnv, getGFun, getModule)


data Stream = Stream { streamOffset :: Integer
                     , streamBound  :: Maybe Int
                     }

emptyStream :: Stream
emptyStream = Stream 0 Nothing

--------------------------------------------------------------------------------
-- Values
--
-- We need a richer notion of stream to support synthesis in the
-- presence of Stream operations.  When we need to invoke the
-- interpreter, we construct the target environment based upon the
-- free variables in the object we are going to interpret.
--
-- Support for Streams is currently very limited --- they may not
-- occur inside another Value, for example.  In practice this should
-- allow most of the stream operations we care about.

data Value = InterpValue I.Value | StreamValue Stream

data SynthEnv = SynthEnv { synthValueEnv  :: Map Name Value
                         , pathSetRoots :: Map Name [SliceId]
                         , currentClass :: FInstId
                         , currentFName :: FName
                         , loopTagInstMap :: Map LoopGeneratorTag Int
                         } deriving Generic

addVal :: Name -> Value -> SynthEnv -> SynthEnv
addVal x v e = e { synthValueEnv = Map.insert x v (synthValueEnv e) }

projectInterpValue :: Value -> Maybe I.Value
projectInterpValue (InterpValue v) = Just v
projectInterpValue _               = Nothing

-- Basically a hack
assertInterpValue :: Value -> I.Value
assertInterpValue (InterpValue v) = v
assertInterpValue _               = panic "Expecting an InterpValue, got a StreamValue" []

-- Projects out the I.Value for each free varable, returns Nothing if we see a non-InterpValue
-- projectEnv :: SynthEnv -> Maybe (Map (TCName K.Value) I.Value)
-- projectEnv se = traverse projectInterpValue (synthValueEnv se)

projectEnvFor :: FreeVars t => t -> I.Env -> SynthEnv -> Maybe I.Env
projectEnvFor tm env0 se = doMerge <$> Map.traverseMaybeWithKey go (synthValueEnv se)
  where
    frees = freeVars tm

    doMerge m = env0 { I.vEnv = Map.union m (I.vEnv env0) } 
    
    go k v | k `Set.member` frees = Just <$> projectInterpValue v
    go _ _                        = Just Nothing

projectEnvForM :: FreeVars t => t -> SynthesisM I.Env
projectEnvForM tm = do
  env0 <- liftStrategy getIEnv
  m_e <- SynthesisM $ asks (projectEnvFor tm env0)
  case m_e of
    Just e  -> pure e
    Nothing -> panic "Captured stream value" []

vUnit :: Value
vUnit = InterpValue I.vUnit

-- FIXME: Means we can't have an array of streams
vArray :: [Value] -> Value
vArray = InterpValue . I.vArray . map assertInterpValue

vMap :: [(Value, Value)] -> Value
vMap els = InterpValue (I.VMap (Map.fromList els'))                               
  where
    els' = [ (assertInterpValue k, assertInterpValue v) | (k, v) <- els ]

vSize :: Integer -> Value
vSize = InterpValue . I.vSize

--------------------------------------------------------------------------------
-- Synthesis state

data SynthesisMState =
  SynthesisMState { seenBytes :: ByteString
                  , curStream :: Stream
                  , nextProvenance :: ProvenanceTag 
                  , provenances :: ProvenanceMap
                  , modelCache :: ModelCache
                  -- | This contains the stack of RHSs that can be
                  -- updated by Many selection (enables lazy selection
                  -- of Many elements).
                  , pathContext :: PathContext
                  } deriving Generic

newtype SynthesisM a =
  SynthesisM { getSynthesisM :: ReaderT SynthEnv (StateT SynthesisMState StrategyM) a }
  deriving (Functor, Applicative, Monad, MonadIO, LiftStrategyM)

addBytes :: ProvenanceTag -> ByteString -> SynthesisM ()
addBytes prov bs = 
  SynthesisM $ 
    modify (\s -> s { seenBytes = seenBytes s <> bs
                    , curStream = updStream (curStream s)
                    , provenances = updProvenances (curStream s) (provenances s)
                    })
  where
    updStream strm = strm { streamOffset = streamOffset strm + (fromIntegral $ BS.length bs) }

    updProvenances strm currprovs = 
      if prov /= randomProvenance then 
        let off = fromIntegral $ streamOffset strm 
            len = fromIntegral $ BS.length bs
        in 
          foldr (\o -> Map.insert o prov) currprovs [off.. (off + (len - 1))]
      else currprovs 

addByte :: ProvenanceTag -> Word8 -> SynthesisM ()
addByte prov word = addBytes prov (BS.singleton word)

-- currentNode :: SynthesisM (Maybe SelectedNode')
-- currentNode = SynthesisM $ state go
--   where go s = let (n, ps') = splitPath (currentPath s)
--                in (n, s { currentPath = ps' })

bindIn :: Name -> Value -> SynthesisM a -> SynthesisM a
bindIn x v = SynthesisM . local (addVal x v) . getSynthesisM 

bindInMaybe :: Maybe Name -> Value -> SynthesisM a -> SynthesisM a
bindInMaybe Nothing _  = id
bindInMaybe (Just x) v = bindIn x v

freshProvenanceTag :: SynthesisM ProvenanceTag
freshProvenanceTag = do 
  p <- SynthesisM $ gets nextProvenance
  SynthesisM $ modify (\s -> s { nextProvenance = p + 1 })
  return p

withPushedContext :: PathContextElement -> SynthesisM a -> SynthesisM (a, PathContextElement)
withPushedContext pce m = do
  SynthesisM $ modify (\s -> s { pathContext = pce : pathContext s })
  r <- m
  pc' <- SynthesisM $ state go
  pure (r, pc')
  where
    go s
      | pc' : rest <- pathContext s = (pc', s { pathContext = rest })
      | otherwise = panic "BUG: empty PathContext stack" []  

overPathContext :: (PathContext -> PathContext) -> SynthesisM ()
overPathContext f = SynthesisM $ field @"pathContext" %= f

printPathContext :: SynthesisM ()
printPathContext = do
  pc <- SynthesisM $ gets pathContext
  liftIO $ print (ppPathContext pc)
  
  
enterLoop :: Maybe LoopGeneratorTag -> Int -> SynthesisM a -> SynthesisM a
enterLoop Nothing _     m = m
enterLoop (Just ltag) i (SynthesisM m) =
  SynthesisM $ local (field @"loopTagInstMap" . at ltag ?~ i) m

getLoopTagInstMap :: SynthesisM (Map LoopGeneratorTag Int)
getLoopTagInstMap = SynthesisM $ asks loopTagInstMap
  
-- -----------------------------------------------------------------------------
-- Top level

type DocumentStream = S.Stream (Of (I.Value, ByteString, ProvenanceMap)) TalosM ()

synthesise :: Maybe Int -> Solver -> AbsEnvTy -> [StrategyInstance] -> FName -> DocumentStream
synthesise m_seed solv (AbsEnvTy p) strats root = S.effect $ do
  allSummaries <- summarise p

  -- Generate a seed if none has been given to us.
  seed    <- liftIO $ maybe randomIO pure m_seed
  -- putStrLn ("Using random seed " ++ show seed)  
  let gen = mkStdGen seed

  sst0 <- makeStrategyMState gen allSummaries
  let solvSt0 = emptySolverState solv
      mc0 = newModelCache strats solvSt0
      
  -- Init solver stdlib
  -- FIXME: probably move?
  liftIO $ makeStdLib solv 

  md <- getModule
  let Just rootDecl = find (\d -> fName d == root) (mGFuns md)

  pure (S.unfold (go rootDecl) (sst0, mc0))
  where
    -- go :: StrategyMState -> ModelCache -> Generator (I.Value, ByteString, ProvenanceMap) ()
    go rootDecl (s0, mc) = do
      let once = synthesiseCallG SelectedHole (fName rootDecl) assertionsFID []
      
      ((a, s), sts) <-
        runStrategyM s0 (runStateT (runReaderT (getSynthesisM once) env0)
                         (initState mc))
                       
      pure (Right $ (assertInterpValue a, seenBytes s, provenances s) :> (sts, modelCache s))

    initState mc = 
      SynthesisMState { seenBytes      = mempty
                      , curStream      = emptyStream
                      , nextProvenance = firstSolverProvenance
                      , provenances    = Map.empty 
                      , modelCache     = mc
                      , pathContext    = []
                      }

    -- Do the actual synthesis by calling the main function. The
    -- 'Assertions' is to tell the system that we don't care about the
    -- result, and so the summary we should use is the one for the
    -- 'Assertions' result class.
    --
    -- The 'Unconstrained' is the current path set --- we have not yet determined any future bytes.

    env0      = SynthEnv
                { synthValueEnv = mempty
                , pathSetRoots  = mempty
                , currentClass  = assertionsFID
                , currentFName  = root
                , loopTagInstMap  = mempty
                }
    
    -- ns        = needsSolver allDecls
    -- rs     = Map.fromList [ (fName d, d) | d <- allDecls ]
  
-- -- -----------------------------------------------------------------------------
-- -- Random bytes

-- getByte :: SynthesisM Value
-- getByte = do b <- rand
--              addByte randomProvenance b 
--              pure (InterpValue $  I.VUInt 8 (fromIntegral b))

-- We could also invoke the solver here.  This is a bit brute force
synthesiseByteSet :: ByteSet -> SynthesisM Value
synthesiseByteSet SetAny = do
  b <- rand
  addByte randomProvenance b 
  pure (InterpValue $ I.vByte b)

-- Special case for constants
synthesiseByteSet (SetSingle (Ap0 (IntL bi _))) = do
  let b = fromIntegral bi
  addByte randomProvenance b 
  pure (InterpValue $ I.vByte b)

synthesiseByteSet (SetSingle expr) = do
  e <- projectEnvForM expr
  let b = I.valueToByte (I.eval expr e)
  addByte randomProvenance b 
  pure (InterpValue $ I.vByte b)

-- Special case for concrete range
synthesiseByteSet (SetRange (Ap0 (IntL bl _)) (Ap0 (IntL bh _))) = do
  b <- randR (fromIntegral bl, fromIntegral bh)
  addByte randomProvenance b 
  pure (InterpValue $ I.vByte b)

synthesiseByteSet bset = do
  e <- projectEnvForM bset
  let bs           = filter (I.evalByteSet bset e) [0 .. 255] -- FIXME!!
  when (bs == []) $ panic "Empty predicate" [showPP bset]
  b <- randL bs
  addByte randomProvenance b 
  pure (InterpValue $ I.vByte b)

synthesiseV :: Expr -> SynthesisM Value
synthesiseV v = do e <- projectEnvForM v
                   pure (InterpValue $ I.eval v e)

{-# NOINLINE mbPure #-}
mbPure :: Sem -> Value -> SynthesisM Value
mbPure SemNo _ = pure vUnit
mbPure _     v = pure v

synthesiseDecl :: SelectedPath -> FInstId -> Fun Grammar -> [Expr] -> SynthesisM Value
synthesiseDecl fp fid Fun { fDef = Def def, ..} args = do
  args' <- mapM synthesiseV args
  roots <- flip (Map.!) fid . flip (Map.!) fName . esRootSlices <$> summaries
  -- Add definitions for the function parameters, mapping to the
  -- actuals.  We also set the path root map for the duration of this
  -- function from the results discovered during analysis.
  let addPs e = foldl (\e' (k, v) -> addVal k v e') e (zip fParams args')
      setEnv e = e { pathSetRoots = roots
                   , currentClass = fid
                   , currentFName = fName
                   }
  -- Update the synthesis scope for the function and synthesise its body.
  SynthesisM $ local (setEnv . addPs) (getSynthesisM (synthesiseG fp def))

synthesiseDecl _ _ f _ = panic "Undefined function" [showPP (fName f)]

synthesiseCallG :: SelectedPath -> FName -> FInstId -> [Expr] -> SynthesisM Value
synthesiseCallG fp n fid args = do
  -- liftIO $ printf "Calling into %s\n" (showPP n)
  decl <- liftStrategy (getGFun n)
  synthesiseDecl fp fid decl args

-- -----------------------------------------------------------------------------
-- Loops

synthesiseLoopBounds :: Bool -> Value -> Maybe Value -> SynthesisM Int
synthesiseLoopBounds canBeNull lv m_uv = do
  -- FIXME: make params  
  let -- Policy 
    -- Number of iterations, unless below the min
    softMaxLoopCount = 10 -- 1000
    -- The minimum loop range (unless constrained by both lower and upper).
    minLoopRangeSize = 10
    altUpperBound = max softMaxLoopCount (l + minLoopRangeSize)
    
    u = maybe altUpperBound (min altUpperBound) m_u
    
  r <- fromIntegral <$> randR (l, u)
  when (r > 100) $ liftIO $ do
    printf "Large loop count (%d)\n" r
    hFlush stdout
  -- liftIO $ if (r == 0)
  --          then do
  --            printf "Zero loop count (%d, %d)\n" l u
  --            hFlush stdout
  --          else do
  --            printf "Non-zero loop count (%d, %d)\n" l u
  --            hFlush stdout
    
  pure r
    
  where
    l = max altLowerBound (I.valueToIntegral (assertInterpValue lv))
    m_u = I.valueToIntegral . assertInterpValue <$> m_uv

    altLowerBound = if canBeNull then 0 else 1

-- | Given a collection of Many elments and a target count, this
-- function selects that many from the given SelectedMany
selectLoopElements :: Int -> SelectedLoopPool -> SynthesisM SelectedLoopPool
selectLoopElements count sm = do
  let paths  = smPaths sm
      nPaths = length paths
      repCount
        | nPaths > count = 1
        | (count `div` nPaths) + nPaths == count = count `div` nPaths
        | otherwise = (count `div` nPaths) + 1
  paths' <- randPermute (concat (replicate repCount paths))
  pure $ sm { smPaths = take count paths' }

synthesiseLoopMorphism :: Maybe LoopGeneratorTag ->
                          Maybe [SelectedPath] ->
                          LoopMorphism Grammar -> SynthesisM Value
synthesiseLoopMorphism m_ltag m_ps lm =
  case lm of
    FoldMorphism s e lc b -> do
      e_v <- synthesiseV e
      (ps, els, bindLC, _mk) <- goLC lc <$> synthesiseV (lcCol lc)
      let goEl el = \acc p -> bindLC el (bindIn s acc (synthesiseG p b))
      synthesiseLoop m_ltag ps (map goEl els) e_v
    MapMorphism lc b -> do
      (ps, els, bindLC, mk) <- goLC lc <$> synthesiseV (lcCol lc)
      let goEl el = \acc p -> (: acc) . (,) (fst el) <$> bindLC el (synthesiseG p b)
      mk <$> synthesiseLoop m_ltag ps (map goEl els) []
  where
    goLC lc v
      | length ps /= length els
      = panic "BUG: length mismatch in synthesiseG" []
      | otherwise = (ps, els, bindLC, mk)
      where
        ps = fromMaybe (replicate (length els) SelectedHole) m_ps
        -- This will not contain holes, by construction
        k_bind = maybe (const id) bindIn (lcKName lc)
        bindLC (kv, ev) = k_bind kv . bindIn (lcElName lc) ev
        (els, mk) = case assertInterpValue v of
          I.VArray vs -> ( zip (map vSize [0..]) (map InterpValue $ toList vs)
                         , vArray . map snd
                         )
          I.VMap m     -> ( [ (InterpValue k, InterpValue v') | (k, v') <- Map.toList m ]
                          , vMap )
          _ -> panic "evalLoopMorphism" [ "Value not a collection" ]
    
-- =============================================================================
-- Tricky Synthesis

--------------------------------------------------------------------------------
-- Selection

choosePath :: SelectedPath -> Name -> SynthesisM SelectedPath
choosePath cp x = do
  m_sl <- SynthesisM $ asks (Map.lookup x . pathSetRoots)
  case m_sl of
    Nothing  -> pure cp
    -- The analysis determined that we have a set of projections on
    -- the value of x that are rooted here.  We don't care about the
    -- projections, just the slices as we will (re)construct the value for
    -- x when we pass the bytes through the interpreter. 
    Just sls -> do
      mc <- SynthesisM $ gets modelCache
      go [] mc sls
  where    
    go acc mc [] = do
      SynthesisM $ modify (\s -> s { modelCache = mc })
      pure (foldl' merge cp acc)
      
    go acc mc (sl : sls) = do
      prov <- freshProvenanceTag
      fn   <- SynthesisM $ asks currentFName
      (m_cp, mc') <- findModel mc prov fn x sl
      case m_cp of
        Nothing -> liftIO $ do
          hPutStrLn stderr $ "All strategies failed for " ++ showPP x
          exitFailure
          
        Just sp -> go (sp : acc) mc' sls
      
-- -----------------------------------------------------------------------------
-- Simple Synthesis

synthesiseDo :: SelectedPath -> Maybe Name -> Grammar -> Grammar ->
                SynthesisM Value
synthesiseDo cp m_x lhs rhs = do
  cp' <- maybe (pure cp) (choosePath cp) m_x
  let (lhsp, rhsp) = splitPath cp'
  (v, pc) <- withPushedContext (PCEDoRHS rhsp) (synthesiseG lhsp lhs)
  let rhsp' = case pc of
                PCEDoRHS r -> r
                _ -> panic "Unexpected path context element" []
              
  bindInMaybe m_x v (synthesiseG rhsp' rhs)

synthesiseLoop :: Maybe LoopGeneratorTag ->
                  [SelectedPath] ->
                  [a -> SelectedPath -> SynthesisM a] ->
                  a -> SynthesisM a
synthesiseLoop m_ltag = go 0 
  where
    -- essentially itraverse with the addition of the context manipulation.
    go _ [] _ acc = pure acc
    go i (p : ps') (m : ms') acc = do
      let pcel = PCELoopBody i ps'
      (acc', pc) <- withPushedContext pcel (enterLoop m_ltag i (m acc p))
      let ps'' = case pc of
                   PCELoopBody _ r -> r
                   _ -> panic "Unexpected path context element" []
      go (i + 1) ps'' ms' acc'
    go _ _ _ _ = panic "Wrong number of loop bodies" []
    
synthesiseMany :: Maybe LoopGeneratorTag -> [SelectedPath] ->
                  Grammar -> SynthesisM [Value]
synthesiseMany m_ltag ps g =
  reverse <$> synthesiseLoop m_ltag ps (repeat go) [] 
  where
    go acc p = (: acc) <$> synthesiseG p g
    
-- Does all the heavy lifting
synthesiseG :: SelectedPath -> Grammar -> SynthesisM Value

synthesiseG p (Annot _ g) = synthesiseG p g

-- This does all the work for internal slices etc.
synthesiseG cp (Do x lhs rhs) = synthesiseDo cp (Just x) lhs rhs
synthesiseG cp (Do_ lhs rhs)  = synthesiseDo cp Nothing  lhs rhs
synthesiseG cp (Let x e rhs)  = synthesiseDo cp (Just x) (Pure e) rhs

synthesiseG (SelectedBytes prov bs) g = do
  addBytes prov bs
  env <- projectEnvForM g

  let inp = newInput "<synthesised bytes>" bs
      res = case runParser (I.evalG g env) SingleError inp of
        NoResults e -> panic "No results" [ show (ppParseError e) ]
        Results  rs -> fst (NE.head rs) -- FIXME: this is maybe a little suspicious
      
  pure (InterpValue res)
      
synthesiseG (SelectedChoice (PathIndex n sp)) (Choice _biased gs)
  | n < length gs = synthesiseG sp (gs !! n)
  | otherwise     = panic "Index out of bounds" []

synthesiseG (SelectedCase (Identity sp)) (GCase cs) = do
  let v = Var (caseVar cs)
  env <- projectEnvForM v
  let base = panic "Failed to match pattern" [showPP cs]
  I.evalCase (\g _ -> synthesiseG sp g) base cs env 

synthesiseG (SelectedCall fid sp) (Call fn args) = synthesiseCallG sp fn fid args

synthesiseG (SelectedLoop (SelectedLoopElements m_ltag ps)) (Loop (ManyLoop sem _bt _lb _m_ub g)) = do
  vs <- synthesiseMany m_ltag ps g
  mbPure sem (vArray vs)
  
-- FIXME: sem
synthesiseG (SelectedLoop (SelectedLoopPool ltag canBeNull ms)) (Loop (ManyLoop sem _bt lb m_ub g)) = do
  lv   <- synthesiseV lb
  m_uv <- traverse synthesiseV m_ub
  count <- synthesiseLoopBounds canBeNull lv m_uv

  -- We need to select count elements from the synthesised models in ms.
  ms' <- mapM (selectLoopElements count) ms

  -- We need this to resolve which indices we are in which loops.
  tagMap <- getLoopTagInstMap
  let (elPaths, targetPaths) = selectedMany ltag tagMap ms'
  
  -- ... which may require us to update the RHS stack to propagate selected models.  
  overPathContext (applyManyTargets targetPaths)
  
  vs <- synthesiseMany (Just ltag) elPaths g
  mbPure sem (vArray vs)

synthesiseG (SelectedLoop (SelectedLoopElements m_ltag ps)) (Loop (RepeatLoop _bt n e g)) = do
  initV <- synthesiseV e
  -- FIXME: do we need the enterLoop here?
  let go v p = bindIn n v (synthesiseG p g)
  synthesiseLoop m_ltag ps (repeat go) initV

synthesiseG (SelectedLoop (SelectedLoopElements m_ltag ps)) (Loop (MorphismLoop lm)) = synthesiseLoopMorphism m_ltag (Just ps) lm
  
synthesiseG SelectedHole g = -- Result of this is unentangled, so we can choose randomly
  case g of
    Pure e            -> synthesiseV e
    GetStream         -> unimplemented
    SetStream {}      -> unimplemented
    Match s (MatchByte bset) ->
      synthesiseByteSet bset >>= mbPure s
    Match s (MatchBytes v) -> do
      bs <- synthesiseV v
      -- prov <- freshProvenanceTag
      addBytes synthVProvenance (I.valueToByteString (assertInterpValue bs)) -- XXX is this the random case?
      mbPure s bs
    Match _s _        -> unimplemented
    Fail {}           -> unimplemented -- probably should be impossible
    -- Handled above
    Do  {}            -> impossible
    Let {}            -> impossible
    Do_ {}            -> impossible
    
    Choice _biased gs -> do
      g' <- randL gs
      synthesiseG SelectedHole g'
    OrBiased {}      -> impossible
    OrUnbiased {}    -> impossible
    
    Call fn args     -> synthesiseCallG SelectedHole fn assertionsFID args
    Annot {}         -> impossible
    GCase c@(Case y _) -> do
      env <- projectEnvForM y
      I.evalCase (\g' _env -> synthesiseG SelectedHole g')
                 (do bs <- SynthesisM $ gets seenBytes
                     env' <- SynthesisM $ asks synthValueEnv
                     panic "Case failed" [showPP g
                                         , show (sep $ map (\(k, v) -> pp k <+> "->" <+> pp v)
                                                 (Map.toList (assertInterpValue <$> env')))
                                         , show bs
                                         ])
                 c env
    Loop (ManyLoop _sem _bt lb m_ub body) -> do
      lv   <- synthesiseV lb
      m_uv <- traverse synthesiseV m_ub
      count <- synthesiseLoopBounds True lv m_uv
      vArray <$> replicateM count (synthesiseG SelectedHole body)

    -- FIXME: probably it is OK to do this, as if there are
    -- constraints on the iteration count we would figure it out in
    -- slicing.
    Loop (RepeatLoop _bt n e body) -> do
      -- FIXME: maybe we should have a separate policy for repeat
      count <- synthesiseLoopBounds True (vSize 0) Nothing
      initV <- synthesiseV e
      let go v _ = bindIn n v (synthesiseG SelectedHole body)
      foldlM go initV (replicate count ())

    Loop (MorphismLoop lm) -> synthesiseLoopMorphism Nothing Nothing lm
  where
    unimplemented = panic "Unimplemented" [showPP g]
    impossible    = panic "Impossible (theoretically)" [showPP g]

-- We didn't match any of the above, so this is a bug
synthesiseG _p _g = panic "Mismatched path/grammar in synthesiseG" []

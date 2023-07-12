{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language ExistentialQuantification #-}


module Talos.Synthesis (synthesise) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as BS
import           Data.Foldable                   (find)
import           Data.Functor.Identity           (Identity (Identity))
import           Data.List                       (foldl')
import qualified Data.List.NonEmpty              as NE
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Data.Word
import           SimpleSMT                       (Solver)
import           System.IO.Streams               (Generator, InputStream)
import qualified System.IO.Streams               as Streams
import           System.Random

import           Daedalus.Core                   hiding (streamOffset)
import           Daedalus.Core.Free
import qualified Daedalus.Core.Semantics.Env     as I
import qualified Daedalus.Core.Semantics.Expr    as I
import qualified Daedalus.Core.Semantics.Grammar as I
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic
import qualified Daedalus.Value                  as I
import           RTS.Input                       (newInput)
import           RTS.Parser                      (runParser)
import           RTS.ParserAPI                   (Result (..), ppParseError)

import           Talos.Analysis                  (summarise)
import           Talos.Analysis.Exported         (esRootSlices, SliceId)
import           Talos.Analysis.Merge            (merge)
import           Talos.Analysis.Slice
-- import Talos.SymExec
import           Talos.SymExec.Path
import           Talos.SymExec.SolverT           (emptySolverState)
import           Talos.SymExec.StdLib

import           Talos.Analysis.AbsEnv           (AbsEnvTy (AbsEnvTy))
import           Talos.Strategy
import           Talos.Strategy.Monad


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
                         }

addVal :: Name -> Value -> SynthEnv -> SynthEnv
addVal x v e = e { synthValueEnv = Map.insert x v (synthValueEnv e) }

-- addValMaybe :: Maybe (TCName K.Value) -> Value -> SynthEnv -> SynthEnv
-- addValMaybe Nothing  _ e = e
-- addValMaybe (Just x) v e = addVal x v e

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
  env0 <- getIEnv
  m_e <- SynthesisM $ asks (projectEnvFor tm env0)
  case m_e of
    Just e  -> pure e
    Nothing -> panic "Captured stream value" []

vUnit :: Value
vUnit = InterpValue I.vUnit

--------------------------------------------------------------------------------
-- Synthesis state

data SynthesisMState =
  SynthesisMState { seenBytes :: ByteString
                  , curStream :: Stream
                  , nextProvenance :: ProvenanceTag 
                  , provenances :: ProvenanceMap
                  , modelCache :: ModelCache
                  }

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

freshProvenanceTag :: SynthesisM ProvenanceTag
freshProvenanceTag = do 
  p <- SynthesisM $ gets nextProvenance
  SynthesisM $ modify (\s -> s { nextProvenance = p + 1 })
  return p

-- -----------------------------------------------------------------------------
-- Top level

synthesise :: Maybe Int -> GUID -> Solver -> AbsEnvTy -> [StrategyInstance] -> FName -> Module 
           -> IO (InputStream (I.Value, ByteString, ProvenanceMap))
synthesise m_seed nguid solv (AbsEnvTy p) strats root md = do
  let (allSummaries, nguid') = summarise p md nguid
  
  -- We do this in one giant step to deal with recursion and deps on
  -- pure functions.
  -- symExecSummaries md allSummaries

  -- let symExecSummary' fun
  --       | Just sm <- Map.lookup (fName fun) allSummaries =
  --         mapM_ (symExecSummary (fName fun)) (Map.elems sm)
  --       | otherwise = pure ()

  -- mapM_ symExecSummary' allDecls

  -- Generate a seed if none has been given to us.
  seed    <- maybe randomIO pure m_seed
  putStrLn ("Using random seed " ++ show seed)
  let gen = mkStdGen seed

  let sst0 = emptyStrategyMState gen allSummaries md nguid'
      solvSt0 = emptySolverState solv
      mc0 = newModelCache strats solvSt0
      
  -- Init solver stdlib
  -- FIXME: probably move?
  makeStdLib solv 

  Streams.fromGenerator (go sst0 mc0)
  
  where
    go :: StrategyMState -> ModelCache -> Generator (I.Value, ByteString, ProvenanceMap) ()
    go s0 mc = do
      ((a, s), sts) <- liftIO $ runStrategyM (runStateT (runReaderT (getSynthesisM once) env0)
                                               (initState mc)) s0
                       
      Streams.yield (assertInterpValue a, seenBytes s, provenances s)
      go sts (modelCache s)

    initState mc = 
      SynthesisMState { seenBytes      = mempty
                      , curStream      = emptyStream
                      , nextProvenance = firstSolverProvenance
                      , provenances    = Map.empty 
                      , modelCache     = mc
                      }
    
    Just rootDecl = find (\d -> fName d == root) allDecls

    -- Do the actual synthesis by calling the main function. The
    -- 'Assertions' is to tell the system that we don't care about the
    -- result, and so the summary we should use is the one for the
    -- 'Assertions' result class.
    --
    -- The 'Unconstrained' is the current path set --- we have not yet determined any future bytes.
    once = synthesiseCallG assertionsCI (fName rootDecl) []

    env0      = SynthEnv Map.empty Map.empty assertionsFID root

    -- FIXME: we assume topologically sorted (by reference)
    allDecls  = mGFuns md
    
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

-- -- Bounds on how many to generate (if none given)
-- minMany, maxMany :: Int
-- minMany = 0
-- maxMany = 100

-- -- Select a number of iterations
-- synthesiseManyBounds :: ManyBounds (TC TCSynthAnnot K.Value) -> SynthesisM Int
-- synthesiseManyBounds bnds =
--   case bnds of
--     Exactly v    -> getV v
--     Between l h ->  do
--       lv <- maybe (pure minMany) getV l
--       hv <- maybe (pure (maxMany + lv)) getV h
--       when (hv < lv) $ panic "Shouldn't happen" []
--       randR (lv, hv)
--   where
--     getV v = fromInteger . I.valueToInteger . assertInterpValue <$> synthesiseV v

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

synthesiseCallG :: CallInstantiation SelectedPath -> FName -> [Expr] -> SynthesisM Value
synthesiseCallG (CallInstantiation fid fp) n args = do
  -- liftIO $ printf "Calling into %s\n" (showPP n)
  decl <- getGFun n
  synthesiseDecl fp fid decl args

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
        Nothing -> panic "All strategies failed" []
        Just sp -> go (sp : acc) mc' sls
        
      -- -- We have a path starting at this node, so we need to call the
      -- -- corresponding SMT function and process any generated model.      
      -- s   <- SynthesisM $ gets solver
      -- cl  <- SynthesisM $ asks currentClass
      -- prov <- freshProvenanceTag 
      -- sp <- liftIO $ solverSynth s cl x prov sl
      -- let new = (merge cp sp)      
      -- -- liftIO $ print ("Got a path at " <> pp x $+$ pp sp $+$ pp new)
      -- pure new
      
-- --------------------------------------------------------------------------------
-- -- Simple Synthesis

-- -- E.g.
-- -- def Foo = {
-- --   x = UInt8;
-- --   y = { x < 10; ^ 0 } | { ^ 1 }
-- -- }


-- arrayFromList :: [Value] -> Value
-- arrayFromList = InterpValue . I.VArray . Vector.fromList . map assertInterpValue

-- matchPatOneOf :: [TCPat] -> I.Value -> Maybe [(Name,I.Value)]
-- matchPatOneOf ps v = msum [ matchPat p v | p <- ps ]

-- matchPat :: TCPat -> I.Value -> Maybe [(Name,I.Value)]
-- matchPat pat =
--   case pat of
--     TCConPat _ l p    -> \v -> case I.valueToUnion v of
--                                  (l1,v1) | l == l1 -> matchPat p v1
--                                  _ -> Nothing
--     TCNumPat _ i      -> \v -> do guard (I.valueToInteger v == i)
--                                   pure []
--     TCBoolPat b       -> \v -> do guard (I.valueToBool v == b)
--                                   pure []
--     TCJustPat p       -> \v -> case I.valueToMaybe v of
--                                  Nothing -> Nothing
--                                  Just v1 -> matchPat p v1
--     TCNothingPat {}   -> \v -> case I.valueToMaybe v of
--                                  Nothing -> Just []
--                                  Just _  -> Nothing
--     TCVarPat x        -> \v -> Just [(x,v)]
--     TCWildPat {}      -> \_ -> Just [

-- -- Does all the heavy lifting
synthesiseG :: SelectedPath -> Grammar -> SynthesisM Value
synthesiseG p (Annot _ g) = synthesiseG p g

-- This does all the work for internal slices etc.
synthesiseG cp (Do x lhs rhs) = do
  cp' <- choosePath cp x
  let (lhsp, rhsp) = splitPath cp' 
  v <- synthesiseG lhsp lhs
  bindIn x v (synthesiseG rhsp rhs)

-- We don't care about the result here, so we can never start an
-- internal slice here.
synthesiseG cp (Do_ lhs rhs) = do
  let (lhsp, rhsp) = splitPath cp  
  void $ synthesiseG lhsp lhs
  synthesiseG rhsp rhs

synthesiseG (SelectedBytes prov bs) g = do
  addBytes prov bs
  env <- projectEnvForM g

  let inp = newInput "<synthesised bytes>" bs
      res = case runParser (I.evalG g env) inp of
        NoResults e -> panic "No results" [ show (ppParseError e) ]
        Results  rs -> NE.head rs -- FIXME: this is maybe a little suspicious
      
  pure (InterpValue res)
      
synthesiseG (SelectedChoice (PathIndex n sp)) (Choice _biased gs)
  | n < length gs = synthesiseG sp (gs !! n)
  | otherwise     = panic "Index out of bounds" []

synthesiseG (SelectedCase (Identity sp)) (GCase cs) = do
  let v = Var (caseVar cs)
  env <- projectEnvForM v
  let base = panic "Failed to match pattern" [showPP cs]
  I.evalCase (\g _ -> synthesiseG sp g) base cs env 

synthesiseG (SelectedCall sc) (Call fn args) = synthesiseCallG sc fn args

synthesiseG p (Let x e rhs) = synthesiseG p (Do x (Pure e) rhs)
  
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
    
    Call fn args     -> synthesiseCallG assertionsCI fn args
    Annot {}         -> impossible
    GCase c@(Case y _) -> do
      env <- projectEnvForM y
      I.evalCase (\g' _env -> synthesiseG SelectedHole g')
                 (do bs <- SynthesisM $ gets seenBytes
                     panic "Case failed" [showPP g
                                         , show (sep $ map (\(k, v) -> pp k <+> "->" <+> pp v)
                                                 (Map.toList $ I.vEnv env))
                                         , show bs
                                         ])
                 c env

    -- TCOffset          -> InterpValue . I.VInteger <$> SynthesisM (gets (streamOffset . curStream))
  where
    unimplemented = panic "Unimplemented" [showPP g]
    impossible    = panic "Impossible (theoretically)" [showPP g]

-- We didn't match any of the above, so this is a bug
synthesiseG _p _g = panic "Mismatched path/grammar in synthesiseG" []

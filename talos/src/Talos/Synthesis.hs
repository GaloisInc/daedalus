{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Talos.Synthesis (synthesise) where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Word
import System.Random

import           System.IO.Streams (Generator, InputStream)
import qualified System.IO.Streams as Streams

-- To represent the partially constructed input
-- import qualified Data.IntervalMap as IMap
-- import Data.IntervalMap (IntervalMap)

import SimpleSMT (Solver)

import Daedalus.GUID
import Daedalus.PP
import Daedalus.Panic
import Daedalus.Rec (forgetRecs)

import Daedalus.Core hiding (streamOffset)
import Daedalus.Core.Free
import qualified Daedalus.Core.Semantics.Grammar as I
import qualified Daedalus.Core.Semantics.Expr as I
import qualified Daedalus.Core.Semantics.Decl as I
import qualified Daedalus.Core.Semantics.Value as I
import qualified Daedalus.Core.Semantics.Env as I

-- import RTS.ParserAPI hiding (SourceRange)

import Talos.Analysis (summarise)
import Talos.Analysis.Monad (Summaries, Summary(..))
import Talos.Analysis.Slice
import Talos.SymExec
import Talos.SymExec.Monad
import Talos.SymExec.Path

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

data SynthEnv = SynthEnv { synthInterpEnv :: I.Env
                         , synthValueEnv  :: Map Name Value
                         , pathSetRoots :: Map Name Slice
                         , currentClass :: SummaryClass                         
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

projectEnvFor :: FreeVars t => t -> SynthEnv -> Maybe I.Env
projectEnvFor tm se = doMerge (synthInterpEnv se)
                      <$> Map.traverseMaybeWithKey go (synthValueEnv se)
  where
    frees = freeVars tm

    doMerge e m = e { I.vEnv = Map.union m (I.vEnv e) } 
    
    go k v | k `Set.member` frees = Just <$> projectInterpValue v
    go _ _                        = Just Nothing

projectEnvForM :: FreeVars t => t -> SynthesisM I.Env
projectEnvForM tm = do
  m_e <- SynthesisM $ asks (projectEnvFor tm)
  e <- case m_e of
         Just e  -> pure e
         Nothing -> panic "Captured stream value" []
  pure e

vUnit :: Value
vUnit = InterpValue I.VUnit

--------------------------------------------------------------------------------
-- Synthesis state

data SynthesisMState =
  SynthesisMState { stdGen    :: StdGen
                  , seenBytes :: ByteString
                  , curStream :: Stream
                  -- , currentPath  :: SelectedPath
                  -- Read only
                  , solver :: Solver
                  , summaries :: Summaries
                  -- Only care about grammar rules
                  , rules  :: Map FName (Fun Grammar)
                  , nextProvenance :: ProvenanceTag 
                  , provenances :: ProvenanceMap 
                  }

newtype SynthesisM a =
  SynthesisM { getSynthesisM :: ReaderT SynthEnv (StateT SynthesisMState IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

addBytes :: ProvenanceTag -> ByteString -> SynthesisM ()
addBytes prov bs = SynthesisM $ 
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

-- FIXME: we don't deal with recursion including in Analysis
-- ScopedIdent here as it is easier to create
synthesise :: Maybe Int -> FName -> Module 
           -> SymExecM (InputStream (I.Value, ByteString, ProvenanceMap))
synthesise m_seed root md = withSolver $ \solv -> do
  mapM_ symExecTDecl orderedTys -- FIXME: filter by need

  allSummaries <- guidState (summarise allDecls)

  let symExecSummary' fun
        | Just sm <- Map.lookup (fName fun) allSummaries =
          mapM_ (symExecSummary (fName fun)) (Map.elems sm)
        | otherwise = pure ()

  mapM_ symExecSummary' allDecls
  
  gen <- maybe getStdGen (pure . mkStdGen) m_seed

  let initState gen' = 
        SynthesisMState { stdGen       = gen'
                        , seenBytes    = mempty
                        , curStream    = emptyStream
                        , solver       = solv
                        , summaries    = allSummaries
                        , rules        = rs
                        , nextProvenance = firstSolverProvenance
                        , provenances  = Map.empty 
                        }

      go :: StdGen -> Generator (I.Value, ByteString, ProvenanceMap) ()
      go gen' = do
        (a, s) <- liftIO $ runStateT (runReaderT (getSynthesisM once) env0) (initState gen')
        Streams.yield (assertInterpValue a, seenBytes s, provenances s)
        go (stdGen s)
  
  liftIO $ Streams.fromGenerator (go gen)
  
  where
    Just rootDecl = find (\d -> fName d == root) allDecls

    -- FIXME: figure out rec tys
    orderedTys = forgetRecs (mTypes md)

    once = synthesiseCallG Assertions Unconstrained (fName rootDecl) []

    env0      = SynthEnv (I.evalModule md I.emptyEnv) Map.empty Map.empty Assertions 

    -- FIXME: we assume topologically sorted (by reference)
    allDecls  = mGFuns md
    
    -- ns        = needsSolver allDecls
    rs     = Map.fromList [ (fName d, d) | d <- allDecls ]
  
-- -- -----------------------------------------------------------------------------
-- -- Random values

-- rand :: Random a => SynthesisM a
-- rand = SynthesisM $ state go
--   where
--     go s = let (b, g') = random (stdGen s) in (b, s { stdGen = g' })

randR :: Random a => (a, a) -> SynthesisM a
randR r = SynthesisM $ state go
  where
    go s = let (b, g') = randomR r (stdGen s) in (b, s { stdGen = g' })

randL :: [a] -> SynthesisM a
randL [] = panic "randL: empty list" []
randL vs = (!!) vs <$> randR (0, length vs - 1)

-- -- -----------------------------------------------------------------------------
-- -- Random bytes

-- getByte :: SynthesisM Value
-- getByte = do b <- rand
--              addByte randomProvenance b 
--              pure (InterpValue $  I.VUInt 8 (fromIntegral b))

-- We could also invoke the solver here.  This is a bit brute force
synthesiseByteSet :: ByteSet -> SynthesisM Value
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

synthesiseDecl :: SummaryClass -> SelectedPath -> Fun Grammar -> [Expr] -> SynthesisM Value
synthesiseDecl cl fp Fun { fDef = Def def, ..} args = do
  args' <- mapM synthesiseV args
  summary <- SynthesisM $ gets (flip (Map.!) cl . flip (Map.!) fName . summaries)
  let addPs e = foldl (\e' (k, v) -> addVal k v e') e (zip fParams args')
      setRoots e = e { pathSetRoots = pathRootMap summary }
      setClass e = e { currentClass = cl }
  SynthesisM $ local (setRoots . addPs . setClass) (getSynthesisM (synthesiseG fp def))

synthesiseDecl _ _ f _ = panic "Undefined function" [showPP (fName f)]

synthesiseCallG :: SummaryClass -> SelectedPath -> FName -> [Expr] -> SynthesisM Value
synthesiseCallG cl fp n args = do
  decl <- SynthesisM $ gets (flip (Map.!) n . rules)
  synthesiseDecl cl fp decl args

-- =============================================================================
-- Tricky Synthesis

--------------------------------------------------------------------------------
-- Selection

choosePath :: SelectedPath -> Name -> SynthesisM SelectedPath
choosePath cp x = do
  m_fps <- SynthesisM $ asks (Map.lookup x . pathSetRoots)
  case m_fps of
    Nothing  -> pure cp
    Just sl -> do
      -- We have a path starting at this node, so we need to call the
      -- corresponding SMT function and process any generated model.      
      s   <- SynthesisM $ gets solver
      cl  <- SynthesisM $ asks currentClass
      prov <- freshProvenanceTag 
      sp <- liftIO $ solverSynth s cl x prov sl
      -- liftIO $ print ("Got a path at " <> pp x $+$ pp sp)
      pure (mergeSelectedPath cp sp)
      
-- --------------------------------------------------------------------------------
-- -- Simple Synthesis

-- -- E.g.
-- -- def Foo = {
-- --   x = UInt8;
-- --   y = { x < 10; ^ 0 } | { ^ 1 }
-- -- }


-- arrayFromList :: [Value] -> Value
-- arrayFromList = InterpValue . I.VArray . Vector.fromList . map assertInterpValue

-- -- FIXME: next 3 copied from Interp.hs
-- -- We can use VUInt instead of mkUInt here b/c we are coming from Word8
byteStringToValue :: ByteString -> I.Value
byteStringToValue = I.VArray (TUInt (TSize 8)) . Vector.fromList . map I.vByte . BS.unpack

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
synthesiseGLHS :: Maybe SelectedNode -> Grammar -> SynthesisM Value
synthesiseGLHS (Just (SelectedMatch prov bs)) g@(Match SemYes m) = do
  addBytes prov bs
  
  -- We could reuse the interpreter, but there aren't that many cases
  case m of
    -- FIXME: check match?
    MatchByte {}  -> pure (InterpValue $ I.vByte (BS.head bs))
    MatchBytes {} -> pure (InterpValue $ byteStringToValue bs)
    _             -> panic "BUG: unexpected term in synthesiseGLHS" [showPP g]
    
synthesiseGLHS (Just (SelectedMatch {})) g = 
  panic "BUG: unexpected term in synthesiseGLHS/SelectedMatch" [showPP g]
  
synthesiseGLHS (Just (SelectedChoice n sp)) (Choice _biased gs) =
  synthesiseG sp (gs !! n)
  
synthesiseGLHS (Just (SelectedChoice {})) g = panic "synthesiseGLHS: expected a choose" [showPP g]

synthesiseGLHS (Just (SelectedCase n sp)) (GCase cs@(Case e alts))
  | n < length alts = do
      v <- synthesiseV e
      let (pat, g) = alts !! n
      if I.matches pat (assertInterpValue v) -- sanity check prover result
        then synthesiseG sp g
        else panic "Failed to match pattern" [show n, showPP cs]
  | otherwise = panic "No matching case" [show n, showPP cs]
  
synthesiseGLHS (Just (SelectedCase {})) g = panic "synthesiseGLHS: expected a case" [showPP g]

synthesiseGLHS (Just (SelectedCall cl sp)) (Call fn args) = synthesiseCallG cl sp fn args
  
synthesiseGLHS (Just (SelectedCall {})) tc = panic "synthesiseGLHS: expected a call" [showPP tc]
  
synthesiseGLHS Nothing g = -- Result of this is unentangled, so we can choose randomly
  case g of
    Pure e            -> synthesiseV e
    GetStream         -> unimplemented
    SetStream {}      -> unimplemented
    Match s (MatchByte bset) ->
      synthesiseByteSet bset >>= mbPure s
    Match s (MatchBytes v) -> do
      bs <- synthesiseV v
      -- prov <- freshProvenanceTag
      addBytes synthVProvenance (I.fromVByteArray (assertInterpValue bs)) -- XXX is this the random case?
      mbPure s bs
    Match _s _        -> unimplemented
    Fail {}           -> unimplemented -- probably should be impossible
    Do  {}            -> impossible
    Do_ {}            -> impossible
    Let {}            -> impossible
    Choice _biased gs -> do
      g' <- randL gs
      synthesiseG Unconstrained g'
    OrBiased {}      -> impossible
    OrUnbiased {}    -> impossible
    
    Call fn args     -> synthesiseCallG Assertions Unconstrained fn args
    Annot {}         -> impossible
    GCase c@(Case e _) -> do
      env <- projectEnvForM e
      I.evalCase (\g' _env -> synthesiseG Unconstrained g')
                 (panic "Case failed" [showPP g])
                 c env

    -- TCOffset          -> InterpValue . I.VInteger <$> SynthesisM (gets (streamOffset . curStream))
  where
    unimplemented = panic "Unimplemented" [showPP g]
    impossible    = panic "Impossible (theoretically)" [showPP g]
    
synthesiseG :: SelectedPath -> Grammar -> SynthesisM Value
synthesiseG cPath g = do
  case g of
    Do_ lhs rhs -> do
      -- no roots here
      let (n, cp') = splitPath cPath
      void $ synthesiseGLHS n lhs
      synthesiseG cp' rhs

    Do x lhs rhs -> do
      cp       <- choosePath cPath x
      let (n, cp') = splitPath cp
      v <- synthesiseGLHS n lhs
      bindIn x v (synthesiseG cp' rhs)

    Let x e g' -> do
      v <- synthesiseV e
      bindIn x v (synthesiseG cPath g')
      
    Annot _ g'       -> synthesiseG cPath g'
    
    -- leaf case
    _lhs            -> synthesiseGLHS (fst (splitPath cPath)) g

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Talos.Synthesis (synthesise) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Data.Foldable (find)
import Data.Word
import Data.Maybe (isJust,fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe (catMaybes)

import System.Random
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE

import qualified Data.Vector as Vector

import           System.IO.Streams (Generator, InputStream)
import qualified System.IO.Streams as Streams

import Data.Parameterized.Some

-- To represent the partially constructed input
import qualified Data.IntervalMap as IMap
import Data.IntervalMap (IntervalMap)

import SimpleSMT (Solver)

import Daedalus.Panic
import Daedalus.PP
import Daedalus.GUID
import qualified Daedalus.AST as K
import Daedalus.Type.AST hiding (Value)
import Daedalus.Type.Free (TCFree(..))

import RTS.ParserAPI hiding (SourceRange)

import qualified Daedalus.Interp as I
import qualified Daedalus.Interp.Value as I

import Daedalus.Rec (forgetRecs, topoOrder)
-- FIXME: roll the topoOrder of type decls into Daedalus
import Daedalus.Type.Traverse (collectTypes)
import Daedalus.Type.Subst    (freeTCons)

import Talos.SymExec
import Talos.SymExec.ModelParser
import Talos.Lib

import Talos.Analysis.Annot
import Talos.Analysis.Monad (Summaries, Summary(..))
import Talos.Analysis.Domain
import Talos.Analysis.PathSet
import Talos.Analysis (summarise)

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
                         , synthValueEnv  :: Map (TCName K.Value) Value
                         , pathSetRoots :: Map (TCName K.Value) (FuturePathSet TCSynthAnnot)
                         , currentClass :: SummaryClass                         
                         }

addVal :: TCName K.Value -> Value -> SynthEnv -> SynthEnv
addVal x v e = e { synthValueEnv = Map.insert x v (synthValueEnv e) }

addValMaybe :: Maybe (TCName K.Value) -> Value -> SynthEnv -> SynthEnv
addValMaybe Nothing  _ e = e
addValMaybe (Just x) v e = addVal x v e

projectInterpValue :: Value -> Maybe I.Value
projectInterpValue (InterpValue v) = Just v
projectInterpValue _               = Nothing

-- Basically a hack
assertInterpValue :: Value -> I.Value
assertInterpValue (InterpValue v) = v
assertInterpValue _               = error "Expecting an InterpValue, got a StreamValue"

-- Projects out the I.Value for each free varable, returns Nothing if we see a non-InterpValue
projectEnv :: SynthEnv -> Maybe (Map (TCName K.Value) I.Value)
projectEnv se = traverse projectInterpValue (synthValueEnv se)

projectEnvFor :: TCFree t => t -> SynthEnv -> Maybe I.Env
projectEnvFor tm se = flip I.setVals (synthInterpEnv se)
                      . Map.mapKeys tcName
                      <$> Map.traverseMaybeWithKey go (synthValueEnv se)
  where
    frees = Set.map (\(Some t) -> tcName t) (Set.filter nameIsValue (tcFree tm))
    
    nameIsValue :: Some TCName -> Bool
    nameIsValue (Some TCName { tcNameCtx = AValue }) = True
    nameIsValue _                                    = False
    
    go k v | tcName k `Set.member` frees = Just <$> projectInterpValue v
    go _ _                               = Just Nothing

vUnit :: Value
vUnit = InterpValue I.vUnit

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
                  , rules  :: Map Name (TCDecl TCSynthAnnot)
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

    updProvenances strm provs = 
      let off = fromIntegral $ streamOffset strm 
          len = fromIntegral $ BS.length bs
      in 
        Map.insertWith Set.union prov (Set.singleton (off,len)) provs 

addByte :: ProvenanceTag -> Word8 -> SynthesisM ()
addByte prov word = addBytes prov (BS.singleton word)

-- currentNode :: SynthesisM (Maybe SelectedNode')
-- currentNode = SynthesisM $ state go
--   where go s = let (n, ps') = splitPath (currentPath s)
--                in (n, s { currentPath = ps' })

bindIn :: TCName K.Value -> Value -> SynthesisM a -> SynthesisM a
bindIn x v = SynthesisM . local (addVal x v) . getSynthesisM 

bindInMaybe :: Maybe (TCName K.Value) -> Value -> SynthesisM a -> SynthesisM a
bindInMaybe Nothing  _ m = m
bindInMaybe (Just x) v m = bindIn x v m

freshProvenanceTag :: SynthesisM ProvenanceTag
freshProvenanceTag = do 
  p <- SynthesisM $ gets nextProvenance
  SynthesisM $ modify (\s -> s { nextProvenance = p + 1 })
  return p

-- -----------------------------------------------------------------------------
-- Top level

-- FIXME: we don't deal with recursion including in Analysis
-- ScopedIdent here as it is easier to create
synthesise :: Solver -> Maybe Int -> ScopedIdent
           -> Map TCTyName TCTyDecl
           -> [TCModule TCSynthAnnot]
           -> GUID
           -> IO (InputStream (I.Value, ByteString, ProvenanceMap))
synthesise solv m_seed root declTys mods nguid = do
  -- Send the needSolver decls to the solver
  -- print (fmap pp (Set.toList ns))
  -- mapM_ (print . pp) orderedTys
  mapM_ (symExecTyDecl solv) orderedTys -- FIXME: filter by need

  let symExecSummary' decl
        | Just sm <- Map.lookup (tcDeclName decl) allSummaries =
          mapM_ (symExecSummary solv) (Map.elems sm)
        | otherwise = pure ()

  mapM_  symExecSummary' allDecls
  
  gen <- maybe getStdGen (pure . mkStdGen) m_seed
  Streams.fromGenerator (go gen)
  where
    go :: StdGen -> Generator (I.Value, ByteString, ProvenanceMap) ()
    go gen = do
      (a, s) <- liftIO $ runStateT (runReaderT (getSynthesisM once) env0) (initState gen)
      Streams.yield (assertInterpValue a, seenBytes s, provenances s)
      go (stdGen s)

    Just rootDecl = find (\d -> nameScopedIdent (tcDeclName d) == root) allDecls

    -- FIXME: figure out rec tys
    unorderedTys = Map.elems declTys
    orderedTys = forgetRecs (topoOrder tyOrd unorderedTys)
    tyOrd d = (tctyName d, collectTypes freeTCons (tctyDef d))

    once = synthesiseCallG Assertions Unconstrained (tcDeclName rootDecl) []

    env0      = SynthEnv (I.compile [] mods) Map.empty Map.empty Assertions 
    allSummaries = summarise declTys [tcDeclName rootDecl] allDecls nguid
    
    initState gen = 
      SynthesisMState { stdGen       = gen
                      , seenBytes    = mempty
                      , curStream    = emptyStream
                      , solver       = solv
                      -- , currentPath  = Unconstrained
                      , summaries    = allSummaries
                      , rules        = rs
                      , nextProvenance = firstSolverProvenance
                      , provenances  = Map.empty 
                      }

    -- Topologically sorted (by reference)
    allDecls  = concatMap (forgetRecs . tcModuleDecls) mods
    -- ns        = needsSolver allDecls
    rs     = Map.fromList [ (tcDeclName d, d) | d <- allDecls ]
  
-- -----------------------------------------------------------------------------
-- Random values

rand :: Random a => SynthesisM a
rand = SynthesisM $ state go
  where
    go s = let (b, g') = random (stdGen s) in (b, s { stdGen = g' })

randR :: Random a => (a, a) -> SynthesisM a
randR r = SynthesisM $ state go
  where
    go s = let (b, g') = randomR r (stdGen s) in (b, s { stdGen = g' })

randL :: [a] -> SynthesisM a
randL [] = error "randL: empty list"
randL vs = (!!) vs <$> randR (0, length vs - 1)

-- -----------------------------------------------------------------------------
-- Random bytes

getByte :: SynthesisM Value
getByte = do b <- rand
             addByte randomProvenance b 
             pure (InterpValue $  I.VUInt 8 (fromIntegral b))

-- We could also invoke the solver here.  This is a bit brute force
synthesiseP :: TC TCSynthAnnot K.Class -> SynthesisM Value
synthesiseP tc = do
  m_e <- SynthesisM $ asks (projectEnvFor tc)
  e <- case m_e of
         Just e  -> pure e
         Nothing -> error "synthesiseP: captured stream value"
  
  let ClassVal p _ = I.compilePredicateExpr e tc
      bs           = filter p [0 .. 255] -- FIXME!!
  when (bs == []) $ error "Empty predicate"
  b <- randL bs
  prov <- freshProvenanceTag
  addByte prov b 
  pure (InterpValue $ I.VUInt 8 (fromIntegral b))

synthesiseV :: TC TCSynthAnnot K.Value -> SynthesisM Value
synthesiseV v = do m_e <- SynthesisM $ asks (projectEnvFor v)
                   case m_e of
                     Just e -> pure (InterpValue $ I.compilePureExpr e v)
                     Nothing -> error "synthesiseV: captured stream value"

{-# NOINLINE mbPure #-}
mbPure :: WithSem -> Value -> SynthesisM Value
mbPure NoSem _ = pure vUnit
mbPure _     v = pure v

-- Bounds on how many to generate (if none given)
minMany, maxMany :: Int
minMany = 0
maxMany = 100

-- Select a number of iterations
-- synthesiseManyBounds :: ManyBounds (TC TCSynthAnnot K.Value) -> SynthesisM Int
-- synthesiseManyBounds bnds =
--   case bnds of
--     Exactly v    -> getV v
--     Between l h ->  do
--       lv <- maybe (pure minMany) getV l
--       hv <- maybe (pure (maxMany + lv)) getV h
--       when (hv < lv) $ error "Shouldn't happen"
--       randR (lv, hv)
--   where
--     getV v = fromInteger . I.valueToInteger . assertInterpValue <$> synthesiseV v

synthesiseArg :: Arg TCSynthAnnot -> SynthesisM Value
synthesiseArg (ValArg v) = synthesiseV v
synthesiseArg _a         = error "Shoudn't happen: synthesiseArg nonValue"

synthesiseDecl :: SummaryClass -> SelectedPath -> TCDecl TCSynthAnnot -> [Arg TCSynthAnnot] -> SynthesisM Value
synthesiseDecl cl fp TCDecl { tcDeclCtxt   = AGrammar, tcDeclDef = Defined def, ..} args = do
  args' <- mapM synthesiseArg args
  summary <- SynthesisM $ gets (flip (Map.!) cl . flip (Map.!) tcDeclName . summaries)
  let addPs e = foldl (\e' (k, v) -> addVal k v e') e (zip (map stripParam tcDeclParams) args')
      setRoots e = e { pathSetRoots = pathRootMap summary }
      setClass e = e { currentClass = cl }
  SynthesisM $ local (setRoots . addPs . setClass) (getSynthesisM (synthesiseG fp def))

synthesiseDecl _ _ _ _ = error "Not a grammar"

-- -- We need to invoke the solver
-- symExecCall :: Name -> Type -> [Arg SourceRange] -> SynthesisM Value
-- symExecCall n typ args = do
--   args' <- mapM (fmap assertInterpValue . synthesiseArg) args
--   let tys = map typeOf args
--   s <- gets solver
--   offset <- gets (streamOffset . curStream)

--   bs <- liftIO $ callG s offset n (typeOfStripG typ) tys args'
--   addBytes bs
--   -- We have the underlying bytes, now we need to get the resulting
--   -- value.  For this we can just use the interpreter

--   e <- asks synthInterpEnv -- Don't need to add in local variables
--   case I.interpCompiled bs e (nameScope n) args' of
--     Results (v :| _) -> pure (InterpValue v)
--     NoResults err    -> error ("No results for " ++ show (pp n) ++ ": "
--                                 ++ show (ppParseError err) ++ "\nInput: " ++ show bs)

synthesiseCallG :: SummaryClass -> SelectedPath -> Name -> [Arg TCSynthAnnot] -> SynthesisM Value
synthesiseCallG cl fp n args = do
  decl <- SynthesisM $ gets (flip (Map.!) n . rules)
  synthesiseDecl cl fp decl args

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- =============================================================================
-- Tricky Synthesis

--------------------------------------------------------------------------------
-- Selection

-- It is easier to think about these when implemented separately, but
-- it means multiple traversals, so they could be merged into a single
-- function.

-- Make sure any path choices made are respected
-- c.f. mergePathSet

-- FIXME: we could avoid this pass by labelling each choice node and
-- then keeping track of which choice was made, and then looking up
-- that choice in selectPathNode
-- refineFuturePaths :: SelectedPath -> FuturePathSet a -> FuturePathSet a
-- refineFuturePaths = go
--   where
--     go psL psR = 
--       case (psL, psR) of
--         (Unconstrained, _)     -> psR
--         (_, Unconstrained)     -> Unconstrained

--         -- probably should't happen
--         (DontCare 0 rest, _)   -> go rest psR
--         (_, DontCare 0 rest)   -> go psL rest
        
--         (DontCare n rest, DontCare m rest') ->
--           let count = min m n
--           in dontCare count (go (dontCare (n - count) rest) (dontCare (m - count) rest'))

--         (DontCare  n rest, PathNode pn ps') -> PathNode pn (go (dontCare (n - 1) rest) ps')
--         (PathNode _pn ps', DontCare n rest) -> dontCare (n - 1) (go ps' rest)

--         (PathNode n1 ps1, PathNode n2 ps2) ->
--           PathNode (comb n1 n2) (go ps1 ps2)

--     comb :: SelectedNode' -> FutureNode a -> FutureNode a
--     comb (FullySelected {}) _ = error "BUG? we care about a node that has already been determined"
--     comb (PartialChoice n cp) (Choice _ fps) = error "FIXME" -- Choice (zipWith (comb1 cp n) [0..] fps)
--     comb (CallWrapper cp) (Call fps)       = Call (go cp fps)
--     comb _          n          = n -- we figure this out later

--     comb1 cp n = \idx fp -> if idx == n then (go cp) <$> fp else Nothing

choosePath :: SelectedPath -> Maybe (TCName K.Value) -> SynthesisM SelectedPath
choosePath cp Nothing = pure cp
choosePath cp (Just x) = do
  m_fps <- SynthesisM $ asks (Map.lookup x . pathSetRoots)
  case m_fps of
    Nothing  -> pure cp
    Just fps -> do
      -- We have a path starting at this node, so we need to call the
      -- corresponding SMT function and process any generated model.      
      s   <- SynthesisM $ gets solver
      cl  <- SynthesisM $ asks currentClass
      prov <- freshProvenanceTag 
      sp <- liftIO $ solverSynth s cl x prov fps -- FIXME: class
      -- liftIO $ print ("Got a path at " <> pp x $+$ pp sp)
      pure (mergeSelectedPath cp sp)
      
--------------------------------------------------------------------------------
-- Simple Synthesis

futurePathsMaybe :: Maybe (TCName K.Value) -> SynthesisM (FuturePathSet TCSynthAnnot)
futurePathsMaybe Nothing  = pure Unconstrained
futurePathsMaybe (Just x) = SynthesisM $ asks (Map.findWithDefault Unconstrained x . pathSetRoots)

-- E.g.
-- def Foo = {
--   x = UInt8;
--   y = { x < 10; ^ 0 } | { ^ 1 }
-- }

-- FIXME: next 3 copied from Interp.hs
-- We can use VUInt instead of mkUInt here b/c we are coming from Word8
byteStringToValue :: ByteString -> I.Value
byteStringToValue = I.VArray . Vector.fromList . map (I.VUInt 8 . fromIntegral) . BS.unpack

matchPatOneOf :: [TCPat] -> I.Value -> Maybe [(TCName K.Value,I.Value)]
matchPatOneOf ps v = msum [ matchPat p v | p <- ps ]

matchPat :: TCPat -> I.Value -> Maybe [(TCName K.Value,I.Value)]
matchPat pat =
  case pat of
    TCConPat _ l p    -> \v -> case I.valueToUnion v of
                                 (l1,v1) | l == l1 -> matchPat p v1
                                 _ -> Nothing
    TCNumPat _ i      -> \v -> do guard (I.valueToInteger v == i)
                                  pure []
    TCBoolPat b       -> \v -> do guard (I.valueToBool v == b)
                                  pure []
    TCJustPat p       -> \v -> case I.valueToMaybe v of
                                 Nothing -> Nothing
                                 Just v1 -> matchPat p v1
    TCNothingPat {}   -> \v -> case I.valueToMaybe v of
                                 Nothing -> Just []
                                 Just _  -> Nothing
    TCVarPat x        -> \v -> Just [(x,v)]
    TCWildPat {}      -> \_ -> Just []

-- Does all the heavy lifting
synthesiseGLHS :: Maybe SelectedNode -> TC TCSynthAnnot Grammar -> SynthesisM Value
synthesiseGLHS (Just (SelectedSimple prov bs)) tc = do
  addBytes prov bs
  -- We could reuse the interpreter, but there aren't that many simple tcs
  case texprValue tc of
    TCPure v         -> synthesiseV v
    TCGetByte {}     -> pure (InterpValue $ I.VUInt 8 (fromIntegral $ BS.head bs))
    TCMatch   {}     -> pure (InterpValue $ I.VUInt 8 (fromIntegral $ BS.head bs)) -- We don't check p
    TCMatchBytes {}  -> pure (InterpValue $ byteStringToValue bs)
    TCCoerceCheck NoSem _ _ _ -> pure vUnit
    TCCoerceCheck YesSem _ t e -> do
      v <- assertInterpValue <$> synthesiseV e
      case I.doCoerceTo (I.evalType I.emptyEnv t) v of
        (v, NotLossy) -> pure (InterpValue v)
        _ -> panic "BUG: unexpected term in synthesiseGLHS" [show (pp tc)]
        
    _ -> panic "BUG: unexpected term in synthesiseGLHS" [show (pp tc)]
  
synthesiseGLHS (Just (SelectedChoice n sp)) (texprValue -> TCChoice _c gs _t) =
  synthesiseG sp (gs !! n)
synthesiseGLHS (Just (SelectedChoice {})) tc = panic "synthesiseGLHS: expected a choose" [showPP tc]

synthesiseGLHS (Just (SelectedCase n sp)) tc@(texprValue -> TCCase e alts m_def)
  | n < length alts = do
      v <- synthesiseV e
      let alt = (alts NE.!! n)
          bindIn' (x, v') = bindIn x (InterpValue v')
      case matchPatOneOf (tcAltPatterns alt) (assertInterpValue v) of
        Just binds -> foldr bindIn' (synthesiseG sp (tcAltBody alt)) binds
        Nothing -> panic "Failed to match pattern" [show n, showPP tc]
  | Just def <- m_def = synthesiseG sp def -- FIXME: check we fail the other pats?
  | otherwise = panic "No matching case" [show n, showPP tc]
  
synthesiseGLHS (Just (SelectedCase {})) tc = panic "synthesiseGLHS: expected a case" [showPP tc]

synthesiseGLHS (Just (SelectedCall cl sp)) (texprValue -> TCCall fn _ args) =
  synthesiseCallG cl sp (tcName fn) args
  
synthesiseGLHS (Just (SelectedCall {})) tc = panic "synthesiseGLHS: expected a call" [showPP tc]

synthesiseGLHS (Just (SelectedNested sp)) tc =
  synthesiseG sp tc -- ???
  
synthesiseGLHS Nothing tc = -- We don't really care
  case texprValue tc of 
    TCDo {}           -> synthesiseG Unconstrained tc -- probably doesn't happen?
    TCPure v          -> synthesiseV v
    TCGetByte ws      -> getByte >>= mbPure ws
    TCMatch ws p      -> synthesiseP p >>= mbPure ws

    TCGuard b         -> do
      r <- synthesiseV b
      when (not (I.valueToBool (assertInterpValue r))) $
        panic "Got a False guard" [showPP tc]
      pure vUnit
    TCMatchBytes ws v -> do
      bs <- synthesiseV v
      prov <- freshProvenanceTag
      addBytes synthVProvenance (I.valueToByteString (assertInterpValue bs)) -- XXX is this the random case?
      mbPure ws bs

    -- we are ignoring commit here
    TCChoice _c gs _t -> do
      g <- randL gs
      synthesiseG Unconstrained g

    TCOptional _c g    -> unimplemented -- do
      -- b <- rand
      -- if b
      --   then InterpValue . I.VMaybe . Just . assertInterpValue <$> synthesiseG g
      --   else pure (InterpValue (I.VMaybe Nothing))

    -- FIXME: commit value here
    TCMany ws _c bnds p -> unimplemented -- do
      -- count <- synthesiseManyBounds bnds
      -- v <- (InterpValue . I.VArray . Vector.fromList . map assertInterpValue)
      --      <$> replicateM count (synthesiseG p)
      -- mbPure ws v

    TCEnd             -> pure vUnit -- FIXME
    TCOffset          -> InterpValue . I.VInteger <$> SynthesisM (gets (streamOffset . curStream))

    TCCurrentStream {} -> unimplemented
    TCSetStream     {} -> unimplemented
    TCStreamLen     {} -> unimplemented
    TCStreamOff     {} -> unimplemented

    -- Maps
    TCMapLookup     {} -> unimplemented
    TCMapInsert     {} -> unimplemented

    -- Array operations
    TCArrayIndex    {} -> unimplemented

    -- coercion
    -- FIXME: copied from the interpreter
    TCCoerceCheck NoSem _ _ _ -> pure vUnit
    TCCoerceCheck YesSem _ t e -> do
      v <- assertInterpValue <$> synthesiseV e
      case I.doCoerceTo (I.evalType I.emptyEnv t) v of
        (v', NotLossy) -> pure (InterpValue v')
        _ -> panic "BUG: unexpected term in synthesiseGLHS" [show (pp tc)]

    -- destructors
    TCFor           {} -> unimplemented

    TCVar           {} -> error "Saw a grammar-valued variable"

    -- Should be no type args
    TCCall n _ args    -> synthesiseCallG Assertions Unconstrained (tcName n) args
    TCErrorMode     {} -> unimplemented
    TCFail {}          -> unimplemented
    _ -> unimplemented
  where
    unimplemented = error ("Unimplemented: " ++ show (pp tc))

synthesiseG :: SelectedPath -> TC TCSynthAnnot Grammar -> SynthesisM Value
synthesiseG cPath tc = do
  case texprValue tc of
    TCDo m_x lhs rhs -> do
      cp       <- choosePath cPath m_x
      let (n, cp') = splitPath cp
      v <- synthesiseGLHS n lhs
      bindInMaybe m_x v (synthesiseG cp' rhs)
      
    TCLabel _ g       -> synthesiseG cPath g
    _lhs              -> synthesiseGLHS (fst (splitPath cPath)) tc

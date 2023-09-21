{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language OverloadedLabels #-}
{-# Language BlockArguments #-}

-- Symbolic but the only non-symbolic path choices are those in
-- recursive functions (i.e., we only unroll loops).

-- FIXME: factor out commonalities with Symbolic.hs
module Talos.Strategy.PathSymbolic (pathSymbolicStrat) where


import           Control.Lens                            (_2, imap, over,
                                                          preview)
import           Control.Monad                           (join, mapAndUnzipM, unless)
import           Control.Monad.Reader                    (asks)
import           Data.Bifunctor                          (second)
import           Data.Foldable                           (toList, traverse_)
import           Data.Generics.Product                   (field)
import           Data.List.NonEmpty                      (NonEmpty)
import qualified Data.Map                                as Map
import           Data.Maybe                              (fromMaybe, isNothing)
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           GHC.Generics                            (Generic)
import qualified SimpleSMT                               as S
import           Text.Printf                             (printf)

import           Daedalus.Core                           hiding (streamOffset,
                                                          tByte)
import qualified Daedalus.Core                           as Core
import           Daedalus.Core.Free                      (freeVars)
import           Daedalus.Core.Type                      (sizeType, typeOf)
import           Daedalus.Panic                          (panic)
import           Daedalus.PP                             (showPP)
import           Daedalus.Rec                            (topoOrder)

import           Talos.Analysis.Exported                 (ExpCallNode (..),
                                                          ExpSlice, SliceId,
                                                          sliceToCallees)
import           Talos.Analysis.Slice
import qualified Talos.Monad                             as T

import           Talos.Lib                               (tByte)
import           Talos.Path
import           Talos.Solver.SolverT                    (contextSize,
                                                          declareName,
                                                          declareSymbol,
                                                          liftSolver, reset,
                                                          scoped)
import           Talos.Strategy.Monad
import qualified Talos.Strategy.OptParser                as P
import           Talos.Strategy.OptParser                (Opt, parseOpts)
import qualified Talos.Strategy.PathSymbolic.Branching   as B
import           Talos.Strategy.PathSymbolic.Monad
import qualified Talos.Strategy.PathSymbolic.MuxValue    as MV
import           Talos.Strategy.PathSymbolic.MuxValue    (MuxValue,
                                                          VSequenceMeta (..))
import           Talos.Strategy.PathSymbolic.PathBuilder (buildPaths)
import qualified Talos.Strategy.PathSymbolic.PathSet     as PS
import qualified Talos.Strategy.PathSymbolic.SymExec     as SE
import Talos.Monad (debug)

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

pathSymbolicStrat :: Strategy
pathSymbolicStrat = Strategy
  { stratName  = name
  , stratDescr = descr
  , stratParse = mkinst
  }
  where
    name  = "pathsymb"
    descr = "Symbolic execution including non-recursive paths"

    mkinst = do
      c <- parseOpts configOpts defaultConfig
      pure StrategyInstance
        { siName  = name
        , siDescr = descr -- <> parens (text s)
        , siFun   = symbolicFun c
        }

-- ----------------------------------------------------------------------------------------
-- Configuration

data Config = Config
  { cMaxRecDepth :: Int
  , cNModels :: Int
  , cMaxUnsat :: Maybe Int
  , cNLoopElements :: Int
  } deriving (Generic)

defaultConfig :: Config
defaultConfig = Config
  { cMaxRecDepth = 10
  , cNModels = 1
  , cMaxUnsat = Nothing
  , cNLoopElements = 10
  }

configOpts :: [Opt Config]
configOpts = [ P.option "max-depth"  (field @"cMaxRecDepth") P.intP
             , P.option "num-models" (field @"cNModels")     P.intP
             , P.option "max-unsat"  (field @"cMaxUnsat")    (Just <$> P.intP)
             , P.option "num-loop-elements" (field @"cNLoopElements") P.intP
             ]

-- ----------------------------------------------------------------------------------------
-- Main functions

-- FIXME: define all types etc. eagerly
symbolicFun :: Config ->
               ProvenanceTag ->
               ExpSlice ->
               StratGen
symbolicFun config ptag sl = StratGen $ do
  -- defined referenced types/functions
  reset -- FIXME

  deps <- sliceToDeps sl

  -- FIXME: this should be calculated once, along with how it is used
  -- by e.g. memoSearch
  scoped $ do
    let topoDeps = topoOrder (second sliceToCallees) deps
    (m_res_sm) <- runSymbolicM (sl, topoDeps) (cMaxRecDepth config) (cNLoopElements config) ptag (stratSlice sl)
    sz <- contextSize
    T.statS (pathKey <> "modelsize") sz
    
    let go ((_, pb), sm) = do
          debug pathKey ("PathBuilder " ++ showPP pb)
          rs <- buildPaths (cNModels config) (cMaxUnsat config) sm pb
          T.info pathKey $ printf "Generated %d models" (length rs)
          pure (rs, Nothing) -- FIXME: return a generator here.

    case m_res_sm of
      Left _   -> pure ([], Nothing)
      Right rs -> go rs

-- We need to get types etc for called slices (including root slice)
sliceToDeps :: (Monad m, LiftStrategyM m) => ExpSlice -> m [(SliceId, ExpSlice)]
sliceToDeps sl = go Set.empty (sliceToCallees sl) []
  where
    go seen new acc
      | Just (n, rest) <- Set.minView new = do
          sl' <- getSlice n
          let new' = sliceToCallees sl' `Set.difference` seen
          go (Set.insert n seen) (new' `Set.union` rest) ((n, sl') : acc)

    go _ _ acc = pure acc

-- We return the symbolic value (which may contain bound variables, so
-- those have to be all at the top level) and a computation for
-- constructing the path --- we do it in this way to avoid doing a lot
-- of work that gets thrown away on backtracking, so this gets run
-- once in the final (satisfying) state.  Note that the path
-- construction code shouldn't backtrack, so it could be in (SolverT IO)

stratSlice :: ExpSlice -> SymbolicM Result
stratSlice = go
  where
    go sl =  do
      -- liftIO (putStrLn "Slice" >> print (pp sl))
      case sl of
        SHole -> pure (MV.VUnit, SelectedHole)

        SPure e -> do
          let mk v = (v, SelectedHole)
          mk <$> synthesiseExpr e

        SDo m_x lsl rsl -> do
          let goL = noteCurrentName (fromMaybe "_" (nameText =<< m_x)) (go lsl)

              goR :: PathBuilder -> SymbolicM Result
              goR lpath = over _2 (SelectedDo lpath) <$> go rsl

          bindNameInMaybe m_x goL goR

        -- FIXME: we could pick concrete values here if we are willing
        -- to branch and have a concrete bset.
        SMatch bset -> do
          n <- makeNicerSym "b"
          sym <- liftSolver (declareSymbol n tByte)
          recordValue TByte sym

          let bv  = MV.vSymbolicInteger TByte sym

          bassn <- synthesiseByteSet bset bv
          -- This just constrains the byte, we expect it to be satisfiable
          -- (byte sets are typically not empty)
          assertSExpr bassn
          
          -- liftSolver check -- required?
          ptag <- asks sProvenance
          pure (bv, SelectedBytes ptag (ByteResult sym))

        SChoice sls -> stratChoice sls Nothing --  =<< asks sCurrentSCC

        SCall cn -> stratCallNode cn

        SCase total c -> stratCase total c Nothing -- =<< asks sCurrentSCC

        SLoop lc -> stratLoop lc

        SInverse n ifn p -> do
          let ty = typeOf n

          -- FIXME: only works on prim types
          sym <- liftSolver (declareName n (SE.symExecTy ty))
          recordValue ty sym
          let n' =  MV.vSymbolicInteger (typeOf n) sym

          primBindName n n' $ do
            pe <- synthesiseExpr p
            assertSExpr (MV.toSExpr pe)

            -- Once we have a model, we need to convert all the free
            -- variables in the inverse function expression into values,
            -- and then call the inverse function in the _interpreter_.
            -- Note that n is free in ifn.
            -- 
            -- We could also have the solver execute the function for
            -- us.
            --
            -- We need to resolve any locally bound variables before we
            -- return the path constructor.

            let fvM = Map.fromSet id $ freeVars ifn
            -- Resolve all Names (to semisexprs and solver names)
            venv <- traverse getName fvM
            ptag <- asks sProvenance
            pure (n', SelectedBytes ptag (InverseResult venv ifn))
    
stratChoice :: [ExpSlice] -> Maybe (Set SliceId) -> SymbolicM Result
-- stratChoice ptag sls (Just sccs)
--   | any hasRecCall sls = do
--       -- Just pick randomly if there is recursion.
--       (i, sl) <- randL (enumerate sls)
--       over _2 (SelectedChoice . ConcreteChoice i) <$> stratSlice ptag sl
--   where
--     hasRecCall sl = not (sliceToCallees sl `Set.disjoint` sccs)
stratChoice sls _
  | null sls = unreachable
  | otherwise = do
      pv <- freshPathVar (length sls)

      let mk i sl = (PS.choiceConstraint pv i, over _2 ((,) i) <$> stratSlice sl)
      b <- branching $ B.branching $ imap mk sls
      v <- liftSemiSolverM (MV.mux (fst <$> b))
      
      let paths = map snd (toList b)
          feasibleIxs = map fst paths
      
      -- Record that we have this choice variable, and the possibilities
      recordChoice pv feasibleIxs

      pure (v, SelectedChoice (SymbolicChoice pv paths))

stratLoop :: SLoopClass Expr ExpSlice ->
             SymbolicM Result
stratLoop lclass =
  case lclass of
    SLoopPool sem b -> do
      ltag <- freshSymbolicLoopTag
      let vsm = VSequenceMeta { vsmGeneratorTag = Just ltag
                              , vsmLoopCountVar = Nothing
                              , vsmMinLength    = 1 -- FIXME: shouldn't matter?
                              , vsmIsBuilder    = False
                              }

      -- No need to constrain the processing of the body.
      (v, m) <- stratSlice b

      let xs = MV.VSequence (B.singleton (vsm, [v]))
          v' = case sem of
                 SemNo  -> MV.VUnit
                 SemYes -> xs
                 
          node = PathLoopGenerator ltag Nothing m

      pure (v', SelectedLoop node)

    -- Should be SLoopPool
    SManyLoop StructureIndependent _lb _m_ub _b ->
      panic "BUG: saw SManyLoop StructureIndependent" []

    SManyLoop StructureIsNull lb m_ub b -> do
      ltag <- freshSymbolicLoopTag
      -- Here we can assert that the list is empty or non-empty (i.e.,
      -- size 1 to allow pooling).  Note that the _length_ of the
      -- _final list_ is not important to this slice, just
      -- whether it is null/non-null
      lv <- freshLoopCountVar 0 1

      -- Bounds check: the null case is only allowed if lb is 0, the non-null case if m_ub /= 0      
      -- Construct return value
      let vsm = VSequenceMeta { vsmGeneratorTag = Just ltag
                              , vsmLoopCountVar = Just lv
                              , vsmMinLength    = 0
                              , vsmIsBuilder    = False
                              }

          pathGuard = PS.loopCountEqConstraint lv

      guardAssertions (pathGuard 0)
        (mkBound (op2 Eq (MV.vInteger sizeType 0)) lb)
      
      -- FIXME: if this is non-0 then we don't need to worry about the null case.
      guardAssertions (pathGuard 1)
        (traverse_ (mkBound (op2 Lt (MV.vInteger sizeType 0))) m_ub)
      
      -- We must be careful to only constrain lv when we are doing
      -- something.
      (elv, m) <- guardAssertions (pathGuard 1) (stratSlice b)

      let v    = MV.VSequence (B.singleton (vsm, [elv]))
          node = PathLoopGenerator ltag (Just lv) m

      pure (v, SelectedLoop node)

    SManyLoop StructureDependent lb m_ub b -> do
      -- How many times to unroll the loop
      nloops <- asks sNLoopElements

      (_slb, m_clb) <- execBnd minimum lb
      m_ubs <- traverse (execBnd maximum) m_ub

      let clb = fromMaybe 0 m_clb
          altub = clb + nloops
          -- Truncate to at most nloops
          -- FIXME: incomplete.
          cub = maybe altub (min altub) (snd =<< m_ubs)

      -- n <- asks sCurrentName
      -- liftIO $ printf "Bounds (%s): %d %d\n" (show n) clb cub

      -- FIXME: this is a bit blunt/incomplete
      lv <- freshLoopCountVar clb cub

      let lvv = MV.vSymbolicInteger sizeType (PS.loopCountVarToSMTVar lv)
      -- Assert bounds
      mkBound (\v -> op2 Leq v lvv) lb
      traverse_ (mkBound (op2 Leq lvv)) m_ub

      let doOne i = guardAssertions (PS.loopCountGtConstraint lv i) (stratSlice b)
      (els, ms) <- mapAndUnzipM doOne [0 .. cub - 1]

      let vsm = VSequenceMeta { vsmGeneratorTag = Nothing -- We don't pool dep. loops
                              , vsmLoopCountVar = Just lv
                              , vsmMinLength    = clb
                              , vsmIsBuilder    = False
                              }
          v = MV.VSequence (B.singleton (vsm, els))
          node = PathLoopUnrolled (Just lv) ms
          
      pure (v, SelectedLoop node)

    SRepeatLoop StructureIndependent _n _e _b ->
      panic "UNEXPECTED: StructureIndependent" []

    -- In this case the body just constrains the environment, so we
    -- just need the empty/non-empty loop count (n and e shouldn't
    -- really matter).  The result for this computation should also
    -- not matter (if it did, it would be StructureDependent)
    SRepeatLoop StructureIsNull _n _e b -> do
      -- Mainly to allow pooling (this won't have users as we don't produce a list) 
      ltag <- freshSymbolicLoopTag

      lv   <- freshLoopCountVar 0 1

      -- We must be careful to only constrain lv when we are doing
      -- something.
      (_elv, m) <- guardAssertions (PS.loopCountEqConstraint lv 1) (stratSlice b)

      let node = PathLoopGenerator ltag (Just lv) m

      pure (MV.VUnit, SelectedLoop node)

    -- Just unfold
    SRepeatLoop StructureDependent n e b -> do
      -- How many times to unroll the loop
      nloops <- asks sNLoopElements

      -- FIXME: this is a bit blunt/incomplete
      lv <- freshLoopCountVar 0 nloops

      se <- synthesiseExpr e

      -- We start from 0 so Gt works
      let eqGuard i = PS.loopCountEqConstraint lv (i + 1)
          -- this allows short-circuiting if we try to unfold too many
          -- times.
          go se' acc i
            | i == nloops = pure (reverse acc)
            | otherwise = do
                m_v_pb <-
                  guardAssertions (PS.loopCountGtConstraint lv i)
                    $ handleUnreachable (primBindName n se' (stratSlice b))
                case m_v_pb of
                  Nothing -> pure (reverse acc)
                  Just (v, pb) -> do
                    -- these should work (no imcompatible assumptions about lv)
                    go v (((eqGuard i, v), pb) : acc) (i + 1)

      (vs, pbs) <- unzip <$> go se [] 0
      let node = PathLoopUnrolled (Just lv) pbs
      v <- liftSemiSolverM (MV.mux (B.branching $ (PS.loopCountEqConstraint lv 0, se) : vs))
      pure (v, SelectedLoop node)

    SMorphismLoop (FoldMorphism n e lc b) -> do
      ltag <- freshSymbolicLoopTag

      se <- synthesiseExpr e
      m_col <- preview #_VSequence <$> synthesiseExpr (lcCol lc)
  
      let bvs = case m_col of
            Nothing -> panic "UNIMPLEMENTED: non-list fold" []
            Just r  -> r
      
      let eqGuard vsm
            | Just lv <- vsmLoopCountVar vsm = PS.loopCountEqConstraint lv
            | otherwise = const PS.true
          
          goOne _mkC _vsm _se' acc [] = pure (reverse acc)
          goOne mkC vsm se' acc ((i, el) : rest) = do
            m_v_pb <- guardedLoopCollection vsm lc (primBindName n se' (stratSlice b)) i el
            case m_v_pb of
              Nothing -> pure (reverse acc)
              Just (v, pb) -> goOne mkC vsm v (((mkC i, v), pb) : acc) rest
                
          go :: (VSequenceMeta, [MuxValue]) ->
                SymbolicM (B.Branching MuxValue, (VSequenceMeta, [PathBuilder]))
          go (vsm, els) = do
            let mkC = eqGuard vsm
            (svs, pbs) <- unzip <$> goOne mkC vsm se [] (zip [1..] els)
            let allvs = (mkC 0, se) : svs
            -- This check shouldn't be required (assuming minLength is
            -- correct) but this is clearer and probably more correct
            v <- -- FIXME: check for empty branching.
              if isNothing (vsmLoopCountVar vsm)
              -- if we have a non-symbolic spine, the val is the last
              then pure (B.singleton (snd (last allvs))) 
              else pure $ B.branching (drop (vsmMinLength vsm) allvs)
            pure (v, (vsm, pbs))
            
      (bvs', nodes) <- B.unzip <$> branching (go <$> bvs)
      v <- liftSemiSolverM (MV.mux (join bvs'))
      
      let node' = PathLoopMorphism ltag nodes
      pure (v, SelectedLoop node')

    SMorphismLoop (MapMorphism lc b) -> do
      ltag <- freshSymbolicLoopTag
      m_col <- preview #_VSequence <$> synthesiseExpr (lcCol lc)
  
      let bvs = case m_col of
            Nothing -> panic "UNIMPLEMENTED: non-list fold" []
            Just r  -> r
      
      let goOne _vsm acc [] = pure (reverse acc)
          goOne vsm acc ((i, el) : rest) = do
            m_v_pb <- guardedLoopCollection vsm lc (stratSlice b) i el
            case m_v_pb of
              Nothing -> pure (reverse acc)
              Just (v, pb) -> goOne vsm ((v, pb) : acc) rest
                
          go :: (VSequenceMeta, [MuxValue]) ->
                SymbolicM ((VSequenceMeta, [MuxValue]), (VSequenceMeta, [PathBuilder]))
          go (vsm, els) = do
            (els', pbs) <- unzip <$> goOne vsm [] (zip [1..] els)
            pure ((vsm, els'), (vsm, pbs))
            
      (bvs', nodes) <- B.unzip <$> branching (go <$> bvs)
      
      let node' = PathLoopMorphism ltag nodes
      pure (MV.VSequence bvs', SelectedLoop node')
  where
    -- FIXME: this duplicates work done in mkBound (the synthesiseExpr)
    execBnd :: (NonEmpty Int -> Int) -> Expr -> SymbolicM (MuxValue, Maybe Int)
    execBnd g bnd = do
      sbnd <- synthesiseExpr bnd
      let m_cbnd = g . fmap fromIntegral <$> MV.asIntegers sbnd
      pure (sbnd, m_cbnd)

    op2 op = MV.op2 op TBool sizeType sizeType

    mkBound f b =
      assert =<< liftSemiSolverM do
        v <- MV.semiExecExpr b
        MV.toAssertion <$> f v

-- Handles early failure as well.
guardedLoopCollection :: VSequenceMeta ->
                         LoopCollection' e ->
                         SymbolicM b ->
                         Int ->
                         MuxValue ->
                         SymbolicM (Maybe b)
guardedLoopCollection vsm lc m i el
  -- Are we allowed to fail here?  If so, handle it, otherwise just propagate.
  | Just lcv <- vsmLoopCountVar vsm, i >= vsmMinLength vsm =
      guardAssertions (PS.loopCountGtConstraint lcv i)
        $ handleUnreachable (bindK (bindE m))
  | otherwise = Just <$> bindK (bindE m)
  where
    kv = MV.vInteger sizeType (fromIntegral i)
    bindK = maybe id (\kn -> primBindName kn kv) (lcKName lc)
    bindE = primBindName (lcElName lc) el    

-- c.f. stratChoice
stratCase ::  Bool -> Case ExpSlice -> Maybe (Set SliceId) -> SymbolicM Result
stratCase _total cs@(Case n pats) _m_sccs
  | null pats = unreachable
  | otherwise = do
      v <- getName n
      pv <- freshPathVar (length pats)
      
      let (assn, missing) = MV.semiExecPatterns v pv (map fst (casePats cs))

      unless (null missing) $ debug pathKey ("Missing patterns for " <> show missing <> " "
                                              <> show (MV.ppMuxValue v))
      
      let go i sl | i `elem` missing = unreachable -- short-circuit, maybe tell the user?
                  | otherwise = over _2 ((,) i) <$> stratSlice sl
          mk i (_pat, sl) = (PS.choiceConstraint pv i, go i sl)
          
      b <- branching $ B.branching $ imap mk pats
      rv <- liftSemiSolverM (MV.mux (fst <$> b))
      
      let paths = map snd (toList b)
          feasibleIxs = map fst paths
      
      assert assn

      -- FIXME: we are pretending that we are a choice
      -- Record that we have this choice variable, and the possibilities
      recordChoice pv feasibleIxs
      
      pure (rv, SelectedCase (SymbolicChoice pv paths))
  
  -- (alts, preds) <- liftSemiSolverM (MV.semiExecCase cs)
  -- -- liftIO $ printf "Length alts is %d\n\t%s\n" (length alts)
  -- --                 (show $ commaSep [ pp p <> " => " <> pp vmr | (p, vmr) <- preds ])
  -- let mk1 (gs, (pat, a)) = guardedCase gs pat (stratSlice a)
  -- case m_sccs of
  --   Just sccs
  --     | any (hasRecCall sccs . snd . snd) alts -> do
  --         -- In this case we just pick a random alt (and maybe
  --         -- backtrack at some point?).  We ignore preds, as we assert
  --         -- that the alt is reachable.

  --         -- FIXME(!): this is incomplete
  --         (v, (_pat, pb)) <- putMaybe . mk1 =<< randL alts
  --         pure (v, SelectedCase (ConcreteCase pb))

  --   _ -> do
  --     stag <- freshSymbolicCaseTag

  --     (vs, paths) <- unzip . catMaybes <$> mapM mk1 alts

  --     v <- hoistMaybe (MV.unions' vs)
  --     -- liftIO $ print ("case " <> pp (caseVar cs) <> " " <> block "[" "," "]" (map (pp . length . MV.guardedValues) vs)
  --     --                 <> " ==> " <> pp (length (MV.guardedValues v))
  --     --                 <> pp (text . typedThing <$> snd (head (MV.guardedValues v))))

  --     -- preds here is a list [(PathCondition, SExpr)] where the PC is the
  --     -- condition for the value, and the SExpr is a predicate on when
  --     -- that value is enabled (matches some guard).  We require that at
  --     -- least 1 alternative is enabled, so we assert the disjunction of
  --     -- (g <-> s) for (g,s) in preds.  We could also assert the
  --     -- conjunction of (g --> s), which does not assert that the guard
  --     -- holds (only that the enabling predicate holds when that value is
  --     -- enabled).

  --     liftIO $ printf "Case on %s: %d -> %d\n" (showPP (caseVar cs)) (length (MV.guardedValues inv)) (length (MV.guardedValues v))

  --     enabledChecks preds
  --     recordCase stag (caseVar cs) [ (pcs, pat) | (pcs, (pat, _)) <- alts ]

  --     pure (v, SelectedCase (SymbolicCase stag inv paths))

  -- where
  --   enabledChecks preds = do
  --     --  | total     = pure ()
  --     -- FIXME: Is it OK to omit this if the case is total?      
  --     --  | otherwise = do
  --         let preds' = over (each . _1) PS.toSExpr preds
  --             patConstraints =
  --               MV.andMany ([ g `sImplies` c | (g, SymbolicMatch c) <- preds' ]
  --                           ++ [ S.not g | (g, NoMatch) <- preds' ])

  --         oneEnabled <- case [ g | (g, vmr) <- preds', vmr /= NoMatch ] of
  --                         [] -> do
  --                           inv <- getName (caseVar cs)
  --                           liftIO $ putStrLn ("No matches " ++ showPP cs ++ "\nValue\n"
  --                                              ++ showPP (text . typedThing <$> inv)
  --                                              ++ "\n" ++ show preds')
  --                           infeasible
  --                         ps -> pure (MV.orMany ps)

  --         assertSExpr oneEnabled
  --         assertSExpr patConstraints

  --   hasRecCall sccs = \sl -> not (sliceToCallees sl `Set.disjoint` sccs)

-- FIXME: this is copied from BTRand
stratCallNode :: ExpCallNode ->
                 SymbolicM Result
stratCallNode cn = do
  -- liftIO $ do print ("Entering: " <> pp cn)
  --             hFlush stdout
  sl <- getSlice (ecnSliceId cn)
  over _2 (SelectedCall (ecnIdx cn))
    <$> noteCurrentName (fnameText (ecnName cn))
          (enterFunction (ecnSliceId cn) (ecnParamMap cn) (stratSlice sl))

-- -----------------------------------------------------------------------------
-- Merging values


-- The idea here is that we have e.g. a choose where we have a number
-- of results, depending on which path is chosen.  Where possible we
-- would like to keep values in Haskell (i.e. avoid turning everything
-- into an sexpr) so we can simplify e.g. field access.  In particular
-- we would like to avoid having to use recursive functions in the
-- solver, so we try to keep e.g. arrays in Haskell too.

-- Consider
--
-- def P = block
--   a = First
--      lA = @${'a']
--      bA = @$['A']
--   b = First
--      lB = @${'b']
--      bB = @$['B']
--   { a is lA; b is lB } | {a is bA; b is bB }
--
-- i.e., we are not allows bA, lB or lA, bB.  We could represent a as
--
--  [ (lA, ca = 0), (bA, ca = 1) ]
--
-- where ca is the choice variable for a, and similarly for b.  Now,
-- when we get to the LHS of the predicate (call the choice variable
-- cP) we will have a 'case a of lA -> ()' for the first 'is', which
-- generates the path condition
--
-- cP = 0 --> (ca = 0 /\ ca \neq 1)
--
-- where the 'ca \neq 1' is because the case is partial (any
-- constructor without a corresponding alternative is
-- negated). Likewise, we get a similar constraint for 'b is lB'.

-- Consider
--
-- def P = block
--   x = Choose
--     x1 = ^ 1
--     x2 = ^ 2
--   n = ^ case x of
--     x1 v -> v
--     x2 v -> v
--   ...
--
-- the interesting question here being what should n look like?  An
-- obvious answer is
--
--  [ (1, cx = 0), (2, cx = 1) ]
--
-- and so for 'n + n' we would have
--
-- [ (2, cx = 0 /\ cx = 0)
-- , (3, cx = 0 /\ cx = 1)
-- , (3, cx = 1 /\ cx = 0)
-- , (4, cx = 1 /\ cx = 1)
-- ]
--
-- where, if no other processing is donw, we may get large numbers of
-- infeasible values (as for 'n + n = 3' in this example).  For choice
-- variables this should be easy, but for cases over constants 'case x
-- of 1,2,3 -> ...' we can have equality between expressions and
-- literals.


-- def M_P n b i =
--   if i < n then
--     block
--       let v = P
--       let b' = push v b
--       M_P n b' (i + 1)
--   else pure b

-- def C_M_P =
--   n = UInt8
--   rs = M_P n emptyBuilder 0
--   s = Q
--   map (r in rs) (R r s)

-- def M = block
--   x = P
--   ys = Many n (Q x)




 --  { r = [0, 1, 2] }

-- M_P n emptyBuilder 0 { [0, 1, 2] }
-- (unfold M_P)
--
-- if i < n then
--   block
--     let v = UInt8
--     M_P n (push v b) (i + 1)
-- else pure b
-- { [0, 1, 2] | bs = [] }

-- i < n /\
--     let v1 = UInt8 /\
--       WP (if i + 1 < n then
--         block
--         let v2 = UInt8
--         M_P n (push v2 (push v1 b)) ((i + 1) + 1)
--        else pure (push v1 b), { [0, 1, 2] | bs = [] })

-- n <= i /\ b = [0, 1, 2] /\ bs = []

-- { [0, 1, 2] | bs = [] }


-- { b = [0, 1, 2] /\ i = n /\ bs = []
-- \/ (b = [1, 2] /\ i + 1 = n /\ bs = [0] )
-- \/ (b = [2] /\ i + 2 = n /\ bs = [0, 1] )
-- \/ (b = [] /\ i + 3 = n /\ bs = [0, 1, 2]) }

--   (0 + 3 = n /\ bs = [0, 1, 2])

--  ==> n = 3 /\ bs = [0, 1, 2]

-- ================================================================================

-- if i < n then
--   block
--     let v = UInt8
--     M_P { n = n, b = push v b', i = i' + 1 }
-- else (i >= n /\ b = [0, 1, 2]) pure b 

-- (i >= n /\ b = [0, 1, 2])

-- {  }
-- if i < n then
--   block
--     let v' = UInt8
--     { i' + 1 >= n /\ push v' b' = [0, 1, 2] } === { i' + 1 >= n /\ v' = 0 /\ b' = [1, 2] }

-- if i < n then
--   block
--     { i' + 1 >= n /\ push v' b' = [0, 1, 2] }

-- ================================================================================

-- if i < n then
--   block
--     let v = UInt8
--     M_P n (push v b) (i + 1)
-- else pure b
-- { RESULT = vs }
--
-- {i >= n /\ b = vs}
--
-- (vs = [] /\ i = 0) -- initial call was the one we wanted
--
-- { b = push v' b', i = i' + 1) (b = vs /\ (vs \neq [] \/ i \neq 0))
--
-- push v' b' = vs /\ (vs \neq [] \/ i + 1 \neq 0)
-- {\exists v'. v' = UInt8 /\ push v' b' = vs /\ i' < n } M_P n b' i'

-- case vs of
--   []      -> emit [] >> pure 0
--   v : vs' -> emit v >> n' = G vs' (i - 1) >> return (n' + 1)
--

-- def M_P n =
--  if n = 0 then pure []
--  else { let v = P; vs = M_P (n - 1); ^ v : vs }

-- { RESULT = vs }

-- def S = { $['(']; $$ = S; $[')']; } <| $[ !['(',')] ]
-- 
-- { RESULT = 'foo' }

-- ----------------------------------------------------------------------------------------
-- Solver helpers

synthesiseExpr :: Expr -> SymbolicM MuxValue
synthesiseExpr e = do
  r <- liftSemiSolverM . MV.semiExecExpr $ e
  -- let vs = MV.guardedValues r
  --     numSymb = length [ () | (_, MV.VOther _) <- vs ]
  -- T.statS (pathKey <> "exprsize") (length vs, numSymb)
  pure r

synthesiseByteSet :: ByteSet -> MuxValue -> SymbolicM S.SExpr
synthesiseByteSet bs b = go bs -- liftSymExecM $ SE.symExecByteSet b bs
  where
    go bs' =
      case bs' of
        SetAny          -> pure (S.bool True)
        SetSingle  e    -> do
          r <- liftSemiSolverM (MV.op2 Eq TBool Core.tByte Core.tByte b =<< symE e)
          pure (MV.toSExpr r)
        SetRange l h    -> do
          (r1, r2) <- liftSemiSolverM $ do
            l' <- symE l
            h' <- symE h
            lb <- MV.op2 Leq TBool Core.tByte Core.tByte l' b
            ub <- MV.op2 Leq TBool Core.tByte Core.tByte b h'
            pure (lb, ub)
          
          pure (S.and (MV.toSExpr r1) (MV.toSExpr r2))
    
        SetComplement c -> S.not <$> go c
        SetUnion l r    -> S.or <$> go l <*> go r
        SetIntersection l r -> S.and <$> go l <*> go r

        SetLet n e bs'' -> do
          e' <- synthesiseExpr e
          primBindName n e' (go bs'')
          
        SetCall {}  -> unexpected bs'
        SetCase {}  -> unexpected bs'
    
    unexpected bs' = panic "Unexpected constructor" [showPP bs']
    symE = MV.semiExecExpr

-- traceGUIDChange :: String -> SymbolicM a -> SymbolicM a
-- traceGUIDChange msg m = do
--   cn <- asks sCurrentName
--   pre <- liftStrategy getRawGUID
--   r <- m
--   post <- liftStrategy getRawGUID
--   when (pre /= post) $ liftIO $ do
--     printf "%s (%s): GUID %s -> %s\n" msg (show cn) (showPP pre) (showPP post)
--     hFlush stdout
--   pure r

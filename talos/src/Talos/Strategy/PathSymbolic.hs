{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language OverloadedLabels #-}

-- Symbolic but the only non-symbolic path choices are those in
-- recursive functions (i.e., we only unroll loops).

-- FIXME: factor out commonalities with Symbolic.hs
module Talos.Strategy.PathSymbolic (pathSymbolicStrat) where


import           Control.Lens                              (_1, _2, each, over, itraverse, preview, traverseOf)
import           Control.Monad                             (forM_, when,
                                                            zipWithM, (<=<), unless)
import           Control.Monad.Reader
import           Control.Monad.Writer.CPS                  (censor, pass)
import           Data.Bifunctor                            (second)
import           Data.Foldable                             (traverse_)
import           Data.Generics.Product                     (field)
import           Data.List.NonEmpty                        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                        as NE
import qualified Data.Map                                  as Map
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe, mapMaybe)
import           Data.Set                                  (Set)
import qualified Data.Set                                  as Set
import           GHC.Generics                              (Generic)
import qualified SimpleSMT                                 as S
import           System.IO                                 (hFlush, stdout)
import           Text.Printf                               (printf)

import           Daedalus.Core                             hiding (streamOffset,
                                                            tByte)
import           Daedalus.Core.Free                        (freeVars)
import qualified Daedalus.Core.Semantics.Env               as I
import           Daedalus.Core.Type
import           Daedalus.Panic
import           Daedalus.PP                               (showPP, text)
import           Daedalus.Rec                              (topoOrder)

import qualified Daedalus.Value                            as V
import           Talos.Analysis.Exported                   (ExpCallNode (..),
                                                            ExpSlice, SliceId,
                                                            sliceToCallees)
import           Talos.Analysis.Slice
import           Talos.Monad                               (getIEnv, getModule,
                                                            getRawGUID, LogKey)
import qualified Talos.Monad as T
                 
import           Talos.Strategy.Monad
import qualified Talos.Strategy.OptParser                  as P
import           Talos.Strategy.OptParser                  (Opt, parseOpts)
import           Talos.Strategy.PathSymbolic.Monad
import qualified Talos.Strategy.PathSymbolic.MuxValue      as MV
import           Talos.Strategy.PathSymbolic.MuxValue      (MuxValue,
                                                            VSequenceMeta (..)
--                                                           , vUInt, vUnit
                                                           )
import           Talos.Strategy.PathSymbolic.PathBuilder   (buildPaths)
import qualified Talos.Strategy.PathSymbolic.PathSet as PS
import           Talos.Strategy.PathSymbolic.PathSet (PathSet,
                                                       PathVar,
                                                       loopCountToSExpr)
import qualified Talos.Strategy.PathSymbolic.SymExec as SE
import           Talos.Path
import           Talos.Solver.SolverT                     (declareName,
                                                           declareSymbol,
                                                           liftSolver, reset,
                                                           scoped, contextSize)
import Talos.Lib (tByte)
import Control.Monad.Except (catchError)
import Data.List ((\\))

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

  md <- liftStrategy getModule
  deps <- sliceToDeps sl

  -- FIXME: this should be calculated once, along with how it is used
  -- by e.g. memoSearch
  scoped $ do
    let topoDeps = topoOrder (second sliceToCallees) deps
    (m_res, sm) <- runSymbolicM (sl, topoDeps) (cMaxRecDepth config) (cNLoopElements config) ptag (stratSlice sl)
    sz <- contextSize
    T.statS (pathKey <> "modelsize") sz
    
    let go (_, pb) = do
          rs <- buildPaths (cNModels config) (cMaxUnsat config) sm pb
          T.info pathKey $ printf "Generated %d models" (length rs)
          pure (rs, Nothing) -- FIXME: return a generator here.

    case m_res of
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

        SDo x lsl rsl -> do
          let goL = noteCurrentName (fromMaybe "_" (nameText x)) (go lsl)

              goR :: PathBuilder -> SymbolicM Result
              goR lpath = over _2 (SelectedDo lpath) <$> go rsl

          bindNameIn x goL goR

        -- FIXME: we could pick concrete values here if we are willing
        -- to branch and have a concrete bset.
        SMatch bset -> do
          n <- makeNicerSym "b"
          sym <- liftSolver (declareSymbol n tByte)
          recordValue TByte sym

          let bse = S.const sym
              bv  = MV.vSymbolicBool sym

          bassn <- synthesiseByteSet bset bse
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
            assertSExpr (MV.asAssertion pe)

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

handleUnreachable :: PathSet -> SymbolicM a -> SymbolicM (Maybe a)
handleUnreachable ps m = pass (catchError go (\() -> pure (Nothing, const notReachable)))
  where
    go = do
      r <- m
      pure (Just r, extendPath (PS.toSExpr ps))
      
    notReachable = mempty { smGuardedAsserts = [ S.not (PS.toSExpr ps) ] }

-- This function runs the given monadic action under the current pathc
-- extended with the give path condition.  In practice, it will run
-- the action, collect any assertions, predicate them by the path
-- condition, and also update the value.  If the computation fails,
-- then the negated path condition will be asserted and 
guardedChoice :: PathVar -> Int -> SymbolicM Result ->
                 SymbolicM (Maybe ((PathSet, MuxValue), (Int, PathBuilder)))
guardedChoice pv i m =
  handleUnreachable ps $ do
    (v, p) <- m
    pure ((ps, v), (i, p))
  where
    ps = PS.choiceConstraint pv i
    
stratChoice :: [ExpSlice] -> Maybe (Set SliceId) -> SymbolicM Result
-- stratChoice ptag sls (Just sccs)
--   | any hasRecCall sls = do
--       -- Just pick randomly if there is recursion.
--       (i, sl) <- randL (enumerate sls)
--       over _2 (SelectedChoice . ConcreteChoice i) <$> stratSlice ptag sl
--   where
--     hasRecCall sl = not (sliceToCallees sl `Set.disjoint` sccs)

stratChoice sls _ = do
  pv <- freshPathVar (length sls)
  (vs, paths) <-
    unzip . catMaybes <$>
    itraverse (\i -> guardedChoice pv i . stratSlice) sls

  when (null vs) unreachable -- all paths failed
  v <- liftSemiSolverM (MV.mux vs Nothing)

  let feasibleIxs = map fst paths
  
  -- Record that we have this choice variable, and the possibilities
  recordChoice pv feasibleIxs
    
  -- liftIO $ print ("choice " <> block "[" "," "]" (map (pp . length . MV.guardedValues) vs)
  --                 <> " ==> " <> pp (length (MV.guardedValues v)))
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

      let xs = MV.VSequence [] (vsm, [v])
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
      let slv = PS.loopCountVarToSExpr lv
          s0  = loopCountToSExpr 0
          s1  = loopCountToSExpr 1

      -- FIXME: if this is non-0 then we don't need to worry about the null case.
      mkBound (\slb -> S.eq slv s0 `sImplies` S.eq slb s0) . fst =<< execBnd minimum lb
      traverse_ (mkBound (\ s_ub -> S.eq slv s1 `sImplies` S.not (S.eq s_ub s0)) . fst
                 <=< execBnd maximum) m_ub

      -- Construct return value
      let vsm = VSequenceMeta { vsmGeneratorTag = Just ltag
                              , vsmLoopCountVar = Just lv
                              , vsmMinLength    = 0
                              , vsmIsBuilder    = False
                              }

          pathGuard = PS.toSExpr (PS.loopCountEqConstraint lv 1)
      
      -- We must be careful to only constrain lv when we are doing
      -- something.
      (elv, m) <- censor (extendPath pathGuard) (stratSlice b)

      let v    = MV.VSequence [] (vsm, [elv])
          node = PathLoopGenerator ltag (Just lv) m

      pure (v, SelectedLoop node)

    SManyLoop StructureDependent lb m_ub b -> do
      -- How many times to unroll the loop
      nloops <- asks sNLoopElements

      (slb, m_clb) <- execBnd minimum lb
      m_ubs <- traverse (execBnd maximum) m_ub

      let clb = fromMaybe 0 m_clb
          altub = clb + nloops
          -- Truncate to at most nloops
          -- FIXME: incomplete.
          cub = maybe altub (min altub) (snd =<< m_ubs)

      n <- asks sCurrentName
      -- liftIO $ printf "Bounds (%s): %d %d\n" (show n) clb cub

      -- FIXME: this is a bit blunt/incomplete
      lv <- freshLoopCountVar clb cub

      manyBoundsCheck (PS.loopCountVarToSExpr lv) slb (fst <$> m_ubs)

      -- Construct result  
      let pathGuard i = PS.toSExpr (PS.loopCountGtConstraint lv i)
          -- FIXME: we could name the results of this and use a SMT
          -- function to avoid retraversing multiple times.
          doOne i = censor (extendPath (pathGuard i)) (stratSlice b)

      (els, ms) <- unzip <$> mapM doOne [0 .. cub - 1]

      let vsm = VSequenceMeta { vsmGeneratorTag = Nothing -- We don't pool dep. loops
                              , vsmLoopCountVar = Just lv
                              , vsmMinLength    = clb
                              , vsmIsBuilder    = False
                              }
          v = MV.VSequence [] (vsm, els)
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

      let pathGuard = PS.toSExpr (PS.loopCountEqConstraint lv 1)

      -- We must be careful to only constrain lv when we are doing
      -- something.
      (_elv, m) <- censor (extendPath pathGuard) (stratSlice b)

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
      let gtGuard i = PS.loopCountGtConstraint lv i
          eqGuard i = PS.loopCountEqConstraint lv (i + 1)

          -- this allows short-circuiting if we try to unfold too many
          -- times.
          go se' acc i
            | i == nloops = pure (reverse acc)
            | otherwise = do
                m_v_pb <- handleUnreachable (gtGuard i)
                           (primBindName n se' (stratSlice b))
                case m_v_pb of
                  Nothing -> pure (reverse acc)
                  Just (v, pb) -> do
                    -- these should work (no imcompatible assumptions about lv)
                    go v (((eqGuard i, v), pb) : acc) (i + 1)

      (vs, pbs) <- unzip <$> go se [] 0
      let node = PathLoopUnrolled (Just lv) pbs
      v <- liftSemiSolverM (MV.mux vs (Just se))
      pure (v, SelectedLoop node)

    SMorphismLoop (FoldMorphism n e lc b) -> do
      ltag <- freshSymbolicLoopTag

      se <- synthesiseExpr e
      m_col <- preview #_VSequence <$> synthesiseExpr (lcCol lc)

      let (vars, base) = case m_col of
            Nothing -> panic "UNIMPLEMENTED: non-list fold" []
            Just r  -> r
      
      -- c.f. SRepeatLoop
      let guards vsm
            | Just lv <- vsmLoopCountVar vsm =
                \i -> ( PS.loopCountGeqConstraint lv i
                      , PS.loopCountEqConstraint lv i
                      )
            | otherwise = const (mempty, mempty)

          -- this allows short-circuiting if we try to unfold too many
          -- times.
          go se' acc i
            | i == nloops = pure (reverse acc)
            | otherwise = do
                m_v_pb <- handleUnreachable (gtGuard i)
                           (primBindName n se' (stratSlice b))
                case m_v_pb of
                  Nothing -> pure (reverse acc)
                  Just (v, pb) -> do
                    -- these should work (no imcompatible assumptions about lv)
                    go v (((eqGuard i, v), pb) : acc) (i + 1)

      
          -- TODO: prune early as for Repeat above
          goOne _vsm (_se', acc) [] = pure (reverse acc)
          goOne vsm (se', acc) ((i, el) : rest) = do
            (v, pb) <- guardedLoopCollection vsm lc (primBindName n se' (stratSlice b)) i el
            let (gtGuard, eqGuard) = guards vsm (i + 1)

            v' <- hoistMaybe (MV.refine eqGuard v)
            se'' <- hoistMaybe (MV.refine gtGuard v)
            goOne vsm (se'', (v', pb) : acc) rest

          go (vsm, els) = do
            (svs, pbs) <- unzip <$> goOne vsm (se, []) (zip [0..] els)

            let (_, eqGuard) = guards vsm 0
            base <- hoistMaybe (MV.refine eqGuard se)

            pure (MV.unions (base :| svs), (g, vsm, pbs))

      -- (vs, nodes) <- unzip <$> collectMaybes (map go (MV.guardedValues col))

      vars' <- traverseOf (each . _2) go vars
      base' <- go base
      
      v <- hoistMaybe (MV.unions' vs)
      let node' = PathLoopMorphism ltag nodes

      pure (v, SelectedLoop node')

    SMorphismLoop (MapMorphism lc b) -> do
      ltag <- freshSymbolicLoopTag

      -- TODO: prune early as for Repeat above
      col <- synthesiseExpr (lcCol lc)
      let go (g, sv)
            | Just (vsm, els) <- MV.gseToList sv = do
                (els', pbs) <- unzip <$> zipWithM (guardedLoopCollection vsm lc (stratSlice b)) [0..] els
                -- We just propagate the vsm for the collection value
                -- to the users of the result of this sequence.  We
                -- also inherit the guard from the collection
                let v = MV.singleton g (VSequence vsm els')
                pure (v, (g, vsm, pbs))

                -- pure (v, node)
            | otherwise =  panic "UNIMPLEMENTED: map over non-lists" []

      (vs, nodes) <- unzip <$> collectMaybes (map go (MV.guardedValues col))

      v <- hoistMaybe (MV.unions' vs)
      let node' = PathLoopMorphism ltag nodes

      pure (v, SelectedLoop node')
  where
    execBnd :: (NonEmpty Int -> Int) -> Expr -> SymbolicM (MuxValue, Maybe Int)
    execBnd g bnd = do
      sbnd <- synthesiseExpr bnd
      let m_cbnd = fmap (g . fmap (fromIntegral . V.valueToSize)) (MV.asValues sbnd)
      pure (sbnd, m_cbnd)

    manyBoundsCheck slv slb m_sub = do
      -- Assert bounds
      mkBound (`S.bvULeq` slv) slb
      traverse_ (mkBound (slv `S.bvULeq`)) m_sub

    -- Constructs a bounds check, and returns a concrete bound (if any)
    mkBound f sbnd = do
      ienv <- liftStrategy getIEnv
      assertSExpr (f (MV.toSExpr (I.tEnv ienv) sizeType sbnd))

guardedLoopCollection :: VSequenceMeta ->
                         LoopCollection' e ->
                         SymbolicM b ->
                         Int ->
                         MuxValue ->
                         SymbolicM b
guardedLoopCollection vsm lc m i el = do
  el' <- hoistMaybe (MV.refine g el)
  let kv = vUInt g 64 (fromIntegral i)
      bindK = maybe id (\kn -> primBindName kn kv) (lcKName lc)
      bindE = primBindName (lcElName lc) el'

  censor doCensor (bindK (bindE m))
  where
    (g, doCensor)
      | Just lcv <- vsmLoopCountVar vsm =
          let g' = PS.insertLoopCount lcv (PS.LCCGt i) mempty
          in ( g', extendPath (PS.toSExpr g') )
      | otherwise = (mempty, id)

-- We represent disjunction by having multiple values; in this case,
-- if we have matching values for the case alternative v1, v2, v3,
-- with guards g1, g2, g3, then for each value returned by the RHS
-- of the pattern we will combine with each g1, g2, g3 (to get 3
-- sets of values) which we then union.  As a formula, we have
--
-- (ga, va) \/ (gb, vb) \/ ... = RHS
-- return  (ga /\ g1, va) \/ (gb /\ g1, vb) \/ (gc /\ g1, vc)
--      \/ (ga /\ g2, va) \/ (gb /\ g2, vb) \/ (gc /\ g2, vc)
--      \/ ...
--
-- This can greatly increase the number of values

-- FIXME: merge with the above?
-- guardedCase :: NonEmpty PathCondition -> Pattern -> SymbolicM Result ->
--                SymbolicM (Maybe (MuxValue, (Pattern, PathBuilder)))
-- guardedCase gs pat m = pass $ do
--   m_r <- getMaybe m
--   -- when (isNothing m_r) $ liftIO $ print ("Infeasible case: " ++ S.ppSExpr pathGuard "" ++ "\n")

--   pure $ case m_r of
--            Just r  -> (mk r, extendPath pathGuard)
--            Nothing -> (Nothing, notFeasible)
--   where
--     mk (v, pb) = do
--       x <-  MV.unions' (mapMaybe (flip MV.refine v) (NE.toList gs))
--       pure (x, (pat, pb))

--     pathGuard = MV.orMany (map PS.toSExpr (NE.toList gs))
--     notFeasible _ = mempty { smGuardedAsserts = [S.not pathGuard] }

stratCase ::  Bool -> Case ExpSlice -> Maybe (Set SliceId) -> SymbolicM Result
stratCase _total cs m_sccs = do
  v <- getName (caseVar cs)
  pv <- freshPathVar
  let (pred, missing) = MV.semiExecPatterns v pv (map fst (caseAlts cs))
  undefined
  
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

synthesiseByteSet :: ByteSet -> S.SExpr -> SymbolicM S.SExpr
synthesiseByteSet bs b = go bs -- liftSymExecM $ SE.symExecByteSet b bs
  where
    go bs' =
      case bs' of
        SetAny          -> pure (S.bool True)
        SetSingle  v    -> S.eq b <$> symExecExpr v
        SetRange l h    -> S.and <$> (flip S.bvULeq b <$> symExecExpr l)
                                 <*> (S.bvULeq b <$> symExecExpr h)
    
        SetComplement c -> S.not <$> go c
        SetUnion l r    -> S.or <$> go l <*> go r
        SetIntersection l r -> S.and <$> go l <*> go r

        SetLet n e bs'' -> do
          (n', r) <- bindNameFreshIn n (go bs'')
          mklet n' <$> symExecExpr e <*> pure r
          
        SetCall f es  -> S.fun (fnameToSMTName f) <$> ((++ [b]) <$> mapM symExecExpr es)
        SetCase {}    -> unimplemented
    
    unimplemented = panic "SymExec (ByteSet): Unimplemented inside" [showPP bs]


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

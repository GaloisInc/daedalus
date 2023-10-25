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


import           Control.Arrow                           ((&&&))
import           Control.Lens                            (imap, over, preview)
import           Control.Monad                           (join, unless, void)
import           Control.Monad.Reader                    (asks)
import           Data.Bifunctor                          (second)
import           Data.Foldable                           (toList, traverse_)
import           Data.Functor                            (($>))
import           Data.Generics.Product                   (field)
import           Data.List.NonEmpty                      (NonEmpty)
import qualified Data.Map                                as Map
import           Data.Maybe                              (fromMaybe)
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
import           Daedalus.PP                             (commaSep, pp, showPP)
import           Daedalus.Rec                            (topoOrder)

import           Talos.Analysis.Exported                 (ExpCallNode (..),
                                                          ExpSlice, SliceId,
                                                          sliceToCallees)
import           Talos.Analysis.Slice                    hiding (Result)
import qualified Talos.Monad                             as T

import           Talos.Lib                               (tByte)
import           Talos.Monad                             (debug)
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

-- ----------------------------------------------------------------------------------------
-- Symbolic strategy

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
               SliceId ->
               ExpSlice ->
               StratGen
symbolicFun config ptag sid sl = StratGen $ do
  debug pathKey $ "Generating models for " <> showPP sid
  -- defined referenced types/functions
  reset -- FIXME

  deps <- sliceToDeps sl

  -- FIXME: this should be calculated once, along with how it is used
  -- by e.g. memoSearch
  scoped $ do
    let topoDeps = topoOrder (second sliceToCallees) deps
    (m_res_sm) <- runSymbolicM (sl, topoDeps) (cMaxRecDepth config) (cNLoopElements config) ptag Nothing (stratSlice sl)
    sz <- contextSize
    T.statS (pathKey <> "modelsize") sz

    let go (r, st, sm) = do
          -- undefined
          rs <- buildPaths (cNModels config) (cMaxUnsat config) sid sm (rBuilder r)
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
    go :: ExpSlice -> SymbolicM Result
    go sl = do
      -- liftIO (putStrLn "Slice" >> print (pp sl))
      case sl of
        SHole m_shs -> do
          traverse_ (consumeFromStream . fst) m_shs
          pure $ Result MV.VUnit SelectedHole

        -- We special case this here so we can add assertions/path
        -- variables and deal with the stream tree.
        SPure (Ap2 DropMaybe lenE strmE) -> do
          (extraAssn, negA, posA, strmV) <- liftSemiSolverM (MV.semiExecDropMaybe strmE lenE)
          v <- branching . snd =<< pathVariants [ assert negA $> MV.vNothing
                                                , assert posA $> MV.vJust strmV
                                                ]
          assert extraAssn
          pure $ Result (MV.mux v) SelectedHole

        -- Likewise, we need to deal with take
        SPure (Ap2 Take lenE strmE) -> do
          (assn, v) <- liftSemiSolverM (MV.semiExecTake strmE lenE)
          assert assn
          pure $ Result v SelectedHole

        SPure e -> do
          let mk v = Result v SelectedHole
          mk <$> synthesiseExpr e

        SGetStream -> do
          -- We don't _have_ to finish the stream here, but we might
          -- as well name it to avoid re-naming the offset term.
          strmV <- finishStreamSegment
          pure $ Result strmV SelectedHole

        SSetStream e -> do
          synthesiseExpr e >>= setStream
          pure $ Result MV.VUnit SelectedHole

        SEnd -> do
          strmV <- finishStreamSegment
          -- This asserts the stream we get after flushing the current
          -- stream usage is empty (i.e., we have no space left). 
          assert (MV.nullStream strmV)
          pure $ Result MV.VUnit SelectedHole

        SDo m_x lsl rsl -> do
          let goL = noteCurrentName (fromMaybe "_" (nameText =<< m_x)) (go lsl)

              goR :: Result -> SymbolicM Result
              goR r = over #rBuilder (SelectedDo (rBuilder r)) <$> go rsl

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
          consumeFromStream (staticSHoleSize 1)

          ptag <- asks sProvenance
          pure $ Result bv (SelectedBytes ptag (ByteResult sym))

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

          -- FIXME
          -- unless (isNothing m_strm) $
          panic "inverses currently broken due to bounded stream" []

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

            -- FIXME: the stream has to be Nothing here as we don't
            -- support inverses in bounded streams (we could).
            pure $ Result n' (SelectedBytes ptag (InverseResult venv ifn))

stratChoice :: [ExpSlice] -> Maybe (Set SliceId) ->
               SymbolicM Result
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
      (_pv, slsB) <- ipathVariants sls
      let mk (i, sl) = (rValue &&& (const i &&& rBuilder)) <$> stratSlice sl
      (vsB, ipathsB) <- B.unzip <$> branching (mk <$> slsB)
      pure $ Result (MV.mux vsB) (SelectedChoice (B.IndexedBranching ipathsB))

stratLoop :: SLoopClass Expr ExpSlice ->
             SymbolicM Result
stratLoop lclass =
  case lclass of
    -- FIXME: is this correct?  We have two reps. of unboundedStreams,
    -- Nothing and MV.unboundedStream but we should never see a
    -- MV.unboundedStream by itself.
    -- SLoopPool {} | Just {} <- m_strm -> streamShouldBeUnboundedErr
    SLoopPool sem b -> do
      ltag <- freshSymbolicLoopTag
      let vsm = VSequenceMeta { vsmGeneratorTag = Just ltag
                              , vsmLoopCountVar = Nothing
                              , vsmMinLength    = 1 -- FIXME: shouldn't matter?
                              , vsmIsBuilder    = False
                              }

      -- No need to constrain the processing of the body.
      r <- stratSlice b -- See comment above for why Nothing.
      -- unless (isNothing m_strm') streamShouldBeUnboundedErr

      let xs = MV.VSequence (B.singleton (vsm, [rValue r]))
          v' = case sem of
                 SemNo  -> MV.VUnit
                 SemYes -> xs

          node = PathLoopGenerator ltag Nothing (rBuilder r)

      pure $ Result v' (SelectedLoop node)

    -- Should be SLoopPool
    SManyLoop StructureIndependent _lb _m_ub _b ->
      panic "BUG: saw SManyLoop StructureIndependent" []

    -- SManyLoop StructureIsNull _lb _m_ub _b
    --   | Just {} <- m_strm -> streamShouldBeUnboundedErr

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

      void $ branching $ B.branching True
        [ (pathGuard 0, mkBound (op2 Eq (MV.vInteger sizeType 0)) lb)
        , (pathGuard 1, traverse_ (mkBound (op2 Lt (MV.vInteger sizeType 0))) m_ub)
        ]
      
      -- We must be careful to only constrain lv when we are doing
      -- something.
      r <- unaryBranching (pathGuard 1) (stratSlice b)

      -- unless (isNothing m_strm') streamShouldBeUnboundedErr

      let v    = MV.VSequence (B.singleton (vsm, [rValue r]))
          node = PathLoopGenerator ltag (Just lv) (rBuilder r)

      pure $ Result v (SelectedLoop node)

    -- FIXME: special case when b is SHole (Just ...)
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

      rs <- unrollLoopPartials lv MV.VUnit (replicate cub ()) $ \_ _i _ ->
        stratSlice b

      let els = map rValue rs
          pbs = map rBuilder rs
          vsm = VSequenceMeta { vsmGeneratorTag = Nothing -- We don't pool dep. loops
                              , vsmLoopCountVar = Just lv
                              , vsmMinLength    = clb
                              , vsmIsBuilder    = False
                              }
          v = MV.VSequence (B.singleton (vsm, els))
          node = PathLoopUnrolled (Just lv) pbs

      pure $ Result v (SelectedLoop node)

    SRepeatLoop StructureIndependent _n _e _b ->
      panic "UNEXPECTED: StructureIndependent" []

    -- SRepeatLoop StructureIsNull _n _e _b
    --   | Just {} <- m_strm -> streamShouldBeUnboundedErr

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
      r <- unaryBranching (PS.loopCountEqConstraint lv 1) (stratSlice b)
      -- unless (isNothing m_strm') streamShouldBeUnboundedErr

      let node = PathLoopGenerator ltag (Just lv) (rBuilder r)

      pure $ Result MV.VUnit (SelectedLoop node)

    -- Just unfold
    SRepeatLoop StructureDependent n e b -> do
      -- How many times to unroll the loop
      nloops <- asks sNLoopElements

      -- FIXME: this is a bit blunt/incomplete
      lv <- freshLoopCountVar 0 nloops

      se <- synthesiseExpr e

      rs <- unrollLoopPartials lv se (replicate nloops ()) $ \se' _i _ ->
        primBindName n se' (stratSlice b)

      let svs = map rValue rs
          pbs = map rBuilder rs
          node = PathLoopUnrolled (Just lv) pbs
          vs' = zip (map (PS.loopCountEqConstraint lv) [0..]) (se : svs)

      let v = MV.mux (B.branching True vs')
      pure $ Result v (SelectedLoop node)

    SMorphismLoop (FoldMorphism n e lc b) -> do
      ltag <- freshSymbolicLoopTag

      se <- synthesiseExpr e
      m_col <- preview #_VSequence <$> synthesiseExpr (lcCol lc)

      let bvs = case m_col of
            Nothing -> panic "UNIMPLEMENTED: non-list fold" []
            Just r  -> r

      let execBody se' i el = 
            bindLoopCollectionEl lc i el $ primBindName n se' (stratSlice b)
          go :: (VSequenceMeta, [MuxValue]) ->
                SymbolicM (B.Branching MuxValue {-, Maybe SymbolicStream -}, (VSequenceMeta, [PathBuilder]))
          go (vsm, els)
            | Just lv <- vsmLoopCountVar vsm = do
                rs <- unrollLoopPartials lv se els execBody
                let v = mkBranching lv (vsmMinLength vsm) (se : map rValue rs)
                pure (v, (vsm, map rBuilder rs))
            | otherwise = do -- just execute on all the elements.
                rs <- unrollLoop se els execBody
                pure (B.singleton (last (se : map rValue rs)), (vsm, map rBuilder rs))

      (bvs', nodes) <- B.unzip <$> branching (go <$> bvs)
      let v = MV.mux (join bvs')
          node' = PathLoopMorphism ltag nodes

      pure $ Result v (SelectedLoop node')

    SMorphismLoop (MapMorphism lc b) -> do
      ltag <- freshSymbolicLoopTag
      m_col <- preview #_VSequence <$> synthesiseExpr (lcCol lc)

      let bvs = case m_col of
            Nothing -> panic "UNIMPLEMENTED: non-list fold" []
            Just r  -> r
            
      let execBody _ i el = bindLoopCollectionEl lc i el (stratSlice b)
          go :: (VSequenceMeta, [MuxValue]) ->
                SymbolicM ((VSequenceMeta, [MuxValue]), (VSequenceMeta, [PathBuilder]))
          go (vsm, els)
            | Just lv <- vsmLoopCountVar vsm = do
                rs <- unrollLoopPartials lv MV.VUnit els execBody
                pure ((vsm, map rValue rs), (vsm, map rBuilder rs))
            | otherwise = do -- just execute on all the elements.
                rs <- unrollLoop MV.VUnit els execBody
                pure ((vsm, map rValue rs), (vsm, map rBuilder rs))

      (bvs', nodes) <- B.unzip <$> branching (go <$> bvs)

      let node' = PathLoopMorphism ltag nodes
      pure $ Result (MV.VSequence bvs') (SelectedLoop node')
  where
    -- streamShouldBeUnboundedErr = panic "Expected unbounded stream" []

    mkBranching lv nIgnored =
      B.branching (nIgnored == 0) . drop nIgnored . zip (map (PS.loopCountEqConstraint lv) [0..])

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

bindLoopCollectionEl :: LoopCollection' e ->
                        Int ->
                        MuxValue ->
                        SymbolicM b ->                        
                        SymbolicM b
bindLoopCollectionEl lc i el m = bindK (bindE m)
  where
    kv = MV.vInteger sizeType (fromIntegral i)
    bindK = maybe id (\kn -> primBindName kn kv) (lcKName lc)
    bindE = primBindName (lcElName lc) el

-- c.f. stratChoice
stratCase :: Bool -> Case ExpSlice -> Maybe (Set SliceId) ->
             SymbolicM Result
stratCase _total cs@(Case n pats) _m_sccs
  | null pats = unreachable
  | otherwise = do
      v <- getName n
      pv <- freshPathVar (length pats)

      let (assn, missing) = MV.semiExecPatterns v pv (map fst (casePats cs))
      assert assn

      unless (null missing) $ do
        debug pathKey $ "Case on " <> showPP n <> " missing " <> show (commaSep (map pp (toList missing)))
          <> ": " <> show (MV.ppMuxValue v)

      let go i sl
            | i `elem` missing = unreachable -- short-circuit, maybe tell the user?
            | otherwise = (rValue &&& (const i &&& rBuilder)) <$> stratSlice sl
          mk i (_pat, sl) = (PS.choiceConstraint pv i, go i sl)
      (vsB, ipathsB) <- B.unzip <$> branching (B.branching True $ imap mk pats)
      pure $ Result (MV.mux vsB) (SelectedCase (B.IndexedBranching ipathsB))

stratCallNode :: ExpCallNode -> SymbolicM Result
stratCallNode cn = do
  -- liftIO $ do print ("Entering: " <> pp cn)
  --             hFlush stdout
  sl <- getSlice (ecnSliceId cn)
  over #rBuilder (SelectedCall (ecnIdx cn))
    <$> noteCurrentName (fnameText (ecnName cn))
          (enterFunction (ecnSliceId cn) (ecnParamMap cn) (stratSlice sl))

-- -----------------------------------------------------------------------------
-- Streams

-- consumeFromStream :: SemiCtxt m =>
--                      SymbolicStream -> SHoleSize ->
--                      SemiSolverM m (Assertion, SymbolicStream)
-- consumeFromStream strm shs = do
--   shsV <- fromHoleSize shs
--   (assn, strm0') <- B.unzip <$> traverse (go shsV) (getSymbolicStreamF (S.const <$> strm))
--   strm' <- SymbolicStreamF <$> traverseOf (traversed . traversed) (bvsNameSExprs sizeType) strm0'
--   pure (A.BAssert assn, strm')
--   where
--     go shsV stn = do
--       len'    <- semiExecOp2 Add sizeType sizeType sizeType shsV (VIntegers (Typed sizeType (stnLength stn)))
--       noWrapV <- semiExecOp2 Leq sizeType sizeType TBool shsV len'
--       let noWrapA = toAssertion' noWrapV
--       pure (noWrapA, stn { stnLength = len' ^?! #_VIntegers . typedIso sizeType })

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

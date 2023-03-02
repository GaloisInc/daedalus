{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
 -- for dealing with TCDecl and existential k

-- We walk through the each decl figuring out if it has things we
-- can't easily handle and thus need to send to the solver.  At the
-- moment this is any Guard.

module Talos.Analysis ( summarise
                      , absEnvTys
                      , Summary
                      -- FIXME: move
                      ) where

import qualified Data.Map                 as Map
import           Data.Monoid              (All (All))
import qualified Data.Set                 as Set
import Control.Lens (over, _1, _2)

import           Daedalus.Core
import           Daedalus.Core.Type
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic


import           Data.Proxy               (Proxy)
import           Talos.Analysis.AbsEnv
import           Talos.Analysis.Domain
import           Talos.Analysis.Merge     (merge)
import           Talos.Analysis.Monad
import           Talos.Analysis.Slice
import           Talos.Analysis.VarAbsEnv (varAbsEnvTy)
import           Talos.Analysis.FieldAbsEnv (fieldAbsEnvTy)


import Debug.Trace (traceM)
import Talos.Analysis.SLExpr (SLExpr (EHole))
import Data.List (partition)
import Control.Monad (when, unless)
import Data.Maybe (maybeToList)

--------------------------------------------------------------------------------
-- Top level function

summarise :: AbsEnv ae =>
             Proxy ae -> Module -> GUID -> (Summaries ae, GUID)
summarise _ md nguid = (summaries, nextGUID s')
  where
    -- FIXME: need to rename bound vars to avoid clashing in synthesis
    (s', summaries) = calcFixpoint doOne wl0 s0
    s0    = initState (mGFuns md) (mFFuns md) nguid

    doOne nm cl fid
      | Just decl <- Map.lookup nm decls = summariseDecl cl fid decl
      | otherwise = panic "Missing decl" [showPP nm]

    wl0   = Set.fromList grammarDecls

    decls = Map.fromList (map (\tc -> (fName tc, tc)) (mGFuns md))

    grammarDecls =
      [ (name, assertionsFID) -- FIXME: using an explicit FID is a bit gross here
      | Fun { fName = name, fDef = Def _ } <- mGFuns md
      ]


-- Not sure these belong here
absEnvTys :: [(String, AbsEnvTy)]
absEnvTys = [ ("vars", varAbsEnvTy)
            , ("fields", fieldAbsEnvTy)
            ]

--------------------------------------------------------------------------------
-- Summary functions

summariseDecl :: AbsEnv ae =>
                 SummaryClass' ae -> FInstId ->
                 Fun Grammar -> IterM ae (Summary ae)
summariseDecl cls fid Fun { fDef = Def def
                          , fParams = ps } = do
  let preds = summaryClassToPreds cls
  d <- runSummariseM (summariseG preds def)

  let newS = Summary { domain = d
                     , params = ps
                     , fInstId = fid
                     }
  -- -- Sanity check
  -- unless (domainInvariant (exportedDomain newS)) $
  --   panic "Failed domain invariant" ["At " ++ showPP fn]
  ---  traceM ("Summarised " ++ showPP fn ++ " for " ++ showPP cls ++ " " ++ showPP newS)

  pure newS

summariseDecl _ _ _ = panic "Expecting a defined decl" []

--------------------------------------------------------------------------------
-- Decl-local monad 

-- newtype SummariseMState =
--   SummariseMState { pathRoots :: PathRootMap }

-- emptySummariseMState :: SummariseMState
-- emptySummariseMState = SummariseMState Map.empty

-- newtype SummariseM a = SummariseM { getSummariseM :: StateT SummariseMState IterM a }
--   deriving (Applicative, Functor, Monad)

type SummariseM = IterM

runSummariseM :: SummariseM ae a -> IterM ae a
runSummariseM m = m -- evalStateT ((,) <$> getSummariseM m <*> gets pathRoots) emptySummariseMState

liftIterM :: IterM ae a -> SummariseM ae a
liftIterM = id -- SummariseM . lift

-- -- v should not already be mapped
-- addPathRoot :: Name -> [(FieldSet, Slice)] -> SummariseM ()
-- addPathRoot _v []  = pure ()
-- addPathRoot v  sls = SummariseM $ modify (\s -> s { pathRoots = Map.insert v sls (pathRoots s) })

--------------------------------------------------------------------------------
-- Transfer function

-- Get the summary and link it to the actual arguments
--
-- For each element in domain, we need to construct a domain element
-- by substituting the free variables in the argument for the
-- paramaters in the entanglement set.  E.g., if we have
--
-- f x y z :: { x: {x, y} => ... x ... y ...; ... }
--
-- and we have a call
--
-- f a b q
--
-- then we produce a node like
--
-- {a, b} => Call args 0
--
-- Where {a, b} is the result of substituting the args in {x, y} and
-- 0 is a name for the path set above.

exprAsVar :: Expr -> Maybe Name
exprAsVar (Var n) = Just n
exprAsVar _       = Nothing

summariseCall :: AbsEnv ae => [AbsPred ae] -> FName -> [Expr] ->
                 SummariseM ae (Domain ae)
summariseCall preds fn args
  | Just vargs <-  mapM exprAsVar args = do
      m_invs <- liftIterM (getDeclInv fn)
      case m_invs of
        -- We only really care about inverses if we have a result
        -- (probably otherwise pure?)
        Just f | not (null preds) -> do
           -- FIXME: we ignore the projection (we over project)
           n <- liftIterM (freshNameSys (fnameType fn))
           let (ifn, pfn) = f n args
           let gs = GuardedSlice
                 { gsEnv   = absInverse n ifn pfn
                 , gsPred  = preds
                 , gsSlice = SInverse n ifn pfn
                 }
           pure (singletonDomain gs)

        _ -> do
          let cl = summaryClassFromPreds preds
          Summary dom ps fid <- liftIterM $ requestSummary fn cl
          -- We need to now substitute the actuals for the params in
          -- summary, and merge the results (the substitution may have
          -- introduced duplicates, so we need to do a pointwise merge)
          let argsMap     = Map.fromList $ zip ps vargs
              argsFor env = zipWith (\param arg -> arg <$ absProj param env) ps vargs
              mkCallNode i gs =
                CallNode { callClass        = fid
                         , callName         = fn
                         , callSlices       = Map.singleton i (argsFor (gsEnv gs))
                         }

              mkCall i gs =
                GuardedSlice { gsEnv   = absSubstEnv argsMap (gsEnv gs)
                             , gsPred  = gsPred gs -- FIXME: subst?
                             , gsSlice = SCall (mkCallNode i gs)
                             }
              res = domainFromElements $ zipWith mkCall [0..] (elements dom)
          -- traceM ("Call result to " ++ showPP fn ++
          --         " for " ++ showPP cl ++ "\n" ++
          --         show (brackets (commaSep [ pp n <+> "->" <+> pp n' | (n, n') <- Map.toList argsMap ])) ++ "\n" ++
          --         show (nest 4 (pp res)))
          pure res
  | otherwise = panic "Saw non-Var arg" []


-- Case is a bit tricky.
-- 
-- A) If (1) the rhss of the case are Unconstrained (impl. that m_x
-- is Nothing) AND (2) the case is total then the case is skipped.
--
-- B) If (1) holds but (2) doesn't (i.e., the case is partial) then
-- we can emit just an assertion that the cased term satisfies one
-- of the patterns.
--
-- C) If (1) doesn't hold and (2) does, then we have something like
-- Choose, with the ordering imposed by the pattern (rather than
-- branch index in te Choose).  E.g.
--
-- def P = {
--   x = UInt8;
--   y = A_or_B;
--   case y is {
--     A -> x < 10 is true;
--     _ -> {}
--   };
-- }
--
-- The question is: should x and y be entangled?  If we have a
-- guard on y later on, we certainly must pick y before x as
-- otherwise we may get an empty grammar; likewise, if there is a
-- later constraint on x (like x > 10) then we need to pick x
-- first.  In this case, we should entangle x and y,
--
-- D) is similar to (C) but we need to add a constraint.

-- This assumes type safety etc. (hence we can get away with length for labels)
caseIsTotal :: Case k -> Bool
caseIsTotal (Case e alts)
  | hasDefault = True
  | otherwise  =
      case typeOf e of
        -- Assume numeric types cannot be total (unless hasDefault)
        TInteger  -> False
        TUInt {}  -> False -- FIXME: for small sizes totality is possible
        TSInt {}  -> False
        TBool     -> nAlts == 2 -- no repeated patterns
        TMaybe {} -> nAlts == 2
        TUser ut  -> nAlts == nLabels ut
        ty -> panic "Unexpected type in caseIsTotal" [showPP ty]
  where
    nAlts = length alts
    -- should be the last pattern.
    hasDefault = any ((==) PAny . fst) alts

    nLabels ut = case tnameFlav (utName ut) of
      TFlavStruct {} -> panic "Unexpected struct" []
      TFlavUnion ls -> length ls
      TFlavEnum  ls -> length ls

-- For case and choose we have to make sure the same result happens on
-- all slices, which we can only do by making sure we have a single
-- slice (well, this is the simplest approach).
collapseDoms :: AbsEnv ae => [AbsPred ae] ->
                ([Slice] -> Slice) ->
                [Domain ae] -> Domain ae
-- The case where all are nullDomain is handled in the caller, as we
-- may want to construct e.g. Case nodes over empty bodies when the
-- case is partial.
collapseDoms _preds _mk [] = emptyDomain
collapseDoms preds mk branchDs = foldl merge (singletonDomain gs') doms
  where
    (envs, sls, doms) = unzip3 (map (asSingleton . squashDomain) branchDs)
    gs' = GuardedSlice { gsEnv   = foldl1 (<>) envs
                       , gsPred  = preds
                       , gsSlice = mk sls
                       }

asSingleton :: AbsEnv ae => Domain ae -> (ae, Slice, Domain ae)
asSingleton dom
  | null (elements dom)  = (mempty, SHole, dom)
  | [gs] <- elements dom = (gsEnv gs, gsSlice gs, dom { elements = [] })
  | otherwise = panic "Saw non-singleton domain" [] -- [show (pp dom)]

summariseCase :: AbsEnv ae => [AbsPred ae] ->
                 Case Grammar -> SummariseM ae (Domain ae)
summariseCase preds cs = do
  bDoms   <- traverse (summariseG preds) cs
  let All trivial = foldMap (All . nullDomain) bDoms
      total       = caseIsTotal cs

  -- This is a bit gross because we need to be careful to merge in any
  -- info from the case, and to construct the new gsEnv properly.
  if trivial && total
    then pure emptyDomain
    else do
    let squashed = asSingleton . squashDomain <$> bDoms
        newEnv   = absCase ((\(ae, _, _) -> ae) <$> squashed)
        newCase  = (\(_, sl, _) -> sl) <$> squashed
        newDoms  = (\(_, _, d ) -> d) <$> squashed
        gs'      =
          GuardedSlice { gsEnv   = newEnv
                       , gsPred  = preds
                       , gsSlice = SCase total newCase
                       }

    -- fn <- currentDeclName
    -- traceM ("Summarising case in " ++ showPP fn ++
    --       "(trivial: " ++ show trivial ++ ", total: " ++ show total ++ ") " ++
    --       showPP cs)
    -- traceM ("\tresult: " ++ showPP gs')

    pure (foldl merge (singletonDomain gs') newDoms)

-- Some examples:
--
-- def ManyEx1 = {
--   x = UInt8
--   Many { y = UInt8; y < x }
-- }
--
-- The result of the Many is uninteresting, but it still appears on
-- the path from x, and may (artificially) entangle other variables
-- if, for example, there are bounds on the Many.  An alternative to
-- this is to existentially quantify the path, so that we just assert
-- that a solution exists for 1 iteration (and so for arbitrarily
-- many) but we don't produce the solution until we are ready to
-- determine the number of iterations.
--
-- Alternately:
--
-- def ManyEx2 = {
--   x = UInt8
--   rs = Many { y = UInt8; y < x }
--   v = ^ makeNumber rs
--   Guard (v < 100)
-- }
--
-- We have that the result is relevant.






-- This calculates the pathset for a grammar.  The first argument is
-- what parts (if any) we care about for the result of this function
-- (in general we only care about this for the final statement,
-- everything else is handled using path sets).

-- Is it necessary to explode the Ors here?  The idea is to make it equally likely in the solver
-- that we choose one.

-- | The main summary function, it constructs a domain based on the
-- argument grammar, with respect to `preds`. The post-condition here
-- is that each pred should appear in the gsPred of some element of
-- the resulting domain (exactly, not via absPredEntails), although we
-- may have more in gsPred than in preds (in particular, loop invariants).
summariseG :: AbsEnv ae => [AbsPred ae] -> Grammar -> SummariseM ae (Domain ae)
summariseG preds tc =
  case tc of
    -- When preds == [] this is emptyDomain
    Pure e -> pure $ domainFromElements
      [ GuardedSlice { gsEnv = env, gsPred = [p], gsSlice = SPure e'}
      | (env, p, e') <- summariseExpr preds e
      ]

    GetStream    -> unimplemented
    SetStream {} -> unimplemented
    Match _ (MatchByte bset) -> pure $ domainFromElements $
      [ GuardedSlice { gsEnv = absByteSet p bset
                     , gsPred = [p]
                     , gsSlice = SMatch bset}
      | p <- preds ]
    Match {}
      | not (null preds) -> panic "Saw a relevant match" [showPP tc]
      | otherwise        -> pure emptyDomain
    Fail {}   -> unimplemented -- FIXME: we will probably handle this specially in branching code

    Do_ lhs rhs -> do
      -- FIXME: we might want to have a nicer name
      n <- freshNameSys (typeOf lhs)
      summariseG preds (Do n lhs rhs)
      -- rhsD <- dontCareD 1 <$> summariseG m_x rhs
      -- merge rhsD <$> summariseG Nothing lhs

    Do x lhs rhs -> summariseBind preds x lhs rhs

    Let n e rhs -> summariseG preds (Do n (Pure e) rhs) -- FIXME: this is a bit of a hack

    -- doms contains a domain for each path in the choose. We create a
    -- diagonal list of domains, like
    --
    --  [ Just Unconstrained, ... , fp, ... Just Unconstrained]
    --
    -- and then merge
    --
    Choice _biased gs -> do
      doms <- mapM (summariseG preds) gs
      if all closedDomain doms
        then pure (foldl merge emptyDomain doms)
        else pure (collapseDoms preds SChoice doms)

    Call fn args  -> summariseCall preds fn args
    Annot _ g     -> summariseG preds g

    GCase cs      -> summariseCase preds cs

    Loop lp -> summariseLoop preds lp

    _ -> panic "impossible" [] -- 'Or's, captured by Choice above

  where
    unimplemented = panic "summariseG unimplemented" [showPP tc]

summariseExpr :: AbsEnv ae => [AbsPred ae] -> Expr -> [(ae, AbsPred ae, SLExpr)]
summariseExpr preds e = [ (env, p, e') | p <- preds, let (env, e') = absPre p e ]

summariseBind :: AbsEnv ae =>
                 [AbsPred ae] -> Name -> Grammar -> Grammar ->
                 SummariseM ae (Domain ae)
summariseBind preds x lhs rhs = do
  rhsD <- summariseG preds rhs

  let (matching, rhsD') = partitionDomainForVar x rhsD
      preds'            = map snd matching
  lhsD <- summariseG preds' lhs

  let (lhsMatching, lhsD') = partitionDomainForResult (not . null) lhsD
      -- For each element of lhsMatching we find the corresponding
      -- rhs(s) from matching, merge if there are multiple, and the
      -- bind them all together.
      --
      -- gsFor gets the rhs(s) for a particular result lhs
      gsFor gs = [ gs' | (gs', p) <- matching, absPredEntails (gsPred gs) p ]

      els = [ bindGuardedSlice x gs gs'
            | gs <- lhsMatching
            , let gs' = foldl1 merge (gsFor gs)
            ]

      -- There are the elements which do not care about x.
      indepLHSD = mapSlices (\sl -> SDo x sl SHole) lhsD'
      indepRHSD = mapSlices (SDo x SHole) rhsD'
      final     = indepLHSD `merge` indepRHSD `merge` domainFromElements els

  -- fn <- currentDeclName
  -- when (showPP fn == "Main") $
  -- traceM ("Summarising bind in " ++ showPP fn ++ " (result? " ++ show (not $ null preds) ++ ")\n" ++
  --         show (nest 4 $ pp (Do x lhs rhs)) ++
  --         "\n (result'? " ++ show (not $ null preds') ++ ")" ++
  --         "\n" ++ show (hang "lhsD" 4 (pp lhsD)) ++          
  --         "\n" ++
  --         show (hang "lhs" 4 (bullets (map pp lhsMatching))) ++ "\n" ++
  --         show (hang "final" 4 (pp final)))

  pure final --  (indepLHSD `merge` indepRHSD `merge` domainFromElements els)

summariseMany :: AbsEnv ae => [AbsPred ae] ->
                 Sem -> Backtrack -> Expr -> Maybe Expr -> Grammar ->
                 SummariseM ae (Domain ae)
summariseMany preds sem bt lb m_ub g = do
  elsD <- summariseG elpreds g

  -- We need to now map the elpreds we used out list predicates) back
  -- into preds, and wrap the slices in a SLoop . ManyLpop.  We also
  -- need to add the sliced bounds to the structural slice (if any).

  let (matching, nonResD) = partitionDomainForResult (not . null) elsD
      nonResD' = mapSlices wrapInvariantSlice nonResD

      -- We use `elem` here over absPredEntails as we should have an
      -- exact match (c.f. the post-cond for summariseG).  There
      -- should be a 1-1 mapping from gsPred into preds.
      updPreds ps = [ p | p <- preds, absPredListElement p `elem` ps ]
      updGS gs = gs { gsPred = updPreds (gsPred gs) }
      matching' = map updGS matching

      (structurals, invs) = partition (any absPredIsStructural . gsPred) matching'
      -- length should be <= 1
      structurals' = map mkStructural structurals
      invs'        = map mkInvariant invs

  when (length structurals' > 1) $ panic "BUG: Saw multiple structural loop slices" []
  pure (domainFromElements (structurals' ++ invs') `merge` nonResD')
  where
    mkLoop lb' m_ub' str sl = SLoop (ManyLoop sem bt lb' m_ub' (str, sl))

    -- Used to construct the Structural slice, if any
    mkStructural gs =
      let (envlb, slb) = absPre absPredTop lb
          m_env_ub = absPredTop absPredTop <$> m_ub
          envub = maybe absEmptyEnv fst m_env_ub
          m_sub = snd <$> m_env_ub
          boundsEnv = envlb `merge` envub
      in gs { gsEnv   = gsEnv gs `merge` boundsEnv
            , gsSlice = mkLoop slb m_sub Structural (gsSlice gs)
            }

    -- Wraps a body slice in the SLoop (ManyLoop ...)
    wrapInvariantSlice sl =
      let boundsHole = EHole sizeType
      in mkLoop boundsHole (boundsHole <$ m_ub) StructureInvariant sl

    mkInvariant gs = gs { gsSlice = wrapInvariantSlice (gsSlice gs) }

    -- The preds we get here should be disjoint             
    -- absDestListPred :: AbsPred ae -> (AbsPred ae, Bool)
    absDestListPred = undefined
    elpreds = map (fst . absDestListPred) preds

summariseLoop :: AbsEnv ae =>
                 [AbsPred ae] -> LoopClass Grammar ->
                 SummariseM ae (Domain ae)
summariseLoop preds lc =
  case lc of
    ManyLoop sem bt lb m_ub g -> summariseMany preds sem bt lb m_ub g

    -- For repeat we nned to find a predicate P s.t.
    --
    --    ... { P } g { P } ... { P } g { preds }
    ---
    -- i.e., an invariant which entails preds
    -- e.g. given
    --
    -- r = many (acc = { count = 0; sum = 0})
    --  block
    --    acc.count < 100 is true
    --    let x = UInt8
    --    count = acc.count + 1
    --    sum   = acc.sum + x
    --
    -- r.sum < 1000 is true
    --
    -- we have preds == [ .sum ] and the result of summarising the
    -- body is acc.[ .sum ; .count ] -> ...
    -- 
    -- Also consider
    -- r = many (acc = { count = 0; last = 0 })
    --  block
    --    acc.count < 100 is true
    --    last = UInt8
    --    count = acc.count + 1
    --
    -- r.last > 0 is true
    --
    -- where last is an output only variable (inside the loop) so with
    -- prems == [ .last ] we get a summary of
    --
    --   acc.[ .last ] -> _; last = UInt8; _
    --   acc.[ .count ] -> acc.count > 100 is true; _; ^ { count - acc.count + 1 }
    --
    -- FIXME: we could also just desugar this and proceed as usual for
    -- recursion
    RepeatLoop bt n e g -> do
      -- here: 
      --  * matching are the slices that have a dep. on n
      --  * preds' is the (>= preds) predicate invariants for the loop
      --  * gD is the domain without matching
      (matching,tstgss, m_stP', nsgss, nspreds', gD) <- repeatFixpoint n g m_stP nspreds

      let mkSlice e' str g' = SLoop (RepeatLoop bt n e' (str, g'))
          eHole = EHole (typeOf n)

      case () of
        -- Nothing interesting to do here.
        _ | nullDomain gD && null preds' -> pure emptyDomain

        -- No dep. on the accumulator, so we can stitch together
        -- arbitrarily. This also entails preds == [] and matching == []
        _ | null preds' -> do
              unless (null preds) $ panic "BUG: non-empty preds" []
              unless (null matching) $ panic "BUG: non-empty matching" []

              pure (mapSlices (mkSlice eHole StructureInvariant) gD)

        -- Otherwise we have a loop which depends on the accumulator
        -- and initial element, so we will need to squash the
        -- structural elements of the domain into a single slice so
        -- during synthesis we agree on the number of iterations
        -- required (the non-result slices can be left unsquashed as
        -- they don't depend on the number of iterations).  
        _ -> do
          
          -- FIXME: we could just check for entailment of pred'?  This
          -- will also capture output-only slices.
          let (matchingPost, gD') = partitionDomainForResult (not . null) gD
          when (null matching) $ panic "BUG: empty matching" []
          let pred' = foldl1 merge preds'
              matchingGS = foldl1 merge matching
              (enve, sle) = absPre pred' e

              -- non-result slices
              gD' = mapSlices (mkSlice eHole StructureInvariant) gD
              
              gs = matchingGS { gsEnv = enve `merge` gsEnv matchingGS
                              , gsSlice = mkSlice sle Structural (gsSlice matchingGS)
                              }

          pure (singletonDomain gs `merge` gD')

    -- Similar for Reapeat above, with the additional use of elements.
    MorphismLoop (FoldMorphism n e lc g) -> do
      (matching, gD) <- calcFixpoint n g preds
      
      let mkSlice e' lc' str g' =
            SLoop (MorphismLoop (FoldMorphism n e' lc' (str, g')))
            
          eHole  = EHole (typeOf n)
          lcHole = lc { lcCol = EHole (typeOf (lcCol lc)) }
          
      case () of
        _ | nullDomain gD && null matching -> pure emptyDomain

        -- Otherwise we have a loop which depends on the accumulator
        -- and initial element, so we will need to squash the
        -- structural elements of the domain into a single slice so
        -- during synthesis we agree on the number of iterations
        -- required (the non-result slices can be left unsquashed as
        -- they don't depend on the number of iterations).  
        _ -> do
          let (structurals, invs) = partition (any absPredIsStructural . gsPred) matching
              
              (enve, sle) = absPre pred' e

              -- non-result slices
              gD' = mapSlices (mkSlice eHole StructureInvariant) gD
              
              gs = matchingGS { gsEnv = enve `merge` gsEnv matchingGS
                              , gsSlice = mkSlice sle Structural (gsSlice matchingGS)
                              }
          undefined
          
        --   pure (singletonDomain gs `merge` gD')
          
        --     else do
        --     let (envs, _, sls) = unzip3 (summariseExpr preds' e)

        --         (env, sl, remainder) = asSingleton (squashDomain gD)

        --         gs =  GuardedSlice { gsEnv = foldl (<>) env envs
        --                            , gsPred = preds -- use original preds
        --                            , gsSlice = SLoop (MorphismLoop (FoldMorphism s sle sl))
        --                            }

        --     pure (singletonDomain gs `merge` remainder)

    MorphismLoop (MapMorphism lc g) -> undefined

  where
    summariseLC gss gD lc = undefined
      
      -- These depend on the key or accum., so they need to be squashed
      -- let gssWithKs
      --       | Just k <- lcKName lc =
      --           [ either (Left . (, m_el)) (Right . (, m_el)) (partitionSliceForVar k gs)
      --           | (gs, m_el) <- matchingEl'
      --           ]
      --       | otherwise = map Right matchingEl'




      --   (matchingEl, elD) = partitionDomainForVar (lcElName lc) gD
      --     gssEl = map (either (over _2 Just) (, Nothing)
      --                  . partitionSliceForVar (lcElName lc)) gss

      --     matchingEl' = gssEl ++ map (over _2 Just) matchingEl
      --     matchingElK
      --       | Just k <- lcKName lc =
      --           [ either (Left . (, m_el)) (Right . (, m_el)) (partitionSliceForVar k gs)
      --           | (gs, m_el) <- matchingEl'
      --           ]
      --       | otherwise = map Right matchingEl'

          
            
      --     -- matchingK don't mention el
      --     (matchingK, kD) = maybe ([], elD) (flip partitionDomainForVar elD) (lcKName lc)
      --     gssWithPreds  = [ (gs, Nothing, Just p) | (gs, p) <- matchingK ] ++ matchingElK
      
          
          
      -- case typeOf e of
      --   TArray elTy -> do

      --     undefined



      --   TMap kTy vTy -> undefined
      --   _ -> panic "BUG: non collection type in loop collection" []


    -- There are 3 types of slices we care about here:
    --  * sources: establish some p in ps, no dep. on x
    --  * sinks: no post-conds, needs some q for x
    --  * transfers: establishes some p, needs some q for x (q and p can differ)
    --
    -- Generally we want to find gslices s.t.
    --  1. all deps on x are entailed by post-cond; and
    --  2. at most 1 slice with structural deps.
    --
    -- although (2) can be constructed by merging.
    calcFixpoint x g ps = do
      (matching, gD) <- repeatFixpoint x g ps
      let (justPost, gD') = partitionDomainForResult (not . null) gD
      pure (mergeForDeps matching justPost, gD')
      
    -- Invariant: at most one gsPreds entails any predicate in the
    -- worklist.
    mergeForDeps [] acc = acc
    mergeForDeps ((gs, p) : rest) acc =
      let gsEntails = flip absPredEntails p . gsPred 
          -- length inrest + length inacc <= 1.  If inrest and inacc
          -- are both empty, then the predicate should be entailed by
          -- gs.
          (inrest, outrest) = partition (gsEntails . fst) rest
          (inacc, outacc)   = partition gsEntails acc
          -- If a gs in rest matches, we merge in the slices and
          -- re-add it to the wl using the matching predicate.
          rest' = [(gs `merge` gs', p') | (gs', p') <- inrest ] ++ outrest
          acc'  = [ gs `merge` gs' | gs' <- inacc ] ++ outacc
      in mergeForDeps rest' acc'
                      
    repeatFixpoint x g ps = do
      gD <- summariseG ps g
      
      let (matching, gD') = partitionDomainForVar x gD
          deps = map snd matching
          ps' = absPredsJoin ps deps
          
      if all (absPredEntails ps) deps
        then pure (matching, gD') -- fixpoint reached
        else repeatFixpoint x g ps'

    -- absListPred :: AbsPred ae -> (AbsPred ae, Bool)
    absDestListPred = undefined

    -- absPredsJoin :: [AbsPred ae] -> [AbsPred ae] -> [AbsPred ae]
    absPredsJoin = undefined

-- -----------------------------------------------------------------------------
-- Special patterns

pattern GuardP :: Expr -> Grammar -> Grammar
pattern GuardP e g <- (caseIsGuard -> Just (e, g))

-- Attempts to catch things ike
--
-- case b of
--   True  -> ...
--   False -> Fail ...

caseIsGuard :: Grammar -> Maybe (Expr, Grammar)
caseIsGuard (GCase (Case x cs)) =
  case cs of
    -- FIXME: Annot
    [(PBool b, g), (_, Fail {})]       -> mk b g
    [(PBool _, Fail {}), (PBool b, g)] -> mk b g
    [(PBool b, Fail {}), (PAny, g)]    -> mk (not b) g
    [(PBool b, g)]                     -> mk b g
    _ -> Nothing
  where
    mk b g = Just (if b then Var x else eNot (Var x), g)
caseIsGuard _ = Nothing

{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

{-# LANGUAGE FlexibleContexts #-}
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
      TFlavStruct   -> panic "Unexpected struct" []
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
  | null (elements dom)  = (absTop, SHole, dom)
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

-- FIXME: as with Choice and Case, we squash here to reduce
-- complexity, but we might be able to avoid this by introducing
-- orderings.

-- summariseMany :: Maybe EntangledVar ->
--                  TC TCSynthAnnot Grammar ->
--                  ManyBounds (TC TCSynthAnnot Value) ->
--                  TC TCSynthAnnot Grammar ->
--                  SummariseM (Domain)
-- summariseMany m_x _tc bnds body = do
--   -- We squash as we need to unify the domain vars with the frees
--   -- in the bounds, modulo m_x
--   bodyD <- squashDomain <$> summariseG m_ret body
--   -- bodyD has 0 or 1 elements
--   if nullDomain bodyD then pure emptyDomain else do
--     let (evs, fps) = asSingleton bodyD
--         evs'       = subst_x evs
--         node       = ManyNode { manyBounds       = bnds
--                               , manyFrees        = maybe evs (flip deleteEntangledVar evs) m_ret
--                               , manyBody         = fps
--                               }
--     pure $ singletonDomain (mergeEntangledVars evs' (tcEntangledVars bnds))
--                            (SMany node)
--     where
--       (m_ret, subst_x) = case m_x of
--         Nothing -> (Nothing, id)
--         Just x  -> let ret = ResultVar (typeOf x)
--                        rSubst y
--                          | y == ret  = singletonEntangledVars x
--                          | otherwise = singletonEntangledVars y
--                    in (Just ret, substEntangledVars rSubst)

-- This calculates the pathset for a grammar.  The first argument is
-- what parts (if any) we care about for the result of this function
-- (in general we only care about this for the final statement,
-- everything else is handled using path sets).

-- Is it necessary to explode the Ors here?  The idea is to make it equally likely in the solver
-- that we choose one.

summariseG :: AbsEnv ae => [AbsPred ae] -> Grammar -> SummariseM ae (Domain ae)
summariseG preds tc =
  case tc of
    -- When preds == [] this is emptyDomain
    Pure e -> pure $ domainFromElements $
      [ GuardedSlice { gsEnv = env, gsPred = [p], gsSlice = SPure e'}
      | p <- preds, let (env, e') = absPre p e ]

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

    -- Cases
    -- GuardP e g    -> do
    --   n <- freshNameSys TUnit
    --   rhsD <- mapSlices (SDo n SHole) <$> summariseG preds g
    --   let lhsD = singletonDomain
    --         (GuardedSlice { gsEnv  = absGuard e
    --                       , gsPred = []
    --                       , gsSlice = SDo n (SAssertion e) SHole
    --                       })
    --   pure (merge lhsD rhsD)

    GCase cs      -> summariseCase preds cs
    _ -> panic "impossible" [] -- 'Or's, captured by Choice above

  where
    unimplemented = panic "summariseG unimplemented" [showPP tc]

summariseBind :: AbsEnv ae =>
                 [AbsPred ae] -> Name -> Grammar -> Grammar ->
                 SummariseM ae (Domain ae)
summariseBind preds x lhs rhs = do
  rhsD <- summariseG preds rhs

  let (matching, rhsD') = partitionDomainForVar x rhsD
      preds'            = map snd matching
  lhsD <- summariseG preds' lhs

  let (lhsMatching, lhsD') = partitionDomainForResult lhsD
      -- For each element of lhsMatching we find the corresponding
      -- rhs(s) from matching, merge if there are multiple, and the
      -- bind them all together.
      --
      -- gsFor gets the rhs(s) for a particular result lhs
      gsFor gs = [ gs' | (gs', p) <- matching, p `elem` gsPred gs ]

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

-- diagonalise :: a -> [b] -> ([a] -> b -> [a] -> c) -> [c]
-- diagonalise el xs f =
--   let pfxs = inits (replicate (length xs - 1) el)
--       sfxs = reverse pfxs
--   in zipWith3 f pfxs xs sfxs


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

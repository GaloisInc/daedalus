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

import           Control.Arrow              (second)
import           Control.Lens               (_1, _2, _3, view)
import           Data.List                  (foldl', foldl1')
import qualified Data.Map                   as Map
import           Data.Maybe                 (isNothing, mapMaybe, maybeToList)
import           Data.Monoid                (All (All))
import qualified Data.Set                   as Set

import           Daedalus.Core
import           Daedalus.Core.CFG          (NodeID, pattern WithNodeID)
import           Daedalus.Core.Type         (typeOf)
import           Daedalus.Panic             (panic)
import           Daedalus.PP                (showPP)

import           Data.Proxy                 (Proxy)
import           Talos.Analysis.AbsEnv
import           Talos.Analysis.Domain
import           Talos.Analysis.FieldAbsEnv (fieldAbsEnvTy)
import           Talos.Analysis.FLAbsEnv    (flAbsEnvTy)
import           Talos.Analysis.Merge       (Merge, merge, mergeOverlapping)
import           Talos.Analysis.Monad
import           Talos.Analysis.SLExpr      (SLExpr (EHole))
import           Talos.Analysis.Slice
import           Talos.Analysis.VarAbsEnv   (varAbsEnvTy)
import           Talos.Monad                (TalosM, getModule)




--------------------------------------------------------------------------------
-- Top level function

-- FIXME: do something nicer with messages
summarise :: AbsEnv ae => Proxy ae -> TalosM (Summaries ae)
summarise _ = do
  md <- getModule
  let decls = Map.fromList (map (\tc -> (fName tc, tc)) (mGFuns md))
      wl0   = Set.fromList grammarDecls
      grammarDecls =
        -- FIXME: using an explicit FID is a bit gross here
        [ (name, assertionsFID)
        | Fun { fName = name, fDef = Def _ } <- mGFuns md
        ]
      doOne nm cl fid
        | Just decl <- Map.lookup nm decls = summariseDecl cl fid decl
        | otherwise = panic "Missing decl" [showPP nm]
      
  snd <$> calcFixpoint doOne wl0

-- Not sure these belong here
absEnvTys :: [(String, AbsEnvTy)]
absEnvTys = [ ("vars", varAbsEnvTy)
            , ("fields", fieldAbsEnvTy)
            , ("fl", flAbsEnvTy)
            ]

--------------------------------------------------------------------------------
-- Summary functions

summariseDecl :: AbsEnv ae =>
                 SummaryClass' ae -> FInstId ->
                 Fun Grammar -> IterM ae (Summary ae)
summariseDecl cls fid Fun { fDef = Def def
                          -- , fName = fn
                          , fParams = ps } = do
  -- logMessage 1 ("* Summarising " ++ showPP fn ++ " for " ++ showPP cls) 
  
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

mergeMaybe :: Merge p => [p] -> Maybe p
mergeMaybe ps = foldl' merge Nothing (Just <$> ps)

summariseCall :: AbsEnv ae => [AbsPred ae] -> NodeID -> FName -> [Expr] ->
                 SummariseM ae (Domain ae)
summariseCall preds nid fn args
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
                 , gsPred  = mergeMaybe preds
                 , gsSlice = SInverse n ifn pfn
                 , gsDominator = nid
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
                             , gsDominator = nid
                             }
              res = domainFromElements $ zipWith mkCall [0..] (elements dom)
          -- logMessage 1 ("** Call result to " ++ showPP fn ++
          --               " for " ++ showPP cl ++ "\n" ++
          --               show (brackets (commaSep [ pp n <+> "->" <+> pp n' | (n, n') <- Map.toList argsMap ])) ++ "\n" ++
          --               show (nest 4 (pp res)))
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
collapseDoms :: AbsEnv ae => [AbsPred ae] -> NodeID -> 
                ([Slice] -> Slice) ->
                [Domain ae] -> Domain ae
-- The case where all are nullDomain is handled in the caller, as we
-- may want to construct e.g. Case nodes over empty bodies when the
-- case is partial.
collapseDoms _preds _nid _mk [] = emptyDomain
collapseDoms preds nid mk branchDs = foldl' merge (singletonDomain gs') doms
  where
    (envs, slices, doms) = unzip3 (map asSingleton branchDs)
    gs' = GuardedSlice { gsEnv   = foldl1 merge envs
                       , gsPred  = mergeMaybe preds
                       , gsSlice = mk slices
                       , gsDominator = nid
                       }

-- We return the elements of the GuardedSlice as we don't have a
-- dominator for the empty-element domain.
asSingleton :: AbsEnv ae => Domain ae -> (ae, Slice, Domain ae)
asSingleton dom
  | null gss = (absEmptyEnv, SHole, dom)
  | otherwise = (gsEnv gs, gsSlice gs, dom')
  where
    (gss, dom') = domainElements dom
    gs = foldl1' merge gss

summariseCase :: AbsEnv ae => [AbsPred ae] -> NodeID ->
                 Case Grammar -> SummariseM ae (Domain ae)
summariseCase preds nid cs = do
  bDoms   <- traverse (summariseG preds) cs
  let All trivial = foldMap (All . nullDomain) bDoms
      total       = caseIsTotal cs

  -- This is a bit gross because we need to be careful to merge in any
  -- info from the case, and to construct the new gsEnv properly.
  if trivial && total
    then pure emptyDomain
    else do
    let squashed = asSingleton <$> bDoms
        newEnv   = foldl1 merge (view _1 <$> squashed)
        newCase  = view _2 <$> squashed
        newDom  = foldl1 merge (view _3 <$> squashed)
        -- Include the case variable too, absPre should leave the var
        -- alone
        (varEnv, _) = absPre absPredTop (Var (caseVar cs)) 
        gs = GuardedSlice { gsEnv   = newEnv `merge` varEnv
                          , gsPred  = mergeMaybe preds
                          , gsSlice = SCase total newCase
                          , gsDominator = nid
                          }

    -- fn <- currentDeclName
    -- traceM ("Summarising case in " ++ showPP fn ++
    --       "(trivial: " ++ show trivial ++ ", total: " ++ show total ++ ") " ++
    --       showPP cs)
    -- traceM ("\tresult: " ++ showPP gs')

    pure (merge (singletonDomain gs) newDom)

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
summariseG preds (WithNodeID nid _annots g) = do
  
  case g of
    -- When preds == [] this is emptyDomain
    Pure e -> pure $ domainFromElements
      [ GuardedSlice { gsEnv = env
                     , gsPred = Just p
                     , gsSlice = SPure e'
                     , gsDominator = nid}
      | (env, p, e') <- summariseExpr preds e
      ]

    GetStream    -> unimplemented
    SetStream {} -> unimplemented
    Match _ (MatchByte bset) -> pure $ domainFromElements $
      [ GuardedSlice { gsEnv = absByteSet p bset
                     , gsPred = Just p
                     , gsSlice = SMatch bset
                     , gsDominator = nid }
      | p <- preds ]
    Match {}
      | not (null preds) -> panic "Saw a relevant match" [showPP g]
      | otherwise        -> pure emptyDomain
    Fail {}   -> unimplemented -- FIXME: we will probably handle this specially in branching code

    Do_ lhs rhs  -> summariseBind preds nid Nothing lhs rhs

    Do x lhs rhs -> summariseBind preds nid (Just x) lhs rhs

    Let {} -> unexpected

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
        else pure (collapseDoms preds nid SChoice doms)

    Call fn args  -> summariseCall preds nid fn args
    Annot {}      -> panic "Unexpected Annot" [] -- should be taken care of by WithNodeID

    GCase cs      -> summariseCase preds nid cs

    Loop lp -> summariseLoop preds nid lp

    _ -> panic "impossible" [] -- 'Or's, captured by Choice above

  where
    unimplemented = panic "summariseG unimplemented" [showPP g]
    unexpected = panic "Unexpected" [showPP g]

summariseExpr :: AbsEnv ae => [AbsPred ae] -> Expr -> [(ae, AbsPred ae, SLExpr)]
summariseExpr preds e = [ (env, p, e') | p <- preds, let (env, e') = absPre p e ]

summariseBind :: AbsEnv ae =>
                 [AbsPred ae] -> NodeID -> Maybe Name -> Grammar -> Grammar ->
                 SummariseM ae (Domain ae)
summariseBind preds nid m_x lhs rhs = do
  rhsD <- summariseG preds rhs

  let (matching, rhsD') = partitionDomainForVar m_x rhsD
      preds'            = map snd matching
  lhsD <- summariseG preds' lhs

  let (lhsMatching, lhsD') = partitionDomainForResult (const True) lhsD
      -- For each element of lhsMatching we find the corresponding
      -- rhs(s) from matching, merge if there are multiple, and the
      -- bind them all together.
      --
      -- gsFor gets the rhs(s) for a particular result lhs
      gsFor gs = [ gs' | (gs', p) <- matching,
                   maybe True (`absPredEntails` p) (gsPred gs) ]

      els = [ bindGuardedSlice nid m_x gs gs'
            | gs <- lhsMatching
            , let gs' = foldl1 merge (gsFor gs)
            ]

      -- There are the elements which do not care about x.
      indepLHSD = mapSlices (\sl -> SDo m_x sl SHole) lhsD'
      indepRHSD = mapSlices (SDo m_x SHole) rhsD'
      final     = indepLHSD `merge` indepRHSD `merge` domainFromElements els

  -- fn <- currentDeclName
  -- when (showPP fn == "Main") $
  -- logMessage 3 ("** Summarising bind in " ++ showPP fn ++ "\n" ++
  --               show (nest 4 $ pp (Do x lhs rhs)) ++
  --               "\n" ++ show (hang ("lhsD: " <> brackets (commaSep (map pp preds'))) 4 (pp lhsD)) ++
  --               "\n" ++
  --               show (hang "lhs" 4 (bullets (map pp lhsMatching))) ++ "\n" ++
  --               show (hang "final" 4 (pp final)))

  pure final --  (indepLHSD `merge` indepRHSD `merge` domainFromElements els)


-- squashDomain' :: AbsEnv ae => String -> Domain ae -> SummariseM ae (Domain ae)
-- squashDomain' msg d = do
--   when (nelems > 1) $ logMessage 1 ("** " ++ msg ++ ": Merging " ++ show nelems ++ " elements")
--   when (nelems > 1) $ logMessage 2 ("** " ++ show (nest 4 (pp d')))
--   pure d'
--   where
--     d'     = squashDomain d
--     nelems = length (elements d)


structureFromLoopBody :: AbsEnv ae => Maybe (AbsPred ae) -> ae -> Structural
structureFromLoopBody m_pred env =
  maximum $ [StructureIndependent]
            ++ maybeToList (absPredStructural <$> m_pred)
            -- note that enve can be empty but we can care about the
            -- accumulator.
            ++ [ StructureIsNull | not (absNullEnv env) ]
            ++ [ StructureDependent | not (null (absNonStructural env)) ]
  
summariseMany :: AbsEnv ae => [AbsPred ae] -> NodeID -> 
                 Sem -> Backtrack -> Expr -> Maybe Expr -> Grammar ->
                 SummariseM ae (Domain ae)
summariseMany preds nid sem _bt lb m_ub g = do
  elsD <- summariseG (mapMaybe absPredListElement preds) g
  -- logMessage 1 ("** Many " ++ showPP elsD)

  -- Invariant: if preds is non-empty then either the domain
  -- isn't closed (because there is some predicate in preds with
  -- a non-Nothing element predicate), or there is some
  -- structural predicate in preds: (StructureIndependent,
  -- Nothing) isn't a valid list predicate.
  let (env, sl, elsD') = asSingleton elsD
      gsDom
        | closedDomain elsD && null preds = emptyDomain
        | otherwise = singletonDomain (makeSlice env sl)
  
  pure (gsDom `merge` elsD')
  
  where
    makeSlice env sl
      -- In this case we can ignore the bounds (FIXME: as far as I can
      -- reason).  For str to be StructureIndependent, the number of
      -- elements cannot impact the rest of the synthesis.
      | str == StructureIndependent = GuardedSlice
        { gsEnv = env
        , gsSlice = SLoop (SLoopPool sem sl)
        , gsPred = m_pred
        , gsDominator = nid
        }
      | otherwise = GuardedSlice
        { gsEnv = env `merge` envlb `merge` envub
        , gsSlice = SLoop (SManyLoop str slb m_sub sl) 
        , gsPred = m_pred
        , gsDominator = nid
        }
      where
        str = structureFromLoopBody m_pred env

    (envlb, slb) = absPre absPredTop lb
    m_env_ub = absPre absPredTop <$> m_ub
    envub = maybe absEmptyEnv fst m_env_ub
    m_sub = snd <$> m_env_ub

    -- need to lift back to list preds from element preds (gsPred
    -- for gs is for the element)
    m_pred = mergeMaybe preds

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
summariseRepeat :: AbsEnv ae => [AbsPred ae] ->
                   Name -> Expr -> Grammar ->
                   SummariseM ae (Domain ae)
summariseRepeat preds n e g = do
  -- gD is closed by construction, gsPred m_gs (if any) entails mergeMaybe preds
  (m_gs, gD) <- gssFixpoint n g preds
  case m_gs of
    Nothing -> pure gD
    Just gs -> do
      let (enve, sle) = maybe (absEmptyEnv, EHole (typeOf n)) (flip absPre e) (gsPred gs)
          useStr = maybe StructureIndependent (const StructureDependent) (gsPred gs)
          str = max useStr (structureFromLoopBody Nothing (gsEnv gs))
          gs' = gs { gsEnv   = gsEnv gs `merge` enve
                   -- We need to forget we have a post if, e.g., none was requested.
                   , gsPred  = mergeMaybe preds
                   , gsSlice = SLoop (SRepeatLoop str n sle (gsSlice gs))
                   }
      pure (merge (singletonDomain gs') gD)

summariseMorphism :: AbsEnv ae =>
                     [AbsPred ae] -> NodeID -> LoopMorphism Grammar ->
                     SummariseM ae (Domain ae)
summariseMorphism preds nid lm =
  case lm of
    -- Similar for Repeat above, with the additional use of elements.
    FoldMorphism n e lc g -> do
      (m_gs, gD) <- gssFixpoint n g preds
      -- logMessage 1 ("** Fold" ++
      --               "\n" ++ show (nest 4 (vcat (map (pp . fst) gss))) ++
      --               "\n" ++ show (nest 4 (pp gD)))
      case m_gs of
        Nothing -> pure gD
        Just gs -> do
          let (enve, sle) = maybe (absEmptyEnv, EHole (typeOf n)) (flip absPre e) (gsPred gs)
              str | Just _p <- gsPred gs        = StructureDependent -- absPredStructural p
                  | otherwise                   = StructureIndependent
              (env', m_lc_str') = summariseLoopCollection lc str (gsEnv gs)

              sl = case m_lc_str' of
                     -- We don't care about the lc, and hence gsPred
                     -- should be Nothing, as otherwise the structure would be important.
                     Nothing
                       | isNothing (gsPred gs) -> SLoopPool SemNo (gsSlice gs)
                       | otherwise -> panic "Expecting gsPred to be Nothing" []
                     Just (lc', _str') -> SMorphismLoop (FoldMorphism n sle lc' (gsSlice gs))
                           
              gs'' = gs { gsEnv = env' `merge` enve, gsSlice = SLoop sl }
              
          pure (merge (singletonDomain gs'') gD)

    MapMorphism lc g -> do
      gD <- summariseG (mapMaybe absPredListElement preds) g
      -- logMessage 1 ("** MapMorphism\n" ++ showPP gD)
      let (gss, gD') = domainElements gD
          m_gs = mergeMaybe gss

          mkMap env sl =
            let str = maybe StructureIndependent absPredStructural m_pred
                    
                (env', m_lc_str') = summariseLoopCollection lc str env
                sl' = case m_lc_str' of
                        -- We don't do the gsPred check here because it
                        -- isn't required/true.
                        Nothing -> SLoopPool SemYes sl
                        Just (lc', _str') -> SMorphismLoop (MapMorphism lc' sl)
                gs' = GuardedSlice { gsEnv = env'
                                   , gsPred = m_pred
                                   , gsSlice = SLoop sl'
                                   , gsDominator = nid
                                   }
              
            in merge (singletonDomain gs') gD'
      -- This is different to the Fold case as we can rely _just_ on
      -- the structure of the list, which fact needs to be propagated
      -- back to the loop collection.
      pure $ case m_gs of
               Nothing | isNothing m_pred -> gD'
               -- Otherwise, we have m_pred has no elements but exists.
               Nothing -> mkMap absEmptyEnv SHole
               Just gs -> mkMap (gsEnv gs) (gsSlice gs)
  where
    m_pred = mergeMaybe preds

summariseLoop :: AbsEnv ae =>
                 [AbsPred ae] -> NodeID -> LoopClass Grammar ->
                 SummariseM ae (Domain ae)
summariseLoop preds nid lcl =
  case lcl of
    ManyLoop sem bt lb m_ub g -> summariseMany preds nid sem bt lb m_ub g
    RepeatLoop _bt n e g -> summariseRepeat preds n e g
    MorphismLoop lm -> summariseMorphism preds nid lm

-- | There are 3 types of slices we care about here:
-- * sources: establish some p in ps, no dep. on x
-- * sinks: no post-conds, needs some q for x
-- * transfers: establishes some p, needs some q for x (q and p can differ)
--
-- Generally we want to find gslices s.t.
-- 1. all deps on x are entailed by post-cond; and
-- 2. at most 1 slice with structural deps.
--
-- For now we produce a single slice (if one exists); we could product
-- multiple slices if we support disjunctive slicing.
gssFixpoint :: AbsEnv ae => Name -> Grammar -> [AbsPred ae] ->
               SummariseM ae (Maybe (GuardedSlice ae), Domain ae)
gssFixpoint x g ps = do
  (matching, gD) <- repeatFixpoint ps
  let (nonDeps, gD') = domainElements gD
      gss = map fst matching ++ nonDeps
      -- ppgss = show . nest 4 . vcat . map pp
      
  -- logMessage 2 ("*** gssFixpoint: " ++ show (brackets (commaSep (map pp ps))) ++
              -- ppgss gss)

  pure (mergeMaybe gss, gD')

  -- let (justPost, gD') = partitionDomainForResult (const True) gD
  --     gss = mergeForDeps (map (\(gs, p) -> (gs, p, p)) matching) ( (, Nothing) <$> justPost)
  --     (strgss, nsgss) = partition (maybe False absPredIsStructural . snd) gss
  where
    -- First argument are those which depend on x, as (gs, dep, post),
    -- second arg. is those which have their precond (if any)
    -- resolved.  Invariant: at most one gsPreds entails any predicate
    -- in the worklist.
    -- mergeForDeps [] acc = acc
    -- mergeForDeps ((gs, depp, p) : rest) acc =
    --   let gsEntails = maybe False (`absPredEntails` p) . gsPred
    --       -- length inrest + length inacc <= 1.  If inrest and inacc
    --       -- are both empty, then the predicate should be entailed by
    --       -- gs.
    --       (inrest, outrest) = partition (gsEntails . view _1) rest
    --       (inacc, outacc)   = partition (gsEntails . view _1) acc
    --       -- If a gs in rest matches, we merge in the slices and
    --       -- re-add it to the wl using the matching predicate.
    --       rest' = [(gs `merge` gs', merge depp depp', p') | (gs', depp', p') <- inrest ] ++ outrest
    --       acc' | null inrest && null inacc = [(gs, Just depp)]                                          
    --            | otherwise = [(gs `merge` gs', merge (Just depp) m_p) | (gs', m_p) <- inacc ]
    --   in mergeForDeps rest' (acc' ++ outacc)
                      
    repeatFixpoint ps' = do
      gD <- summariseG ps' g
      
      let (matching, gD') = partitionDomainForVar (Just x) gD
          deps = map snd matching
          ps'' = mergeOverlapping absPredOverlaps deps ps'

      -- logMessage 2 ("*** repeatFixpoint: " ++ show (brackets (commaSep (map pp ps'))) ++
      --              "\n   Depends on bound var (" ++ showPP x ++
      --              ")\n" ++ (show . nest 4 . vcat . map (pp . fst) $ matching) ++
      --              "\n   Remainder\n" ++ show (nest 4 (pp gD')))
          
      -- looks expensive, although the lists should be pretty short.
      if all (\p -> any (`absPredEntails` p) ps') deps
        then pure (matching, gD') -- fixpoint reached
        else repeatFixpoint ps''

summariseLoopCollection :: AbsEnv ae => LoopCollection' Expr -> Structural -> ae ->
                           (ae, Maybe (LoopCollection' SLExpr, Structural))
summariseLoopCollection lc useStr env
  | Just lcp <- absPredCollection ty str m_kp m_elp =
      let (lcenv, sllc) = absPre lcp (lcCol lc)
      in ( env_no_lcvars `merge` lcenv
         , Just (lc {lcCol = sllc}, absPredStructural lcp)
         )
  | otherwise = (env_no_lcvars, Nothing)
  where
    (env_no_el, m_elp) = projectMaybe (lcElName lc) env
    (env_no_lcvars, m_kp)
      = maybe (env_no_el, Nothing) (flip projectMaybe env_no_el) (lcKName lc)

    str = max useStr (structureFromLoopBody Nothing env_no_el)
    projectMaybe x env' = maybe (env', Nothing) (second Just) (absProj x env')
    ty = typeOf (lcCol lc)
      
-- -----------------------------------------------------------------------------
-- Special patterns

-- pattern GuardP :: Expr -> Grammar -> Grammar
-- pattern GuardP e g <- (caseIsGuard -> Just (e, g))

-- Attempts to catch things ike
--
-- case b of
--   True  -> ...
--   False -> Fail ...

-- caseIsGuard :: Grammar -> Maybe (Expr, Grammar)
-- caseIsGuard (GCase (Case x cs)) =
--   case cs of
--     -- FIXME: Annot
--     [(PBool b, g), (_, Fail {})]       -> mk b g
--     [(PBool _, Fail {}), (PBool b, g)] -> mk b g
--     [(PBool b, Fail {}), (PAny, g)]    -> mk (not b) g
--     [(PBool b, g)]                     -> mk b g
--     _ -> Nothing
--   where
--     mk b g = Just (if b then Var x else eNot (Var x), g)
-- caseIsGuard _ = Nothing

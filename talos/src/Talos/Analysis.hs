{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-} -- for dealing with TCDecl and existential k

-- We walk through the each decl figuring out if it has things we
-- can't easily handle and thus need to send to the solver.  At the
-- moment this is any Guard.

module Talos.Analysis ( summarise
                      , Summary
                      ) where

import Control.Monad.State
import Data.List (inits)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set

import Daedalus.GUID
import Daedalus.PP
import Daedalus.Panic
import Daedalus.Core

import Talos.Analysis.Domain
import Talos.Analysis.EntangledVars
import Talos.Analysis.Monad
import Talos.Analysis.Slice

import Debug.Trace

--------------------------------------------------------------------------------
-- Top level function

summarise :: [Fun Grammar] -> GUID -> (Summaries, GUID)
summarise decls nguid  = (summaries s', nextGUID s')
  where
    s' = calcFixpoint s0 
    s0 = initState decls nguid 

--------------------------------------------------------------------------------
-- Summary functions

calcFixpoint :: IterState -> IterState
calcFixpoint s@(IterState { worklist = wl, allDecls = decls})
  | Just ((fn, cl), wl') <- Set.minView wl
  , Just decl <- Map.lookup fn decls -- should always succeed
  = calcFixpoint (-- traceShow ("Entering " <+> pp fn <+> pp cl) $
                  runIterM (summariseDecl cl decl) (s { worklist = wl' }))
         
-- Empty worklist
calcFixpoint s = s

summariseDecl :: SummaryClass -> Fun Grammar -> IterM ()
summariseDecl cls Fun { fName = fn
                      , fDef = Def def
                      , fParams = ps } = do
  IterM $ modify (\s -> s { currentDecl = fn, currentClass = cls })

  m_oldS <- lookupSummary fn cls
  
  newS   <- doSummary
  IterM $ modify (addSummary newS)
  propagateIfChanged newS m_oldS
  where
    -- Do we need to propagate?  We only do so if the argument domain
    -- changes (FIXME: is this sound?)
    propagateIfChanged newS (Just oldS)
      | domainEqv (exportedDomain oldS) (exportedDomain newS) = pure ()
    propagateIfChanged _newS _ = propagate fn cls
    
    -- We always insert the new summary, as even if the pre-domain
    -- hasn't changed, internal paths sets may have (which we could
    -- check if this is too expensive).
    --
    -- FIXME: we don't merge domains, as the
    -- summary should always get larger(?)
    addSummary summary s =
      let summaries' =
            Map.insertWith Map.union fn (Map.singleton cls summary) (summaries s)
      in s { summaries = summaries' }
    
    doSummary :: IterM Summary
    doSummary = do
      let m_ret = case cls of
            Assertions     -> Nothing
            FunctionResult -> Just ResultVar
                        
      (d, m) <- runSummariseM (summariseG m_ret def)
      
      pure (Summary { exportedDomain = d
                    , pathRootMap = m
                    , params = ps
                    , summaryClass = cls
                    })

summariseDecl _ _ = panic "Expecting a defined decl" []

--------------------------------------------------------------------------------
-- Decl-local monad 

data SummariseMState =
  SummariseMState { pathRoots :: PathRootMap }
  
emptySummariseMState :: SummariseMState
emptySummariseMState = SummariseMState Map.empty

newtype SummariseM a = SummariseM { getSummariseM :: StateT SummariseMState IterM a }
  deriving (Applicative, Functor, Monad)

runSummariseM :: SummariseM a -> IterM (a, PathRootMap)
runSummariseM m = evalStateT ((,) <$> getSummariseM m <*> gets pathRoots) emptySummariseMState

liftIterM :: IterM a -> SummariseM a
liftIterM = SummariseM . lift

addPathRoot :: Name -> Slice -> SummariseM ()
addPathRoot v fp = SummariseM $ modify (\s -> s { pathRoots = Map.insert v fp (pathRoots s) })

--------------------------------------------------------------------------------
-- Transfer function

-- Get the summary and link it to the actual arguments
--
-- For each element in expDom, we need to construct a domain element
-- by substituting the free variables in the argument for the
-- paramaters in the entanglement set.  E.g., if we have
--
-- f x y z :: { x: {x, y} => ... x ... y ...; ... }
--
-- and we have a call
--
-- f (a + 1) (b + c) q
--
-- then we produce a node like
--
-- {a, b, c} => Call m_res args {x}
--
-- Where {a, b, c} is the result of substituting the args in {x, y};
-- m_res is Nothing unless 'x' is ResultVar and m_x is not Nothing;
-- args are all the actuals (this is informative, and doesn't depend
-- on the analysis); and {x} is a name for the path set above
-- (indexed by the variable returned by explodeDomain).

summariseCall :: Maybe EntangledVar -> FName -> [Expr] -> SummariseM Domain
summariseCall m_x fn args = do
  -- We ignore rMap for this bit, it is only used during synthesis of
  -- the internal bytes.  If cl is FunctionResult then ResultVar will
  -- occur in expDom.
  Summary expDom _rMap ps _cl <- liftIterM $ requestSummary fn cl

  -- do d <- liftIterM $ currentDeclName
  --    traceShowM ("Calling" <+> pp fn <+> "from" <+> pp d $+$ pp expDom)

  -- We need to now substitute the actuals for the params in
  -- summary, and merge the results (the substitution may have
  -- introduced duplicates, so we need to do a pointwise merge)
  let argsMap    = Map.fromList $ zip ps args
      argsSubst  = freeEntangledVars <$> argsMap
      paramMap p =
        case p of
          ProgramVar v | Just evs <- Map.lookup v argsSubst -> evs
                       | otherwise -> panic "Missing parameter" [showPP v]
          ResultVar {} -> maybe mempty singletonEntangledVars m_x

      mkCallNode r (evs, sl) =
        CallNode { callClass        = cl
                 , callAllArgs      = argsMap
                 , callName         = fn
                 , callPaths        = Map.singleton r (CallInstance evs sl)
                 }

      mkCall r b@(evs, _) = 
        singletonDomain (substEntangledVars paramMap evs)
                        (SLeaf . SCall $ mkCallNode r b)
  
  pure $ Map.foldMapWithKey mkCall (explodeDomain expDom)

  where
    cl | isJust m_x = FunctionResult
       | otherwise  = Assertions

-- asSingleton :: Domain -> (EntangledVars, Slice)
-- asSingleton dom
--   | nullDomain dom = (mempty, SUnconstrained)
--   | [r] <- elements dom = r
--   | otherwise = panic "Saw non-singleton domain" [show (pp dom)]

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
-- summariseCase :: Maybe EntangledVar ->
--                  Grammar ->
--                  Expr ->
--                  [(Pattern, Grammar)] ->
--                  SummariseM Domain
-- summariseCase m_x tc e alts = do
--   declTys <- liftIterM declaredTypes
--   bDoms   <- mapM (summariseG m_x . tcAltBody) alts'
--   defDom  <- traverse (summariseG m_x) m_def
--   let (caseClass, cSummary) = PC.summariseCase declTys tc
--       trivial   = all nullDomain (maybe emptyDomain id defDom : bDoms)
--       total     = caseClass == PC.Complete
      
--   if trivial && total then pure emptyDomain else do
--     -- We have a non-trivial node, so we construct a singleton domain.
--     -- This breaks the domain abstraction, but it is a bit simpler to
--     -- write like this.
--     let (altVs, altFPs) = unzip $ zipWith mkAltPath alts' (map squashDomain bDoms)
--         m_defVF = asSingleton . squashDomain <$> defDom
--         vs     = mergeEntangledVarss (tcEntangledVars e : maybe mempty fst m_defVF : altVs)
--         cNode = CaseNode { caseCompleteness = caseClass
--                          , caseSummary      = cSummary
--                          , caseTerm         = e
--                          , caseAlts         = NE.fromList altFPs
--                          , caseDefault      = snd <$> m_defVF
--                          } 
--     pure (singletonDomain vs (SCase cNode))
--   where
--     -- We have at least 1 non-empty domain (c.f. trivial)
--     mkAltPath alt dom =
--       let (fvs, fps) = asSingleton dom
--           binds      = altBinds alt -- singleton or empty
--       in (foldr deleteEntangledVar fvs (map ProgramVar binds)
--          , SAlt (tcAltPatterns alt) fps)
         
--     alts' = NE.toList alts
      

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

summariseG :: Maybe EntangledVar -> -- Do we want the result of tc or not?
              Grammar -> SummariseM Domain
summariseG m_x tc = do
  -- d <- liftIterM $ currentDeclName
  -- cl <- liftIterM $ currentSummaryClass
  
  -- traceShowM ((pp d <> "@" <> pp cl) <+> maybe "Nothing" ((<+>) "Just" . pp) m_x <+> braces (commaSep $ map pp (Set.toList frees)) <+> pp tc)

  case tc of
    Pure v    -> simple (SPure v)
    GetStream    -> unimplemented
    SetStream {} -> unimplemented
    Match _ m -> simple (SMatch m)
    Fail {}   -> unimplemented -- FIXME: we will probably handle this specially in branching code
    
    Do_ lhs rhs -> do
      rhsD <- dontCareD 1 <$> summariseG m_x rhs
      merge rhsD <$> summariseG Nothing lhs
    
    Do x' lhs rhs -> do
      -- we add the dontCare to leave a spot to merge in the dom for lhs
      rhsD <- dontCareD 1 <$> summariseG m_x rhs
      if memberVar (ProgramVar x') rhsD
        then do
        -- we care about the variable, so we need a FunctionResult summary
          let ex' = ProgramVar x'
          lhsD <- summariseG (Just ex') lhs
          let fromLeaf (SLeaf sl') = sl'
              fromLeaf sl'         = panic "Expecting leaf" [showPP sl']
              -- FIXME
              lhsD' = mapDomain (\evs sl -> if memberEntangledVars ex' evs
                                            then SDo (Just x') (fromLeaf sl) SUnconstrained
                                            else sl) lhsD
              -- inefficient, but simple
              dom = merge rhsD lhsD'

          -- x' should always be assigned as we check above.
          let (Just (ns, sl), dom') = splitOnVar ex' dom
            
          -- traceM (show $ "in" <+> pp d <+> vcat [ "var" <+> pp x' <+> "size" <+> pp (sizeEntangledVars ns)
          --                                       , "binding" <+> pp ns <+> pp sl
          --                                       , "dom"  <+> pp dom
          --                                       , "lhsD" <+> pp lhsD
          --                                       , "rhsD" <+> pp rhsD
          --                                       ])
            
          -- if ns contains just x', then x' is the root of this path.
          if sizeEntangledVars ns == 1
            then dom' <$ addPathRoot x' sl
            else pure (primAddDomainElement (deleteEntangledVar ex' ns, sl) dom')
            
        -- There is no variable, or no path from here is entangled with it
        else merge rhsD <$> summariseG Nothing lhs

    Let n e rhs -> summariseG m_x (Do n (Pure e) rhs) -- FIXME: this is a bit of a hack

    -- doms contains a domain for each path in the choose. We create a
    -- diagonal list of domains, like
    --
    --  [ Just Unconstrained, ... , fp, ... Just Unconstrained]
    --
    -- and then merge
    --
    Choice _biased gs -> do
      doms <- mapM (summariseG m_x) gs
      let mkOne p s fp = SLeaf (SChoice (p ++ [fp] ++ s))
          mk p d' s = mapDomain (\_ -> mkOne p s) d'
          doms' = diagonalise SUnconstrained doms mk
      pure (squashDomain $ mconcat doms') -- FIXME: do we _really_ have to squash here?

    Call fn args  -> summariseCall m_x fn args
    Annot _ g     -> summariseG m_x g

    -- Cases
    GuardP e g    -> do
      rhsD <- dontCareD 1 <$> summariseG m_x g
      let lhsD = singletonDomain (freeEntangledVars e) (SLeaf (SAssertion (GuardAssertion e)))
      pure (merge lhsD rhsD)
    
    GCase _cs      -> unimplemented -- SLeaf . SCase <$> traverse (summariseG 
    _ -> panic "impossible" [] -- 'Or's, captured by Choice above
    
  --   Choice _c gs _t -> do
  --     when (null gs) $ error "empty list of choices"
  --     doms <- mapM (summariseG m_x) gs

  --     -- doms contains a domain for each path in the choose. We create
  --     -- a diagonal list of domains, like
  --     --
  --     --  [ Just Unconstrained, ... , fp, ... Just Unconstrained]
  --     --
  --     -- and then merge
  --     --
  --     let mkOne p s fp = SChoice (p ++ [fp] ++ s)
  --         mk p d' s = mapDomain (\_ -> mkOne p s) d'
  --         doms' = diagonalise SUnconstrained doms mk
  --     pure (squashDomain $ mconcat doms') -- FIXME: do we _really_ have to squash here?
  where
    simple n
      | Just x <- m_x = pure (singletonDomain (insertEntangledVar x (freeEntangledVars tc))
                                              (SLeaf n))
      | otherwise     = pure emptyDomain
    
    unimplemented = panic "summariseG unimplemented" [showPP tc]

diagonalise :: a -> [b] -> ([a] -> b -> [a] -> c) -> [c]
diagonalise el xs f =
  let pfxs = inits (replicate (length xs - 1) el)
      sfxs = reverse pfxs
  in zipWith3 f pfxs xs sfxs 
   
 
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
caseIsGuard (GCase (Case e cs)) =
  case cs of
    -- FIXME: Annot
    [(PBool b, g), (_, Fail {})]       -> mk b g
    [(PBool _, Fail {}), (PBool b, g)] -> mk b g
    [(PBool b, Fail {}), (PAny, g)]    -> mk (not b) g    
    [(PBool b, g)]                     -> mk b g
    _ -> Nothing
  where
    mk b g = Just (if b then e else eNot e, g)
caseIsGuard _ = Nothing

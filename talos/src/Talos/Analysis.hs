{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
 -- for dealing with TCDecl and existential k

-- We walk through the each decl figuring out if it has things we
-- can't easily handle and thus need to send to the solver.  At the
-- moment this is any Guard.

module Talos.Analysis ( summarise
                      , Summary
                      -- FIXME: move
                      ) where

import Control.Monad.State
import Data.List (inits)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe ( isJust )

import Daedalus.GUID
import Daedalus.PP
import Daedalus.Panic

import Daedalus.Core
import Daedalus.Core.Type

import Talos.Analysis.Domain
import Talos.Analysis.EntangledVars
import Talos.Analysis.Monad
import Talos.Analysis.Slice
import Talos.Analysis.Projection (freeEntangledVars, freeVarsToEntangledVars)

-- import Debug.Trace

--------------------------------------------------------------------------------
-- Top level function

summarise :: Module -> GUID -> (Summaries, GUID)
summarise md nguid = (summaries, nextGUID s')
  where
    (s', summaries) = calcFixpoint seqv doOne wl0 s0
    s0    = initState (mGFuns md) (mFFuns md) nguid

    seqv oldS newS = domainEqv (exportedDomain oldS) (exportedDomain newS)
    
    doOne nm cl
      | Just decl <- Map.lookup nm decls = summariseDecl cl decl
      | otherwise = panic "Missing decl" [showPP nm]

    wl0   = Set.fromList grammarDecls
    
    decls = Map.fromList (map (\tc -> (fName tc, tc)) (mGFuns md))
    
    grammarDecls =
      [ (name, Assertions)
      | Fun { fName = name, fDef = Def _ } <- mGFuns md
      ]

--------------------------------------------------------------------------------
-- Summary functions

summariseDecl :: SummaryClass -> Fun Grammar -> IterM Summary
summariseDecl cls Fun { fName = fn
                      , fDef = Def def
                      , fParams = ps } = do
  let m_ret = case cls of
        Assertions           -> Nothing
        FunctionResult fsets -> Just (ResultVar, fsets)

  (d, m) <- runSummariseM (summariseG m_ret def)

  let newS = Summary { exportedDomain = d
                     , pathRootMap = m
                     , params = ps
                     , summaryClass = cls
                     }
  -- Sanity check
  unless (domainInvariant (exportedDomain newS)) $
    panic "Failed domain invariant" ["At " ++ showPP fn]
             
  pure newS

summariseDecl _ _ = panic "Expecting a defined decl" []

--------------------------------------------------------------------------------
-- Decl-local monad 

newtype SummariseMState =
  SummariseMState { pathRoots :: PathRootMap }
  
emptySummariseMState :: SummariseMState
emptySummariseMState = SummariseMState Map.empty

newtype SummariseM a = SummariseM { getSummariseM :: StateT SummariseMState IterM a }
  deriving (Applicative, Functor, Monad)

runSummariseM :: SummariseM a -> IterM (a, PathRootMap)
runSummariseM m = evalStateT ((,) <$> getSummariseM m <*> gets pathRoots) emptySummariseMState

liftIterM :: IterM a -> SummariseM a
liftIterM = SummariseM . lift

-- v should not already be mapped
addPathRoot :: Name -> [(FieldSet, Slice)] -> SummariseM ()
addPathRoot _v []  = pure ()
addPathRoot v  sls = SummariseM $ modify (\s -> s { pathRoots = Map.insert v sls (pathRoots s) })

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


-- Given a call F(e1, e2, e3) to F(x, y, z) with domain
--
-- { {x.a.b, x.c, y.a} |-> sl1 , {x.d, y.c} |-> sl2 }
--
-- we need to construct the new var sets for the derived domain
-- (i.e. where the node is Call F) by figuring out the free evs in the
-- arguments w.r.t the domain field sets, so in the above we have
--
-- { e1/{a.b, c}, e2/{a} } and { e1/{d}, e2/{c} }
--
-- noting that two resulting var sets may overlap.

substituteArgs :: Map Name Expr -> Maybe (BaseEntangledVar, a) ->
                  EntangledVars -> EntangledVars
substituteArgs argsMap m_x evs =
  substEntangledVars evs $ \bv fset ->
    case bv of
      -- fset here is the result of the analysis, fset' was the
      -- requested field set, so it should be contained in fset, but
      -- may not be equal (if the analysis overapproximated, for
      -- example)
      ResultVar
        | Just (x, _fsets) <- m_x -> singletonEntangledVars x fset
        | otherwise -> mempty

      ProgramVar n
        | Just e <- Map.lookup n argsMap -> freeEntangledVars fset e
        | otherwise -> panic "Missing parameter" [showPP n]

summariseCall :: Maybe (BaseEntangledVar, Set FieldSet) -> FName -> [Expr] ->
                 SummariseM Domain
summariseCall m_x fn args = do
  m_invs <- liftIterM (getDeclInv fn)
  case m_invs of
    -- We only really care about inverses if we have a result
    -- (probably otherwise pure?)
    Just f | Just (x, _) <- m_x -> do -- FIXME: we ignore the projection (we over project)
      n <- liftIterM (freshNameSys (fnameType fn))
      let (ifn, pfn) = f n args
      let sl  = SLeaf $ SInverse n ifn pfn
          evs0 = freeEntangledVars emptyFieldSet ifn
                <> freeEntangledVars emptyFieldSet pfn
          evs  = snd $ deleteBaseEV (ProgramVar n) evs0
          
      pure $ singletonDomain (singletonEntangledVars x emptyFieldSet <> evs) sl
      
    _ -> do
      -- We ignore rMap for this bit, it is only used during synthesis of
      -- the internal bytes.  If cl is FunctionResult then ResultVar will
      -- occur in expDom.
      Summary expDom _rMap ps _cl <- liftIterM $ requestSummary fn cl

      -- do d <- liftIterM $ currentDeclName
      --    traceShowM ("Calling" <+> pp fn <+> "from" <+> pp d $+$ pp expDom)

      -- We need to now substitute the actuals for the params in
      -- summary, and merge the results (the substitution may have
      -- introduced duplicates, so we need to do a pointwise merge)
      let argsMap    = Map.fromList $ zip ps args -- FIXME: This gets recomputed for each fset
          mkCallNode (evs, _sl) =
            CallNode { callClass        = cl
                     , callAllArgs      = argsMap
                     , callName         = fn
                     , callPaths        = Set.singleton evs {- sl -}
                     }

          mkCall b@(evs, _) =
            singletonDomain (substituteArgs argsMap m_x evs)
                            (SLeaf . SCall $ mkCallNode b)

      pure $ foldMap mkCall (explodeDomain expDom)
      
  where
    cl | Just (_, fsets) <- m_x = FunctionResult fsets
       | otherwise              = Assertions

asSingleton :: Domain -> (EntangledVars, Slice)
asSingleton dom
  | nullDomain dom = (mempty, SUnconstrained)
  | [r] <- elements dom = r
  | otherwise = panic "Saw non-singleton domain" [show (pp dom)]

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

summariseCase :: Maybe (BaseEntangledVar, Set FieldSet) ->
                 Case Grammar ->
                 SummariseM Domain
summariseCase m_x cs@(Case e alts) = do
  bDoms   <- mapM (summariseG m_x . snd) alts
  let trivial   = all nullDomain bDoms
      total     = caseIsTotal cs

  if trivial && total then pure emptyDomain else do
    -- We have a non-trivial node, so we construct a singleton domain.
    -- This breaks the domain abstraction, but it is a bit simpler to
    -- write like this.
    let pats = map fst alts
        (vs, els) = unzip (map (asSingleton . squashDomain) bDoms)
        cs'       = Case e (zip pats els)
        vs'       = mergeEntangledVarss (freeEntangledVars emptyFieldSet e : vs)
    pure (singletonDomain vs' (SLeaf (SCase total cs')))

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

summariseG :: Maybe (BaseEntangledVar, Set FieldSet) ->
              -- The assigned variable, along with fields we care
              -- about (the set of projections is non-empty, it can
              -- contain [emptyFieldSet] representing the entire
              -- object)
              Grammar -> SummariseM Domain
summariseG m_x tc =
  case tc of
    Pure e
      | Just (x, fsets) <- m_x -> pure $ mconcat
          [ singletonDomain (singletonEntangledVars x fset <> freeEntangledVars fset e)
                            (SLeaf (SPure fset e))
          | fset <- Set.toList fsets ]
      | otherwise     -> pure emptyDomain
    GetStream    -> unimplemented
    SetStream {} -> unimplemented
    Match _ (MatchByte bset)
        -- fsets should be {emptyFieldSet} here as we are returning a byte
      | Just (x, _fsets) <- m_x ->
          pure $ singletonDomain (singletonEntangledVars x emptyFieldSet
                                  <> freeVarsToEntangledVars bset)
                                 (SLeaf (SMatch bset))
      | otherwise -> pure emptyDomain
    Match {} | isJust m_x -> panic "Saw a relevant match" [showPP tc]
             | otherwise  -> pure emptyDomain
    Fail {}   -> unimplemented -- FIXME: we will probably handle this specially in branching code

    Do_ lhs rhs -> do
      rhsD <- dontCareD 1 <$> summariseG m_x rhs
      merge rhsD <$> summariseG Nothing lhs

    Do x' lhs rhs -> do
      -- we add the dontCare to leave a spot to merge in the dom for lhs
      rhsD <- dontCareD 1 <$> summariseG m_x rhs
      let ex' = ProgramVar x'
      case domainFileSets ex' rhsD of
        -- There is no variable, or no path from here is entangled with it
        [] -> merge rhsD <$> summariseG Nothing lhs
        fset -> do
          -- we care about the variable, so we need a FunctionResult summary
          lhsD <- summariseG (Just (ex', Set.fromList fset)) lhs
          let lhsD' = mapDomain (bindBaseEV x') lhsD
              -- inefficient, but simple
              dom = merge rhsD lhsD'

          let (newRoots, dom') = splitRemoveVar ex' dom
          dom' <$ addPathRoot x' newRoots

          -- d <- liftIterM $ currentDeclName

          -- traceM (show $ "in" <+> pp d <+> vcat [ "var" <+> pp x' <+> "size" <+> pp (sizeEntangledVars ns)
          --                                       , "binding" <+> pp ns <+> pp sl
          --                                       , "dom"  <+> pp dom
          --                                       , "lhsD" <+> pp lhsD
          --                                       , "rhsD" <+> pp rhsD
          --                                       ])

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
      let lhsD = singletonDomain (freeEntangledVars emptyFieldSet e)
                                 (SLeaf (SAssertion (GuardAssertion e)))
      pure (merge lhsD rhsD)

    GCase cs      -> summariseCase m_x cs
    _ -> panic "impossible" [] -- 'Or's, captured by Choice above

  where

    unimplemented = panic "summariseG unimplemented" [showPP tc]

    bindBaseEV x evs sl
      | Just _ <- lookupBaseEV (ProgramVar x) evs = SDo (Just x) sl SUnconstrained
      | otherwise                                 = sl

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

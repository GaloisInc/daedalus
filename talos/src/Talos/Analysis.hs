{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-} -- for dealing with TCDecl and existential k

-- We walk through the each decl figuring out if it has things we
-- can't easily handle and thus need to send to the solver.  At the
-- moment this is any Guard.

module Talos.Analysis ( summarise
                      , Summary
                      ) where

import Data.Map (Map)

import Control.Monad.State
import Data.List (inits)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set

import Daedalus.GUID
import Daedalus.PP
import Daedalus.Panic
import Daedalus.Type.AST
import qualified Daedalus.Type.PatComplete as PC 

import Talos.Analysis.Domain
import Talos.Analysis.EntangledVars
import Talos.Analysis.Monad
import Talos.Analysis.PathSet

--------------------------------------------------------------------------------
-- Top level function

summarise :: Map TCTyName TCTyDecl ->
             [Name] -> [TCDecl TCSynthAnnot] -> GUID -> (Summaries, GUID)
summarise declTys _roots decls nguid  = (summaries s', nextGUID s')
  where
    s' = calcFixpoint s0 
    s0 = initState declTys decls nguid 

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

summariseDecl :: SummaryClass -> TCDecl TCSynthAnnot ->  IterM ()
summariseDecl cls TCDecl { tcDeclName = fn
                         , tcDeclDef = Defined def
                         , tcDeclCtxt = AGrammar
                         , tcDeclParams = ps } = do
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
      let ty = case typeOf def of
            Type (TGrammar ty') -> ty'
            ty' -> panic "Expecting a Grammar type" [showPP ty']
            
          m_ret = case cls of
            Assertions     -> Nothing
            FunctionResult -> Just (ResultVar ty)
            
          ps'    = map paramToValParam ps
            
      (d, m) <- runSummariseM (summariseG m_ret id def)
      
      pure (Summary { exportedDomain = d
                    , pathRootMap = m
                    , params = ps'
                    , summaryClass = cls
                    })

    paramToValParam (ValParam v) = v
    paramToValParam _ = panic "paramToValParam" []

summariseDecl _ _ = panic "Expecting a grammar-level, defined, decl" []


-- summariseCtors :: Predicate -> [(Label, TC a Value)] -> IterM Assertion
-- summariseCtors Top cs         = mconcat <$> mapM (summariseTC Top . snd) cs
-- summariseCtors (Fields fs) cs = mconcat <$> mapM go cs
--   where
--     go (l, v) | Just p <- Map.lookup l fs = summariseTC p v
--     go _ = pure emptyAssertion

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

addPathRoot :: TCName Value -> FuturePathSet TCSynthAnnot -> SummariseM ()
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

summariseCall :: Maybe EntangledVar -> TCName Grammar -> [Arg TCSynthAnnot]
              -> SummariseM (Domain TCSynthAnnot)
summariseCall m_x fn args = do
  -- We ignore rMap for this bit, it is only used during synthesis of
  -- the internal bytes.  If cl is FunctionResult then ResultVar will
  -- occur in expDom.
  Summary expDom _rMap ps _cl <- liftIterM $ requestSummary (tcName fn) cl

  -- do d <- liftIterM $ currentDeclName
  --    traceShowM ("Calling" <+> pp fn <+> "from" <+> pp d $+$ pp expDom)

  -- We need to now substitute the actuals for the params in
  -- summary, and merge the results (the substitution may have
  -- introduced duplicates, so we need to do a pointwise merge)
  let argsMap    = Map.fromList $ zip ps (map argToVal args)
      argsSubst  = tcEntangledVars <$> argsMap
      paramMap p =
        case p of
          ProgramVar v | Just evs <- Map.lookup v argsSubst -> evs
                       | otherwise -> panic "Missing parameter" [showPP v]
          ResultVar {} -> maybe mempty singletonEntangledVars m_x

      mkCallNode r body =
        let m_res = case r of
              ResultVar {} -> m_x
              _            -> Nothing
        in CallNode { callClass = cl
                    , callResultAssign = m_res
                    , callAllArgs      = argsMap
                    , callName         = tcName fn
                    , callPaths        = Map.singleton r (Wrapped body)
                    }

      mkCall r b@(evs, _) = 
        singletonDomain (substEntangledVars paramMap evs)
                        (PathNode (Call $ mkCallNode r b) Unconstrained)
  
  pure $ Map.foldMapWithKey mkCall (explodeDomain expDom)

  where
    cl | isJust m_x = FunctionResult
       | otherwise  = Assertions
    
    argToVal (ValArg v) = v
    argToVal _ = panic "Shoudn't happen: summariseCall nonValue" []

asSingleton :: Domain a -> (EntangledVars, FuturePathSet a)
asSingleton dom
  | nullDomain dom = (mempty, Unconstrained)
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
summariseCase :: Maybe EntangledVar ->
                 TC TCSynthAnnot Grammar ->
                 TC TCSynthAnnot Value ->
                 NonEmpty (TCAlt TCSynthAnnot Grammar) ->
                 Maybe (TC TCSynthAnnot Grammar) ->
                 SummariseM (Domain TCSynthAnnot)
summariseCase m_x tc e alts m_def = do
  declTys <- liftIterM declaredTypes
  bDoms   <- mapM (summariseG m_x id . tcAltBody) alts'
  defDom  <- traverse (summariseG m_x id) m_def
  let (caseClass, cSummary) = PC.summariseCase declTys tc
      trivial   = all nullDomain (maybe emptyDomain id defDom : bDoms)
      total     = caseClass == PC.Complete
      
  if trivial && total then pure emptyDomain else do
    -- We have a non-trivial node, so we construct a singleton domain.
    -- This breaks the domain abstraction, but it is a bit simpler to
    -- write like this.
    let (altVs, altFPs) = unzip $ zipWith mkAltPath alts' (map squashDomain bDoms)
        m_defVF = asSingleton . squashDomain <$> defDom
        vs     = mergeEntangledVarss (tcEntangledVars e : maybe mempty fst m_defVF : altVs)
        cNode = CaseNode { caseCompleteness = caseClass
                         , caseSummary      = cSummary
                         , caseTerm         = e
                         , caseAlts         = NE.fromList altFPs
                         , caseDefault      = snd <$> m_defVF
                         } 
    pure (singletonDomain vs (PathNode (FNCase cNode) Unconstrained))
  where
    -- We have at least 1 non-empty domain (c.f. trivial)
    mkAltPath alt dom =
      let (fvs, fps) = asSingleton dom
          binds      = altBinds alt -- singleton or empty
      in (foldr deleteEntangledVar fvs (map ProgramVar binds)
         , FNAlt (tcAltPatterns alt) fps)
         
    alts' = NE.toList alts
  --         mkCase alts' m_def'
  --           = PathNode (FNCase isMissing e alts' m_def') Unconstrained
  --         mkAlt alt = FNAlt (tcAltPatterns alt)
  --         mkCase' p s fp =
  --           mkCase (NE.zipWith mkAlt alts (NE.fromList (p ++ [fp] ++ s)))
  --                  (const Unconstrained <$> m_def)
  --         mk p d s  = mapDomain (mkCase' p s) d
  --         bdoms'  = diagonalise Unconstrained bdoms mk
      
  --     defDom <- mapDomain (\fp -> mkCase (replicate (NE.length alts) Unconstrained))
  --                         <$> 
                
  --     pure (mconcat (defDom : bdoms'))
      

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

summariseMany :: Maybe EntangledVar ->
                 TC TCSynthAnnot Grammar ->
                 ManyBounds (TC TCSynthAnnot Value) ->
                 TC TCSynthAnnot Grammar ->
                 SummariseM (Domain TCSynthAnnot)
summariseMany m_x _tc bnds body = do
  -- We squash as we need to unify the domain vars with the frees
  -- in the bounds, modulo m_x
  bodyD <- squashDomain <$> summariseG m_ret id body
  -- bodyD has 0 or 1 elements
  if nullDomain bodyD then pure emptyDomain else do
    let (evs, fps) = asSingleton bodyD
        evs'       = subst_x evs
        node       = ManyNode { manyResultAssign = m_x
                              , manyBounds       = bnds
                              , manyFrees        = maybe evs (flip deleteEntangledVar evs) m_ret
                              , manyBody         = fps
                              }
    pure $ singletonDomain (mergeEntangledVars evs' (tcEntangledVars bnds))
                           (PathNode (FNMany node) Unconstrained)
    where
      (m_ret, subst_x) = case m_x of
        Nothing -> (Nothing, id)
        Just x  -> let ret = ResultVar (typeOf x)
                       rSubst y
                         | y == ret  = singletonEntangledVars x
                         | otherwise = singletonEntangledVars y
                   in (Just ret, substEntangledVars rSubst)

-- This calculates the pathset for a grammar.  The first argument is
-- what parts (if any) we care about for the result of this function
-- (in general we only care about this for the final statement,
-- everything else is handled using path sets).

summariseG :: Maybe EntangledVar -> -- ^ the variable to bind the result of this do block
              (FuturePathSet TCSynthAnnot -> FuturePathSet TCSynthAnnot) -> -- ^ A wrapper for nested do blocks
              TC TCSynthAnnot Grammar -> SummariseM (Domain TCSynthAnnot)
summariseG m_x doWrapper tc = do
  -- d <- liftIterM $ currentDeclName
  -- cl <- liftIterM $ currentSummaryClass
  
  -- traceShowM ((pp d <> "@" <> pp cl) <+> maybe "Nothing" ((<+>) "Just" . pp) m_x <+> braces (commaSep $ map pp (Set.toList frees)) <+> pp tc)

  case texprValue tc of
    TCDo m_x' lhs rhs -> do
      -- we add the dontCare to leave a spot to merge in the dom for lhs
      rhsD <- dontCareD 1 <$> summariseG m_x id rhs
      mapDomain doWrapper <$> case m_x' of
        -- we care about the variable, so we need to assign it in lhs.
        Just x' | Just _ <- lookupVar (ProgramVar x') rhsD -> do
          let ev = ProgramVar x'          
          lhsD <- summariseG (Just ev) wrapNested lhs
          -- inefficient, but simple
          let dom = mergeDomain rhsD lhsD
          -- x' should always be assigned in lhsD
          let (Just (ns, fp), dom') = splitOnVar ev dom
          -- traceM (show $ "in" <+> pp d <+> vcat [ "var" <+> pp x' <+> "size" <+> pp (sizeEntangledVars ns)
          --                                       , "binding" <+> pp ns <+> pp fp
          --                                       , "dom"  <+> pp dom
          --                                       , "lhsD" <+> pp lhsD
          --                                       , "rhsD" <+> pp rhsD
          --                                       ])
          -- if ns contains just x', then x' is the root of this path.
          if sizeEntangledVars ns == 1
            then do addPathRoot x' fp
                    pure dom'
            else pure (primAddDomainElement (deleteEntangledVar ev ns, fp) dom')
            
        -- There is no variable, or no path from here is entangled with it
        _ -> mergeDomain rhsD <$> summariseG Nothing wrapNested lhs
            
    TCLabel _ g     -> summariseG m_x doWrapper g

    TCGuard b       -> mkNode (Assertion (GuardAssertion b))
      
    TCMatchBytes {} -> simple
    TCPure {}       -> simple
    TCGetByte {}    -> simple
    TCMatch {}      -> simple

    TCChoice _c gs _t -> do
      when (null gs) $ error "empty list of choices"
      doms <- mapM (summariseG m_x id) gs

      -- doms contains a domain for each path in the choose. We create
      -- a diagonal list of domains, like
      --
      --  [ Just Unconstrained, ... , fp, ... Just Unconstrained]
      --
      -- and then merge
      --
      let mkOne p s fp = PathNode (Choice (p ++ [fp] ++ s)) Unconstrained
          mk p d' s = mapDomain (mkOne p s) d'
          doms' = diagonalise Unconstrained doms mk
      pure (squashDomain $ mconcat doms') -- FIXME: do we _really_ have to squash here?
      
    TCOptional {}      -> unimplemented

    TCMany _s _c bnds body -> summariseMany m_x tc bnds body 
      -- | Just _ <- m_x -> panic "UNIMPLEMENTED: Relevant many is currently unsupported" [ show (pp tc), show (pp d), show (pp cl) ]
      -- | otherwise -> do
      --     bodyD <- summariseG Nothing id body
      --     unless (nullDomain bodyD) (panic "UNIMPLEMENTED: Non-empty domain for Many" [ show (pp tc), show (pp d), show (pp cl) ])
      --     pure emptyDomain

    TCEnd              -> unimplemented
    TCOffset           -> unimplemented

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
    TCCoerceCheck   {} -> simple

    -- destructors
    TCFor           {} -> unimplemented

    TCVar           {} -> error "Saw a grammar-valued variable"

    -- Should be no type args
    TCCall fn _ args -> summariseCall m_x fn args
    
    TCCase e alts m_def -> summariseCase m_x tc e alts m_def
    
    TCErrorMode     {} -> unimplemented
    TCFail {}          -> unimplemented
  where
    frees = maybe id insertEntangledVar m_x $ tcEntangledVars tc

    mkNode n = pure (singletonDomain frees (PathNode n Unconstrained))
    -- These correspond more or less to isSimpleTC
    simple
      | Just v <- m_x = mkNode (SimpleNode v tc)
      | otherwise     = pure emptyDomain

    wrapNested fps = PathNode (NestedNode fps) Unconstrained
    
    unimplemented = error ("Unimplemented: " ++ show (pp tc))


diagonalise :: a -> [b] -> ([a] -> b -> [a] -> c) -> [c]
diagonalise el xs f =
  let pfxs = inits (replicate (length xs - 1) el)
      sfxs = reverse pfxs
  in zipWith3 f pfxs xs sfxs 
   
 

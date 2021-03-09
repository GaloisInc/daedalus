{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving, ScopedTypeVariables, RankNTypes #-}

-- | Symbolically execute a fragment of DDL
module Talos.SymExec ( solverSynth
                     , symExecTy, symExecTDecl, symExecSummaries, symExecSummary
                     ) where


import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (find, foldrM)

-- import qualified Data.Text as T

import SimpleSMT (SExpr, Solver)
import qualified SimpleSMT as S

import Daedalus.Panic
import Daedalus.PP
import Daedalus.Rec

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.Type

import Talos.SymExec.Monad
import Talos.SymExec.ModelParser
import Talos.SymExec.Core
import Talos.SymExec.StdLib
import Talos.SymExec.Path

import Talos.Analysis.Domain
import Talos.Analysis.EntangledVars
import Talos.Analysis.Slice
import Talos.Analysis.Monad (Summaries, Summary(..))

--------------------------------------------------------------------------------
-- Solver synthesis

solverSynth :: Solver -> SummaryClass -> Name -> ProvenanceTag -> Slice -> IO SelectedPath
solverSynth s cl root prov sl = S.inNewScope s $ do
  model <- S.declare s modelN tModel
  S.assert s (S.fun "is-success" [ S.fun (mkPredicateN cl root) [model]])
  r <- S.check s
  case r of
    S.Sat -> pure ()
    -- FIXME: pretty gross
    S.Unsat   -> error "SMT solver result: Unsat"
    S.Unknown -> error "SMT solver result: Unknown"

  sexp <- getValue modelN  

  case evalModelP (parseModel prov sl) sexp of
    [] -> panic "No parse" []
    sp : _ -> pure sp
    
  where
    getValue :: String -> IO SExpr
    getValue a = do
      res <- S.command s $ S.fun "get-value" [S.List [S.const a]]
      case res of
        S.List [S.List [_, v]] -> pure v
        _ -> panic (unlines
                    [ "Unexpected response from the SMT solver:"
                    , "  Exptected: a value"
                    , "  Result: " ++ S.showsSExpr res ""
                    ]) []

-- This has to follow symExecFuturePathSet
parseModel :: ProvenanceTag -> Slice -> ModelP SelectedPath
parseModel prov = go 
  where
    go sl =
      case sl of
        SDontCare n sl' -> dontCare n <$> go sl'
        SDo _ (SPure {})      rhs -> dontCare 1 <$> go rhs
        SDo _ (SAssertion {}) rhs -> dontCare 1 <$> go rhs
        SDo _ lhs             rhs -> uncurry PathNode <$> pSeq (parseLeafModel prov lhs) (go rhs)
        SUnconstrained            -> pure Unconstrained
        SLeaf (SPure {})          -> pure Unconstrained
        SLeaf (SAssertion {})     -> pure Unconstrained
        SLeaf sl'                 -> flip PathNode Unconstrained <$> parseLeafModel prov sl'
        
parseLeafModel :: ProvenanceTag -> SliceLeaf -> ModelP SelectedNode
parseLeafModel prov sl = 
  case sl of
    SMatch {}   -> SelectedMatch prov <$> pBytes
    SChoice sls -> pIndexed (\n -> SelectedChoice n <$> parseModel prov (sls !! n))

    SCall (CallNode { callClass = cl, callPaths = paths }) ->
      let doOne (CallInstance { callSlice = sl' }) = parseModel prov sl'
          (lpaths, m_rsl, rpaths) = Map.splitLookup ResultVar paths
          base = case m_rsl of
            Nothing  -> Unconstrained <$ pMUnit -- make sure we are looking at a unit.
            Just rsl -> doOne rsl
          
      in SelectedCall cl <$> foldr (\fc rest -> uncurry mergeSelectedPath <$> pSeq fc rest)
                                   base
                                   (map doOne (Map.elems lpaths ++ Map.elems rpaths))

    SCase _ (Case _ alts) ->
      let go n | n < length alts = SelectedCase n <$> parseModel prov (snd (alts !! n))
               | otherwise = panic "Case result out of bounds" [show n]
      in pIndexed go
    
    _ -> panic "Impossible" [showPP sl] 

-- -----------------------------------------------------------------------------
-- Names
--
-- We can reuse the DDL names as the GUIDs make them unique, althouh
-- we have to use the summary class for top-level names

nameToSMTNameWithClass :: SummaryClass -> Name -> String
nameToSMTNameWithClass cl n = nameToSMTName n ++ clPart
  where
    clPart = case cl of
               FunctionResult -> "F"
               Assertions     -> "A"

fnameToSMTNameWithClass :: SummaryClass -> FName -> String
fnameToSMTNameWithClass cl n = fnameToSMTName n ++ clPart
  where
    clPart = case cl of
               FunctionResult -> "F"
               Assertions     -> "A"
               
-- -----------------------------------------------------------------------------
-- Symbolic execution of slices

data SMTFunDef = SMTFunDef { sfdName :: String
                           , sfdArgs :: [(String, SExpr)]
                           , sfdRet  :: SExpr
                           , sfdBody :: SExpr
                           , sfdDeps  :: Set String -- called deps in smt
                           }
                 
defineSMTFunDefs :: Solver -> Rec SMTFunDef -> IO ()
defineSMTFunDefs s (NonRec sfd) =
  void $ S.defineFun s (sfdName sfd) (sfdArgs sfd) (sfdRet sfd) (sfdBody sfd)
defineSMTFunDefs s (MutRec sfds) = S.defineFunsRec s defs
  where
    defs = map (\sfd -> (sfdName sfd, sfdArgs sfd, sfdRet sfd, sfdBody sfd)) sfds

-- This could be more efficient, but we generate all the bodies of the
-- SMT terms and run the SCC analysis to get recursive groupings, then
-- send them to the solver.
symExecSummaries :: Module -> Summaries -> SymExecM ()
symExecSummaries md allSummaries = do
  mapM_ symExecTDecl orderedTys -- FIXME: filter by need
  fdefs <- Map.foldMapWithKey (\fn -> foldMap (symExecSummary fn)) allSummaries
  let rFDefs = topoOrder (\sfd -> (sfdName sfd, sfdDeps sfd)) fdefs
  withSolver $ \s -> liftIO $ mapM_ (defineSMTFunDefs s) rFDefs
  where
    -- FIXME: figure out rec tys
    orderedTys = forgetRecs (mTypes md)


-- Turn a summary into a SMT formula (+ associated types)
symExecSummary :: FName -> Summary -> SymExecM [SMTFunDef]
symExecSummary fn summary = do
  -- Send the roots to the solver
  rootFuns <- Map.foldMapWithKey (\root sl -> go (ProgramVar root) (mempty, sl)) (pathRootMap summary)
  -- Send the argument domain slices to the solver
  argFuns  <- Map.foldMapWithKey go (explodeDomain (exportedDomain summary))
  pure (rootFuns ++ argFuns)
  where
    cl = summaryClass summary
    go ev (evs, sl) = do
      let predN     = evPredicateN cl fn ev
          ty | ev == ResultVar = fnameType fn
             | otherwise       = TUnit -- no return value
          frees     = [ v | ProgramVar v <- Set.toList (getEntangledVars evs) ]
      (: []) <$> sliceToFun predN ty frees sl

mkPredicateN :: SummaryClass -> Name -> String
mkPredicateN cl root = "Rel-" ++ nameToSMTNameWithClass cl root

mkFPredicateN :: SummaryClass -> FName -> String
mkFPredicateN cl root = "Rel-" ++ fnameToSMTNameWithClass cl root

evPredicateN :: SummaryClass -> FName -> EntangledVar -> String
evPredicateN cl fn ev =
  case ev of
    ResultVar    -> mkFPredicateN cl fn
    ProgramVar v -> mkPredicateN cl v

-- Used to turn future path sets and arg domains into SMT terms.
sliceToFun :: String -> Type -> 
              [Name] -> Slice -> SymExecM SMTFunDef
sliceToFun predN ty frees sl = do
  body  <- withFail sty <$> symExecSlice sl
  pure (SMTFunDef { sfdName = predN, sfdArgs = args, sfdRet = tResult sty
                  , sfdBody = body
                  , sfdDeps = deps
                  })
  -- withSolver $ \s -> void $ liftIO $ S.defineFun s predN args  body
  where
    sty     = symExecTy ty
    args    = map (\v -> (nameToSMTName v, symExecTy (typeOf v))) frees
              ++ [(modelN, tModel)]

    allFDeps = freeFVars sl
    gdeps    = sliceToGDeps sl
    gFDeps   = Set.map (\(_, fn, _) -> fn) gdeps
    deps     = Set.map (\(cl, fn, ev) -> evPredicateN cl fn ev) gdeps
               <> (Set.map fnameToSMTName (allFDeps `Set.difference` gFDeps))

sliceToGDeps :: Slice -> Set (SummaryClass, FName, EntangledVar)
sliceToGDeps = go
  where
    go sl =
      case sl of
        SDontCare _ sl' -> go sl'
        SDo _ l r       -> goLeaf l <> go r
        SUnconstrained  -> mempty
        SLeaf s         -> goLeaf s

    goLeaf sl =
      case sl of
        SPure {}               -> mempty
        SMatch {}              -> mempty
        SAssertion {}          -> mempty
        SChoice cs            -> foldMap go cs
        SCall cn              -> goCallNode cn
        SCase _ (Case _ alts) -> foldMap (go . snd) alts

    goCallNode cn =
      Set.map (\ev -> (callClass cn, callName cn, ev)) (Map.keysSet (callPaths cn))
      
-- The SMT datatype looks something like this:
-- data SMTPath =
--   Bytes ByteString
--   | Branch   Int SMTPath
--   | Sequence SMTPath SMTPath

-- Define a predicate over a corresponding model.  This holds when a
-- path is valid --- a model for this tells us the path and bytes.

leafModelN, modelN, failN :: String
leafModelN = "$leaf-model"
modelN = "$model"
failN  = "$fail"

--------------------------------------------------------------------------------
-- SMT Helpers

-- mkAnd, mkOr :: [SExpr] -> SExpr
-- mkOr []  = S.bool True
-- mkOr [x] = x
-- mkOr xs  = S.orMany xs
  
-- mkAnd []  = S.bool True
-- mkAnd [x] = x
-- mkAnd xs  = S.andMany xs

--------------------------------------------------------------------------------
-- Model predicates

-- mkIsSeq :: SExpr -> SExpr
-- mkIsSeq model' = S.fun "(_ is seq)" [model']

mkSeq :: SExpr -> SExpr -> SExpr -> SExpr -> SExpr
mkSeq m n1 n2 body = mkMatch m [ (S.fun "seq" [n1, n2], body)
                               , (S.const "_", sFail)
                               ]

mkMatch :: SExpr -> [(SExpr, SExpr)] -> SExpr
mkMatch e cs =
  S.fun "match" [ e
                , S.List [ S.List [ pat, rhs ] | (pat, rhs) <- cs ]
                ]

-- Shared between case and choice, although case doesn't strictly
-- required this (having the idx make replaying the model in
-- synthesise a bit nicer)

mkIndexed :: String -> SExpr -> SExpr -> SExpr
mkIndexed idxN model body =
  mkMatch model [ ( S.fun "indexed" [S.const idxN, S.const modelN], body)
                , ( S.const "_", sFail)
                ]

mkBranch :: SExpr -> [SExpr] -> SExpr
mkBranch model branches = mkIndexed idxN model body
  where
    body = foldr mkOne sFail (zip [0..] branches)
    mkOne (n, branch) = S.ite (check n) branch
    check n = (S.eq idx (S.int n))
    
    idxN = "$idx"
    idx  = S.const idxN
           
--------------------------------------------------------------------------------
-- Other helpers

-- evToRHS :: Maybe SExpr -> EntangledVar -> SExpr
-- evToRHS _ (ProgramVar v)          = symExecName v
-- evToRHS (Just res) (ResultVar {}) = res
-- evToRHS _ _                       = panic "Expected a result" []


-- -----------------------------------------------------------------------------
-- Monadic operations

sBind' :: Maybe (String, SExpr) -> SExpr -> SExpr -> SExpr
sBind' m_x lhs rhs =
  mkMatch lhs' [ ( S.fun "success" [S.const v], rhs )
              , ( S.const "failure", sFail )
              ]
  where
    (v, sty) = maybe ("_", tUnit) id m_x
    lhs'     = withFail sty lhs
    
sBind :: Maybe Name -> SExpr -> SExpr -> SExpr
sBind (Just x) = sBind' (Just (nameToSMTName x, symExecTy (typeOf x)))
sBind Nothing  = sBind' Nothing

sBind_ :: SExpr -> SExpr -> SExpr
sBind_ = sBind Nothing

sPure :: SExpr -> SExpr
sPure v = S.fun "success" [v]

sGuard :: SExpr -> SExpr -> SExpr
sGuard b rest = 
  S.ite b rest sFail

sFail :: SExpr
sFail = S.const failN

withFail :: SExpr -> SExpr -> SExpr
withFail ty = mklet failN (S.as (S.const "failure") (tResult ty))

-- withModel :: SExpr -> SExpr -> SExpr
-- withModel model
--   | model == S.const modelN = id
--   | otherwise               = mklet modelN model
  
--------------------------------------------------------------------------------
-- Symbolically executing a slice
                             
-- We need to be in a monad as we need aux. definitions.

-- We use 2 special variables, $model and $fail, which are shadowed
-- --- $model is to the current model, and $fail is Failure at the
-- right type.  This is a little fragile, but it avoids plumbing them
-- through.

symExecCase :: SExpr -> Bool -> Case Slice -> SymExecM SExpr
symExecCase m total (Case e alts) = mkIndexed idxN m <$> body
  where
    -- modelN is bound in body
    body = case typeOf e of
      TBool     -> S.ite e' <$> match 0 (PBool True) <*>  match 1 (PBool False)
      TMaybe {} -> do
        nc <- match 0 PNothing
        jc <- match 1 PJust
        pure (mkMatch e' [ (S.const "Nothing", nc)
                         , (S.fun "Just" [S.const "_"], jc)
                         ])

      -- We translate a case into a smt case
      -- FIXME: is it more efficient to ite on the index?
      TUser ut -> do
        bodies <- zipWithM (goAlt ut) [0..] alts
        pure (mkMatch e' (bodies ++ defCase))

      -- numeric cases
      ty -> do
        let pats = [ (n, sl) | (PNum n, sl) <- alts ]
            resN = "$r"
            res  = S.const resN              
            go (n, (patn, sl)) rest =
              S.ite (S.eq res (mkLit ty patn)) <$> check n sl <*> pure rest                
        base <- match (fromIntegral (length pats)) PAny
        mklet resN e' <$> foldrM go base (zip [0..] pats)

    mkLit ty n = -- a bit hacky
      symExecOp0 (IntL n ty)

    goAlt ut n (p, sl) = do
      let sp = case p of
                 PAny   -> wildcard
                 PCon l -> S.fun (labelToField (utName ut) l) [wildcard]
                 _      -> panic "Unknown pattern" [showPP p]
                 
      alt <- check n sl
      pure (sp, alt)

    defCase = if total then [] else [(wildcard, sFail)]
  
    wildcard = S.const "_"      
      
    check n sl = sGuard (S.eq idx (S.int n)) <$> symExecSlice sl
        
    match n p =
      case find (\(p', _) -> p == p' || p' == PAny) alts of
        Nothing      -> pure sFail
        Just (_, sl) -> check n sl

    e' = symExecV e
    
    idxN = "$i"
    idx  = S.const idxN
  

symExecSliceLeaf :: SExpr -> SliceLeaf -> SymExecM SExpr
symExecSliceLeaf m sl =
  case sl of
    -- Similar to Do case.
    SPure e  -> pure (mkPure m (symExecV e))
    
    SMatch (MatchByte bset) -> do
      let byteN  = "$b"
          byte   = S.const byteN
          go bytes = sBind' (Just (byteN, tByte)) (S.fun "$get-byte" [bytes])
                            (sGuard (symExecByteSet bset byte) (sPure byte))
      pure (mkBytes m go)
      
    SMatch (MatchBytes e) ->
      pure (mkBytes m (\bytes -> sGuard (S.eq bytes (symExecV e)) (sPure bytes)))
    SMatch _ -> panic "Unsupported match" []
      
    SAssertion a -> pure (sGuard (symExecAssertion a) (mkPure m sUnit))
    
    SChoice sls -> mkBranch m <$> mapM symExecSlice sls
    
    SCall (CallNode { callClass = cl
                    , callName = fn
                    , callAllArgs = args
                    , callPaths = paths }) ->
      let doOne (ev, CallInstance { callParams = evs }) =
            let -- c.f. symExecDomain
                execArg x | Just v <- Map.lookup x args = symExecV v
                          | otherwise = panic "Missing argument" [showPP x]
                actuals = [ execArg x | ProgramVar x <- Set.toList (getEntangledVars evs) ]
            in S.fun (evPredicateN cl fn ev) (actuals ++ [m])
          (lpaths, m_rsl, rpaths) = Map.splitLookup ResultVar paths
          base = case m_rsl of
            Nothing  -> mkPure m sUnit
            Just rsl -> doOne (ResultVar, rsl)
            
      in pure (foldr (\fc rest -> mkSeq m leafModel model (sBind_ fc rest))
                     base
                     (map doOne (Map.toList lpaths ++ Map.toList rpaths)))

    SCase total cs -> symExecCase m total cs
    
  where
    mkPure m' = sGuard (S.eq m' (S.const "munit")) . sPure

    mkBytes model' bodyf =
      let bytes  = S.const "$bytes"
      in mkMatch model' [ ( S.fun "bytes" [bytes], bodyf bytes )
                        , ( S.const "_", sFail )
                        ]
    -- FIXME: clag
    model      = S.const modelN
    leafModel = S.const leafModelN

symExecSlice :: Slice -> SymExecM SExpr
symExecSlice = go 
  where
    go sl =
      case sl of
        SDontCare _n sl'  -> go sl'
        SDo (Just x) (SPure e) rest ->
          mklet (nameToSMTName x) (symExecV e) <$> go rest
        SDo _ (SAssertion a) rest -> sGuard (symExecAssertion a) <$> go rest
        SDo m_x sl' rest -> do
          let mk = mkSeq model leafModel model
          mk <$> (sBind m_x <$> symExecSliceLeaf leafModel sl' <*> go rest)
            
        SUnconstrained  -> pure (mkPure sUnit)
        SLeaf sl'       -> symExecSliceLeaf model sl'
  
    model      = S.const modelN
    leafModel = S.const leafModelN
    -- FIXME: copied
    mkPure = sGuard (S.eq model (S.const "munit")) . sPure

symExecAssertion :: Assertion -> SExpr
symExecAssertion (GuardAssertion tc) = symExecV tc

-- -- -----------------------------------------------------------------------------
-- -- Dealing with Many
-- -- Many is modeled as a recursive function (essentially n*), and so
-- -- needs to be pulled out into a separate function.  This returns both
-- -- the list and its length
-- --
-- -- We define it slightly awkwardly as otherwise the solver doesn't terminate.
-- --
-- -- FIXME: lists now carry their length, so we could simplify here
-- defineMany :: Solver -> String -> [(String, SExpr)] -> SExpr
--            -> SParserM -> SolverM ()
-- defineMany s name args pT p = do
--   void $ do
--     st    <- freshSym "state"
--     body' <- runSParserM body (tTuple S.tInt (tListWithLength pT)) (S.const st)
--     liftIO $ S.defineFunRec s name (args ++ [(st, tState)]) (tParserM bodyT) (\_ -> body')
--   where
--     bodyT = tTuple S.tInt (tListWithLength pT)
--     body =  do st0 <- getState -- should be i from above
--                withConts (failC st0) successC p

--     -- This is a bit subtle - if o fails, we need to ignore any input
--     -- it has consumed (spure would otherwise return the current input)
--     failC st      = withInput st $ spure (sTuple (S.int 0) (sNil pT))
--     successC st x = withInput st $
--       sbind (scallP name (map (S.const . fst) args))
--             (\n_xs -> spure (sTuple (S.add (sFst n_xs) (S.int 1))
--                                     (sCons x (sSnd n_xs))))

-- -- This is a bit evil as it relies on having IO inside SParserM
-- symExecMany :: Env -> WithSem -> Commit -> ManyBounds (TC a Value) -> TC a Grammar
--             -> SParserM
-- symExecMany e ws _c bnds body = do
--   manyN <- freshGlobalSym "many"

--   -- Figure out what arguments to pass to the new procedure.  We could
--   -- just use all of env
--   let frees  = map valueName (Set.toList (tcFree bnds `Set.union` tcFree body))
--       params = map (lookupEnv e) frees
--       bodyT  = symExecTy (typeOfStripG body)

--   (e', args) <- lift . lift $ envFromParams (solver e) frees

--   -- FIXME: lift . lift
--   lift . lift $ defineMany (solver e) manyN args bodyT (symExecG e' body)

--   nres    <- S.const <$> freshSym "n"
--   boundsP <- symExecManyBounds e bnds nres

--   -- call the new function and check the bounds
--   sbind (scallP manyN params)
--         (\n_xs -> sbind (sguard (mklet nres (sFst n_xs) boundsP))
--                         (\_ -> mbPure ws (sSnd n_xs)))
--   where
--     valueName :: Some TCName -> TCName Value
--     valueName (Some n@(TCName { tcNameCtx = AValue })) = n
--     valueName _ = error "Sholdn't happen: symExecMany/valueName"

-- -- -----------------------------------------------------------------------------
-- -- Symbolic execution of loops (over lists)
-- --
-- -- As with Many, this will create a recursive function.

-- -- We translate for (acc = e; v in xs) { F frees acc v }
-- --
-- -- into
-- --
-- -- (define-fun-rec loop ( ...frees... (acc T) (xs (List t)))
-- --   (ite (is-nil xs)
-- --        acc
-- --        (let ((acc' (F frees acc (head xs))))
-- --             (loop frees acc' (tail xs)))))

-- -- defineLoop :: Solver -> String -> [(String, SExpr)]
-- --            -> String -> SExpr -> String -> SExpr
-- --            -> SParserM -> IO ()
-- -- defineLoop s name args accV accT elV elT body = do




-- --   void $ runSolverM $ do
-- --     i  <- freshSym "input"
-- --     body' <- runReaderT (body p) (SParserMArgs bodyT (S.const i))
-- --     liftIO $ S.defineFunRec s name (args ++ [(i, tInput)]) (tParserM bodyT) (\_ -> body')
-- --   where
-- --     bodyT = tTuple S.tInt (tList pT)

-- --     body p = do
-- --       p' <- local (\x -> x { sType = pT }) p
-- --       r <- S.const <$> freshSym "r"
-- --       rhs <- S.ite (S.fun "is-fail" [r])
-- --                <$> (spure (sTuple (S.int 0) (S.as (S.symbol "nil") (tList pT))))
-- --                <*> (let x = S.fun "result" [r]
-- --                     in local (\i -> i { sInput = S.fun "input" [r] })
-- --                              (sbind (N "n-xs" bodyT)
-- --                                     (scallP name (map (S.const . fst) args))
-- --                                     (\n_xs -> spure (sTuple (S.add (sFst n_xs) (S.int 1))
-- --                                                             (S.fun "insert" [x, sSnd n_xs])))))
-- --       pure (mklet r p' rhs)

-- symExecVLoop :: Env -> Loop a Value -> SParserM
-- -- FIXME: we ignore kname
-- symExecVLoop e loop@(Loop { loopFlav = Fold acc accInitE }) = do
--   loopN <- freshGlobalSym "loop"

--   -- Figure out what arguments to pass to the new procedure.
--   let bound  = Set.fromList [Some acc, Some (loopElName loop)]
--       frees  = map valueName (Set.toList (tcFree (loopBody loop) `Set.difference` bound))
--       params = map (lookupEnv e) frees

--   (e', args) <- lift . lift $ envFromParams (solver e) frees      

--   accV  <- freshSym "acc"
--   elV   <- S.const <$> freshSym "el"
--   listV <- freshSym "els"

--   f     <- symExecV (extendEnv acc (S.const accV) (extendEnv (loopElName loop) elV e')) (loopBody loop)

--   let accT = symExecTy (tcType acc)
--       elT  = symExecTy (tcType (loopElName loop))
--       body = S.ite (S.fun "is-nil" [S.const listV])
--                    (S.const accV)
--                    (mklet elV (S.fun "head" [S.const listV])
--                           (S.fun loopN (map (S.const . fst) args ++ [f, S.fun "tail" [S.const listV]])))

--   _ <- liftIO $ S.defineFunRec (solver e) loopN (args ++ [(accV, accT), (listV, tList elT)]) accT
--                                (\_ -> body)

--   -- The collection (list in this case)
--   accInit <- symExecV e accInitE
--   col     <- symExecV e (loopCol loop)

--   pure (S.fun loopN (params ++ [ accInit, S.fun "get-list" [col] ]))

--   where -- FIXME: copied from above
--     valueName :: Some TCName -> TCName Value
--     valueName (Some n@(TCName { tcNameCtx = AValue })) = n
--     valueName _ = error "Sholdn't happen: symExecMany/valueName"

-- symExecVLoop _e (Loop { loopFlav = LoopMap }) =
--   error "symExecVLoop map is unimplemented"


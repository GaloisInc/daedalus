{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving, ScopedTypeVariables, RankNTypes #-}

-- | Symbolically execute a fragment of DDL
module Talos.SymExec ( solverSynth
                     , symExecTy, symExecTyDecl, symExecSummary
                     ) where


import Control.Monad.Reader
import Data.Maybe (maybeToList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


-- import qualified Data.Text as T

import SimpleSMT (SExpr, Solver)
import qualified SimpleSMT as S

import Daedalus.Panic
import Daedalus.PP
import Daedalus.Type.AST hiding (tByte, tUnit, tMaybe, tMap)
import Daedalus.Type.PatComplete (CaseSummary(..))

import Talos.SymExec.Monad
import Talos.SymExec.ModelParser
import Talos.SymExec.TC
import Talos.SymExec.StdLib

import Talos.Analysis.Domain
import Talos.Analysis.EntangledVars
import Talos.Analysis.Slice
import Talos.Analysis.Monad (Summary(..))


--------------------------------------------------------------------------------
-- Solver synthesis

solverSynth :: Solver -> SummaryClass -> TCName Value -> ProvenanceTag -> FuturePathSet a -> IO SelectedPath
solverSynth s cl root prov fps = S.inNewScope s $ do
  model <- S.declare s modelN tModel
  S.assert s (S.fun (mkPredicateN cl (tcName root)) [model])
  r <- S.check s
  case r of
    S.Sat -> pure ()
    -- FIXME: pretty gross
    S.Unsat   -> error "SMT solver result: Unsat"
    S.Unknown -> error "SMT solver result: Unknown"

  sexp <- getValue modelN  

  case evalModelP (parseModel prov fps) sexp of
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
parseModel :: ProvenanceTag -> FuturePathSet a ->  ModelP SelectedPath
parseModel prov fps0 = go fps0
  where
    go fps =
      case fps of
        Unconstrained   -> pure Unconstrained 
        DontCare n fps' -> dontCare n <$> go fps'
        PathNode (Assertion {}) fps' -> dontCare 1 <$> go fps'
        PathNode n fps'    -> uncurry PathNode <$> pSeq (parseNodeModel prov n) (go fps')

parseNodeModel :: ProvenanceTag -> FutureNode a -> ModelP SelectedNode
parseNodeModel prov fpn = 
  case fpn of
    -- We could also declare an aux type here which would make things
    -- a fair bit more readable.  We could aso use a tree instead of a
    -- list (for term size)
    Choice fpss         -> pIndexed (\n -> SelectedChoice n <$> parseModel prov (fpss !! n))
    FNCase (CaseNode { caseAlts = alts, caseDefault = m_fps }) ->
      let go n | n < NE.length alts = SelectedCase n <$> parseModel prov (fnAltBody (alts NE.!! n))
               | Just fps <- m_fps  = SelectedCase n <$> parseModel prov fps
               | otherwise = panic "Case result out of bounds" [show n]
      in pIndexed go

    Call (CallNode { callClass = cl, callPaths = paths }) ->
      let doOne (Wrapped (_, fps)) = parseModel prov fps
      in SelectedCall cl <$> foldr1 (\fc rest -> uncurry mergeSelectedPath <$> pSeq fc rest)
                                    (map doOne (Map.elems paths))

    FNMany (ManyNode { manyBody = fps }) -> do
      let pfps = parseModel prov fps
          doOne l r = uncurry (:) <$> pSeq l r
          
      SelectedMany <$> (pIndexed $ \n -> foldr doOne (pure []) (replicate (fromIntegral n) pfps))
      
    NestedNode fps        -> SelectedNested <$> parseModel prov fps
    SimpleNode {}         -> SelectedSimple prov <$> pBytes
    
    Assertion {} -> panic "Impossible" [] 

-- -----------------------------------------------------------------------------
-- Types

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


-- -----------------------------------------------------------------------------
-- Names
--
-- We can reuse the DDL names as the GUIDs make them unique, althouh
-- we have to use the summary class for top-level names

nameToSMTNameWithClass :: SummaryClass -> Name -> String
nameToSMTNameWithClass cl n = show (pp (nameScopedIdent n) <> "@" <> pp (nameID n) <> clPart)
  where
    clPart = case cl of
               FunctionResult -> "F"
               Assertions     -> "A"
    
-- -----------------------------------------------------------------------------
-- Symbolic execution of future path sets

-- Turn a summary into a SMT formula (+ associated types)
symExecSummary :: Name -> Summary -> SymExecM ()
symExecSummary fn summary = do
  -- Send the roots to the solver
  Map.foldMapWithKey (\root fps -> go (ProgramVar root) (mempty, fps)) (pathRootMap summary)  
  -- Send the argument domain fpss to the solver
  Map.foldMapWithKey go (explodeDomain (exportedDomain summary))
  where
    cl = summaryClass summary
    go ev (evs, fps) = do
      let predN     = evPredicateN cl fn ev
          ty        = typeOf ev
          hasResult = case ev of { ResultVar {} -> True; ProgramVar {} -> False }
          frees     = [ v | ProgramVar v <- Set.toList (getEntangledVars evs) ]
      symExecFuturePathSet hasResult predN ty frees fps

mkPredicateN :: SummaryClass -> Name -> String
mkPredicateN cl root = "Rel-" ++ nameToSMTNameWithClass cl root

evPredicateN :: SummaryClass -> Name -> EntangledVar -> String
evPredicateN cl fn ev =
  case ev of
    ResultVar {} -> mkPredicateN cl fn
    ProgramVar v   -> mkPredicateN cl (tcName v)

-- Used to turn future path sets and arg domains into SMT terms.
-- Configs have a single constuctor, so we use define-sort to aid in
-- readibility of the output.
symExecFuturePathSet :: Bool -> String -> Type -> 
                        [TCName Value] -> FuturePathSet a -> SymExecM ()
symExecFuturePathSet hasResult predN ty frees fps = do
  body  <- mkExists (Set.toList (assignedVars fps))
                   <$> futurePathSetPred (S.const modelN) m_resN fps
  withSolver $ \s -> void $ liftIO $ S.defineFun s predN args S.tBool body
  where
    m_resN = if hasResult then Just (S.const resN) else Nothing
    args    = map (\v -> (tcNameToSMTName v, symExecTy (typeOf v))) frees
              ++ [(modelN, tModel)]
              ++ if hasResult then [(resN, symExecTy ty)] else []

-- The SMT datatype looks something like this:
-- data SMTPath =
--   Bytes ByteString
--   | Branch   Int SMTPath
--   | Sequence SMTPath SMTPath

-- Define a predicate over a corresponding config.  This holds when a
-- path is valid --- a model for this (i.e. a config) tells us the
-- path and bytes.

-- FIXME: we will use the '$cfg' variable to refer to the current
-- config, and use shadowing whe updating.
modelN, resN :: String
modelN  = "$model"
resN = "$res"

-- Does not include the result
assignedVars :: FuturePathSet a -> Set (TCName Value)
assignedVars = go
  where
    go Unconstrained    = Set.empty
    go (DontCare _ fps) = go fps
    go (PathNode n fps) = go fps `Set.union`
      case n of
        SimpleNode rv _ -> evToSet rv
        Choice     fpss -> foldMap go fpss
        FNCase (CaseNode { caseAlts = alts, caseDefault = m_def }) ->
          foldMap (go . fnAltBody) alts `Set.union` foldMap go m_def
        Call (CallNode { callResultAssign = Just ev }) -> evToSet ev
        Call {} -> mempty
        -- the body is treated specially, so doesn't contribute.
        FNMany (ManyNode { manyResultAssign = Just ev }) -> evToSet ev
        FNMany {} -> mempty

        Assertion {} -> mempty        
        NestedNode fps' -> go fps'

    evToSet (ProgramVar v) = Set.singleton v
    evToSet (ResultVar {}) = mempty                                                 

--------------------------------------------------------------------------------
-- SMT Helpers

mkAnd, mkOr :: [SExpr] -> SExpr
mkOr []  = S.bool True
mkOr [x] = x
mkOr xs  = S.orMany xs
  
mkAnd []  = S.bool True
mkAnd [x] = x
mkAnd xs  = S.andMany xs

--------------------------------------------------------------------------------
-- Model predicates

mkIsSeq :: SExpr -> SExpr
mkIsSeq model' = S.fun "(_ is seq)" [model']

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
                , ( S.const "_", S.bool False)
                ]

mkBranch :: SExpr -> [SExpr] -> SExpr
mkBranch model branches =
  mkIndexed idxN model (mkOr (zipWith mkOne [0..] branches))
  where
    mkOne n branch = S.and (S.eq (S.const idxN) (S.int n)) branch
    idxN = "$idx"
    
--------------------------------------------------------------------------------
-- Other helpers

evToRHS :: Maybe SExpr -> EntangledVar -> SExpr
evToRHS _ (ProgramVar v)          = symExecTCName v
evToRHS (Just res) (ResultVar {}) = res
evToRHS _ _                       = panic "Expected a result" []

--------------------------------------------------------------------------------
-- Many

-- Returns a predicate over the last argument
symExecManyBounds ::  ManyBounds (TC a Value) -> SExpr -> [SExpr]
symExecManyBounds (Exactly t)   v = [ S.eq v (symExecV t) ]
symExecManyBounds (Between l h) v = mBound l (S.geq v) ++ mBound h (S.leq v)
  where
    mBound Nothing  _ = []
    mBound (Just t) f = [ f (symExecV t) ]

-- We turn Many (l .. h) b into an assertion over l and h, and a call
-- into a recursive procedure for b, using tuples as a list.
symExecMany :: SExpr -> Maybe SExpr -> ManyNode a -> SymExecM SExpr
symExecMany model m_funres (ManyNode { manyResultAssign = m_res
                                     , manyBounds       = bnds
                                     , manyFrees        = evFrees
                                     , manyBody         = fps
                                     }) = do
  auxN <- freshSym "many"

  symExecBody auxN
  
  let bndsP = symExecManyBounds bnds count
      resP  = S.eq count . sArrayLen <$> resCallArgs
      callP = [ S.fun auxN (passArgs ++ resCallArgs ++ [S.int 0 {- i -}, model ]) ]
      p     = mkAnd (bndsP ++ resP ++ callP)
  
  pure (mkIndexed countN model p)

  where
    count  = S.const countN
    countN = "$count"
    iN = "$i"
    i  = S.const iN

    -- Dealing with optional returns
    (resParam, resCallArgs, m_res') = case m_res of
      Nothing  -> ([], [], Nothing)
      Just res -> ( [(resN, symExecTy (typeOf res))]
                  , [(evToRHS m_funres res)]
                  , Just (sSelectL (S.const resN) i)
                  )
    
    frees     = [ v | ProgramVar v <- Set.toList (getEntangledVars evFrees) ]

    -- These are passed through.
    passParams  = map (\v -> (tcNameToSMTName v, symExecTy (typeOf v))) frees
                  ++ [(countN, S.tInt)]

    passArgs    = map (S.const . fst) passParams
    
    -- c.f. symExecFuturePathSet
    symExecBody predN = do
      let varArgs = [ S.add i (S.int 1)
                    , S.fun "msnd" [S.const modelN]
                    ]

          resArgs  = map (S.const . fst) resParam
          recCallP = S.fun predN (passArgs ++ resArgs ++ varArgs)
      
      fpsP <- mkExists (Set.toList (assignedVars fps))
              . mklet modelN (S.fun "mfst" [S.const modelN])
              <$> futurePathSetPred (S.const modelN) m_res' fps

      let body = S.ite (S.lt i count)
                 (S.and fpsP recCallP)
                  -- the prover can probably figure this out itself?
                 (S.and (S.eq i count) (S.eq (S.const modelN) (S.const "munit")))

      let ps = passParams ++ resParam ++ [(iN, S.tInt), (modelN, tModel)]

      withSolver $ \s -> void $ liftIO $ S.defineFunRec s predN ps S.tBool (\_ -> body)
                             
-- FIXME: we use exists here as an implementation technique, they
-- aren't really requires as they are all assigned (well, asserted
-- equal) eventually.  It is convenient to do it this was as then we
-- can push assignments to simple nodes in e.g. Choose.  It is unclear
-- if this causes solver issues.
--
-- We do traverse the fps again, so that might be expensive
--
-- We need to be in a monad as we need aux. definitions for Many etc.
futurePathSetPred :: forall a. SExpr -> Maybe SExpr -> FuturePathSet a -> SymExecM SExpr
futurePathSetPred model m_res fps0 = collect model fps0
  where
    collect model' fps = mkAnd <$> collect' model' fps

    collect' :: SExpr -> FuturePathSet a -> SymExecM [SExpr]
    collect' model' fps = do
      let mkRest fps'        = do
            rest <- mklet modelN (S.fun "msnd" [model']) <$> collect (S.const modelN) fps'
            pure [ mkIsSeq model', rest ]
          mkRestNoModel fps' = collect' model' fps'
          thisModel          = S.fun "mfst" [model']
      case fps of
        Unconstrained -> pure [] 
        DontCare _ fps' -> mkRestNoModel fps'
            
        PathNode (Assertion a) fps' ->
         -- Doesn't have an associated element in the model, so we
         -- don't increment idx
          (:) (symExecAssertion a) <$> mkRestNoModel fps'
        
        PathNode (SimpleNode rv tc) fps' -> -- Binds a variable.
          (:) (mkBytes thisModel (\bytes -> symExecG bytes (evToRHS m_res rv) tc))
              <$> mkRest fps'
          
        PathNode n fps' -> (++) <$> goN thisModel n <*> mkRest fps'

    -- model' is possibly a complex expression here, so it needs to be bound if it is used multipe times.
    
    -- goN :: SExpr -> FutureNode a -> SymExecM [SExpr]
    goN model' fpn =
      case fpn of
        Assertion {} -> error "futurePathSetRel Impossible"
        SimpleNode {} -> error "futurePathSetRel Impossible"

        -- We use shadowing of modelN here (mkBranch binds it)
        Choice fpss -> do
          branches <- mapM (collect (S.const modelN)) fpss
          pure [ mkBranch model' branches ]
        
        -- The idx isn't strictly required here, as we could just figure it out from the values.
        FNCase (CaseNode { caseCompleteness = _compl  -- we don't care here, we assert an alt is taken
                         , caseSummary      = summary
                         , caseTerm         = e
                         , caseAlts         = alts
                         , caseDefault      = m_def}) -> do
          let matchN    = "$match"

          altsPreds <- mapM (mkCaseAlt (S.const matchN)) (NE.toList alts)
          defPred <- case m_def of
            Nothing  -> pure []
            Just fps -> do r <- S.and (assertIsMissingLabel (S.const matchN) summary)
                                <$> collect (S.const modelN) fps
                           pure [r]
          pure [ mklet matchN (symExecV e) (mkBranch model' (altsPreds ++ defPred)) ]
          
        Call (CallNode { callClass = cl
                       , callResultAssign = m_res'
                       , callName = fn
                       , callAllArgs = args
                       , callPaths = paths }) ->
          let mkOne (ev, Wrapped (evs, _)) =
                let resArg = maybeToList (evToRHS m_res <$> m_res')
                    -- c.f. symExecDomain
                    execArg x | Just v <- Map.lookup x args = symExecV v
                              | otherwise = panic "Missing argument" [showPP x]
                    actuals = [ execArg x | ProgramVar x <- Set.toList (getEntangledVars evs) ]
                in S.fun (evPredicateN cl fn ev) (actuals ++ [S.const modelN] ++ resArg)
          in pure [ mklet modelN model'
                    ( foldr1 (\fc rest -> mkAnd [ mkIsSeq (S.const modelN)
                                                , mklet modelN (S.fun "mfst" [S.const modelN]) fc
                                                , mklet modelN (S.fun "msnd" [S.const modelN]) rest])
                      (map mkOne (Map.toList paths)))
                  ]

        FNMany mn -> (: []) <$> symExecMany model' m_res mn
        
        NestedNode fps -> collect' model' fps

    mkBytes model' bodyf =
      let bytes  = S.const "$bytes"
      in mkMatch model' [ ( S.fun "bytes" [bytes], bodyf bytes )
                        , ( S.const "_", S.bool False)
                        ]
     
    mkCaseAlt e (FNAlt { fnAltPatterns = pats, fnAltBody = b }) = do
      b' <- collect' (S.const modelN) b
      pure (mkExists (Set.toList (Set.unions (map patBindsSet pats)))
            (mkAnd (concatMap (relPattern e) pats ++ b')))
      

    -- Assert we match a pattern
    relPattern e pat =
      case pat of
        TCConPat (TCon tyName _) lbl pat' ->
          let getter = S.fun ("get-" ++ labelToField tyName lbl) [e]
          in assertLabel e tyName lbl : bindLabel getter pat'
        TCConPat {}    -> panic "Missing type name" [showPP pat]
        TCNumPat {}    -> panic "Unimplemented (number pattern)" [showPP pat]
        TCBoolPat b    -> [ S.eq e (S.bool b) ]
        TCJustPat pat'  -> S.fun "is-Just" [e] : bindLabel (S.fun "fromJust" [e]) pat'
        TCNothingPat {} -> [ S.fun "is-Nothing" [e] ]
        _               -> panic "Saw an unexpected  pat" [showPP pat]
      
    -- We assert that the expression must be of a form not matched by the other cases.
    assertIsMissingLabel e summary =
      case summary of
        UnionCase tyName _seen missing ->
          S.orMany (map (assertLabel e tyName) missing)
        MaybeCase seen ->
          mkAnd ([ S.fun "is-Nothing" [e] | Nothing `notElem` seen]
                 ++ [ S.fun "is-Just" [e] | Just () `notElem` seen]
                )
        NumberCase {} -> panic "Unimplemented (case over numbers)" []
        BoolCase seen ->
          mkAnd ([ e | True `notElem` seen] ++ [ S.not e | False `notElem` seen])

    assertLabel e tyName lbl = S.fun ("is-" ++ labelToField tyName lbl) [e]
    
    bindLabel e pat =
      case pat of
        TCVarPat v -> [ S.eq (symExecTCName v) e ]
        _          -> [] 
                               

  

mkExists :: [TCName Value] -> SExpr -> SExpr
mkExists [] b = b
mkExists xs b =
  S.fun "exists" [ S.List (map (\x -> S.List [ symExecTCName x, symExecTy (typeOf x) ]) xs)
                 , b
                 ]

symExecAssertion :: Assertion a -> SExpr
symExecAssertion (GuardAssertion tc) = symExecV tc

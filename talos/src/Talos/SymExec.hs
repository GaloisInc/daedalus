{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}

-- | Symbolically execute a fragment of DDL
module Talos.SymExec ({- symExec, ruleName, -} symExecV, symExecP -- , Env(..)
                     , symExecTy, symExecTyDecl, symExecSummary, solverSynth
                     {- , symExecG -}) where


import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Maybe (maybeToList)
import qualified Data.List.NonEmpty as NE

import Data.Map(Map)
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

import Talos.Lib
import Talos.SymExec.Monad
import Talos.SymExec.ModelParser
import Talos.Analysis.Domain
import Talos.Analysis.Monad (Summary(..))

-- -- -----------------------------------------------------------------------------
-- -- Decls

-- -- Grammars
-- symExec :: Solver -> TCDecl a -> IO ()
-- symExec s (TCDecl { tcDeclName = name, tcDeclCtxt = AGrammar
--                   , tcDeclDef = Defined def, tcDeclParams = params }) =
--   void $ runSolverM (ruleName name) $ do
--     (env0, args) <- envFromParams s (map stripParam params)

--     st <- freshSym "state"
--     let resT = symExecTy (typeOfStripG def)

--     body <- runSParserM (symExecG env0 def) resT (S.const st)
--     liftIO $ S.defineFun s (ruleName name) (args ++ [(st, tState)]) (tParserM resT) body

-- -- FIXME: should we merge these?
-- -- Values
-- symExec s (TCDecl { tcDeclName = name, tcDeclCtxt = AValue
--                   , tcDeclDef = Defined def, tcDeclParams = params }) =
--   void $ runSolverM (ruleName name) $ do
--     (env0, args) <- envFromParams s (map stripParam params)

--     let resT = symExecTy (typeOf def)

--     body <- runSParserM (symExecV env0 def)
--                         (error "symExecV examined the conts")
--                         (error "symExecV examined the input") -- FIXME (yuck)
--     liftIO $ S.defineFun s (ruleName name) args resT body

-- -- Predicates
-- symExec s (TCDecl { tcDeclName = name, tcDeclCtxt = AClass
--                   , tcDeclDef = Defined def, tcDeclParams = params }) =
--   void $ runSolverM (ruleName name) $ do
--     (env0, args) <- envFromParams s (map stripParam params)

--     b <- freshSym "byte"
--     body <- runSParserM (symExecP env0 def (S.const b))
--                         (error "symExecC examined the conts")
--                         (error "symExecC examined the input") -- FIXME (yuck)
--     liftIO $ S.defineFun s (ruleName name) (args ++ [(b, tByte)]) S.tBool body

-- symExec _s (TCDecl { tcDeclName = name, tcDeclDef = ExternDecl _}) =
--   error ("symExec: saw an external decl " ++ show (pp name))

-- -- -----------------------------------------------------------------------------
-- -- Enironments (mapping bound variables to their smt equiv.)

-- data Env = Env { symVars :: Map (TCName Value) SExpr }
-- --                , solver  :: Solver -- used to for Many
-- --                }

-- extendEnv :: TCName Value -> SExpr -> Env -> Env
-- extendEnv k v e = e { symVars = Map.insert k v (symVars e) }

-- lookupEnv :: Env -> TCName Value -> SExpr
-- lookupEnv e k = symVars e Map.! k

-- envFromParams :: Solver -> [TCName Value] -> SolverM (Env, [(String, SExpr)])
-- envFromParams s ns = do
--   args <- mapM (\a -> (,) a <$> freshSym (show (pp a))) ns
--   let env  = Env (S.const <$> Map.fromList args) s
--       args' = map (\(n, v) -> (v, symExecTy (tcType n))) args
--   pure (env, args')

-- -----------------------------------------------------------------------------
-- Types

symExecTCTyName :: TCTyName -> String
symExecTCTyName = show . pp

labelToField :: TCTyName -> Label -> String
labelToField n l = symExecTCTyName n ++ "-" ++ show (pp l)

typeNameToCtor :: TCTyName -> String
typeNameToCtor n = "mk-" ++ symExecTCTyName n

-- | Construct SMT solver datatype declaration based on DaeDaLus type
-- declarations.
symExecTyDecl :: Solver -> TCTyDecl -> IO ()
symExecTyDecl s TCTyDecl { tctyName = name, tctyParams = ps, tctyDef = td } =
  S.declareDatatype s n tvs sflds
  where
    tvs = map (\x -> "t" ++ show (tvarId x)) ps
    env = Map.fromList (zipWith (\p tv -> (p, S.const tv)) ps tvs)
    n = symExecTCTyName name
    sflds = case td of
              TCTyStruct flds -> [(typeNameToCtor name, map mkOneS flds) ]
              TCTyUnion flds  -> map mkOneU flds
    mkOneS (l, t) = (lblToFld l, symExecTy' env t)
    mkOneU (l, t) = (lblToFld l, [ ("get-" ++ lblToFld l, symExecTy' env t) ])
    lblToFld = labelToField name

symExecTy' :: Map TVar SExpr -> Type -> SExpr
symExecTy' env ty = go ty
  where
    go ty' = 
      case ty' of
        TVar x | Just t <- env Map.!? x -> t
        TVar x -> error ("Unknown type variable " ++ show (pp x))
     
        TCon tname args -> S.fun (symExecTCTyName tname) (map go args)
        Type t ->
          case t of
            TGrammar {} -> error "Shouldn't happen (symExecTy: TGrammar)"
            TStream     -> error "Unimplemented (symExecTy: TStream)"
            TByteClass  -> error "Unimplemented (symExecTy: TByteClass)" -- Shouldn't happen?
            TNum {}     -> error "Shouldn't happen (symExecTy: TNum)"
            TFun {}     -> panic "Shouldn't happen" [showPP ty']
            TUInt (Type (TNum n))
                        -> S.tBits n
            TUInt {}    -> error "Shouldn't happen (symExecTy: TUInt)"
            TSInt (Type (TNum n))
                        -> S.tBits n
            TSInt {}    -> error "Shouldn't happen (symExecTy: TSInt)"
            TInteger    -> S.tInt
            TBool       -> S.tBool
            TUnit       -> tUnit
            TArray _t'   -> error "Unimplemented (symExecTy: TArray)" --  tListWithLength (go t') -- S.tArray S.tInt (symExecTy t)
            TMaybe t'   -> tMaybe (go t')
            TMap kt vt  -> tMap (go kt) (go vt)

symExecTy :: Type -> SExpr
symExecTy = symExecTy' mempty

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

-- -- Returns a predicate over the last argument
-- symExecManyBounds :: Env -> ManyBounds (TC a Value) -> SExpr -> SParserM
-- symExecManyBounds e (Exactly t)   v = S.eq v <$> symExecV e t
-- symExecManyBounds e (Between l h) v =
--   S.and <$> mBound l (S.geq v) <*> mBound h (S.leq v)
--   where
--     mBound Nothing  _ = pure (S.bool True)
--     mBound (Just t) f = f <$> symExecV e t

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

tcNameToSMTName :: TCName k -> String
tcNameToSMTName tcN = show (pp (nameScopedIdent n) <> "@" <> pp (nameID n))
  where
    n = tcName tcN

symExecTCName :: TCName Value -> SExpr
symExecTCName =  S.const . tcNameToSMTName
    
-- -----------------------------------------------------------------------------
-- Symbolic execution of future path sets

-- Turn a summary into a SMT formula (+ associated types)
symExecSummary :: Solver -> Summary -> IO ()
symExecSummary s summary = do
  -- Send the roots to the solver
  Map.foldMapWithKey (\root fps -> go (ProgramVar root) (mempty, fps)) (pathRootMap summary)  
  -- Send the argument domain fpss to the solver
  Map.foldMapWithKey go (explodeDomain (exportedDomain summary))
  where
    cl = summaryClass summary
    go ev (evs, fps) = do
      let predN     = evPredicateN cl ev
          ty        = typeOf ev
          hasResult = case ev of { ResultVar {} -> True; ProgramVar {} -> False }
          frees     = [ v | ProgramVar v <- Set.toList (getEntangledVars evs) ]
      symExecFuturePathSet s hasResult predN ty frees fps

mkPredicateN :: SummaryClass -> Name -> String
mkPredicateN cl root = "Rel-" ++ nameToSMTNameWithClass cl root

evPredicateN :: SummaryClass -> EntangledVar -> String
evPredicateN cl ev =
  case ev of
    ResultVar fn _ -> mkPredicateN cl fn
    ProgramVar v   -> mkPredicateN cl (tcName v)

-- Used to turn future path sets and arg domains into SMT terms.
-- Configs have a single constuctor, so we use define-sort to aid in
-- readibility of the output.
symExecFuturePathSet :: Solver -> Bool -> String -> Type -> 
                        [TCName Value] -> FuturePathSet a -> IO ()
symExecFuturePathSet s hasResult predN ty frees fps =
  void $ S.defineFun s predN args S.tBool body
  where
    body  = mkExists (Set.toList (assignedVars fps))
                     (futurePathSetPred (S.const modelN) m_resN fps)
    m_resN = if hasResult then Just (S.const resN) else Nothing
    args    = map (\v -> (tcNameToSMTName v, symExecTy (typeOf ty))) frees
              ++ [(modelN, tModel)]
              ++ if hasResult then [(resN, symExecTy ty)] else []

-- The SMT datatype looks something like this:
-- data SMTPath =
--   Bytes ByteString
--   | Branch   Int SMTPath
--   | Sequence SMTPath SMTPath

-- FIXME: move
solverSynth :: Solver -> SummaryClass -> TCName Value -> ProvenanceTag -> FuturePathSet a -> IO SelectedPath
solverSynth s cl root prov fps = S.inNewScope s $ do
  model <- S.declare s modelN tModel
  S.assert s (S.fun (mkPredicateN cl (tcName root)) [model])
  r <- S.check s
  case r of
    S.Sat -> pure ()
    -- FIXME: pretty gross
    S.Unsat   -> error "Unsat"
    S.Unknown -> error "Unknown"

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
    Choice fpss         -> pBranch (\n -> SelectedChoice n <$> parseModel prov (fpss !! n))
    FNCase (CaseNode { caseAlts = alts, caseDefault = m_fps }) ->
      let go n | n < NE.length alts = SelectedCase n <$> parseModel prov (fnAltBody (alts NE.!! n))
               | Just fps <- m_fps  = SelectedCase n <$> parseModel prov fps
               | otherwise = panic "Case result out of bounds" [show n]
      in pBranch go

    Call (CallNode { callClass = cl, callPaths = paths }) ->
      let doOne (Wrapped (_, fps)) = parseModel prov fps
      in SelectedCall cl <$> foldr1 (\fc rest -> uncurry mergeSelectedPath <$> pSeq fc rest)
                                    (map doOne (Map.elems paths))
      
    NestedNode fps        -> SelectedNested <$> parseModel prov fps
    SimpleNode {}         -> SelectedSimple prov <$> pBytes
    
    Assertion {} -> panic "Impossible" [] 

-- Define a predicate over a corresponding config.  This holds when a
-- path is valid --- a model for this (i.e. a config) tells us the
-- path and bytes.

-- FIXME: we will use the '$cfg' variable to refer to the current
-- config, and use shadowing whe updating.
modelN, mArrayN, resN :: String
modelN  = "$model"
mArrayN = "$marray"
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
        Assertion {} -> mempty
        NestedNode fps' -> go fps'

    evToSet (ProgramVar v) = Set.singleton v
    evToSet (ResultVar {}) = mempty                                                 
          
-- FIXME: we use exists here as an implementation technique, they
-- aren't really requires as they are all assigned (well, asserted
-- equal) eventually.  It is convenient to do it this was as then we
-- can push assignments to simple nodes in e.g. Choose.  It is unclear
-- if this causes solver issues.
--
-- We do traverse the fps again, so that might be expensive
futurePathSetPred :: SExpr -> Maybe SExpr -> FuturePathSet a -> SExpr
futurePathSetPred model m_res fps0 = collect model fps0
  where
    collect model' fps = mkAnd (collect' model' fps)
    
    collect' model' fps =
      let mkRest fps'        =
            [ (S.fun "(_ is seq)" [model'])
            , mklet modelN (S.fun "msnd" [model']) (collect (S.const modelN) fps')
            ]
          mkRestNoModel fps' = collect' model' fps'
          thisModel          = S.fun "mfst" [model']
      in case fps of
        Unconstrained -> [ ] -- maybe not required?
        DontCare _ fps' -> mkRestNoModel fps'
            
        PathNode (Assertion a) fps' ->
         -- Doesn't have an associated element in the model, so we
         -- don't increment idx
          symExecAssertion a : mkRestNoModel fps'
        
        PathNode (SimpleNode rv tc) fps' -> -- Binds a variable.
          (mkBytes thisModel (\bytes -> symExecG bytes (evToRHS rv) tc)) : mkRest fps'
          
        PathNode n fps' -> goN thisModel n ++ mkRest fps'

    -- rel' is possibly a complex expression here, so it needs to be bound if it is used multipe times.
    -- goN :: SExpr -> FutureNode a -> [SExpr]
    goN model' fpn =
      case fpn of
        Assertion {} -> error "futurePathSetRel Impossible"
        SimpleNode {} -> error "futurePathSetRel Impossible"

        -- We use shadowing of modelN here (mkBranch binds it)
        Choice fpss -> [ mkBranch model' [ collect (S.const modelN) fps | fps <- fpss ] ]
        
        -- The idx isn't strictly required here, as we could just figure it out from the values.
        FNCase (CaseNode { caseCompleteness = _compl  -- we don't care here, we assert an alt is taken
                         , caseSummary      = summary
                         , caseTerm         = e
                         , caseAlts         = alts
                         , caseDefault      = m_def}) ->
          let matchN    = "$match"
              altsPreds = map (mkCaseAlt (S.const matchN)) (NE.toList alts)
              defPred fps = S.and (assertIsMissingLabel (S.const matchN) summary)
                                  (collect (S.const modelN) fps)
          in [ mklet matchN (symExecV e)
                     (mkBranch model' (altsPreds ++ maybeToList (defPred <$> m_def))) ]
          
        Call (CallNode { callClass = cl
                       , callResultAssign = m_res'
                       , callAllArgs = args
                       , callPaths = paths }) ->
          let mkOne (ev, Wrapped (evs, _)) =
                let resArg = maybeToList (evToRHS <$> m_res')
                    -- c.f. symExecDomain
                    execArg x | Just v <- Map.lookup x args = symExecV v
                              | otherwise = panic "Missing argument" [showPP x]
                    actuals = [ execArg x | ProgramVar x <- Set.toList (getEntangledVars evs) ]
                in S.fun (evPredicateN cl ev) (actuals ++ [S.const modelN] ++ resArg)
          in [ mklet modelN model'
               ( foldr1 (\fc rest -> mkAnd [ mkIsSeq (S.const modelN)
                                           , mklet modelN (S.fun "mfst" [S.const modelN]) fc
                                           , mklet modelN (S.fun "msnd" [S.const modelN]) rest])
                        (map mkOne (Map.toList paths)))
             ]
             
        NestedNode fps -> [ collect model' fps ]

    mkIsSeq model' = S.fun "(_ is seq)" [model']


    mkBytes model' bodyf =
      let bytes  = S.const "$bytes"
      in mkMatch model' [ ( S.fun "bytes" [bytes], bodyf bytes )
                        , ( S.const "_", S.bool False)
                        ]
     
    -- Shared between case and choice, although case doesn't strictly
    -- required this (having the idx make replaying the model in
    -- synthesise a bit nicer)

    mkBranch model' branches =
      let mkOne n branch = S.and (S.eq (S.const "$idx") (S.int n)) branch
      in mkMatch model' [ ( S.fun "branch" [S.const "$idx", S.const modelN]
                          , mkOr (zipWith mkOne [0..] branches)
                          )
                        , ( S.const "_", S.bool False)
                        ]

    mkCaseAlt e (FNAlt { fnAltPatterns = pats, fnAltBody = b }) =
      mkExists (Set.toList (Set.unions (map patBindsSet pats)))
               (mkAnd (concatMap (relPattern e) pats ++ [ collect (S.const modelN) b ]))
      

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
                               
    evToRHS ev =
      case ev of
        ProgramVar v -> symExecTCName v
        ResultVar {}
          | Just res <- m_res -> res
          | otherwise         -> panic "Expected a result" []

    mkOr []  = S.bool True
    mkOr [x] = x
    mkOr xs  = S.orMany xs
      
    mkAnd []  = S.bool True
    mkAnd [x] = x
    mkAnd xs  = S.andMany xs

  
    mkMatch e cs =
      S.fun "match" [ e
                    , S.List [ S.List [ pat, rhs ] | (pat, rhs) <- cs ]
                    ]

mkExists :: [TCName Value] -> SExpr -> SExpr
mkExists [] b = b
mkExists xs b =
  S.fun "exists" [ S.List (map (\x -> S.List [ symExecTCName x, symExecTy (typeOf x) ]) xs)
                 , b
                 ]

symExecAssertion :: Assertion a -> SExpr
symExecAssertion (GuardAssertion tc) = symExecV tc

-- -----------------------------------------------------------------------------
-- Symbolic execution of terms

-- -- This causes GHC to simpl loop
-- {-# NOINLINE mbPure #-}
-- mbPure :: WithSem -> SExpr -> SParserM
-- mbPure NoSem _ = spure sUnit
-- mbPure _     v = spure v

symExecBinop :: BinOp -> Type -> SExpr -> SExpr -> SExpr
symExecBinop bop (isBits -> Just (signed, _n)) =
  case bop of
    Add    -> S.bvAdd
    Sub    -> S.bvSub
    Mul    -> S.bvMul
    Div    -> if signed then S.bvSDiv else S.bvUDiv
    Mod    -> if signed then S.bvSRem else S.bvURem
    Lt     -> if signed then S.bvSLt else S.bvULt
    Leq    -> if signed then S.bvSLeq else S.bvULeq
    Eq     -> S.eq
    NotEq  -> \x y -> S.distinct [x, y]
    Cat    -> S.concat
    LCat   -> error "Unimplemented"
    LShift -> S.bvShl
    RShift -> if signed then S.bvAShr else S.bvLShr
    BitwiseAnd -> S.bvAnd
    BitwiseOr  -> S.bvOr
    BitwiseXor -> S.bvXOr
    LogicAnd   -> unimplemented
    LogicOr    -> unimplemented
    ArrayStream -> unimplemented
  where unimplemented = panic "Unimplemented" []

symExecBinop bop t | isInteger t =
  case bop of
    Add    -> S.add
    Sub    -> S.sub
    Mul    -> S.mul
    Div    -> S.div
    Mod    -> S.mod
    Lt     -> S.lt
    Leq    -> S.leq
    Eq     -> S.eq
    NotEq  -> \x y -> S.distinct [x, y]
    Cat    -> unimplemented
    LCat   -> unimplemented
    LShift -> unimplemented
    RShift -> unimplemented
    BitwiseAnd -> unimplemented
    BitwiseOr  -> unimplemented
    BitwiseXor -> unimplemented
    LogicAnd   -> unimplemented
    LogicOr    -> unimplemented
    ArrayStream -> unimplemented
  where unimplemented = panic "Unimplemented" []
            
symExecBinop _bop t  = error ("Shouldn't happend: symExecBinop got a " ++ show (pp t))

symExecCoerce :: Type -> Type -> SExpr -> SExpr
symExecCoerce fromT toT v | fromT == toT = v

-- from Integers
-- FIXME: sign?
symExecCoerce fromT (isBits -> Just (_, n)) v | isInteger fromT = do
   S.app (S.fam "int2bv" [n]) [v]

-- From UInts
symExecCoerce (isUInt -> Just _) toT v | isInteger toT =
   S.fun "bv2int" [v]
symExecCoerce (isUInt -> Just n) (isBits -> Just (_signed, m)) v
  | n == m    = v -- included for completeness
  | n < m     = S.zeroExtend (m - n) v
  | otherwise = S.extract v (m - 1) 0

    -- From SInts
symExecCoerce (isSInt -> Just _) _toT _v  =
  error "symExecCoerce from SInt is unimplemented"

symExecCoerce fromT toT _v  =
  error ("Shouldn't happen (symExecCoerce non-reflexive/non-numericb) "
         ++ show (pp fromT) ++ " to " ++ show (pp toT))

symExecV :: TC a Value -> SExpr
symExecV tc =
  case texprValue tc of
    -- Maps
    TCMapEmpty    {} -> unimplemented

    -- Array operations
    TCArrayLength {} -> unimplemented -- sLength $ symExecV v

    -- coercion
    TCCoerce fromT toT v ->
      symExecCoerce fromT toT $ symExecV v

    -- Value constructors
    TCLiteral (LNumber i) t | isInteger t    -> S.int i
    TCLiteral (LNumber i) (isBits -> Just (_, n)) ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    TCLiteral (LNumber {}) _ -> panic "Shouldn't happen: symExecV/TCNumber" []

    TCLiteral (LBool b) _ -> S.bool b
    TCNothing ty -> sNothing (symExecTy ty)
    TCJust     v -> sJust $ symExecV v
    TCLiteral (LByte b) _ -> sByte b

    TCUnit         -> sUnit
    TCStruct ctors (TCon tname _targs) ->
      S.fun (typeNameToCtor tname) $ map (symExecV . snd) ctors
      -- FIXME: we assume order in ctors matches the order in the type.
    TCStruct _ctors _ty -> error "BUG: non-tcon in TCStruct case for symExecV"

    TCLiteral (LBytes bs) _ -> sFromList tByte (map sByte (BS.unpack bs))
    TCArray  vs ty -> sFromList (symExecTy ty) $ map (symExecV) vs

    TCIn lbl v ty@(TCon tname _targs) ->
      S.app (S.as (S.const (labelToField tname lbl)) (symExecTy ty)) . (: []) $ symExecV v
    TCIn _lbl _v _ty -> error "BUG: non-tcon in TCIn case for symExecV"

    TCTriOp     {} -> unimplemented

    TCBinOp op e1 e2 _resT ->
      symExecBinop op (typeOf e1) (symExecV e1) (symExecV e2)
    TCUniOp     {} -> unimplemented
    TCFor {}       -> unimplemented -- symExecVLoop e loop

    TCSelStruct {} -> unimplemented
    TCIf b l r -> do
      S.ite (symExecV b) (symExecV l) (symExecV r)

    TCVar n -> symExecTCName n -- lookupEnv e n
    TCCall {} -> unimplemented -- S.fun (ruleName (tcName fname)) <$> mapM (symExecArg e) args
    TCCase {} -> unimplemented
  where
    unimplemented = error ("SymExecV: Unimplemented: " ++ show (pp tc))

symExecP :: TC a Class -> SExpr -> SExpr
symExecP tc b =
  case texprValue tc of
    -- a set of bytes (aka a byte predicate)
    TCSetAny          -> S.bool True
    TCSetSingle  v    -> S.eq b (symExecV v)
    TCSetComplement c -> S.not (symExecP c b)
    TCSetUnion cs     ->
      S.orMany $ map (\tc' -> symExecP tc' b) cs
    TCSetOneOf bytes  ->
      S.orMany (map (S.eq b . sByte) (BS.unpack bytes))
    TCSetDiff c c'    -> S.and (symExecP c b) (S.not (symExecP c' b))
    TCSetRange l h    -> S.and (S.bvULeq (symExecV l) b)
                               (S.bvULeq b (symExecV h))
    TCFor  {} -> unimplemented
    TCVar  {} -> error "Shoudn't happen: symExecP/TCVar"
    TCCall {} -> unimplemented -- S.fun (ruleName (tcName fname)) <$> ((++) <$> mapM (symExecArg e) args <*> pure [b])
    TCCase {} -> unimplemented
  where
    unimplemented = error ("SymExecP: Unimplemented: " ++ show (pp tc))

-- symExecArg :: Env -> Arg a -> SParserM
-- symExecArg e (ValArg v) = symExecV v
-- symExecArg _ _          = error "Shoudn't happen: symExecArg nonValue"

symExecG :: SExpr -> SExpr -> TC a Grammar -> SExpr
symExecG rel res tc
  | not (isSimpleTC tc) = error "Saw non-simple node in symExecG"
  | otherwise = 
    case texprValue tc of
      -- We (probably) don't care about the bytes here, but this gives smaller models(?)
      TCPure v       -> S.and (S.fun "is-nil" [rel])
                              (S.eq  res (symExecV v))
      TCGetByte {}    -> S.fun "getByteP" [rel, res]
      TCMatch NoSem p -> -- FIXME, this should really not happen (we shouldn't see it)
        S.fun "exists" [ S.List [ S.List [ S.const "$res", tByte ]]
                       , S.and (S.fun "getByteP" [rel, S.const resN]) (symExecP p (S.const resN)) ]
      TCMatch _s p    -> S.and (S.fun "getByteP" [rel, res]) (symExecP p res)
      
      TCMatchBytes _ws v ->
        S.and (S.eq rel res) (S.eq res (symExecV v))

      -- Convert a value of tyFrom into tyTo.  Currently very limited
      TCCoerceCheck ws (Type tyFrom) (Type tyTo) e
        | TUInt _ <- tyFrom, TInteger <- tyTo ->
            S.and (S.fun "is-nil" [rel])
                  (if ws == YesSem then (S.fun "bv2int" [symExecV e]) else S.bool True)
      TCCoerceCheck _ws tyFrom tyTo _e ->
        panic "Unsupported types" [showPP tyFrom, showPP tyTo]
        
      _ -> panic "BUG: unexpected term in symExecG" [show (pp tc)]
     
--------------------------------------------------------------------------------
-- Simple nodes
--
-- A simple node is one where the synthesis can completely determine
-- the value and bytes, e.g. Match, UInt8, etc.  Simple nodes are
-- annotated with a corresponding BytesVar which is used to track
-- bytes assigned earlier.

isSimpleTC :: TC a k -> Bool
isSimpleTC tc =
  case texprValue tc of
    TCMatchBytes {}  -> True
    TCPure {}        -> True
    TCGetByte {}     -> True
    TCMatch {}       -> True
    TCCoerceCheck {} -> True
    _               -> False -- includes unsupported operations as well

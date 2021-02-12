{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}

-- | Symbolically execute a fragment of DDL
module Talos.SymExec ({- symExec, ruleName, -} symExecV, symExecP -- , Env(..)
                     , symExecTy, symExecTyDecl, symExecSummary, solverSynth
                     , symExecFuturePathSet , futurePathSetConfig, futurePathSetRel -- debugging
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
import Talos.Analysis.Annot (isSimpleTC)

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
            TStream     -> error "Unimplemented"
            TByteClass  -> error "Unimplemented" -- Shouldn't happen?
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
            TArray t'   -> tListWithLength (go t') -- S.tArray S.tInt (symExecTy t)
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
      let tyName    = evRelTyName cl ev
          relName   = evRelName cl ev
          ty        = typeOf ev
          hasResult = case ev of { ResultVar {} -> True; ProgramVar {} -> False }
          frees     = [ v | ProgramVar v <- Set.toList (getEntangledVars evs) ]
      symExecFuturePathSet s hasResult tyName relName ty frees fps

configRelTyName :: SummaryClass -> Name -> String
configRelTyName cl root = "RelT-" ++ nameToSMTNameWithClass cl root

configRelName :: SummaryClass -> Name -> String
configRelName cl root = "Rel-" ++ nameToSMTNameWithClass cl root

evRelName, evRelTyName :: SummaryClass -> EntangledVar -> String
evRelName cl ev =
  case ev of
    ResultVar fn _ -> configRelName cl fn
    ProgramVar v   -> configRelName cl (tcName v)

evRelTyName cl ev =
  case ev of
    ResultVar fn _ -> configRelTyName cl fn
    ProgramVar v   -> configRelTyName cl (tcName v)


-- Used to turn future path sets and arg domains into SMT terms.
-- Configs have a single constuctor, so we use define-sort to aid in
-- readibility of the output.
symExecFuturePathSet :: Solver -> Bool -> String -> String -> Type -> 
                        [TCName Value] -> FuturePathSet a -> IO ()
symExecFuturePathSet s hasResult tyName relName ty  frees fps = do
  S.ackCommand s (S.fun "define-sort" [S.const tyName, S.List [], futurePathSetConfig fps])
  void $ S.defineFun s relName args S.tBool (futurePathSetRel (S.const relN) m_resN fps)
  where
    m_resN = if hasResult then Just (S.const resN) else Nothing
    args    = map (\v -> (tcNameToSMTName v, symExecTy ty)) frees
              ++ [(relN, S.const tyName)]
              ++ if hasResult then [(resN, symExecTy ty)] else []

-- FIXME: define Config and Rel simultaneously?

-- This functions constructs a configuration for a future path set,
-- which is the values of the bytes along a particular path.  This
-- avoids having to write the predicate over all bound variables (and
-- will help with recursion too).  A configuration is basically a
-- model for the FPS, and is very close to a parse tree (it is a parse
-- tree over an erased grammar, and can represent trees that don't
-- belong in the grammar, hence the predicate)
futurePathSetConfig :: FuturePathSet a -> SExpr
futurePathSetConfig = collect
  where
    collect fps = 
      case fps of
        Unconstrained -> tUnit
        DontCare _ fps' -> collect fps'
        PathNode (Assertion {}) fps' -> collect fps'
        PathNode n fps' -> tTuple (futureNodeConfig n) (collect fps')
        
futureNodeConfig :: FutureNode a -> SExpr
futureNodeConfig fpn =
  case fpn of
    -- We could also declare an aux type here which would make things
    -- a fair bit more readable.  We could aso use a tree instead of a
    -- list (for term size)
    Choice _ fpss         -> foldr (\fps s -> tSum (futurePathSetConfig fps) s) tUnit fpss
    FNCase (CaseNode { caseAlts = alts, caseDefault = m_fps}) -> encodeCase (NE.toList alts) m_fps
    Call (CallNode { callClass = cl, callPaths = paths }) -> encodeCalls cl (Map.keys paths)

    NestedNode fps -> futurePathSetConfig fps
    -- Base case
    SimpleNode {} -> tBytes
    
    Assertion {} -> panic "Impossible" [] 
  where
    -- Written explicitly to make the decoding clearer
    encodeCalls _  [] = tUnit
    encodeCalls cl (root : rest) =
      tTuple (S.const $ evRelTyName cl root) (encodeCalls cl rest)

    encodeCase [] Nothing    = tUnit
    encodeCase [] (Just fps) = futurePathSetConfig fps
    encodeCase (alt : alts) m_def =
      tSum (futurePathSetConfig (fnAltBody alt)) (encodeCase alts m_def)

-- FIXME: move
solverSynth :: Solver -> SummaryClass -> TCName Value -> ProvenanceTag -> FuturePathSet a -> IO SelectedPath
solverSynth s cl root prov fps = S.inNewScope s $ do
  let modelN = "$model"
  model <- S.declare s modelN (S.const $ configRelTyName cl (tcName root))
  S.assert s (S.fun (configRelName cl (tcName root)) [model])
  r <- S.check s
  case r of
    S.Sat -> pure ()
    -- FIXME: pretty gross
    S.Unsat   -> error "Unsat"
    S.Unknown -> error "Unknown"

  sexp <- getValue modelN  
  ress <- evalVParser s (parseModel prov fps) sexp
  case ress of
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

parseModel :: ProvenanceTag -> FuturePathSet a -> VParser SelectedPath
parseModel prov fps =
  case fps of
    Unconstrained -> Unconstrained <$ pUnit
    DontCare n fps' -> dontCare n <$> parseModel prov fps' 
    PathNode (Assertion {}) fps' -> dontCare 1 <$> parseModel prov fps'
    PathNode n fps'    -> uncurry PathNode <$> pTuple (parseNodeModel prov n) (parseModel prov fps')

parseNodeModel :: ProvenanceTag -> FutureNode a -> VParser SelectedNode
parseNodeModel prov fpn =
  case fpn of
    -- We could also declare an aux type here which would make things
    -- a fair bit more readable.  We could aso use a tree instead of a
    -- list (for term size)
    Choice _ fpss         -> pChoice 0 fpss
    FNCase (CaseNode { caseAlts = alts, caseDefault = m_fps }) -> pCase 0 (NE.toList alts) m_fps
    Call (CallNode { callClass = cl, callPaths = paths }) ->
      SelectedCall cl <$> pCalls (Map.toList paths)
    NestedNode fps        -> SelectedNested <$> parseModel prov fps

    SimpleNode {}         -> (\bs -> SelectedSimple prov bs) <$> pByteString 
    
    Assertion {} -> panic "Impossible" [] 
  where
    pChoice _ [] = panic "Ran out of choices" []
    pChoice n (fps : fpss) = pSum (SelectedChoice n <$> parseModel prov fps) (pChoice (n + 1) fpss)

    pCase _ [] Nothing         = panic "Ran out of cases" []
    pCase n [] (Just fps)      = SelectedCase n <$> parseModel prov fps 
    pCase n (alt : alts) m_def = pSum (SelectedCase n <$> parseModel prov (fnAltBody alt))
                                      (pCase (n + 1) alts m_def)

    pCalls [] = pure Unconstrained
    pCalls ((_, Wrapped (_, fps)) : rest) = do
      (l, rest') <- pTuple (parseModel prov fps) (pCalls rest)
      pure (mergeSelectedPath l rest')

-- Define a predicate over a corresponding config.  This holds when a
-- path is valid --- a model for this (i.e. a config) tells us the
-- path and bytes.

-- FIXME: we will use the '$cfg' variable to refer to the current
-- config, and use shadowing whe updating.
relN, resN :: String
relN = "$rel"
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
        Choice _ fpss -> foldMap go fpss
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
futurePathSetRel :: SExpr -> Maybe SExpr -> FuturePathSet a -> SExpr
futurePathSetRel rel m_res fps0 =
  mkExists (Set.toList (assignedVars fps0))
           (mkAnd (collect rel fps0))
  where
    collect rel' fps =
      let mkRest fps' = mklet relN (sSnd rel') (mkAnd (collect (S.const relN) fps')) in
      case fps of
        Unconstrained -> []
        DontCare _ fps' -> collect rel' fps'
            
        PathNode (Assertion a) fps' -> -- Doesn't update rel
          -- We don't have an associated cfg, so we just pass the
          -- current cfg on.
          symExecAssertion a : collect rel' fps'
        
        PathNode (SimpleNode rv tc) fps' -> -- Binds a variable.
          [ mklet relN (sFst rel') (symExecG (S.const relN) (evToRHS rv) tc)
          , mkRest fps'
          ]
          
        PathNode n fps' -> goN (sFst rel') n ++ [mkRest fps']

    -- We inline the Node cases as they impact how we unpack the
    -- cfg.  For the moment we turn Choose { A; B; C} into
    -- match $rel with
    --  Inl $rel -> ... A ...
    --  Inr $rel -> match $rel with
    --      Inl $rel -> ... B ...
    --      Inr $rel -> match $rel with
    --          Inl $rel -> ... C ...
    --          Inr $rel -> false
    -- goN :: SExpr -> FutureNode a -> [SExpr]
    goN rel' fpn =
      case fpn of
        Assertion {} -> error "futurePathSetRel Impossible"
        SimpleNode {} -> error "futurePathSetRel Impossible"

        -- We use shadowing of relN here
        Choice _ fpss -> 
          [ mklet relN rel'
            (foldr (\fps'' s ->
                      mkSumMatch (S.const relN) relN (mkAnd (collect (S.const relN) fps'')) relN s
                  ) (S.bool False) fpss)
          ]

        -- There are a few ways to do this, we could (1) case over the
        -- model, and assert the ctor; (2) case over the ctor and
        -- assert the model; or (3) use 'or' and assert both.  (1) is
        -- probably the easist, so that is what we will do for now.
        -- This also gives a nicer story for union patterns.
        FNCase (CaseNode { caseCompleteness = _compl  -- we don't care here, we assert an alt is taken
                         , caseSummary      = summary
                         , caseTerm         = e
                         , caseAlts         = alts
                         , caseDefault      = m_def}) -> 
          let matchN = "$match"
          in [ mklet matchN (symExecV e)
               (relCase rel' (S.const matchN) summary (NE.toList alts) m_def)
             ]
          
        Call (CallNode { callClass = cl, callResultAssign = res', callAllArgs = args, callPaths = paths })
          -> [ relCalls cl rel' res' args (Map.toList paths) ]

        NestedNode fps -> collect rel' fps

    -- matchDef m_summary m_def =
    --   case (m_summary, m_def) of
    --     (Nothing, Nothing) -> [] -- case is total, no default
    --     (Nothing, Just fps) -> [(S.const "_", mkAnd   )]
  
    -- expandAlts rel res alt = 

    relCalls _   _    _      _    []                    = S.bool True
    relCalls cl  rel' m_res' args ((ev, Wrapped (evs, _)) : rest) = 
      let resArg = maybeToList (evToRHS <$> m_res')
          -- c.f. symExecDomain
          actuals = [ maybe (panic "Missing argument" [showPP v]) symExecV (Map.lookup v args)
                    | ProgramVar v <- Set.toList (getEntangledVars evs) ]
          callRel = S.fun (evRelName cl ev) (actuals ++ [sFst rel'] ++ resArg)
          restRel = mklet relN (sSnd rel') (relCalls cl (S.const relN) m_res' args rest)
      in S.and callRel restRel

    -- FIXME: Maybe lift these out to a top-level function?
    relCase _rel' _e _summary [] Nothing    = S.bool False
    relCase rel' e summary [] (Just fps) =
      mkAnd (assertIsMissingLabel e summary : collect rel' fps)
    relCase rel' e summary (FNAlt { fnAltPatterns = pats, fnAltBody = b } : alts) m_def =
      let lhs = mkExists (Set.toList (Set.unions (map patBindsSet pats)))
                         (mkAnd (concatMap (relPattern e) pats ++ collect (S.const relN) b))
          rhs = relCase (S.const relN) e summary alts m_def
      in mkSumMatch rel' relN lhs relN rhs

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
      
    mkAnd []  = S.bool True
    mkAnd [x] = x
    mkAnd xs  = S.andMany xs

    mkExists [] b = b
    mkExists xs b =
      S.fun "exists" [ S.List (map (\x -> S.List [ symExecTCName x, symExecTy (typeOf x) ]) xs)
                     , b
                     ]
  
    mkMatch e cs =
      S.fun "match" [ e
                    , S.List [ S.List [ pat, rhs ] | (pat, rhs) <- cs ]
                    ]

    mkSumMatch e vl l vr r =
      mkMatch e [ (S.fun "inl" [S.const vl], l)
                , (S.fun "inr" [S.const vr], r)
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
    TCArrayLength v -> sLength $ symExecV v

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
     

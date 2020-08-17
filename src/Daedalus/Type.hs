{-# Language BlockArguments, OverloadedStrings, NamedFieldPuns #-}
{-# Language DataKinds, GADTs #-}
{-# Language RecordWildCards #-}
{-# Language RankNTypes #-}
{-# Language ParallelListComp #-}
module Daedalus.Type where

import Control.Monad(forM,forM_,unless)
import Data.Graph.SCC(stronglyConnComp)
import Data.Graph(SCC(..))
import Data.List(foldl',sort,group)
import Data.Maybe(mapMaybe,catMaybes)
import Control.Monad(zipWithM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Parameterized.Some(Some(..))

import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.Rec(sccToRec)
import Daedalus.Panic(panic)

import Daedalus.AST
import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Type.Constraints
import Daedalus.Type.Kind
import Daedalus.Type.Traverse
import Daedalus.Type.Subst
import Daedalus.Type.InferContext

inferRules :: Module -> MTypeM (TCModule SourceRange)
inferRules m = go [] (moduleRules m)
  where
  getDeps d = (d, tctyName d, Set.toList (collectTypes freeTCons (tctyDef d)))

  go done todo =
    case todo of
      [] -> pure TCModule
                   { tcModuleName = moduleName m
                   , tcModuleImports = moduleImports m
                   , tcModuleTypes = map sccToRec
                                   $ stronglyConnComp
                                   $ map getDeps
                                   $ concatMap (Map.elems . tcTypeDefs) done
                   , tcModuleDecls = map tcDecls (reverse done)
                   }

      x : more ->
        do info <- runSTypeM (generalize =<< inferRuleRec x)
           let env = [ (tcDeclName d, declTypeOf d)
                     | d <- recToList (tcDecls info)
                     ]
           extEnvManyRules env
             $ extGlobTyDefs (tcTypeDefs info)
             $ go (info : done) more



--------------------------------------------------------------------------------


data DeclInfo = DeclInfo
  { tcDecls     :: Rec (TCDecl SourceRange)
  , tcTypeDefs  :: Map TCTyName TCTyDecl
  }



generalize :: Rec (TCDecl SourceRange) -> STypeM DeclInfo
generalize ds =
  do (lcs,tds,ds1) <- simpCtrs ds

     -- Check no left-over mono types
     cs <- forM lcs \lc ->
              case thingValue lc of
                IsNamed _ -> reportError lc "Failed to infer type."
                x         -> pure x

     -- Check that all types that needed definitions were defined
     todo <- getNeedsDef
     forM_ todo \l ->
         do let nm = thingValue l
            unless (nm `Map.member` tds) $
               do mb <- lookupTypeDef nm
                  case mb of
                    Nothing -> reportError l ("Name" <+> backticks (pp nm)
                                         <+> "does not refer to a named type.")
                    _ -> pure ()





     -- Since we don't have local definitions there should be no free
     -- type variable in the environment
     let freeInTys = completeFreeInTD tds
         as        = Set.toList
                   $ Set.unions ( freeTVS ds1
                                : freeTVS cs
                                : Map.elems freeInTys
                                )

     pure $ doGeneralize as cs (Set.toList <$> freeInTys)
            DeclInfo { tcDecls    = ds1
                     , tcTypeDefs = tds
                     }


simpCtrs :: Rec (TCDecl SourceRange) ->
            STypeM ( [Located Constraint]
                   , Map TCTyName TCTyDecl
                   , Rec (TCDecl SourceRange)
                   )
simpCtrs ds =
  do lcs  <- simplifyConstraints
     ds1  <- traverse (traverseTypes zonkT) ds
     tds  <- getNewTypeDefs
     let (lcs1,tds1,ds2,hasRen) = renameAnonTC lcs tds ds1

     -- if some types were renamed, add back constraints and keep simplifying
     if hasRen
       then do forM_ lcs1 \lc -> addConstraint lc (thingValue lc)
               replaceNewTypeDefs tds1
               simpCtrs ds2
       else pure (lcs1,tds1,ds2)


{- | If the result of a declaration is an anonymous named type that
matches the declaration's name, then we make this type not-anonymous.
Note that as a result, we may have to revisit some of the existing
constraints (e.g. Has), which were not solved because the non-anonymous
type was not yet defined.
-}
renameAnonTC ::
  [Located Constraint] ->
  Map TCTyName TCTyDecl ->
  Rec (TCDecl SourceRange) ->
  ( [Located Constraint]
  , Map TCTyName TCTyDecl
  , Rec (TCDecl SourceRange)
  , Bool
  )
renameAnonTC lcs tys ds
  | Map.null renSu = (lcs,tys,ds,False)
  | otherwise = ( [ mapTypes renTC lc | lc <- lcs ]
                , Map.fromList
                      [ (tctyName d,d) | d <- map renTD (Map.elems tys) ]
                , renD <$> ds
                , True
                )
  where
  -- renaming substitutin
  renSu = Map.fromList (mapMaybe shouldRename (recToList ds))

  -- replace anonymous types with the entries from the renaming substitutin.
  renTC ty = case ty of
               TVar _ -> ty
               TCon x ts -> TCon y (map renTC ts)
                  where y = Map.findWithDefault x x renSu
               Type tf -> Type (renTC <$> tf)

  renD = mapTypes renTC

  renTD t = let nm = Map.findWithDefault (tctyName t) (tctyName t) renSu
            in t { tctyName = nm
                 , tctyDef = mapTypes renTC (tctyDef t)
                 }

  -- Check if the result type of declaration matches one of the
  -- anonymous types from the declaration's definition.
  shouldRename TCDecl { tcDeclDef, tcDeclName } =
    case typeOf tcDeclDef of
      Type (TGrammar ty) -> matches ty
      ty -> matches ty
    where
    matches ty =
      case ty of
        TCon nm@(TCTyAnon x _) _
          | tcDeclName == x -> Just (nm, TCTy x)
        _ -> Nothing


{- | Free variables in a collection of (possible recursive) declarations.
Example:

    data T1 = MkT1 T2
    data T2 = MkT2 ?a T1

We'd like to generalize the free variable `?a`.  Note that it is not enough
to compute just the free variables in each type individually, as this would
give us:

    data T1   = MkT1 T2     -- WRONG:
    data T2 a = MkT2 a T1

To get the correct answer each type needs not only its parameters, but
also the paraemetrs for its dependency.  Mutually recursive types, like
in the above example, are processed together.
-}

completeFreeInTD :: Map TCTyName TCTyDecl -> Map TCTyName (Set TVar)
completeFreeInTD tds = foldl' addFree Map.empty
                     $ stronglyConnComp
                     $ map getDeps
                     $ Map.elems tds
  where
  freeInTys = freeTVS <$> tds

  getDeps d = let me   = tctyName d
                  deps = Set.toList (collectTypes freeTCons (tctyDef d))
              in ((me,deps),me,deps)

  lkp mp x = Map.findWithDefault Set.empty x mp
  addFree mp scc =
    let one (me,deps) = Set.unions (lkp freeInTys me : map (lkp mp) deps)
    in case scc of
         AcyclicSCC t -> Map.insert (fst t) (one t) mp
         CyclicSCC ts -> let vs      = Set.unions (map one ts)
                             ins m x = Map.insert (fst x) vs m
                          in foldl' ins mp ts




doGeneralize :: [TVar] {- ^ Params for decl -} ->
                [Constraint] {- ^ Constraints on type (for decl) -} ->
                Map TCTyName [TVar] {- ^ Params for each type -} ->
                DeclInfo -> DeclInfo
doGeneralize as cs tparams decls
  | null as   = decls -- tparams are asssumed to be a subset of as
  | otherwise = DeclInfo
                  { tcTypeDefs = addTPsTy <$> tcTypeDefs decls
                  , tcDecls =
                      case tcDecls decls of
                        NonRec d  -> NonRec (addTPs False d)
                        MutRec ds -> MutRec (map (addTPs True) ds)
                  }
  where
  ts      = map TVar as
  tconMap = map TVar <$> tparams
  dMap    = Map.fromList [ (tcDeclName d, ts) | d <- recToList (tcDecls decls) ]

  addTPs :: Bool -> TCDecl SourceRange -> TCDecl SourceRange
  addTPs r TCDecl { .. } =
           TCDecl { tcDeclName     = tcDeclName
                  , tcDeclTyParams = as
                  , tcDeclCtrs     = map (fixUpTCons tconMap) cs
                  , tcDeclParams   = fixUpTCons tconMap tcDeclParams
                  , tcDeclDef      =
                      case fixUpTCons tconMap tcDeclDef of
                        Defined d | r -> Defined (fixUpRecCallSites dMap d)
                        res ->  res
                  , tcDeclCtxt     = tcDeclCtxt
                  }

  addTPsTy d = fixUpTCons tconMap d { tctyParams = tparams Map.! tctyName d }





fixUpTCons :: TraverseTypes a => Map TCTyName [Type] -> a -> a
fixUpTCons mp = if Map.null mp then id else mapTypes (fixUpTConsT mp)


fixUpTConsT :: Map TCTyName [Type] -> Type -> Type
fixUpTConsT mp ty =
  case ty of
    TVar {} -> ty
    TCon x [] | Just ts <- Map.lookup x mp -> TCon x ts
    TCon x ts -> TCon x (map (fixUpTConsT mp) ts)
    Type tf -> Type (fixUpTConsT mp <$> tf)

fixUpRecCallSites :: Map Name [Type] -> TC SourceRange k -> TC SourceRange k
fixUpRecCallSites ch expr =
  exprAt expr $
    case mapTCF (fixUpRecCallSites ch) (texprValue expr) of
      TCCall x [] es | Just ts <- Map.lookup (tcName x) ch -> TCCall x ts es
      e                                                    -> e


inferRuleRec :: Rec Rule -> STypeM (Rec (TCDecl SourceRange))
inferRuleRec sr =
  case sr of
    NonRec r ->
      do d <- fst <$> inferRule r
         d1 <- traverseTypes zonkT d
         mapM_ defaultByKind (Set.toList (freeTVS d1))
         pure (NonRec d1)
    MutRec rs ->
      do rts <- mapM guessRuleType rs
         res <- extEnvManyRules rts (zipWithM checkRule rs (map snd rts))
         res1 <- traverseTypes zonkT res
         let decls = MutRec res1

         -- Default type variables based on kind
         mapM_ defaultByKind (Set.toList (freeTVS decls))
         pure decls

  where
  -- we only allow for polymorphism at kinds Type, and Num
  defaultByKind v =
    case tvarKind v of

      KGrammar ->
        do a <- newTVar v KValue
           unify (tGrammar a) (v,TVar v)

      KClass ->
        unify tByteClass (v,TVar v)

      KValue -> pure ()

      KNumber -> pure ()




  guessType :: Name -> STypeM Type
  guessType nm@Name { nameContext } =
    case nameContext of
      AGrammar -> newTVar nm KGrammar
      AClass   -> newTVar nm KClass
      AValue   -> newTVar nm KValue

  guessParamType :: RuleParam -> STypeM Type
  guessParamType p = guessType (paramName p)

  guessRuleType r =
    do ts <- mapM guessParamType (ruleParams r)
       t  <- guessType (ruleName r)
       let rt = ts :-> t
       pure (ruleName r, Poly [] [] rt)

  checkRule r (Poly _ _ (gAs :-> gR)) =
    do (res, actualAs :-> actualR) <- inferRule r
       forM_ (zip3 (ruleParams r) gAs actualAs) \(l,g,a) ->
          unify g (l,a)

       unify gR (ruleName r, actualR)
       pure res


-- | Infer a mono type for a collection of rules.
inferRule :: Rule -> STypeM (TCDecl SourceRange, RuleType)
inferRule r = runTypeM (ruleName r) (addParams [] (ruleParams r))
  where
  checkSig :: Name -> Maybe SrcType -> TypeM ctx Param
  checkSig n@Name { nameContext } mb =
    let nm t = TCName { tcName    = n
                      , tcType    = t
                      , tcNameCtx = nameContext }
    in
    case nameContext of
      AGrammar -> GrammarParam . nm <$>
                  case mb of
                    Nothing   -> newTVar n KGrammar
                    Just srct -> tGrammar <$> checkType KValue srct
                    -- for now sigs are not functions, XXX

      AClass   -> ClassParam . nm <$>
                  case mb of
                    Nothing   -> newTVar n KClass
                    Just srct -> checkType KClass srct

      AValue   -> ValParam . nm <$>
                  case mb of
                    Nothing   -> newTVar n KValue
                    Just srct -> checkType KValue srct

  checkRet :: Name -> SrcType -> TypeM ctx Type
  checkRet Name { nameContext } srct =
     case nameContext of
       AGrammar -> tGrammar <$> checkType KValue srct
       AValue   -> checkType KValue srct
       AClass   -> checkType KClass srct



  addParams done ps =
    case ps of
      [] ->
        case ruleName r of
          Name { nameContext } ->
            inContext nameContext
            do def <- case ruleDef r of
                        Nothing ->
                          case ruleResTy r of
                            Nothing -> reportError (ruleName r)
                                  "Primitive declarations need a result type."
                            Just srct ->
                               ExternDecl <$> checkRet (ruleName r) srct
                        Just def ->
                          do (e1,t) <- inferExpr def
                             case kindOf t of
                               KGrammar ->
                                 do a <- newTVar e1 KValue
                                    unify (tGrammar a) (e1,t)
                               _ -> pure ()
                             ty1 <- typeOf <$>
                                            checkSig (ruleName r) (ruleResTy r)
                             unify ty1 (e1,t)
                             Defined <$> traverseTypes zonkT e1
               let tps = reverse done
                   d   = TCDecl { tcDeclName     = ruleName r
                                , tcDeclTyParams = []
                                , tcDeclCtrs     = []
                                , tcDeclParams   = tps
                                , tcDeclDef      = def
                                , tcDeclCtxt     = nameContext
                                }
               pure (d, map typeOf tps :-> typeOf def)

      p : more ->
        do pa <- checkSig (paramName p) (paramType p)
           extEnv (paramName p) (typeOf pa) (addParams (pa : done) more)


--------------------------------------------------------------------------------




data BindStmt = BindStmt (TCName Value) (TC SourceRange Grammar)

addBind :: BindStmt -> TC SourceRange Grammar -> TC SourceRange Grammar
addBind (BindStmt n e1) e2 = exprAt (e1 <-> e2) (TCDo (Just n) e1 e2)

addBinds :: [BindStmt] -> TC SourceRange Grammar -> TC SourceRange Grammar
addBinds xs e = foldr addBind e xs

-- | Lift a value argument of a function, when the function is called
-- in a monadic context.
liftValArg ::
  Expr ->
  TypeM ctx ((Arg SourceRange,Type), Maybe BindStmt)
liftValArg e =

  case ctx of

    Some AGrammar ->
      do (e1,t) <- allowPartialApps False $ inContext AGrammar (inferExpr e)
         a      <- newTVar e KValue
         unify (tGrammar a) (e1,t)
         n      <- newName e a
         pure ( (ValArg (exprAt e1 (TCVar n)), a), Just (BindStmt n e1) )

    Some AValue ->
      do (e1,t) <- inContext AValue (inferExpr e)
         pure ((ValArg e1, t), Nothing)

    Some AClass ->
      reportError e "Expected a value, but the argument is a character class."

  where ctx = inferContext e



-- | This allowes pure functions to be applied to parsers, automatically
-- lifting the results using bind.  Note that this assumes that the callback
-- will ensure that the result is of type `Grammar t`.
-- For example @F P a Q@ becomes @do x <- P; y <- q; F x a y@
liftApp ::
  [Expr] ->
  ([(Arg SourceRange, Type)] -> TypeM ctx (TC SourceRange Grammar, Type)) ->
  TypeM ctx (TC SourceRange Grammar, Type)
liftApp es f =
  do (args,mbs) <- unzip <$> mapM liftValArg es
     (rE,rT) <- f args
     let expr = addBinds (catMaybes mbs) rE
     pure (expr, rT)



-- | Lift a function with value-only parameters. Lifts to bind:
-- `f P q` becomes `do x <- P; f x q`.  Arguments that may be either grmmar
-- or value (e.g. "A") are interepreted as values and left alone.
liftValApp ::
  HasRange r =>
  r ->
  [Expr] ->
  ( [(TC SourceRange Value, Type)] ->
    TypeM ctx (TC SourceRange Grammar, Type)
  ) ->
  TypeM ctx (TC SourceRange ctx, Type)
liftValApp r es f =
  do ctx <- getContext
     case ctx of
       AGrammar -> liftApp es \args -> f =<< mapM validateArg args
       AClass {} ->
         reportError r "Expected a character-class but found a grammar."
       AValue {} ->
         reportError r "Expected a value, but found a grammar."
  where
  validateArg (a,t) =
    case a of
      ValArg v   -> pure (v,t)
      ClassArg e ->
        reportError e "Expected a value, but found a character-class."
      GrammarArg _ ->
        panic "liftValApp" ["Unexpected grammar argument in lift."]


-- | Lift a pure function as a functor
-- (if in a grammar context, otherwise just call it)
-- This converts `f P q` into `do x <- P; ^ f x q`.
liftValAppPure ::
  HasRange r =>
  r ->
  [Expr] ->
  ([(TC SourceRange Value, Type)] -> TypeM ctx (TC SourceRange Value, Type)) ->
  TypeM ctx (TC SourceRange ctx, Type)
liftValAppPure r es f =
  do ctx <- getContext
     case ctx of
       AGrammar -> liftApp es \args ->
                      do vs <- mapM validateArg args
                         (res,t) <- f vs
                         pure (exprAt r (TCPure res), tGrammar t)
       AClass {} ->
         reportError r "Expected a character-class but encountered a value."
       AValue {} -> f =<< mapM inferExpr es
  where
  validateArg (a,t) =
    case a of
      ValArg v -> pure (v,t)
      ClassArg e -> reportError e
                      "Expected a value, but encountered a character-class."
      GrammarArg _ ->
        panic "liftValApp" ["Unexpected grammar argument in lift."]



inferExpr :: Expr -> TypeM ctx (TC SourceRange ctx,Type)
inferExpr expr =
  case exprValue expr of

    EBool b -> liftValAppPure expr [] \_ -> pure (exprAt expr (TCBool b), tBool)

    ENothing -> liftValAppPure expr [] \_ ->
                do a <- newTVar expr KValue
                   pure (exprAt expr (TCNothing a), tMaybe a)

    EJust e ->
      liftValAppPure expr [e] \ ~[(e',t)] ->
        pure (exprAt expr (TCJust e'), tMaybe t)

    ENumber n ->
      do ctxt <- getContext
         case ctxt of
           AClass
             | 0 <= n && n < 256 ->
                pure ( exprAt expr $ TCSetSingle
                     $ exprAt expr $ TCByte $ fromInteger n
                    , tByteClass
                    )
             | otherwise ->
                reportError expr "Byte literal out of range."

           _ -> liftValAppPure expr [] \_ ->
                do a <- newTVar expr KValue
                   addConstraint expr (Literal n a)
                   pure (exprAt expr (TCNumber n a), a)

    EByte w ->
      do ctxt <- getContext
         case ctxt of
           AClass   -> promoteValueToSet =<< inContext AValue (inferExpr expr)

           _ -> liftValAppPure expr [] \_ ->
                 pure (exprAt expr (TCByte w), tByte)

    EBytes bs ->
      do ctxt <- getContext
         case ctxt of
           AClass   -> pure (exprAt expr (TCSetOneOf bs), tByteClass)

           _ -> liftValAppPure expr [] \_ ->
                pure (exprAt expr (TCByteArray bs), tArray tByte)

    EMatch1 e ->
      grammarOnly expr $
      inContext AClass
      do (e1,_) <- inferExpr e
         pure (exprAt expr (TCMatch YesSem e1), tGrammar tByte)

    EMatch e ->
      liftValApp expr [e] \ ~[(e1,t)] ->
      do let ty = tArray tByte
         unify ty (e1,t)
         pure (exprAt expr (TCMatchBytes YesSem e1), tGrammar ty)

    EUniOp op e ->
      case op of

        Not ->
          do ctxt <- getContext
             case ctxt of
               AClass -> do (e1,_) <- inferExpr e
                            pure (exprAt expr (TCSetComplement e1), tByteClass)

               _ -> liftValAppPure expr [e] \ ~[(e1,t)] ->
                    do unify tBool (e,t)
                       pure (exprAt expr (TCUniOp Not e1), tBool)

        -- XXX: We don't seem to have surface syntax for this?
        Neg ->
          liftValAppPure expr [e] \ ~[(e1,t)] ->
          do addConstraint expr (Numeric t)
             pure (exprAt expr (TCUniOp Neg e1), t)

        Concat ->
          do ctxt <- getContext
             case ctxt of
               AClass ->
                 do (e1,t) <- inContext AValue (inferExpr e)
                    promoteValueToSet =<< concatVal e1 t

               _ -> liftValAppPure expr [e] \ ~[(e1,t)] -> concatVal e1 t

          where
          concatVal :: TC SourceRange Value -> Type ->
                       TypeM ctx (TC SourceRange Value,Type)
          concatVal e1 t =
            do a <- newTVar e KValue
               unify (tArray (tArray a)) (e,t)
               pure (exprAt expr (TCUniOp Concat e1), tArray a)

        BitwiseComplement ->
          liftValAppPure expr [e] \ ~[(e1',t)] ->
          do a       <- newTVar e1' KNumber
             unify (tUInt a) (e1', t)
             pure (exprAt expr (TCUniOp BitwiseComplement e1'), t)

    ETriOp op e1 e2 e3 ->
      case op of
        RangeUp -> rangeOp
        RangeDown -> rangeOp

      where
      rangeOp =
        liftValAppPure expr [e1,e2,e3] \ ~[ r1@(e1',t1)
                                          , r2@(e2',_)
                                          , r3@(e3',_)
                                          ] ->
        do unify r1 r2
           unify r1 r3
           addConstraint expr (Numeric t1)
           pure (exprAt expr (TCTriOp op e1' e2' e3' (tArray t1)), tArray t1)


    EBinOp op e1 e2 ->
      case op of

        ArrayStream ->
          liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
          do unify (tArray tByte) (e1',t1)
             unify (tArray tByte) (e2',t2)
             pure (exprAt expr (TCBinOp op e1' e2' tStream), tStream)


        LCat ->
          liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
          do addConstraint expr (Numeric t1)
             r <- newTVar e2 KNumber
             unify (tUInt r) (e2',t2)

             pure (exprAt expr (TCBinOp LCat e1' e2' t1), t1)


        Cat ->
          liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
          do l1       <- newTVar e1 KNumber
             unify (tUInt l1) (e1,t1)

             l2       <- newTVar e2 KNumber
             unify (tUInt l2) (e2,t2)

             l3 <- newTVar expr KNumber
             addConstraint expr (CAdd l1 l2 l3)

             let res = tUInt l3
             pure (exprAt expr (TCBinOp op e1' e2' res), res)

        LShift -> shiftOp
        RShift -> shiftOp
        BitwiseAnd -> bitwiseOp
        BitwiseOr  -> bitwiseOp
        BitwiseXor -> bitwiseOp

        Add   -> num2
        Sub   -> num2
        Mul   -> num2
        Div   -> num2
        Mod   -> num2
        Lt    -> rel
        Leq   -> rel
        Eq    -> relEq
        NotEq -> relEq

      where
      bitwiseOp =
        liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
        do addConstraint expr (Numeric t1)
           unify (e1', t1) (e2', t2)
           pure (exprAt expr (TCBinOp op e1' e2' t1), t1)

      shiftOp =
        liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
        do addConstraint expr (Numeric t1)
           unify tInteger (e2',t2)
                              -- XXX: either overload or maybe use another type
                              -- Same as Many
           pure (exprAt expr (TCBinOp op e1' e2' t1), t1)

      num2 = liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
             do addConstraint e1 (Numeric t1)
                addConstraint e2 (Numeric t2)
                unify (e1', t1) (e2',t2)
                pure (exprAt expr (TCBinOp op e1' e2' t1), t1)

      -- XXX: what types should allow to be compared:
      -- only numeric? or do structural comparisons?
      rel = liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
            do unify (e1',t1) (e2',t2)
               pure (exprAt expr (TCBinOp op e1' e2' tBool), tBool)

      -- In case we want to specialise either < or =
      relEq = rel


    ESel e l ->
      do a      <- newTVar expr KValue
         ctxt   <- getContext
         case ctxt of
           AClass -> promoteValueToSet =<< inContext AValue (inferExpr expr)

           _ ->

             case l of
               SelStruct f ->
                 liftValAppPure expr [e] \ ~[(e1,t)] ->
                 do let lab = thingValue f
                    addConstraint f (HasStruct t lab a)
                    pure (exprAt expr (TCSelStruct e1 lab a), a)

               SelUnion f ->
                 grammarOnly expr $
                 liftValApp expr [e] \ ~[(e1,t)] ->
                 do let lab = thingValue f
                    addConstraint f (HasUnion t lab a)
                    pure (exprAt expr (TCSelUnion YesSem e1 lab a), tGrammar a)

               SelTrue ->
                 grammarOnly expr $
                 liftValApp expr [e] \ ~[(e1,t)] ->
                 do unify tBool (e1,t)
                    pure (exprAt expr (TCGuard e1), tGrammar tUnit)

               SelFalse ->
                 grammarOnly expr $
                 liftValApp expr [e] \ ~[(e1,t)] ->
                 do unify tBool (e,t)
                    pure (exprAt expr $ TCGuard $
                           exprAt expr $ TCUniOp Not e1, tGrammar tUnit)

               SelNothing ->
                 grammarOnly expr $
                 liftValApp expr [e] \ ~[(e1,t)] ->
                 do unify (tMaybe a) (e,t)
                    pure ( exprAt expr $ TCGuard
                         $ exprAt expr $ TCBinOp Eq e1
                                                    (exprAt expr (TCNothing a))
                                                    tBool
                         , tGrammar tUnit
                         )

               SelJust ->
                 grammarOnly expr $
                 liftValApp expr [e] \ ~[(e1,t)] ->
                 do unify (tMaybe a) (e1,t)
                    pure (exprAt expr (TCSelJust YesSem e1 a), tGrammar a)


    EEnd ->
      grammarOnly expr
      (pure (exprAt expr TCEnd, tGrammar tUnit))

    EOffset ->
      grammarOnly expr
      (pure (exprAt expr TCOffset, tGrammar tInteger))

    ECurrentStream ->
      grammarOnly expr
      (pure (exprAt expr TCCurrentStream, tGrammar tStream))

    ESetStream s ->
      grammarOnly expr $
      liftValApp expr [s] \ ~[(e,t)] ->
      do unify tStream (e,t)
         pure (exprAt expr (TCSetStream e), tGrammar tUnit)

    EStreamLen i s ->
      grammarOnly expr $
      liftValApp expr [i,s] \ ~[(ie,it),(se,st)] ->
      do unify tInteger (ie,it)
         unify tStream (se,st)
         pure (exprAt expr (TCStreamLen YesSem ie se), tGrammar tStream)

    EStreamOff i s ->
      grammarOnly expr $
      liftValApp expr [i,s] \ ~[(ie,it),(se,st)] ->
      do unify tInteger (ie,it)
         unify tStream (se,st)
         pure (exprAt expr (TCStreamOff YesSem ie se), tGrammar tStream)

    EMapEmpty ->
      liftValAppPure expr [] \_ ->
      do kt <- newTVar expr KValue
         vt <- newTVar expr KValue
         let t = tMap kt vt
         pure (exprAt expr (TCMapEmpty t), t)

    EMapInsert k v m ->
      grammarOnly expr $
      liftValApp expr [k,v,m] \ ~[(k1,kt),(v1,vt),(m1,mt)] ->
      do unify (tMap kt vt) (m, mt)
         pure (exprAt expr (TCMapInsert YesSem k1 v1 m1), tGrammar mt)

    EMapLookup k m ->
      grammarOnly expr $
      liftValApp expr [k,m] \ ~[(k1,kt),(m1,mt)] ->
      do vt <- newTVar m KValue
         unify (tMap kt vt) (m, mt)
         pure (exprAt expr (TCMapLookup YesSem k1 m1), tGrammar vt)

    EArray es ->
      do ctxt <- getContext
         case ctxt of
           AClass ->
             do es1 <- mapM inferExpr es
                pure (exprAt expr (TCSetUnion (map fst es1)), tByteClass)

           _ -> liftValAppPure expr es \res ->
                do t <- newTVar expr KValue
                   mapM_ (unify t) res
                   pure (exprAt expr (TCArray (map fst res) t), tArray t)

    EArrayLength e ->
      liftValAppPure expr [e] \ ~[(e1,et)] ->
      do vt <- newTVar e KValue
         unify (tArray vt) (e, et)
         pure (exprAt expr (TCArrayLength e1), tInteger)

    EArrayIndex e ix ->
      grammarOnly expr $
      liftValApp expr [e,ix] \ ~[(e1,et), (ix1,ixt)] ->
      do vt         <- newTVar e KValue
         unify (tArray vt) (e, et)
         unify tInteger (ix, ixt)
         pure (exprAt expr (TCArrayIndex YesSem e1 ix1), tGrammar vt)

    EStruct fs ->
      do ctxt <- getContext
         case ctxt of
           AClass   -> reportError expr "Unexpected struct in a byte set."

           AValue ->
             do fs1 <- forM fs \sf ->
                         case sf of
                           COMMIT r -> reportError r
                                    "COMMIT may not appear in a semantic value."
                           Anon e -> reportError e "Struct value needs a label."
                           x := e ->
                             do (e1,t1) <- inContext AValue (inferExpr e)
                                pure (x,e1,t1)

                           -- this could make sense as a `let`?
                           x :@= _ -> reportError x "Unexpected local variable."

                let (xs,es,ts) = unzip3 fs1
                    ls = map nameScopeAsLocal xs
                pureStruct expr ls ts es


           AGrammar -> inferStructGrammar expr fs


    EChoiceU cmt e1 e2 ->
      do ctxt <- getContext
         case ctxt of
           AClass -> do (e1',_) <- inferExpr e1
                        (e2',_) <- inferExpr e2
                        pure ( exprAt expr (TCSetUnion [e1',e2'])
                             , tByteClass
                             )


           -- XXX: reusing | for both or and choice is highly questionable
           -- Because of the reuse, lifint doesn't work
           AValue -> do (e1',t1) <- inferExpr e1
                        (e2',t2) <- inferExpr e2
                        unify (e1',t1) (e2',t2)
                        addConstraint expr (Numeric t1)
                        pure ( exprAt expr (TCBinOp BitwiseOr e1' e2' t1)
                             , t1
                             )

           AGrammar ->
             do (eL,tL) <- inferExpr e1
                (eR,tR) <- inferExpr e2
                unify (eL,tL) (eR,tR)
                a <- grammarResult expr tL
                pure (exprAt expr (TCChoice cmt [eL,eR] a), tL)

    EChoiceT c [] ->
      grammarOnly expr
      do a <- newTVar expr KValue
         pure (exprAt expr (TCChoice c [] a), tGrammar a)

    EChoiceT c fs ->
      do ctxt <- getContext
         case ctxt of

           AClass ->
             reportError expr "Invalid character class"

           -- XXX: use different notation to support lifting
           AValue ->
             case fs of
               [ lf :> e ] ->
                do (e1,t) <- inferExpr e
                   ty     <- newTVar expr KValue
                   let l = thingValue lf
                   addConstraint lf (HasUnion ty l t)
                   addConstraint expr (IsNamed ty)
                   pure (exprAt expr (TCIn l e1 ty), ty)

               _ -> reportError expr
                            "Tagged values should have only a single field"


           AGrammar ->
             do ty   <- newTVar expr KValue

                fsT <- forM fs \(f :> e) ->
                       do (e1,t0) <- inferExpr e
                          t       <- grammarResult expr t0
                          x       <- newName f t
                          let rng  = f <-> e
                              lab  = thingValue f
                              stmt = exprAt rng $ TCLabel lab
                                   $ exprAt rng $ TCDo (Just x) e1
                                   $ exprAt rng $ TCPure
                                   $ exprAt rng $ TCIn lab (exprAt x (TCVar x)) ty
                          pure (f,stmt,t)

                let (xs,stmts,ts) = unzip3 fsT

                -- If the same constructor appears multiple times,
                -- make sure that its fields always have the same type
                let labs        = map thingValue xs
                    withLoc x t = [Located { thingRange = range x, thingValue = t }]
                    tagMap      = Map.fromListWith (++)
                                                   (zip labs (zipWith withLoc xs ts))
                tagTy <- forM (Map.toList tagMap) \(x,lt:more) ->
                           do forM_  more \t1 -> unify (lt, thingValue lt)
                                                       (t1, thingValue t1)
                              pure (x, lt)

                tcon <- newTyDefName
                addConstraint expr (TyDef UnionDef (Just tcon) ty tagTy)

                pure ( exprAt expr (TCChoice c stmts ty)
                     , tGrammar ty
                     )

    EApp f es ->
      do ruleInfo <- lookupRuleTypeOf f
         case ruleInfo of
           TopRule tys sig -> checkTopRuleCall (range expr) f tys sig es
           LocalRule ty -> inferLocalCall (range expr) f es ty



    EVar x@Name { nameContext } ->
      do mb <- Map.lookup x <$> getEnv
         case mb of
           Just t ->
             do let nm = TCName { tcName = x, tcType = t
                                            , tcNameCtx = nameContext }
                checkPromote nm (exprAt expr (TCVar nm)) t
           Nothing -> inferExpr (Expr Located { thingRange = range expr
                                              , thingValue = EApp x [] })

    EAnyByte ->
      do ctxt <- getContext
         case ctxt of
           AValue   -> reportError expr "Invalid semantic value."
           AClass   -> pure (exprAt expr TCSetAny, tByteClass)
           AGrammar -> pure (exprAt expr (TCGetByte YesSem), tGrammar tByte)

    EOptional cmt e ->
      grammarOnly expr
      do (e1,t) <- inferExpr e
         a      <- grammarResult e t
         pure ( exprAt expr (TCOptional cmt e1)
              , tGrammar (tMaybe a)
              )

    EMany com bnds e ->
      grammarOnly expr
      do (e1,t) <- inferExpr e
         a      <- grammarResult e t
         newBnds <- checkManyBounds bnds
         pure ( exprAt expr (TCMany YesSem com newBnds e1)
              , tGrammar (tArray a)
              )

    EQuiet e ->
      grammarOnly expr
      do (e1,_t) <- inferExpr e
         pure ( exprAt expr $ TCDo Nothing e1
              $ exprAt expr $ TCPure
              $ exprAt expr $ TCUnit
              , tGrammar tUnit
              )

    EPure e ->
      grammarOnly expr
      do (e1,t) <- inContext AValue (inferExpr e)
         pure (exprAt expr (TCPure e1), tGrammar t)

    EFail msg ->
      grammarOnly expr $
      liftApp expr [msg] \ ~[(msgE,msgT)] ->
      do unify (tArray tByte) (msgE,msgT)
         a <- newTVar expr KValue
         pure (exprAt expr (TCFail (Just msgE) a), tGrammar a)

    EInRange e1 e2 ->
      do ctxt <- getContext
         case ctxt of
           AValue -> reportError expr "Invalid semantic value."

           AClass ->
             do let checkBound d b =
                      case b of
                        Nothing -> pure (exprAt expr (TCNumber d tByte))
                        Just e ->
                          do (e1',t) <- inContext AValue (inferExpr e)
                             unify tByte (e1',t)
                             pure e1'
                e1' <- checkBound 0   e1
                e2' <- checkBound 255 e2
                pure (exprAt expr (TCSetRange e1' e2'), tByteClass)

            -- XXX: Maybe require explicit match?
           AGrammar -> promoteSetToGrammar =<< inContext AClass (inferExpr expr)

    EFor fl mbIx i is e ->
      case fl of

        FMap ->
          do (is1,it) <- inContext AValue (inferExpr is)
             kT       <- newTVar i KValue
             addConstraint e (ColKeyType it kT)

             elIn     <- newTVar i KValue
             addConstraint e (ColElType it elIn)

             outColT  <- newTVar i KValue
             addConstraint e (ColKeyType outColT kT)

             elOut <- newTVar i KValue
             addConstraint e (ColElType outColT elOut)

             addConstraint e (Mappable it outColT)

             let addKey = case mbIx of
                            Nothing -> id
                            Just kx -> extEnv kx kT

             (e1,et)  <- addKey $ extEnv i elIn $ inferExpr e
             (expect, result) <-
                do ctxt <- getContext
                   case ctxt of
                     AValue   -> pure (elOut, outColT)
                     AClass   -> reportError expr "`for` is not a valid set."
                     AGrammar -> pure (tGrammar elOut, tGrammar outColT)
             unify (expect :: Type) (e,et)


             let toName n t = TCName { tcName = n,
                                       tcNameCtx = AValue,
                                       tcType = t }
                 k1 = (`toName` kT) <$> mbIx
                 i1 = toName i elIn
             pure ( exprAt expr (TCFor Loop
                                        { loopFlav = LoopMap
                                        , loopKName = k1
                                        , loopElName = i1
                                        , loopCol = is1
                                        , loopBody = e1
                                        , loopType = result
                                        })
                  , result
                  )

        FFold x s ->
          do (s1,st)  <- inContext AValue (inferExpr s)
             (is1,it) <- inContext AValue (inferExpr is)
             addConstraint e (Traversable it)

             kT       <- newTVar i KValue
             elT      <- newTVar i KValue
             addConstraint e (ColKeyType it kT)
             addConstraint e (ColElType  it elT)

             let addKey = case mbIx of
                            Nothing -> id
                            Just kx -> extEnv kx kT

             (e1,et)  <- extEnv x st $ addKey $ extEnv i elT $ inferExpr e
             expect   <-
                do ctxt <- getContext
                   case ctxt of
                     AValue -> pure st
                     AClass -> reportError expr "`for` is not a valid set."
                     AGrammar -> pure (tGrammar st)
             unify expect (e,et)

             let toName n t = TCName { tcName = n,
                                       tcNameCtx = AValue,
                                       tcType = t }
                 x1 = toName x st
                 k1 = (`toName` kT) <$> mbIx
                 i1 = toName i elT
             pure ( exprAt expr (TCFor Loop
                                        { loopFlav = Fold x1 s1
                                        , loopKName = k1
                                        , loopElName = i1
                                        , loopCol = is1
                                        , loopBody = e1
                                        , loopType = et
                                        })
                  , expect
                  )

    -- XXX: This one is not lifted the usual way, because we
    -- want it to be lazy
    EIf be te fe ->
      valueOnly expr -- We don't support conditionals a the grammar level (yet?)
      do -- FIXME: probably don't need the inContext AValue bits?
         (be', bt) <- inContext AValue (inferExpr be)
         unify tBool (be',bt)
         (te', tt) <- inContext AValue (inferExpr te)
         (fe', ft) <- inContext AValue (inferExpr fe)
         unify (te',tt) (fe,ft)
         pure (exprAt expr (TCIf be' te' fe'), tt)

    EHasType MatchType e ty ->
      do ctx <- getContext
         let checkK :: TypeM ctx Type
             checkK = case ctx of
                        AGrammar -> tGrammar <$> checkType KValue ty
                        AValue   -> checkType KValue ty
                        AClass   -> checkType KClass ty
         twant <- checkK
         (e1,t) <- inferExpr e
         unify (ty,twant) (e1,t)
         pure (e1,t)

    -- This one has a non-standard lifting: in grammar contexts we do
    -- a dynamic check, instead of `fmap`ing the static check.
    EHasType CoerceCheck e ty ->
      do ctxt <- getContext
         case ctxt of
           AClass ->
              promoteValueToSet =<< inContext AValue (inferExpr expr)

           AGrammar ->
             do t       <- checkType KValue ty
                liftValApp expr [e] \ ~[(e1,t1)] ->
                   pure (exprAt expr (TCCoerceCheck YesSem t1 t e1), tGrammar t)

           AValue ->
             do t <- checkType KValue ty
                (e1,t1) <- inContext AValue (inferExpr e)
                addConstraint ty (Coerce NotLossy t1 t)
                pure (exprAt expr (TCCoerce t1 t e1), t)


    EHasType CoerceForce e ty ->
      liftValAppPure expr [e] \ ~[(e1,t1)] ->
      do t <- checkType KValue ty
         addConstraint ty (Coerce Lossy t1 t)
         pure (exprAt expr (TCCoerce t1 t e1), t)

    ETry e ->
      grammarOnly expr
      do (e1,t) <- inferExpr e
         pure (exprAt expr (TCErrorMode Backtrack e1), t)




-- This doesn't do argument lifting because types are inferred.
-- If we add a way to specify kinds then lifting makes sense.
inferLocalCall ::
  SourceRange ->
  Name -> [Expr] -> Type -> TypeM ctx (TC SourceRange ctx, Type)
inferLocalCall erng f@Name { nameContext } es ty =
  grammarOnly erng
  case nameContext of
    AGrammar ->
      do (args,ts,_stmts) <- unzip3 <$> mapM (checkArg Nothing) es
         -- for now _stmts should always be []
         out <- newTVar erng KGrammar
         let actual = tFunMany ts out
         unify actual (erng,ty)

         let nm = TCName { tcName = f, tcType = out, tcNameCtx = nameContext }
             call = exprAt erng (TCCall nm [] args)
         pure (call, out)

    _ -> reportError erng ("Cannot call a non-grammar parameter" <+>
                           backticks (pp f))



checkTopRuleCall ::
  SourceRange ->
  Name     {- ^ Name of function -} ->
  [Type]   {- ^ Type arguments -}   ->
  RuleType {- ^ Instantiate type of called function -} ->
  [Expr]   {- ^ Epxression arguments -} ->
  TypeM ctx (TC SourceRange ctx, Type)
checkTopRuleCall r f@Name { nameContext = fctx } tys (inTs :-> outT) es =
  do ctx <- getContext
     let ppf = backticks (pp f)
     (args,_,mbStmts) <-
                      unzip3 <$> zipWithM checkArg (map Just inTs) es
     let have  = length args
         need  = length inTs
         stmts = catMaybes mbStmts
     out1 <- case compare have need of
               EQ -> pure outT
               GT -> reportError r $
                       hsep [ int (have - need)
                            , "too many arguments in call to"
                            , ppf
                            ]
               LT -> do ok <- arePartialAppsOK
                        unless (ok && null stmts) $
                          reportError r $
                            hsep [ int (have - need)
                                 , "too few arguments in call to"
                                 , ppf
                                 ]
                        pure (tFunMany (drop have inTs) outT)
     let nm = TCName { tcName = f
                     , tcType = out1
                     , tcNameCtx = fctx
                     }
         call = exprAt r (TCCall nm tys args)
     case ctx of
       AGrammar ->
          do (e1,t1) <- checkPromote nm call out1
             pure (foldr addBind e1 stmts, t1)
       _ -> checkPromote nm call out1


checkArg ::
  Maybe Type -> Expr -> TypeM ctx (Arg SourceRange, Type, Maybe BindStmt)
checkArg mbT e =
  do ctx <- getContext
     case (ctx,argCtx) of
       (AGrammar, Some AValue) -> -- calling grammar, expecting a value
         do ((e1,t),mbS) <- liftValArg e    -- optional lifting
            checkTy (e1,t)
            pure (e1,t,mbS)

       (_,Some actx) ->
         do (e1,t) <- allowPartialApps partOk (inContext actx (inferExpr e))
            checkTy (e1,t)
            case actx of
              AGrammar -> pure (GrammarArg e1, t, Nothing)
              AValue   -> pure (ValArg e1, t, Nothing)
              AClass   -> pure (ClassArg e1, t, Nothing)
  where
  checkTy :: HasRange r => (r,Type) -> TypeM ctx ()
  checkTy ty =
    case mbT of
      Nothing   -> pure ()
      Just sigT -> unify sigT ty

  partOk = case argCtx of
             Some AGrammar -> True
             _             -> False

  argCtx = case mbT of
             Nothing -> inferContext e
             Just sigT ->
               case kindOf sigT of
                 KGrammar -> Some AGrammar
                 KValue   -> Some AValue
                 KClass   -> Some AClass
                 KNumber  -> panic "checkArg" [ "KNumber in argument?" ]



promoteValueToSet :: (TC SourceRange Value,Type) ->
                     TypeM ctx (TC SourceRange Class,Type)
promoteValueToSet (e,t) =
  do unify tByte (e,t)
     pure (exprAt e (TCSetSingle e), tByteClass)

promoteSetToGrammar :: (TC SourceRange Class,Type) ->
                       TypeM ctx (TC SourceRange Grammar,Type)
promoteSetToGrammar (e,t) =
  do unify tByteClass (e,t)
     pure (exprAt e (TCMatch YesSem e), tGrammar tByte)


checkPromote ::
  TCName k -> TC SourceRange k -> Type -> TypeM ctx (TC SourceRange ctx, Type)
checkPromote x e t =
  do ctxt <- getContext
     let ppx = backticks (pp x)
     case (tcNameCtx x, ctxt) of
       (AValue, AValue)   -> pure (e, t)
       (AValue, AClass)   -> promoteValueToSet (e, t)
       (AValue, AGrammar) ->
          liftValAppPure e [] \_ -> pure (e,t)

       (AClass,AValue) ->
          reportError x
            ("Expected a value, but" <+> ppx <+> "is a set of bytes.")

       (AClass, AClass) -> pure (e,t)
       (AClass, AGrammar) -> promoteSetToGrammar (e,t)

       (AGrammar,AValue) ->
          reportError x ("Expected a value, but" <+> ppx <+> "is a grammar.")

       (AGrammar,AClass) ->
          reportError x
              ("Expected a set of bytes, but" <+> ppx <+> "is a grammar.")

       (AGrammar, AGrammar) -> pure (e,t)




-- XXX: Remove when we generalize `if`
valueOnly :: HasRange r => r -> TypeM Value (TC SourceRange Value, Type) ->
                                TypeM ctx (TC SourceRange ctx, Type)
valueOnly r k =
  do ctxt <- getContext
     case ctxt of
       AValue -> k
       AClass ->
          reportError r "Expected a value, but encountered a set of bytes."
       AGrammar ->
         reportError r "Expected a value, but encountered a grammar."


grammarOnly ::
  HasRange r =>
  r ->
  TypeM Grammar (TC SourceRange Grammar, Type) ->
  TypeM ctx (TC SourceRange ctx, Type)
grammarOnly r k =
  do ctxt <- getContext
     case ctxt of
       AValue ->
         reportError r "Expected a grammar, but encountered a value."
       AClass ->
         reportError r "Expected a grammar, but encountered a set of bytes."
       AGrammar -> k


grammarResult :: HasRange r => r -> Type -> TypeM ctx Type
grammarResult r t =
  do a <- newTVar r KValue
     unify (tGrammar a) (r,t)
     pure a


checkNameContext :: Name -> TypeM ctx ()
checkNameContext n@Name { nameContext } =
  do ctxt <- getContext
     case (ctxt,nameContext) of
       (AGrammar,AGrammar) -> pure ()
       (AValue,AValue) -> pure ()
       (AClass,AClass) -> pure ()
       _ -> reportDetailedError n ("Invalid use of" <+> backticks (pp n))
               [ "expected" <+> pp ctxt
               , backticks (pp n) <+> "is" <+> pp nameContext
               ]


checkManyBounds ::
  ManyBounds Expr -> TypeM ctx (ManyBounds (TC SourceRange Value))
checkManyBounds bnds =
  case bnds of
    Exactly e     -> Exactly <$> checkNum e
    Between e1 e2 -> Between <$> traverse checkNum e1 <*> traverse checkNum e2
  where
  -- XXX: allow different numeric types
  checkNum e =
    do (e1,t) <- inContext AValue (inferExpr e)
       unify tInteger (e,t)
       -- isNumeric e t
       pure e1




inferStructGrammar ::
  Expr -> [StructField Expr] -> TypeM Grammar (TC SourceRange Grammar, Type)
inferStructGrammar r = go [] []
  where
  go mbRes done fs =
    let checkBind x e kont =
          do (e1,t) <- inferExpr e
             a      <- grammarResult e t
             let x' = TCName { tcName = x, tcType = a, tcNameCtx = AValue }
             (ke,kt) <- extEnv x a (kont x')
             pure (exprAt e (TCDo (Just x') e1 ke), kt)
    in
    case fs of
      [Anon e] | null mbRes && null done ->
                  do let x = Name { nameScope = Local "$$"
                                  , nameRange = range e
                                  , nameContext = AValue
                                  }
                     go mbRes done [x := e]

      [COMMIT rn] -> reportError rn "COMMIT at the end of a struct"

      f : more ->
        case f of
          COMMIT rn -> do (res,kt) <- go mbRes done more
                          pure (exprAt rn (TCErrorMode Commit res), kt)
          x := e  -> checkBind x e \x' ->
                       if nameScopeAsLocal (tcName x') == "$$"
                         then go (x' : mbRes) done        more
                         else go mbRes        (x' : done) more
          x :@= e -> checkBind x e \_  -> go mbRes done  more
          Anon e ->
            do (e1,_t) <- inferExpr e
               (ke,kt) <- go mbRes done more
               pure (exprAt e (TCDo Nothing e1 ke), kt)

      [] ->
        case (mbRes, done) of
          ([], _) ->
            do let xs'   = reverse done
                   ls    = map (nameScopeAsLocal . tcName) xs'
               (e,ty) <- pureStruct r ls (map tcType xs')
                                         [ exprAt x (TCVar x) | x <- xs' ]
               pure ( exprAt r (TCPure e)
                    , tGrammar ty
                    )
          ([x],[]) -> pure ( exprAt x $ TCPure $
                             exprAt x $ TCVar x
                           , tGrammar (tcType x)
                           )
          (x : y : more, []) ->
             reportDetailedError y "Cannot have multiple `_` fields. See:"
                            [ pp (range z) | z <- x : y : more
                            ]

          (x : _, _) -> reportError x "Cannot mix `_` and named fields."


pureStruct ::
  HasRange r =>
  r ->
  [Label] -> [Type] -> [TC SourceRange Value] ->
  TypeM ctx (TC SourceRange Value, Type)
pureStruct r ls ts es
  | l : _ <- repeated =
      reportError r ("Multiple entries for field" <+> backticks (pp l))
  | otherwise =
    case ls of
      [] -> pure (exprAt r TCUnit, tUnit)
      _  -> do ty <- newTVar r KValue
               nm <- newTyDefName
               addConstraint r $
                  TyDef StructDef (Just nm) ty
                    [ (l, Located { thingRange = range e, thingValue = t })
                    | l <- ls
                    | e <- es
                    | t <- ts
                    ]
               pure (exprAt r (TCStruct (zip ls es) ty), ty)
  where
  repeated = [ l | (l : _ : _) <- group (sort ls) ]







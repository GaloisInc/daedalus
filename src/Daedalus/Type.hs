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
import Data.Maybe(mapMaybe)
import Control.Monad(zipWithM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.Rec(sccToRec)

import Daedalus.AST
import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Type.Constraints
import Daedalus.Type.Kind
import Daedalus.Type.Traverse
import Daedalus.Type.Subst


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
    NonRec r -> (NonRec . fst) <$> inferRule r
    MutRec rs ->
      do rts <- mapM guessRuleType rs
         res <- extEnvManyRules rts (zipWithM checkRule rs (map snd rts))
         pure (MutRec res)

      where
      guessType :: Name -> STypeM Type
      guessType nm@Name { nameContext } =
        case nameContext of
          AGrammar -> tGrammar <$> newTVar nm KValue
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
      AGrammar -> GrammarParam . nm . tGrammar <$>
                  case mb of
                    Nothing   -> newTVar n KValue
                    Just srct -> checkType KValue srct

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
                             ty1 <- typeOf <$>
                                            checkSig (ruleName r) (ruleResTy r)
                             unify ty1 (e1,t)
                             pure (Defined e1)
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

inferExpr :: Expr -> TypeM ctx (TC SourceRange ctx,Type)
inferExpr expr =
  case exprValue expr of

    EBool b -> valueOnly expr $ pure (exprAt expr (TCBool b), tBool)

    ENothing -> valueOnly expr
                do a <- newTVar expr KValue
                   pure (exprAt expr (TCNothing a), tMaybe a)

    EJust e -> valueOnly expr
               do (e',t) <- inferExpr e
                  pure (exprAt expr (TCJust e'), tMaybe t)

    ENumber n ->
      do ctxt <- getContext
         case ctxt of
           AValue -> do a <- newTVar expr KValue
                        addConstraint expr (Literal n a)
                        pure (exprAt expr (TCNumber n a), a)
           AClass
             | 0 <= n && n < 256 ->
                pure ( exprAt expr $ TCSetSingle
                     $ exprAt expr $ TCByte $ fromInteger n
                    , tByteClass
                    )
             | otherwise ->
                reportError expr "Byte literal out of range."

           AGrammar -> promoteSetToGrammar =<< inContext AClass (inferExpr expr)

    EUniOp op e ->
      case op of

        Not ->
          do ctxt <- getContext
             case ctxt of
               AValue -> do (e1,t) <- inferExpr e
                            unify tBool (e,t)
                            pure (exprAt expr (TCUniOp Not e1), tBool)
               AClass -> do (e1,_) <- inferExpr e
                            pure (exprAt expr (TCSetComplement e1), tByteClass)
               AGrammar -> do res <- inContext AClass (inferExpr expr)
                              promoteSetToGrammar res

        Neg ->
          valueOnly expr
          do (e1,t) <- inferExpr e
             addConstraint expr (Numeric t)
             pure (exprAt expr (TCUniOp Neg e1), t)

        Concat ->
          do ctxt <- getContext
             case ctxt of
               AValue -> do (e1,t) <- inferExpr e
                            a      <- newTVar e KValue
                            unify (tArray (tArray a)) (e,t)
                            pure ( exprAt expr (TCUniOp Concat e1)
                                 , tArray a
                                 )
               AClass -> promoteValueToSet =<< inContext AValue (inferExpr expr)
               AGrammar ->
                 do (e1,t) <- inferExpr e
                    a      <- newTVar e KValue
                    unify (tGrammar (tArray (tArray a))) (e,t)
                    n <- newName e (tArray (tArray a))
                    pure ( exprAt expr $ TCDo (Just n) e1 $
                           exprAt expr $ TCPure $
                           exprAt expr $ TCUniOp Concat $
                           exprAt expr $ TCVar n
                        , tGrammar (tArray a)
                        )
        BitwiseComplement ->
          valueOnly expr
          do (e1',t) <- inferExpr e
             a       <- newTVar e1' KNumber
             unify (tUInt a) (e1', t)
             pure (exprAt expr (TCUniOp BitwiseComplement e1'), t)

    ETriOp op e1 e2 e3 ->
      case op of
        RangeUp -> rangeOp
        RangeDown -> rangeOp

      where
      rangeOp =
        valueOnly expr
        do r1@(e1',t1) <- inferExpr e1
           r2@(e2',_) <- inferExpr e2
           r3@(e3',_) <- inferExpr e3
           unify r1 r2
           unify r1 r3
           addConstraint expr (Numeric t1)
           pure (exprAt expr (TCTriOp op e1' e2' e3' (tArray t1)), tArray t1)

    EBinOp op e1 e2 ->
      case op of
        ArrayStream ->
          valueOnly expr
          do (e1',t1) <- inferExpr e1
             (e2',t2) <- inferExpr e2
             unify (tArray tByte) (e1',t1)
             unify (tArray tByte) (e2',t2)
             pure (exprAt expr (TCBinOp op e1' e2' tStream), tStream)



        LCat ->
          valueOnly expr
          do (e1',t1) <- inferExpr e1
             addConstraint expr (Numeric t1)

             (e2',t2) <- inferExpr e2
             r <- newTVar e2 KNumber
             unify (tUInt r) (e2',t2)

             pure (exprAt expr (TCBinOp LCat e1' e2' t1), t1)


        Cat ->
          valueOnly expr
          do (e1',t1) <- inferExpr e1
             l1       <- newTVar e1 KNumber
             unify (tUInt l1) (e1,t1)

             (e2',t2) <- inferExpr e2
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
        Sub   -> num2   --- XXX: character classes?
        Mul   -> num2
        Div   -> num2
        Mod   -> num2
        Lt    -> rel
        Leq   -> rel
        Eq    -> relEq
        NotEq -> relEq

      where
      bitwiseOp =
        valueOnly expr
        do (e1',t1) <- inferExpr e1
           addConstraint expr (Numeric t1)
           (e2',t2) <- inferExpr e2
           unify (e1', t1) (e2', t2)
           pure (exprAt expr (TCBinOp op e1' e2' t1), t1)

      shiftOp =
        valueOnly expr
        do (e1',t1) <- inferExpr e1
           addConstraint expr (Numeric t1)
           (e2',t2) <- inferExpr e2
           unify tInteger (e2',t2) -- XXX: either overload or maybe use another type
                               -- Same as Many
           pure (exprAt expr (TCBinOp op e1' e2' t1), t1)

      num2 = valueOnly expr
             do (e1',t1) <- inferExpr e1
                addConstraint e1 (Numeric t1)
                (e2',t2) <- inferExpr e2
                addConstraint e2 (Numeric t2)
                unify (e1', t1) (e2',t2)
                pure (exprAt expr (TCBinOp op e1' e2' t1), t1)

      -- XXX: what types should allow to be compared:
      -- only numeric? or do structural comparisons?
      rel = do ctx <- getContext
               case ctx of
                 AValue ->
                   do (e1',t1) <- inferExpr e1
                      (e2',t2) <- inferExpr e2
                      unify (e1',t1) (e2',t2)
                      pure (exprAt expr (TCBinOp op e1' e2' tBool), tBool)

                 AClass -> reportError expr $
                              "Relation" <+> backticks (pp op) <+>
                              "cannot be used as a byte-class."
                 AGrammar -> inferExpr $ Expr
                             Located { thingRange = range expr
                                     , thingValue = ESel expr SelTrue
                                     }

      -- In case we want to specialise either < or =
      relEq = rel


    ESel e l ->
      do a      <- newTVar expr KValue
         ctxt   <- getContext
         case ctxt of
           AValue   ->
             let badSel = reportError e
                            "Expected a value, but this is a grammar"

             in
             case l of
               SelStruct f ->
                 do (e1,t) <- inferExpr e
                    let lab = thingValue f
                    addConstraint f (HasStruct t lab a)
                    pure (exprAt expr (TCSelStruct e1 lab a), a)
               SelUnion _ -> badSel
               SelTrue    -> badSel
               SelFalse   -> badSel
               SelNothing -> badSel
               SelJust    -> badSel

           AClass -> promoteValueToSet =<< inContext AValue (inferExpr expr)
           AGrammar ->
             do res <- case l of
                         SelStruct f ->
                           do (e1,tg) <- inferExpr e
                              t       <- grammarResult expr tg
                              let lab = thingValue f
                              addConstraint f (HasStruct t lab a)
                              x      <- newName e1 t
                              let xe = exprAt expr (TCVar x)
                              pure $ exprAt expr $ TCDo (Just x) e1
                                   $ exprAt expr $ TCPure
                                   $ exprAt expr $ TCSelStruct xe lab a

                         SelUnion f ->
                           do (e1,t) <- inContext AValue (inferExpr e)
                              let lab = thingValue f
                              addConstraint f (HasUnion t lab a)
                              pure $ exprAt expr (TCSelUnion YesSem e1 lab a)

                         SelTrue ->
                           do (e1,t) <- inContext AValue (inferExpr e)
                              unify tBool (e1,t)
                              unify tUnit (e,a)
                              pure $ exprAt expr $ TCGuard e1

                         SelFalse ->
                           do (e1,t) <- inContext AValue (inferExpr e)
                              unify tBool (e,t)
                              unify tUnit (e,a)
                              pure $ exprAt expr $ TCGuard $
                                     exprAt expr $ TCUniOp Not e1

                         SelNothing ->
                           do (e1,t) <- inContext AValue (inferExpr e)
                              unify (tMaybe a) (e,t)
                              pure $ exprAt expr $ TCGuard
                                   $ exprAt expr $ TCBinOp Eq e1
                                                    (exprAt expr (TCNothing a))
                                                    tBool
                         SelJust ->
                           do (e1,t) <- inContext AValue (inferExpr e)
                              unify (tMaybe a) (e1,t)
                              pure $ exprAt expr $ TCSelJust YesSem e1 a

                pure (res, tGrammar a)

    EByte w ->
      do ctxt <- getContext
         case ctxt of
           AValue   -> pure (exprAt expr (TCByte w), tByte)

           AClass   -> promoteValueToSet =<< inContext AValue (inferExpr expr)

           AGrammar -> promoteSetToGrammar =<< inContext AClass (inferExpr expr)

    EBytes bs ->
      do ctxt <- getContext
         case ctxt of
           AValue   -> pure (exprAt expr (TCByteArray bs), tArray tByte)
           AClass   -> pure (exprAt expr (TCSetOneOf bs), tByteClass)
           AGrammar ->
             pure ( exprAt expr $ TCMatchBytes YesSem $
                    exprAt expr $ TCByteArray bs
                  , tGrammar (tArray tByte)
                  )

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
      grammarOnly expr
      do (e,t) <- inContext AValue (inferExpr s)
         unify tStream (e,t)
         pure (exprAt expr (TCSetStream e), tGrammar tUnit)

    EStreamLen i s ->
      grammarOnly expr $
      inContext AValue
      do (ie,it) <- inferExpr i
         unify tInteger (ie,it)
         (se,st) <- inferExpr s
         unify tStream (se,st)
         pure (exprAt expr (TCStreamLen YesSem ie se), tGrammar tStream)

    EStreamOff i s ->
      grammarOnly expr $
      inContext AValue
      do (ie,it) <- inferExpr i
         unify tInteger (ie,it)
         (se,st) <- inferExpr s
         unify tStream (se,st)
         pure (exprAt expr (TCStreamOff YesSem ie se), tGrammar tStream)

    EMapEmpty ->
      valueOnly expr
      (do kt <- newTVar expr KValue
          vt <- newTVar expr KValue
          let t = tMap kt vt
          (pure (exprAt expr (TCMapEmpty t), t)))

    EMapInsert k v m ->
      grammarOnly expr
      (do (k1, kt)  <- inContext AValue (inferExpr k)
          (v1, vt)  <- inContext AValue (inferExpr v)
          (m1, mt)  <- inContext AValue (inferExpr m)
          unify (tMap kt vt) (m, mt)
          (pure (exprAt expr (TCMapInsert YesSem k1 v1 m1), tGrammar mt)))

    EMapLookup k m ->
      grammarOnly expr
      (do (k1, kt)  <- inContext AValue (inferExpr k)
          (m1, mt)  <- inContext AValue (inferExpr m)
          vt        <- newTVar m KValue
          unify (tMap kt vt) (m, mt)
          (pure (exprAt expr (TCMapLookup YesSem k1 m1), tGrammar vt)))

    EArray es ->
      do ctxt <- getContext
         case ctxt of
           AValue ->
             do res <- mapM inferExpr es
                t   <- case res of
                         (e,t) : more ->
                            do forM_ more \(e1,t1) -> unify (e,t) (e1,t1)
                               pure t
                         [] -> newTVar expr KValue
                pure (exprAt expr $ TCArray (map fst res) t, tArray t)

           AClass ->
             do es1 <- mapM inferExpr es
                pure (exprAt expr (TCSetUnion (map fst es1)), tByteClass)

           AGrammar ->
             do t   <- newTVar expr KValue
                es' <- forM es \e ->
                         do (e',t') <- inferExpr e
                            unify (expr,tGrammar t) (e',t')
                            x <- newName e' t
                            pure (x,e')
                let bind (x,e) k = exprAt e $ TCDo (Just x) e k
                    val (x,e) = exprAt e (TCVar x)
                    done = exprAt expr $ TCPure
                         $ exprAt expr $ TCArray (map val es') t
                pure (foldr bind done es', tGrammar (tArray t))


    EArrayLength e ->
      valueOnly expr
      (do (e1, et)  <- inContext AValue (inferExpr e)
          vt        <- newTVar e KValue -- We don't use this, can we just assert?
          unify (tArray vt) (e, et)
          (pure (exprAt expr (TCArrayLength e1), tInteger)))

    EArrayIndex e ix -> 
      grammarOnly expr
      (do (e1, et)   <- inContext AValue (inferExpr e)
          vt         <- newTVar e KValue
          unify (tArray vt) (e, et)      
          (ix1, ixt) <- inContext AValue (inferExpr ix)
          unify tInteger (ix, ixt)
          (pure (exprAt expr (TCArrayIndex YesSem e1 ix1), tGrammar vt)))

    EStruct fs ->
      do ctxt <- getContext
         case ctxt of
           AValue ->
             do fs1 <- forM fs \sf ->
                         case sf of
                           COMMIT r -> reportError r
                                    "COMMIT may not appear in a semantic value."
                           Anon e -> reportError e "Struct value needs a label."
                           x := e ->
                             do (e1,t1) <- inContext AValue (inferExpr e)
                                pure (x,e1,t1)
                           x :@= _ -> reportError x "Unexpected local variable."

                let (xs,es,ts) = unzip3 fs1
                    ls = map nameScopeAsLocal xs
                pureStruct expr ls ts es

           AClass   -> reportError expr "Unexpected struct in a byte set."

           AGrammar -> inferStructGrammar expr fs


    EChoiceU cmt e1 e2 ->
      do ctxt <- getContext
         case ctxt of
           AValue -> do (e1',t1) <- inferExpr e1
                        (e2',t2) <- inferExpr e2
                        unify (e1',t1) (e2',t2)
                        addConstraint expr (Numeric t1)
                        pure ( exprAt expr (TCBinOp BitwiseOr e1' e2' t1)
                             , t1
                             )

           AClass -> do (e1',_) <- inferExpr e1
                        (e2',_) <- inferExpr e2
                        pure ( exprAt expr (TCSetUnion [e1',e2'])
                             , tByteClass
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

           AClass ->
             reportError expr "Invalid character class"
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

    EApp f@Name { nameContext } es ->
      do (tys, ins :-> out) <- lookupRuleTypeOf f
         es1 <- checkArgs ins es
         let nm = TCName { tcName = f, tcType = out, tcNameCtx = nameContext }
         checkPromoteToGrammar nm (exprAt expr (TCCall nm tys es1)) out
      where
      checkArgs ts as =
        case (ts,as) of
          ([],[]) -> pure []
          (t : moreT, a : moreA) ->
             case kindOf t of
               KGrammar ->
                 do (e1,t1) <- inContext AGrammar (inferExpr a)
                    unify t (e1,t1)
                    moreEs <- checkArgs moreT moreA
                    pure (GrammarArg e1 : moreEs)

               KValue ->
                 do (e1,t1) <- inContext AValue (inferExpr a)
                    unify t (e1,t1)
                    moreEs <- checkArgs moreT moreA
                    pure (ValArg e1 : moreEs)



               k -> error ("bug: unexpected parameter kind, " ++ show k)
          ([], a : _) ->
            reportError a ("Too many arguments in call to" <+> backticks (pp f))
          (_, []) -> reportDetailedError f
                     ("Not enough arguments in call to" <+> backticks (pp f))
                     [ "Need" <+> int (length ts) <+> "more." ]


    EVar x@Name { nameContext } ->
      do mb <- Map.lookup x <$> getEnv
         case mb of
           Just t ->
             do let nm = TCName { tcName = x, tcType = t, tcNameCtx = nameContext }
                checkPromoteToGrammar nm (exprAt expr (TCVar nm)) t
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
      inContext AValue
      do (msgE,msgT) <- inferExpr msg
         unify (tArray tByte) (msgE,msgT)
         a <- newTVar expr KValue
         pure (exprAt expr (TCFail (Just msgE) a), tGrammar a)

    EInRange e1 e2 ->
      do ctxt <- getContext
         case ctxt of
           AValue -> reportError expr "Invalid semantic value."
                     -- XXX: this could mean an array of bytes

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

    EHasType CoerceCheck e ty ->
      do ctxt <- getContext
         case ctxt of
           AGrammar ->
             do t       <- checkType KValue ty
                (e1,t1) <- inContext AValue (inferExpr e)
                pure (exprAt expr (TCCoerceCheck YesSem t1 t e1), tGrammar t)
           AValue ->
             do t <- checkType KValue ty
                (e1,t1) <- inContext AValue (inferExpr e)
                addConstraint ty (Coerce NotLossy t1 t)
                pure (exprAt expr (TCCoerce t1 t e1), t)
           AClass ->
              promoteValueToSet =<< inContext AValue (inferExpr expr)

    EHasType CoerceForce e ty ->
      do ctxt <- getContext
         case ctxt of
           AGrammar ->
             promoteValueToGrammar =<< inContext AValue (inferExpr expr)
             -- uhm, this would only work if we coerced into an array of bytes?
             -- seems unlikely to be useful.
           AClass ->
             promoteValueToSet =<< inContext AValue (inferExpr expr)
           AValue ->
             do t <- checkType KValue ty
                (e1,t1) <- inferExpr e
                addConstraint ty (Coerce Lossy t1 t)
                pure (exprAt expr (TCCoerce t1 t e1), t)

    ETry e ->
      grammarOnly expr
      do (e1,t) <- inferExpr e
         pure (exprAt expr (TCErrorMode Backtrack e1), t)



promoteValueToSet :: (TC SourceRange Value,Type) ->
                     TypeM ctx (TC SourceRange Class,Type)
promoteValueToSet (e,t) =
  do unify tByte (e,t)
     pure (exprAt e (TCSetSingle e), tByteClass)

promoteValueToGrammar :: (TC SourceRange Value,Type) ->
                         TypeM ctx (TC SourceRange Grammar,Type)
promoteValueToGrammar (e,t) =
  do unify (tArray tByte) (e,t)
     pure (exprAt e (TCMatchBytes YesSem e), tGrammar (tArray tByte))

promoteSetToGrammar :: (TC SourceRange Class,Type) ->
                       TypeM ctx (TC SourceRange Grammar,Type)
promoteSetToGrammar (e,t) =
  do unify tByteClass (e,t)
     pure (exprAt e (TCMatch YesSem e), tGrammar tByte)



checkPromoteToGrammar ::
  TCName k -> TC SourceRange k -> Type -> TypeM ctx (TC SourceRange ctx, Type)
checkPromoteToGrammar x e t =
  do ctxt <- getContext
     case (tcNameCtx x, ctxt) of
       (AValue, AValue)   -> pure (e, t)
       (AValue, AClass)   -> promoteValueToSet (e, t)
       (AValue, AGrammar) -> promoteValueToGrammar (e, t)

       (AClass,AValue) ->
          reportError x ("Set" <+> backticks (pp x) <+>
                         "may not be used as a semantic value.")
       (AClass, AClass) -> pure (e,t)
       (AClass, AGrammar) -> promoteSetToGrammar (e,t)

       (AGrammar,AValue) ->
          reportError x ("Grammar" <+> backticks (pp x) <+>
                          "may not be used as a semantic action.")
       (AGrammar,AClass) ->
          reportError x ("Set" <+> backticks (pp x) <+>
                          "may not be used as a grammar.")
       (AGrammar, AGrammar) -> pure (e,t)




valueOnly :: HasRange r => r -> TypeM Value (TC SourceRange Value, Type) ->
                                TypeM ctx (TC SourceRange ctx, Type)
valueOnly r k =
  do ctxt <- getContext
     case ctxt of
       AValue -> k
       AClass ->
          reportError r "Semantic value is not a valid byte set."
       AGrammar ->
         reportError r "Semantic value may not be used a grammar."


grammarOnly ::
  HasRange r =>
  r ->
  TypeM Grammar (TC SourceRange Grammar, Type) ->
  TypeM ctx (TC SourceRange ctx, Type)
grammarOnly r k =
  do ctxt <- getContext
     case ctxt of
       AValue ->
         reportError r "Grammar expression cannot be used as a semantic value."
       AClass ->
         reportError r "Grammar expression cannot be used as a byte set."
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







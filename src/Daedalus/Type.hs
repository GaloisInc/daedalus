{-# Language BlockArguments, OverloadedStrings, NamedFieldPuns #-}
{-# Language DataKinds, GADTs #-}
{-# Language RecordWildCards #-}
{-# Language RankNTypes #-}
{-# Language ParallelListComp #-}
module Daedalus.Type where

import qualified Data.Text as Text
import Control.Monad(forM,forM_,unless)
import Data.Graph.SCC(stronglyConnComp)
import Data.List(sort,group)
import Data.Maybe(catMaybes,maybeToList)
import Control.Monad(zipWithM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Parameterized.Some(Some(..))
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)) )

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
import Daedalus.Type.IP
import Daedalus.Type.Generalize
import Daedalus.Type.BitData
import qualified Daedalus.Type.CheckUnused as CheckUnused


inferRules :: Module -> MTypeM (TCModule SourceRange)
inferRules m =
  do tcm <- goBD [] (moduleBitData m)
     let unusedWarnings = CheckUnused.checkTCModule tcm
     forM_ unusedWarnings \r -> addWarning (WarnNoOpStatement r)
     pure tcm
  where
  getDeps d = (d, tctyName d, Set.toList (collectTypes freeTCons (tctyDef d)))

  goBD done [] = go done [] [] (moduleRules m)
  goBD done (x : more) = do
    newDecls <- inferBitData x
    extGlobTyDefs newDecls $ goBD (newDecls : done) more

  go bds tys done todo =
    case todo of
      [] -> pure TCModule
                   { tcModuleName = moduleName m
                   , tcModuleImports = moduleImports m
                   , tcEntries = []
                   , tcModuleTypes = map sccToRec
                                   $ stronglyConnComp
                                   $ map getDeps
                                   $ concatMap Map.elems
                                   $ bds ++ tys ++ map tcTypeDefs done
                   , tcModuleDecls = map tcDecls (reverse done)
                   }

      x : more ->
        do mb <- inferTRuleRec x
           case mb of
             Right tds -> extGlobTyDefs tds (go bds (tds : tys) done more)
             Left info -> extEnvManyRules env
                        $ extGlobTyDefs (tcTypeDefs info)
                        $ go bds tys (info : done) more
               where env = [ (tcDeclName d, declTypeOf d)
                           | d <- recToList (tcDecls info)
                           ]

--------------------------------------------------------------------------------

inferTRuleRec :: Rec TRule -> MTypeM (Either DeclInfo (Map TCTyName TCTyDecl))
inferTRuleRec ins =
  runSTypeM
  case (rDs,tDs) of
    (_,[])  -> Left <$> (generalize =<< inferRuleRec (mk rDs))
    ([],_)  -> Right <$> checkTypeDecls (mk tDs)
    (_,t:_) -> reportDetailedError (tyName t)
                  "A type declaration and a rule may not depend on each other"
                  (map (pp . ruleName) rDs ++ map (pp . tyName) tDs)
  where
  rDs = [ r | DRule r <- recToList ins ]
  tDs = [ t | DType t <- recToList ins ]

  mk :: [a] -> Rec a
  mk a = case ins of
           NonRec _ -> NonRec (head a)
           MutRec _ -> MutRec a

checkTypeDecls :: Rec TypeDecl -> STypeM (Map TCTyName TCTyDecl)
checkTypeDecls r =
  case r of
    NonRec td ->
      case tyParams td of
        [] ->
          runTypeM (tyName td)
          do fs <- mapM checkField (tyData td)
             let def = case tyFlavor td of
                         Struct -> TCTyStruct Nothing fs
                         Union  -> TCTyUnion [ (l, (t,Nothing)) | (l,t) <- fs ]
                 nm = TCTy (tyName td)
             pure (Map.singleton nm
                    TCTyDecl
                      { tctyName   = nm
                      , tctyParams = []
                      , tctyBD     = Nothing
                      , tctyDef    = def
                      })
        _ -> reportError (tyName td) "XXX: Type parameters in data declarations"
    MutRec ~(d:_) -> reportError (tyName d) "XXX: Recursive type declaration"
  where
  checkField (l,t) =
    do t' <- checkType KValue t
       pure (thingValue l, t')




inferRuleRec :: Rec Rule -> STypeM (Rec (TCDecl SourceRange))
inferRuleRec sr =
  case sr of
    NonRec r0 ->
      do ~[(r,ips)] <- doIPs [r0]
         d <- fst <$> inferRule r
         d1 <- traverseTypes zonkT d
         mapM_ defaultByKind (Set.toList (freeTVS d1))
         pure (NonRec d1 { tcDeclImplicit = ips } )
    MutRec rs0 ->
      do (rs,ips) <- unzip <$> doIPs rs0
         rts <- mapM guessRuleType rs
         res <- extEnvManyRules rts (zipWithM checkRule rs (map snd rts))
         res1 <- traverseTypes zonkT res
         let decls = MutRec [ r { tcDeclImplicit = is }
                            | (r,is) <- res1 `zip` ips ]

         -- Default type variables based on kind
         mapM_ defaultByKind (Set.toList (freeTVS decls))
         pure decls

  where
  doIPs rs =
    do env <- getRuleEnv
       let onlyIp (Poly _ _ ((is,_) :-> _)) = map fst is
       rs1 <- resolveRuleIPs (onlyIp <$> env) rs
       pure rs1


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
       let rt = ([],ts) :-> t
       pure (ruleName r, Poly [] [] rt)

  checkRule r (Poly _ _ ((_,gAs) :-> gR)) =
    do (res, (_,actualAs) :-> actualR) <- inferRule r
       forM_ (zip3 (ruleParams r) gAs actualAs) \(l,g,a) ->
          unify g (l,a)

       unify gR (ruleName r, actualR)
       pure res


-- | Infer a mono type for a single rule.
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
                                , tcDeclImplicit = []
                                , tcDeclParams   = tps
                                , tcDeclDef      = def
                                , tcDeclCtxt     = nameContext
                                , tcDeclAnnot    = ruleRange r
                                }
               pure (d, ([], map typeOf tps) :-> typeOf def)

      p : more ->
        do pa <- checkSig (paramName p) (paramType p)
           extEnv (paramName p) (typeOf pa) (addParams (pa : done) more)


--------------------------------------------------------------------------------
-- Lifting



data BindStmt = BindStmt (TCName Value) (TC SourceRange Grammar)

addBind :: BindStmt -> TC SourceRange Grammar -> TC SourceRange Grammar
addBind (BindStmt n e1) e2 = exprAt (n <-> e2) (TCDo (Just n) e1 e2)

addBinds :: [BindStmt] -> TC SourceRange Grammar -> TC SourceRange Grammar
addBinds xs e = foldr addBind e xs


-- | This is used when we have a function/construct that expects a value.
-- The parameter is the actual argument provided.
liftValExpr ::
  Expr ->
  TypeM ctx ((TC SourceRange Value,Type), Maybe BindStmt)
liftValExpr e =

  case inferContext e of

    Some AGrammar ->
      do (e1,t) <- allowPartialApps False $ inContext AGrammar (inferExpr e)
         a      <- newTVar e KValue
         unify (tGrammar a) (e1,t)
         n      <- newName e a
         pure ( (exprAt e1 (TCVar n), a), Just (BindStmt n e1) )

    Some AValue ->
      do (e1,t) <- inContext AValue (inferExpr e)
         pure ((e1, t), Nothing)

    Some AClass ->
      liftValExpr (pExprAt e (EMatch1 e))





-- | This allowes pure functions to be applied to parsers, automatically
-- lifting the results using bind.  Note that this assumes that the callback
-- will ensure that the result is of type `Grammar t`.
-- For example @F P a Q@ becomes @do x <- P; y <- q; F x a y@
liftApp ::
  [Expr] ->
  ( [(TC SourceRange Value, Type)] ->
    TypeM ctx (TC SourceRange Grammar, Type)
  ) ->
  TypeM ctx (TC SourceRange Grammar, Type)
liftApp es f =
  do (args,mbs) <- unzip <$> mapM liftValExpr es
     (rE,rT) <- f args
     let expr = addBinds (catMaybes mbs) rE
     pure (expr, rT)


-- | Lift a pure function as a functor
-- (if in a grammar context, otherwise just call it)
-- This converts `f P q` into `do x <- P; ^ f x q`.
liftValAppPure ::
  HasRange r =>
  r ->
  [Expr] ->
  ( [(TC SourceRange Value, Type)] ->
    TypeM ctx (TC SourceRange Value, Type)
  ) ->
  TypeM ctx (TC SourceRange ctx, Type)
liftValAppPure r es f =
  do ctx <- getContext
     case ctx of
       AGrammar -> liftApp es \vs ->
                      do (res,t) <- f vs
                         pure (exprAt r (TCPure res), tGrammar t)
       AClass {} ->
         reportError r "Expected a byte class but encountered a value."
       AValue {} -> f =<< mapM inferExpr es


--------------------------------------------------------------------------------

checkCommit :: HasRange r => r -> Commit -> TypeM ctx ()
checkCommit r cmt =
  case cmt of
    Commit -> pure ()
    Backtrack -> addWarning (WarnUnbiasedChoice (range r))


inferExpr :: Expr -> TypeM ctx (TC SourceRange ctx,Type)
inferExpr expr =
  case exprValue expr of

    EBuilder -> liftValAppPure expr [] \_ ->
                do a <- newTVar expr KValue
                   pure (exprAt expr (TCBuilder a), tBuilder a)

    ENothing -> liftValAppPure expr [] \_ ->
                do a <- newTVar expr KValue
                   pure (exprAt expr (TCNothing a), tMaybe a)

    EJust e ->
      liftValAppPure expr [e] \ ~[(e',t)] ->
        pure (exprAt expr (TCJust e'), tMaybe t)

    -- FIXME: maybe unify literal code?
    ELiteral l@(LBool _) ->
      liftValAppPure expr [] \_ -> pure (exprAt expr (TCLiteral l tBool), tBool)

    ELiteral LPi ->
      liftValAppPure expr [] \_ ->
        do a <- newTVar expr KValue
           addConstraint expr (FloatingType a)
           pure (exprAt expr (TCLiteral LPi a), a)

    ELiteral l@(LFloating _) -> liftValAppPure expr [] \_ ->
      do a <- newTVar expr KValue
         addConstraint expr (FloatingType a)
         pure (exprAt expr (TCLiteral l a), a)

    ELiteral l@(LNumber n txt) ->
      do ctxt <- getContext
         case ctxt of
           AClass
             | 0 <= n && n < 256 ->
                pure ( exprAt expr $ TCSetSingle
                     $ exprAt expr $ TCLiteral (LByte (fromInteger n) txt)
                                                                      tByteClass
                    , tByteClass
                    )
             | otherwise ->
                reportError expr "Byte literal out of range."

           _ -> liftValAppPure expr [] \_ ->
                do a <- newTVar expr KValue
                   addConstraint expr (Literal n a)
                   pure (exprAt expr (TCLiteral l a), a)

    ELiteral l@(LByte _ _) ->
      do ctxt <- getContext
         case ctxt of
           AClass   -> promoteValueToSet =<< inContext AValue (inferExpr expr)

           _ -> liftValAppPure expr [] \_ ->
                 pure (exprAt expr (TCLiteral l tByte), tByte)

    ELiteral l@(LBytes bs) -> 
      do ctxt <- getContext
         case ctxt of
           AClass   -> pure (exprAt expr (TCSetOneOf bs), tByteClass)

           _ -> liftValAppPure expr [] \_ ->
                pure (exprAt expr (TCLiteral l (tArray tByte)), tArray tByte)

    EMatch1 e ->
      grammarOnly expr $
      inContext AClass
      do (e1,_) <- inferExpr e
         pure (exprAt expr (TCMatch YesSem e1), tGrammar tByte)

    EMatch e ->
      grammarOnly expr $
      liftApp [e] \ ~[(e1,t)] ->
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
          do addConstraint expr (Arith t)
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

        WordToFloat ->
          liftValAppPure expr [e] \ ~[(e1',t)] ->
          do unify (tUInt (tNum 32)) (e1',t)
             pure (exprAt expr (TCUniOp WordToFloat e1'), tFloat)

        WordToDouble ->
          liftValAppPure expr [e] \ ~[(e1',t)] ->
          do unify (tUInt (tNum 64)) (e1',t)
             pure (exprAt expr (TCUniOp WordToDouble e1'), tDouble)

        IsNaN ->
          liftValAppPure expr [e] \ ~[(e1',t)] ->
          do addConstraint expr (FloatingType t)
             pure (exprAt expr (TCUniOp IsNaN e1'), tBool)

        IsInfinite ->
          liftValAppPure expr [e] \ ~[(e1',t)] ->
          do addConstraint expr (FloatingType t)
             pure (exprAt expr (TCUniOp IsInfinite e1'), tBool)

        IsDenormalized ->
          liftValAppPure expr [e] \ ~[(e1',t)] ->
          do addConstraint expr (FloatingType t)
             pure (exprAt expr (TCUniOp IsDenormalized e1'), tBool)

        IsNegativeZero ->
          liftValAppPure expr [e] \ ~[(e1',t)] ->
          do addConstraint expr (FloatingType t)
             pure (exprAt expr (TCUniOp IsNegativeZero e1'), tBool)

        BytesOfStream ->
          liftValAppPure expr [e] \ ~[(e1',t)] ->
            do unify tStream (e1',t)
               pure (exprAt expr (TCUniOp BytesOfStream e1'), tArray tByte)

        BuilderBuild ->
          liftValAppPure expr [e] \ ~[(e1',bt)] ->
            do t <- newTVar expr KValue
               unify (tBuilder t) (e1',bt)
               pure (exprAt expr (TCUniOp BuilderBuild e1'), tArray t)


    ETriOp op e1 e2 e3 ->
      case op of
        RangeUp -> rangeOp
        RangeDown -> rangeOp
        MapDoInsert ->
          liftValAppPure expr [e1,e2,e3] \ ~[(k1,kt),(v1,vt),(m1,mt)] ->
          do unify (tMap kt vt) (e3, mt)
             pure (exprAt expr (TCTriOp op k1 v1 m1 mt), mt)

      where
      rangeOp =
        liftValAppPure expr [e1,e2,e3] \ ~[ r1@(e1',t1)
                                          , r2@(e2',_)
                                          , r3@(e3',_)
                                          ] ->
        do unify r1 r2
           unify r1 r3
           addConstraint expr (Integral t1)
           pure (exprAt expr (TCTriOp op e1' e2' e3' (tArray t1)), tArray t1)


    EBinOp op e1 e2 ->
      case op of

        ArrayStream ->
          liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
          do unify (tArray tByte) (e1',t1)
             unify (tArray tByte) (e2',t2)
             pure (exprAt expr (TCBinOp op e1' e2' tStream), tStream)

        LookupMap ->
          liftValAppPure expr [e1,e2] \ ~[(e1',kt),(e2',mt)] ->
          do vt <- newTVar e2' KValue
             unify (tMap kt vt) (e2',mt)
             let ty = tMaybe vt
             pure (exprAt expr (TCBinOp op e1' e2' ty), ty)

        LCat ->
          liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
          do addConstraint expr (Integral t1)
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

        LogicOr  ->
          inferExpr $ pExprAt expr
                    $ EIf e1 (pExprAt expr (ELiteral (LBool True))) e2
        LogicAnd ->
          inferExpr $ pExprAt expr
                    $ EIf e1 e2 (pExprAt expr (ELiteral (LBool False)))

        Add   -> num2 Arith
        Sub   -> num2 Arith
        Mul   -> num2 Arith
        Div   -> num2 Arith
        Mod   -> num2 Integral
        Lt    -> rel
        Leq   -> rel
        Eq    -> relEq
        NotEq -> relEq

        BuilderEmit ->
          liftValAppPure expr [e1,e2] \ ~[(e1',bt),(e2',t)] ->
          do 
             unify (tBuilder t) (e1',bt)
             pure (exprAt expr (TCBinOp op e1' e2' bt), bt)

      where
      bitwiseOp =
        liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
        do addConstraint expr (Integral t1)
           unify (e1', t1) (e2', t2)
           pure (exprAt expr (TCBinOp op e1' e2' t1), t1)

      shiftOp =
        liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
        do addConstraint expr (Integral t1)
           unify tSize (e2',t2)
           pure (exprAt expr (TCBinOp op e1' e2' t1), t1)

      num2 c = liftValAppPure expr [e1,e2] \ ~[(e1',t1),(e2',t2)] ->
               do addConstraint e1 (c t1)
                  addConstraint e2 (c t2)
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

               -- turn 'e is l' into 'case e is { l x => ^ x }'
               SelUnion f ->
                 grammarOnly expr $
                 liftApp [e] \ ~[(e1,t)] ->
                 do let lab = thingValue f
                    addConstraint f (HasUnion t lab a)
                    addConstraint f (IsNamed t)
                    resVar <- newName expr a
                    let pat = TCConPat t lab (TCVarPat resVar)
                        alt = TCAlt [pat] (exprAt expr $ TCPure
                                          $ exprAt expr $ TCVar resVar)
                    pure (exprAt expr (TCCase e1 (alt :| []) Nothing), tGrammar a)

               SelTrue ->
                 grammarOnly expr $
                 liftApp [e] \ ~[(e1,t)] ->
                 do unify tBool (e1,t)
                    let pat = TCBoolPat True
                        alt = TCAlt [pat]
                                (exprAt expr $ TCPure $ exprAt expr tcUnit)
                    pure ( exprAt expr (TCCase e1 (alt :| []) Nothing)
                         , tGrammar tUnit
                         )

               SelFalse ->
                 grammarOnly expr $
                 liftApp [e] \ ~[(e1,t)] ->
                 do unify tBool (e1,t)
                    let pat = TCBoolPat False
                        alt = TCAlt [pat]
                                (exprAt expr $ TCPure $ exprAt expr tcUnit)
                    pure ( exprAt expr (TCCase e1 (alt :| []) Nothing)
                         , tGrammar tUnit
                         )

               SelNothing ->
                 grammarOnly expr $
                 liftApp [e] \ ~[(e1,t)] ->
                 do unify (tMaybe a) (e,t)
                    let pat = TCNothingPat a
                        alt = TCAlt [pat] (exprAt expr $ TCPure $ exprAt expr tcUnit)
                    pure (exprAt expr (TCCase e1 (alt :| []) Nothing), tGrammar tUnit)

               SelJust ->
                 grammarOnly expr $
                 liftApp [e] \ ~[(e1,t)] ->
                 do unify (tMaybe a) (e1,t)
                    resVar <- newName expr a
                    let pat = TCJustPat (TCVarPat resVar)
                        alt = TCAlt [pat] (exprAt expr $ TCPure
                                          $ exprAt expr $ TCVar resVar)
                    pure (exprAt expr (TCCase e1 (alt :| []) Nothing), tGrammar a)

    EEnd ->
      grammarOnly expr
      (pure (exprAt expr TCEnd, tGrammar tUnit))

    EOffset ->
      grammarOnly expr
      (pure (exprAt expr TCOffset, tGrammar tSize))

    ECurrentStream ->
      grammarOnly expr
      (pure (exprAt expr TCCurrentStream, tGrammar tStream))

    ESetStream s ->
      grammarOnly expr $
      liftApp [s] \ ~[(e,t)] ->
      do unify tStream (e,t)
         pure (exprAt expr (TCSetStream e), tGrammar tUnit)

    EStreamLen i s ->
      grammarOnly expr $
      liftApp [i,s] \ ~[(ie,it),(se,st)] ->
      do unify tSize (ie,it)
         unify tStream (se,st)
         pure (exprAt expr (TCStreamLen YesSem ie se), tGrammar tStream)

    EStreamOff i s ->
      grammarOnly expr $
      liftApp [i,s] \ ~[(ie,it),(se,st)] ->
      do unify tSize (ie,it)
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
      liftApp [k,v,m] \ ~[(k1,kt),(v1,vt),(m1,mt)] ->
      do unify (tMap kt vt) (m, mt)
         pure (exprAt expr (TCMapInsert YesSem k1 v1 m1), tGrammar mt)

    EMapLookup k m ->
      grammarOnly expr $
      liftApp [k,m] \ ~[(k1,kt),(m1,mt)] ->
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
         pure (exprAt expr (TCArrayLength e1), tSize)

    EArrayIndex e ix ->
      grammarOnly expr $
      liftApp [e,ix] \ ~[(e1,et), (ix1,ixt)] ->
      do vt         <- newTVar e KValue
         unify (tArray vt) (e, et)
         unify tSize (ix, ixt)
         pure (exprAt expr (TCArrayIndex YesSem e1 ix1), tGrammar vt)

    EStruct fs ->
      do ctxt <- getContext
         case ctxt of
           AClass   -> reportError expr "Unexpected struct in a byte set."
           AValue   -> inferStructPure expr fs
           AGrammar -> inferStructGrammar expr fs

    EIn (lf :> e) ->
      liftValAppPure expr [e] \ ~[(e1,t)] ->
      do ty     <- newTVar expr KValue
         tcon   <- newTyDefName
         let l = thingValue lf
         addConstraint lf (UnionCon tcon ty l lf { thingValue = t })
         addConstraint expr (IsNamed ty)
         pure (exprAt expr (TCIn l e1 ty), ty)

    EChoiceU cmt e1 e2 ->
      do ctxt <- getContext
         case ctxt of
           AClass -> do (e1',_) <- inferExpr e1
                        (e2',_) <- inferExpr e2
                        pure ( exprAt expr (TCSetUnion [e1',e2'])
                             , tByteClass
                             )
           _ ->
             grammarOnly expr
             do checkCommit expr cmt
                (eL,tL) <- inferExpr e1
                (eR,tR) <- inferExpr e2
                unify (eL,tL) (eR,tR)
                a <- grammarResult expr tL
                pure (exprAt expr (TCChoice cmt [eL,eR] a), tL)

    EChoiceT c [] ->
      grammarOnly expr
      do checkCommit expr c
         a <- newTVar expr KValue
         pure (exprAt expr (TCChoice c [] a), tGrammar a)

    EChoiceT c fs ->
      grammarOnly expr
      do checkCommit expr c
         ty   <- newTVar expr KValue

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
         tcon <- newTyDefName
         forM_ (Map.toList tagMap) \(x,lt:more) ->
                    do forM_  more \t1 -> unify (lt, thingValue lt)
                                                (t1, thingValue t1)
                       addConstraint lt (UnionCon tcon ty x lt)

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

    EImplicit ip ->
      panic "inferExpr" [ "Unexpected implicit parameter", show (pp ip) ]

    EAnyByte ->
      do ctxt <- getContext
         case ctxt of
           AValue   -> reportError expr "Invalid semantic value."
           AClass   -> pure (exprAt expr TCSetAny, tByteClass)
           AGrammar -> pure (exprAt expr (TCGetByte YesSem), tGrammar tByte)

    EOptional cmt e ->
      grammarOnly expr
      do checkCommit expr cmt
         (e1,t) <- inferExpr e
         a      <- grammarResult e t
         pure ( exprAt expr (TCOptional cmt e1)
              , tGrammar (tMaybe a)
              )

    EMany com bnds e ->
      grammarOnly expr
      do checkCommit e com
         (e1,t) <- inferExpr e
         a      <- grammarResult e t
         (newBnds,stmts) <- checkManyBounds bnds
         pure ( addBinds stmts (exprAt expr (TCMany YesSem com newBnds e1))
              , tGrammar (tArray a)
              )

    EQuiet e ->
      grammarOnly expr
      do (e1,_t) <- inferExpr e
         pure ( exprAt expr $ TCDo Nothing e1
              $ exprAt expr $ TCPure
              $ exprAt expr $ tcUnit
              , tGrammar tUnit
              )

    EPure e ->
      grammarOnly expr
      do (e1,t) <- inContext AValue (inferExpr e)
         pure (exprAt expr (TCPure e1), tGrammar t)

    EFail msg ->
      grammarOnly expr $
      liftApp [msg] \ ~[(msgE,msgT)] ->
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
                        Nothing -> pure (exprAt expr (TCLiteral (LNumber d (Text.pack (show d))) tByte))
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
          do ((is1,it),mbB) <- liftValExpr is
             kT       <- newTVar i KValue
             addConstraint e (ColKeyType it kT)

             elIn     <- newTVar i KValue
             addConstraint e (ColElType it elIn)

             outColT  <- newTVar i KValue
             addConstraint e (ColKeyType outColT kT)

             elOut <- newTVar i KValue
             addConstraint e (ColElType outColT elOut)

             addConstraint e (Mappable it outColT)

             let toName n t = TCName { tcName = n,
                                       tcNameCtx = AValue,
                                       tcType = t }
                 k1 = (`toName` kT) <$> mbIx
                 i1 = toName i elIn


             let addKey = case mbIx of
                            Nothing -> id
                            Just kx -> extEnv kx kT

             (e1,et)  <- addKey $ extEnv i elIn $ inferExpr e

             ctxt <- getContext
             case (ctxt, maybeToList mbB) of
               (AClass,_) ->
                  reportError expr "Expected a byte set but found `map`"

               (AValue,[]) ->
                  do let result = outColT
                     unify elOut (e,et)
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

               (AValue,_) ->
                 reportError expr "Expected a semantic value but found a parser"

               (AGrammar,bs) ->
                 do let result = tGrammar outColT
                    unify (tGrammar elOut) (e,et)
                    pure ( addBinds bs
                           $ exprAt expr (TCFor Loop
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
          do ((s1,st),bs1)  <- liftValExpr s
             ((is1,it),bs2) <- liftValExpr is
             addConstraint e (Traversable it)

             kT       <- newTVar i KValue
             elT      <- newTVar i KValue
             addConstraint e (ColKeyType it kT)
             addConstraint e (ColElType  it elT)

             let toName n t = TCName { tcName = n,
                                       tcNameCtx = AValue,
                                       tcType = t }
                 x1 = toName x st
                 k1 = (`toName` kT) <$> mbIx
                 i1 = toName i elT


             let addKey = case mbIx of
                            Nothing -> id
                            Just kx -> extEnv kx kT

             (e1,et)  <- extEnv x st $ addKey $ extEnv i elT $ inferExpr e

             ctxt <- getContext
             case (ctxt, catMaybes [bs1,bs2]) of
               (AClass, _) ->
                  reportError expr "Expected a byte set but found `for`"

               (AValue,[]) ->
                  do unify st (e,et)
                     pure ( exprAt expr (TCFor Loop
                                                { loopFlav = Fold x1 s1
                                                , loopKName = k1
                                                , loopElName = i1
                                                , loopCol = is1
                                                , loopBody = e1
                                                , loopType = et
                                                })
                          , st
                          )


               (AValue,_) ->
                  reportError expr
                        "Expected a semantic value but found a parser."

               (AGrammar, bs) ->
                  do unify (tGrammar st) (e,et)
                     pure ( addBinds bs
                            $ exprAt expr (TCFor Loop
                                                  { loopFlav = Fold x1 s1
                                                  , loopKName = k1
                                                  , loopElName = i1
                                                  , loopCol = is1
                                                  , loopBody = e1
                                                  , loopType = et
                                                  })
                          , tGrammar st
                          )

    EIf be te fe -> inferIf expr be te fe

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
      grammarOnly expr
      do t    <- checkType KValue ty
         liftApp [e] \ ~[(e1,t1)] ->
          do addConstraint ty (Coerce Dynamic t1 t)
             pure (exprAt expr (TCCoerceCheck YesSem t1 t e1), tGrammar t)

    EHasType CoerceSafe e ty ->
      liftValAppPure expr [e] \ ~[(e1,t1)] ->
      do t <- checkType KValue ty
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

    -- e should have the same type as the 
    ECase e ps -> inferCase expr e ps



--------------------------------------------------------------------------------
-- Patterns & Case & If

inferIf ::
  HasRange r =>
  r -> Expr -> Expr -> Expr -> TypeM ctx (TC SourceRange ctx, Type)
inferIf r eCond eThen eElse =
  do ((eCond',tCond),mbS) <- liftValExpr eCond
     unify tBool (eCond',tCond)
     (eThen',tThen) <- inferExpr eThen
     (eElse',tElse) <- inferExpr eElse
     unify (eThen',tThen) (eElse,tElse)
     let expr1 = exprAt r (TCIf eCond' eThen' eElse')
     expr <- case mbS of
               Nothing -> pure expr1
               Just s ->
                 do ctx <- getContext
                    case ctx of
                      AGrammar -> pure (addBind s expr1)
                      _ -> panic "inferIf" [ "Lifted in non-grammar context"]
     pure (expr, tThen)



checkPattern :: Type -> Pattern -> TypeM ctx TCPat
checkPattern ty pat =
  case pat of

    LitPattern l ->
      case thingValue l of
        LNumber i txt ->
          do addConstraint l (Literal i ty)
             pure (TCNumPat ty i txt)
        LByte i txt ->
          do unify ty (pat,tByte)
             pure (TCNumPat tByte (toInteger i) txt)
        LBool b ->
          do unify ty (pat,tBool)
             pure (TCBoolPat b)

        LBytes bs ->
          do unify ty (pat, tArray tByte)
             pure (TCStrPat bs)
        _ -> reportError pat "Unsuported literal pattern"

    WildPattern _ ->
      pure (TCWildPat ty)

    VarPattern x ->
      pure (TCVarPat TCName { tcName = x, tcNameCtx = AValue, tcType = ty })

    ConPattern c p ->
      case thingValue c of
        ConUser l ->
          do a  <- newTVar c KValue
             p' <- checkPattern a p
             addConstraint c (HasUnion ty l a)
             addConstraint c (IsNamed ty)
             pure (TCConPat ty l p')

        ConNothing ->
          do a <- newTVar c KValue
             unify ty (c,tMaybe a)
             pure (TCNothingPat a)

        ConJust ->
          do a <- newTVar c KValue
             unify ty (c,tMaybe a)
             p1 <- checkPattern a p
             pure (TCJustPat p1)


checkPatternCase ::
  Type -> Type -> [Pattern] -> Expr -> TypeM ctx (TCAlt SourceRange ctx)
checkPatternCase tIn tOut ps e =
  do qs <- mapM (checkPattern tIn) ps
     let vars = sort (patBinds (head qs))
     forM_ (zip ps qs) \(p,q1) ->
        do unify tIn (p,typeOf q1)
           let check v1 v2 = unify (v1,typeOf v1) (v2,typeOf v2)
           zipWithM check vars (patBinds q1)
     let addVar x = extEnv (tcName x) (tcType x)

     ctx <- getContext
     case ctx of
       AGrammar ->
        do r@(e',_) <- foldr addVar (inferExpr e) vars
           unify tOut r
           let lab = "case branch " <+> commaSep (map pp qs)
           pure (TCAlt qs $ exprAt e' $ TCLabel (Text.pack (show lab)) e')
       _ ->
        do r@(e',_) <- foldr addVar (inferExpr e) vars
           unify tOut r
           pure (TCAlt qs e') -- (exprAt e' (TCLabel lab e')))

checkPatternCases :: HasRange r =>
  r ->
  Type -> Type ->
  [TCAlt SourceRange ctx] ->
  [PatternCase Expr] ->
  TypeM ctx (NonEmpty (TCAlt SourceRange ctx), Maybe (TC SourceRange ctx))
checkPatternCases rng tIn tOut done cases =
  case cases of

    PatternCase pats rhs : rest ->
      do alt <- checkPatternCase tIn tOut pats rhs
         checkPatternCases rng tIn tOut (alt:done) rest

    [PatternDefault e] ->
      do r@(e1,_) <- inferExpr e
         unify tOut r
         checkNonEmpty (Just e1)
    PatternDefault e : _ -> reportError e "The catch-all case must come last"

    [] -> checkNonEmpty Nothing

  where
  checkNonEmpty mb =
    case reverse done of
      []       -> reportError rng "`case` needs at least one non-default pattern."
      (x : xs) -> pure (x :| xs, mb)

inferCase ::
  HasRange r =>
  r -> Expr -> [PatternCase Expr] ->
  TypeM ctx (TC SourceRange ctx, Type)
inferCase rng e ps =
  do ((e1,tIn),mbS) <- liftValExpr e
     tOut <-
        do ctx <- getContext
           case ctx of
             AGrammar -> tGrammar <$> newTVar e KValue
             AValue   -> newTVar e KValue
             AClass   -> newTVar e KClass

     (alts,mbDefault) <- checkPatternCases e tIn tOut [] ps
     let expr1 = exprAt rng (TCCase e1 alts mbDefault)
     expr <- case mbS of
               Nothing -> pure expr1
               Just s ->
                 do ctx <- getContext
                    case ctx of
                      AGrammar -> pure (addBind s expr1)
                      _ -> reportError e "Found a parser, but expecte a value"
     pure (expr, tOut)



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
  RuleType {- ^ Instantiated type of called function -} ->
  [Expr]   {- ^ Epxression arguments -} ->
  TypeM ctx (TC SourceRange ctx, Type)
checkTopRuleCall r f@Name { nameContext = fctx } tys
                    ((impTs,inTs0) :-> outT) es =
  do ctx <- getContext
     let ppf = backticks (pp f)

     let inTs = map snd impTs ++ inTs0

     (args,_,mbStmts) <- unzip3 <$> zipWithM checkArg (map Just inTs) es
     let have  = length es
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
                            hsep [ int (need - have)
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
         do ((e1,t),mbS) <- liftValExpr e    -- optional lifting
            checkTy (e1,t)
            pure (ValArg e1,t,mbS)

       (AGrammar, Some AClass) ->     -- calling grammar, expecting a value
         do ((e1,t),mbS) <- liftValExpr e    -- optional lifting
            checkTy (e1,t)
            pure (ValArg e1,t,mbS)

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
checkPromote x = checkPromoteFrom (tcNameCtx x) x


checkPromoteFrom ::
  (HasRange r, PP r) =>
  Context k -> r ->
  TC SourceRange k -> Type -> TypeM ctx (TC SourceRange ctx, Type)
checkPromoteFrom fromCtxt x e t =
  do ctxt <- getContext
     let ppx = backticks (pp x)
     case (fromCtxt, ctxt) of
       (AValue, AValue)   -> pure (e, t)
       (AValue, AClass)   -> promoteValueToSet (e, t)
       (AValue, AGrammar) -> liftValAppPure e [] \_ -> pure (e,t)

       (AClass,AValue) ->
          reportError x
            ("Expected a value but" <+> ppx <+> "is a bytes set.")

       (AClass, AClass) -> pure (e,t)
       (AClass, AGrammar) -> promoteSetToGrammar (e,t)

       (AGrammar,AValue) ->
          reportError x ("Expected a value but" <+> ppx <+> "is a parser.")

       (AGrammar,AClass) ->
          reportError x
              ("Expected a bytes set but" <+> ppx <+> "is a parser.")

       (AGrammar, AGrammar) -> pure (e,t)




grammarOnly ::
  HasRange r =>
  r ->
  TypeM Grammar (TC SourceRange Grammar, Type) ->
  TypeM ctx (TC SourceRange ctx, Type)
grammarOnly r k =
  do ctxt <- getContext
     case ctxt of
       AValue ->
         reportError r "Expected a value but encountered a parser."
       AClass ->
         reportError r "Expected a byte class but encountered a parser."
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
  ManyBounds Expr -> TypeM ctx (ManyBounds (TC SourceRange Value),[BindStmt])
checkManyBounds bnds =
  case bnds of
    Exactly e ->
      do (e1,bs) <- checkNum e
         pure (Exactly e1, bs)
    Between e1 e2 ->
      do (e1',bs1) <- checkNumMb e1
         (e2',bs2) <- checkNumMb e2
         pure (Between e1' e2', bs1 ++ bs2)
  where
  checkNumMb e =
    case e of
      Nothing -> pure (Nothing,[])
      Just e' ->
        do (et,bs) <- checkNum e'
           pure (Just et, bs)

  checkNum e =
    do ((e1,t),mb) <- liftValExpr e
       unify tSize (e1,t)
       pure (e1,maybeToList mb)


inferStructPure ::
  HasRange r =>
    r -> [StructField Expr] -> TypeM Value (TC SourceRange Value, Type)
inferStructPure toploc = check Set.empty []


  where
  check allLs done fs =
    case fs of
      COMMIT r : _ ->
        reportError r "COMMIT may not appear in a semantic value."

      Anon e : more ->
        case more of
          [] | null done -> inferExpr e
          _  -> reportError e "Struct value needs a label."

      x :?= _ : _ -> panic "inferStructPure"
                       [ "Unexpected implicit parameter", show (pp x) ]

      x := e : more
        | x `Set.member` allLs ->
          reportError x ("Multiple definitions for " <+> pp x)

        | otherwise ->
        do (e1,t1) <- inferExpr e
           let x' = TCName { tcName = x, tcType = t1, tcNameCtx = AValue }
               e' = exprAt x (TCVar x')

           (e2,ty) <- extEnv x t1 $ check (Set.insert x allLs)
                                          ((nameScopeAsLocal x,t1,e'):done)
                                          more
           pure (exprAt (x <-> e2) (TCLet x' e1 e2), ty)

      x :@= e : more
        | x `Set.member` allLs ->
          reportError x ("Multiple definitions for " <+> pp x)

        | otherwise ->
          case more of
            [] -> reportError x "`block` may not end with a `let`"
            _  -> do (e1,t1) <- inferExpr e
                     (e2,ty) <- extEnv x t1 (check (Set.insert x allLs)
                                                   done more)
                     let x' = TCName { tcName = x, tcType = t1,
                                                        tcNameCtx = AValue }
                     pure (exprAt (x <-> e2) (TCLet x' e1 e2), ty)

      [] -> pureStruct toploc ls ts es
        where
        (ls,ts,es) = unzip3 (reverse done)





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
             pure (exprAt (x <-> ke) (TCDo (Just x') e1 ke), kt)
    in
    case fs of
      [Anon e] | null mbRes && null done ->
                  do -- This is ugly, but the tUnit is just do we can
                     -- call newName, we discard it when projecing out
                     -- the new Name
                     nm <- tcName <$> newName e tUnit 
                     let x = nm { nameScopedIdent = Local "$$" }
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
               pure (exprAt (e <-> ke) (TCDo Nothing e1 ke), kt)

          x :?= _ -> panic "inferStructGrammar"
                      [ "Unexpected implicit parameter", show (pp x) ]

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
             reportDetailedError y "Cannot have multiple `$$` fields. See:"
                            [ pp (range z) | z <- x : y : more
                            ]

          (x : _, _) -> reportError x "Cannot mix `$$` and named fields."



pureStruct ::
  HasRange r =>
  r ->
  [Label] -> [Type] -> [TC SourceRange Value] ->
  TypeM ctx (TC SourceRange Value, Type)
pureStruct r ls ts es
  | l : _ <- repeated =
      reportError r ("Multiple entries for field" <+> backticks (pp l))
  | otherwise =
    do ty <- newTVar r KValue
       nm <- newTyDefName
       addConstraint r $
          StructCon nm ty
            [ (l, Located { thingRange = range e, thingValue = t })
            | l <- ls
            | e <- es
            | t <- ts
            ]
       pure (exprAt r (TCStruct (zip ls es) ty), ty)
  where
  repeated = [ l | (l : _ : _) <- group (sort ls) ]

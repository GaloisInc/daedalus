{-# Language BlockArguments #-}
{-# Language GADTs #-}
{-# Language ImplicitParams #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Daedalus.DDL2Core where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad(ap,liftM)
import Data.Maybe(maybeToList)
import Data.List((\\))
import Data.Either(partitionEithers)
import qualified Data.ByteString.Char8 as BS8

import Daedalus.Panic(panic)

import qualified Daedalus.Type.AST as TC
import Daedalus.Type.AST (Commit(..), WithSem(..))
import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.Type(typeOf)


--------------------------------------------------------------------------------

-- | Assumes that the modules are in dependency order, with leaves first.
importModules :: [TC.TCModule a] -> TC.ScopedIdent -> ([Module], FName)
importModules ms entry = fst
                       $ runM Map.empty Map.empty
                       $ do ms1 <- mapM fromModule ms
                            f   <- scopedIdent entry
                            pure (ms1,f)


fromModule :: TC.TCModule a -> M Module
fromModule mo =
  withModuleName (TC.tcModuleName mo) $
  do mapM_ (newTName . TC.tctyName)
         $ concatMap recToList $ TC.tcModuleTypes mo
     tds <- mapM fromTCTyDeclRec (TC.tcModuleTypes mo)

     let ds = concatMap recToList (TC.tcModuleDecls mo)
     mapM_ addDeclName ds
     (dffs,dgfs) <- partitionEithers <$> mapM fromDecl ds
     (effs,egfs) <- removeNewFuns

     m <- getCurModule
     pure
       Module { mName  = m
              , mImports = map (MName . TC.thingValue) (TC.tcModuleImports mo)
              , mTypes  = tds
              , mFFuns  = effs ++ dffs
              , mGFuns  = egfs ++ dgfs
              }

addDeclName :: TC.TCDecl a -> M ()
addDeclName TC.TCDecl { .. } =
  case tcDeclCtxt of
    TC.AValue   ->
      case tcDeclDef of
        TC.ExternDecl t -> fromFDefName tcDeclName t
        TC.Defined v    -> fromFDefName tcDeclName (TC.typeOf v)

    TC.AClass -> fromCDefName tcDeclName

    TC.AGrammar ->
      case tcDeclDef of
        TC.ExternDecl t -> fromGDefName tcDeclName t
        TC.Defined v    -> fromGDefName tcDeclName (TC.typeOf v)


sysErr :: Type -> String -> Grammar
sysErr t msg = Fail ErrorFromSystem t (Just (byteArrayL (BS8.pack msg)))

fromDecl :: TC.TCDecl a -> M (Either (Fun Expr) (Fun Grammar))
fromDecl TC.TCDecl { .. }
  | null tcDeclTyParams
  , null tcDeclCtrs =
    do xs0 <- mapM fromParam tcDeclParams
       let xs = map snd xs0
       f <- topName tcDeclName
       withSourceLocals xs0
         case tcDeclCtxt of
           TC.AValue   ->
              case tcDeclDef of
                TC.ExternDecl _ ->
                  pure $ Left Fun { fName = f, fParams = xs, fDef = External }

                TC.Defined v ->
                   do e <- fromExpr v
                      pure $ Left Fun { fName = f, fParams = xs, fDef = Def e }

           TC.AClass ->
             do x <- newLocal tByte
                case tcDeclDef of
                  TC.ExternDecl _ ->
                    pure $ Left
                           Fun { fName = f, fParams = x : xs, fDef = External }

                  TC.Defined v ->
                    do e <- fromClass v x
                       pure $ Left
                              Fun { fName = f, fParams = x : xs, fDef = Def e }

           TC.AGrammar ->
             case tcDeclDef of

               TC.ExternDecl _ ->
                 pure $ Right Fun { fName = f, fParams = xs, fDef = External }

               TC.Defined v ->
                 do e <- fromGrammar v
                    pure $ Right Fun { fName = f, fParams = xs, fDef = Def e }


  | otherwise =
    panic "fromDeclG" [ "Type parmaeters/constraints" ]



fromGrammar :: TC.TC a TC.Grammar -> M Grammar
fromGrammar gram =
  case TC.texprValue gram of

    TC.TCFail mbE t ->
      Fail ErrorFromUser <$> fromTypeM t <*> traverse fromExpr mbE

    TC.TCPure e ->
      Pure <$> fromExpr e

    TC.TCDo Nothing g1 g2 ->
      Do_ <$> fromGrammar g1 <*> fromGrammar g2

    -- XXX: binder
    TC.TCDo (Just x) g1 g2 ->
      do x' <- fromName x
         Do (snd x') <$> fromGrammar g1 <*> withSourceLocal x' (fromGrammar g2)


    TC.TCLabel t g ->
      Annot (SrcAnnot t) <$> fromGrammar g

    TC.TCGetByte sem ->
      do x <- newLocal TStream
         let xe = Var x
             ty = resTy sem tByte
         pure $ Do x GetStream
              $ If (isEmptyStream xe) (sysErr ty "unexpected end of input")
                 $ Do_ (SetStream (eDrop (intL 1 TInteger) xe))
                 $     Pure $ result sem $ eHead xe

    TC.TCMatch sem c ->
      do x <- newLocal TStream
         y <- newLocal tByte
         let xe = Var x
         let ye = Var y

         p <- fromClass c y
         let ty = resTy sem tByte

         pure $ Do x GetStream
              $ If (isEmptyStream xe) (sysErr ty "unexpected end of input")
              $ Let y (eHead xe)
              $ If p ( Do_ (SetStream (eDrop (intL 1 TInteger) xe))
                     $     Pure (result sem ye)
                     )
                     (sysErr ty "unexpected byte")


    TC.TCGuard e ->
      do v <- fromExpr e
         pure $ If v (Pure unit) (sysErr TUnit "guard failed")

    TC.TCMatchBytes sem e ->
      do x <- newLocal TStream
         y <- newLocal (TArray tByte)
         let xe = Var x
             ye = Var y
             ty = resTy sem (TArray tByte)

         v <- fromExpr e
         pure $ Do x GetStream
              $    Let y v
              $    If (isPrefix ye xe)
                      (Do_ (SetStream (eDrop (arrayLen ye) xe))
                         $ Pure (result sem ye))
                      (sysErr ty "unexpected byte sequence")

    TC.TCChoice cmt opts ty ->
      case opts of
        [] -> fromTypeM ty >>= \t -> pure (sysErr t "empty choice")
        _  -> foldr1 (orOp cmt) <$> mapM fromGrammar opts


    TC.TCOptional cmt g ->
      do ty <- fromTypeM (TC.typeOf g)
         x <- newLocal ty
         ge <- fromGrammar g
         let lhs = Do x ge
                 $    Pure (just (Var x))
         pure (orOp cmt lhs (Pure (nothing ty)))


    TC.TCEnd ->
      do x <- newLocal TStream
         pure $ Do x GetStream
              $    If (isEmptyStream (Var x))
                      (Pure unit)
                      (sysErr TUnit "unexpected leftover input")


    TC.TCOffset ->
      do x <- newLocal TStream
         pure $ Do x GetStream
              $    Pure (streamOffset (Var x))

    TC.TCCurrentStream ->
      pure GetStream

    TC.TCSetStream e ->
      SetStream <$> fromExpr e

    TC.TCStreamLen sem n s ->
      do lenE <- fromExpr n
         strE <- fromExpr s
         case sem of
           NoSem -> pure $ If (lenE `leq` streamLen strE)
                              (Pure unit)
                              (sysErr TUnit "unexpected end of input")


           YesSem -> do len <- newLocal TInteger
                        str <- newLocal TStream
                        pure $ Let len lenE
                             $ Let str strE
                             $ If (Var len `leq` streamLen (Var str))
                                  (Pure $ eTake (Var len) (Var str))
                                  (sysErr TUnit "unexpected end of input")

    TC.TCStreamOff sem n s ->
      do lenE <- fromExpr n
         strE <- fromExpr s
         case sem of
           NoSem -> pure $ If (lenE `leq` streamLen strE)
                              (Pure unit)
                              (sysErr TUnit "unexpected end of input")
           YesSem ->
             do len <- newLocal TInteger
                str <- newLocal TStream
                pure $ Let len lenE
                     $ Let str strE
                     $ If (Var len `leq` streamLen (Var str))
                          (Pure $ eDrop (Var len) (Var str))
                          (sysErr TUnit "unexpected end of input")


    TC.TCMany sem cmt bnd g ->
      do ge <- fromGrammar g
         ty   <- fromGTypeM (TC.typeOf g)
         cbnd <- fromManybeBound bnd
         let vs = Set.toList (freeVars ge)

         case cbnd of
           TC.Exactly e ->
              do x <- newLocal TInteger
                 let xe = Var x
                     vs1 = x : vs

                 Let x e <$>
                   case sem of
                     NoSem  -> checkAtLeast Nothing xe =<< pSkipAtMost vs1 xe ge
                     YesSem -> checkAtLeast (Just ty) xe
                                                   =<< pParseAtMost ty vs1 xe ge

           TC.Between Nothing Nothing ->
              case sem of
                NoSem  -> pSkipMany cmt vs ge
                YesSem -> pParseMany cmt ty vs ge

           TC.Between Nothing (Just e) ->
              case sem of
                NoSem  -> pSkipAtMost vs e ge
                YesSem -> pParseAtMost ty vs e ge

           TC.Between (Just e) Nothing ->
              case sem of
                NoSem  -> checkAtLeast Nothing e =<< pSkipMany cmt vs ge
                YesSem -> checkAtLeast (Just ty) e =<< pParseMany cmt ty vs ge

           TC.Between (Just lb) (Just ub) ->
              case sem of
                NoSem  -> checkAtLeast Nothing lb =<< pSkipAtMost vs ub ge
                YesSem -> checkAtLeast (Just ty) lb =<< pParseAtMost ty vs ub ge

    TC.TCMapLookup sem k mp ->
      do kE  <- fromExpr k
         mpE <- fromExpr mp
         case typeOf mpE of
           TMap _ t -> fromMb sem t (mapLookup mpE kE)
           _ -> panic "ddl2core" ["Lookup in a non-map"]

    TC.TCMapInsert sem k v mp ->
      do kE  <- fromExpr k
         vE  <- fromExpr v
         mpE <- fromExpr mp
         ty  <- fromTypeM (TC.typeOf mp)
         pure case sem of
                NoSem  -> If (mapMember mpE kE)
                             (sysErr TUnit "duplicate key in map")
                             (Pure unit)
                YesSem ->
                  If (mapMember mpE kE)
                     (sysErr ty "duplicate key in map")
                     (Pure (mapInsert mpE kE vE))

    TC.TCArrayIndex sem arr ix ->
      do aE <- fromExpr arr
         iE <- fromExpr ix
         case sem of
           NoSem -> pure $ If (arrayLen aE `leq` iE)
                              (sysErr TUnit "array index out of bounds")
                              (Pure unit)
           YesSem ->
             do ty0 <- fromTypeM (TC.typeOf arr)
                let ty = case ty0 of
                           TArray t -> t
                           _ -> panic "fromGrammar"
                                   [ "TCArrayIndex: not an array" ]
                a <- newLocal TInteger
                i <- newLocal TInteger
                pure $ Let a aE
                     $ Let i iE
                     $ If (arrayLen (Var a) `leq` Var i)
                          (sysErr ty "array index out of bounds")
                          (Pure (arrayIndex (Var a) (Var i)))

    TC.TCVar x -> panic "fromGrammar"
                  [ "Unexpected grammar variable: " ++ show x ]

    TC.TCCoerceCheck sem _t1 t2 v ->
      do e   <- fromExpr v
         tgt <- fromTypeM t2
         fromMb sem tgt (coerceMaybeTo tgt e)

    TC.TCSelJust sem v t ->
      do e <- fromExpr v
         ty <- fromTypeM t
         fromMb sem ty e

    TC.TCSelUnion sem v l t ->
      do e <- fromExpr v
         ty <- fromTypeM t
         case sem of
           NoSem -> pure $ If (hasTag l e)
                              (Pure unit)
                              (sysErr TUnit "unexpected semantic value shape")
           YesSem ->
             do x <- newLocal ty
                let xe = Var x
                pure $ Let x e
                     $ If (hasTag l xe)
                          (Pure (fromUnion ty l xe))
                          (sysErr ty "unexpected semantic value shape")


    TC.TCFor l -> doLoopG l

    TC.TCCall f ts as ->
      case ts of
        [] -> do vs <- mapM fromArg as
                 g  <- fromGName f
                 pure (Call g vs)

        _ -> panic "fromGrammar" [ "Call with type parameters" ]

    -- XXX
    TC.TCErrorMode cmt g ->
      case cmt of
        Backtrack -> fromGrammar g
        Commit -> panic "fromGrammar" ["Commit is not yet supported"]


--------------------------------------------------------------------------------
-- Loops


doLoopG :: TC.Loop a TC.Grammar -> M Grammar
doLoopG lp =
  do keyVar <- traverse fromName (TC.loopKName lp)
     elVar  <- fromName (TC.loopElName lp)
     colE   <- fromExpr (TC.loopCol lp)

     colT   <- fromTypeM $ TC.typeOf (TC.loopCol lp)

     let doBody = withSourceLocal elVar
                $ maybe id withSourceLocal keyVar
                $ fromGrammar $ TC.loopBody lp

     case TC.loopFlav lp of

       TC.Fold x s ->
         do sVar     <- fromName x
            initS    <- fromExpr s
            g        <- withSourceLocal sVar doBody
            let free = freeVars g
            ty       <- fromGTypeM $ TC.loopType lp
            foldLoopG
              colT ty (Set.toList free)
              (snd sVar) initS
              (snd <$> keyVar) (snd elVar) colE
              \_ -> g

       TC.LoopMap ->
         fromGTypeM (TC.loopType lp) >>= \resT ->
         case resT of

           TArray elTy ->
             do sVar <- newLocal (TBuilder elTy)
                newEl <- newLocal elTy
                g <- doBody
                let free = freeVars g
                let ty = TBuilder elTy
                step1 <- foldLoopG
                           colT ty (Set.toList free)
                           sVar (newBuilder elTy)
                           (snd <$> keyVar) (snd elVar) colE
                           \_ ->
                             Do newEl g
                              $ Pure (consBuilder (Var newEl) (Var sVar))

                x <- newLocal (TBuilder elTy)
                pure $ Do x step1 (Pure (finishBuilder (Var x)))

           ty@(TMap tk tv) ->
              do sVar <- newLocal ty
                 newEl <- newLocal tv
                 g <- doBody
                 let free = freeVars g
                 foldLoopG colT ty (Set.toList free)
                   sVar (mapEmpty tk tv)
                   (snd <$> keyVar) (snd elVar) colE
                   \i -> Do newEl g
                          $ Pure (mapInsert (Var sVar)
                                            (iteratorKey i)
                                            (Var newEl))

           _ -> panic "doLoopG/MapLoop" ["Unexpected result type"]


foldLoopG ::
  Type               {- ^ Collection type -} ->
  Type               {- ^ Result type -} ->
  [Name]             {- ^ Free vars -} ->
  Name               {- ^ State var name -} ->
  Expr               {- ^ Initial state -} ->
  Maybe Name         {- ^ Key var, if any -} ->
  Name               {- ^ Element var -} ->
  Expr               {- ^ Collection to fold -} ->
  (Expr -> Grammar)  {- ^ Loop body, parameterized by iterator -} ->
  M Grammar
foldLoopG colT ty vs0 sVar initS keyVar elVar colE g =
  do let vs  = vs0 \\ (maybeToList keyVar ++ [sVar,elVar])
         es  = map Var vs
         maybeAddKey e = case keyVar of
                           Nothing -> id
                           Just k  -> Let k e

     f <- newGName ty
     i <- newLocal (TIterator colT)
     nextS <- newLocal ty
     defFunG f (sVar : i : vs)
         $ If (iteratorDone (Var i))
              (Pure (Var sVar))
         $    Let elVar (iteratorVal (Var i))
         $    maybeAddKey (iteratorNext (Var i))
         $    Do nextS (g (Var i))
         $       Call f (Var nextS : iteratorNext (Var i) : es)
     pure $ Call f (initS : newIterator colE : es)




pSkipMany :: Commit -> [Name] -> Grammar -> M Grammar
pSkipMany cmt vs p =
  do f <- newGName TUnit
     let es = map Var vs
     defFunG f vs $
        orOp cmt (Do_ p (Call f es)) (Pure unit)
     pure (Call f es)

pParseMany :: Commit -> Type -> [Name] -> Grammar -> M Grammar
pParseMany cmt ty vs p =
  do f <- newGName (TBuilder ty)
     let es = map Var vs
     x <- newLocal (TBuilder ty)
     y <- newLocal ty
     let xe = Var x
     defFunG f (x:vs)
       $ orOp cmt (Do y p (Call f (consBuilder (Var y) xe : es))) (Pure xe)

     z <- newLocal (TBuilder ty)
     pure $ Do z (Call f (newBuilder ty : es))
                 (Pure $ finishBuilder $ Var z)

pSkipAtMost :: [Name] -> Expr -> Grammar -> M Grammar
pSkipAtMost vs tgt p =
  do f <- newGName TInteger
     let es = map Var vs
     x <- newLocal TInteger
     let xe = Var x
     defFunG f (x:vs)
             $ If (tgt `leq` xe)
                  (Pure xe)
                  ( Do_ p
                  $     Call f (add xe (intL 1 TInteger) : es)
                  )
     pure (Call f (intL 0 TInteger : es))


pParseAtMost :: Type -> [Name] -> Expr -> Grammar -> M Grammar
pParseAtMost ty vs tgt p =
  do f <- newGName (TArray ty)
     let es = map Var vs
     x <- newLocal TInteger
     b <- newLocal (TBuilder ty)
     z <- newLocal ty
     let xe = Var x
         be = Var b
         ze = Var z
     defFunG f (x : b : vs)
              $ If (tgt `leq` xe)
                   ( Pure $ finishBuilder be )
                   ( Do z p
                   $    Call f ( add xe (intL 1 TInteger)
                               : consBuilder ze be
                               : es
                               )
                   )

     pure $ Call f (intL 0 TInteger : newBuilder ty : es)


-- | Use to check the lower bounds for `many`
checkAtLeast :: Maybe Type -> Expr -> Grammar -> M Grammar
checkAtLeast mb tgt g =
  case mb of
    Nothing ->
      do x <- newLocal TInteger
         pure $ Do x g $ If (Var x `lt` tgt)
                            (nope TUnit)
                            (Pure unit)
    Just ty ->
      do x <- newLocal (TArray ty)
         pure $ Do x g $ If (arrayLen (Var x) `lt` tgt) (nope (TArray ty))
                                                        (Pure (Var x))

  where
  nope ty = sysErr ty "insufficient element occurances"


--------------------------------------------------------------------------------



fromClass :: TC.TC a TC.Class -> Name -> M Expr
fromClass cla x =
  case TC.texprValue cla of
    TC.TCSetAny          -> pure (boolL True)
    TC.TCSetSingle v     -> do e <- fromExpr v
                               pure (Var x `eq` e)
    TC.TCSetComplement c -> do e <- fromClass c x
                               pure (eNot e)

    TC.TCSetUnion cs     -> do es <- mapM (`fromClass` x) cs
                               pure case es of
                                      [] -> boolL False
                                      _  -> foldr1 eOr es

    TC.TCSetOneOf xs     -> pure (oneOf xs (Var x))

    TC.TCSetDiff c1 c2   -> do e1 <- fromClass c1 x
                               e2 <- fromClass c2 x
                               pure (eAnd e1 (eNot e2))

    TC.TCSetRange v1 v2  -> do e1 <- fromExpr v1
                               e2 <- fromExpr v2
                               pure (eAnd (e1 `leq` Var x) (Var x `leq` e2))

    TC.TCCall f ts as ->
      case ts of
        [] ->
          do fc <- fromCName f
             es <- mapM fromArg as
             pure (callF fc (Var x : es))

        _ -> panic "fromClass" ["Unexptect type parameters"]

    TC.TCFor {} -> panic "fromClass" ["Unexpected loop"]
    TC.TCVar {} -> panic "fromClass" ["Unexpected var"]



--------------------------------------------------------------------------------


fromExpr :: TC.TC a TC.Value -> M Expr
fromExpr expr =
  case TC.texprValue expr of
    TC.TCMapEmpty t ->
      fromTypeM t >>= \resT ->
      case resT of
        TMap k v -> pure (mapEmpty k v)
        _ -> panic "fromExpr" ["MapEmpty not a map"]

    TC.TCArrayLength v ->
      arrayLen <$> fromExpr v

    TC.TCCoerce _t1 t2 v ->
      coerceTo <$> fromTypeM t2 <*> fromExpr v

    TC.TCNumber n t ->
      intL n <$> fromTypeM t

    TC.TCBool b ->
      pure (boolL b)

    TC.TCNothing t ->
      nothing <$> fromTypeM t

    TC.TCJust e ->
      just <$> fromExpr e

    TC.TCByte x ->
      pure (intL (toInteger x) tByte)

    TC.TCUnit ->
      pure unit

    TC.TCStruct fs t ->
      Struct <$> userTypeM t <*> mapM field fs

        where field (l,v) = do e <- fromExpr v
                               pure (l,e)

    TC.TCByteArray bs ->
      pure (byteArrayL bs)

    TC.TCArray vs t ->
      arrayL <$> fromTypeM t <*> mapM fromExpr vs

    TC.TCIn l v t ->
      inUnion <$> userTypeM t <*> pure l <*> fromExpr v

    TC.TCSelStruct v l t ->
      selStruct <$> fromTypeM t <*> pure l <*> fromExpr v

    TC.TCIf v1 v2 v3 ->
      do e1 <- fromExpr v1
         e2 <- fromExpr v2
         e3 <- fromExpr v3
         pure (eIf e1 e2 e3)

    TC.TCVar x ->
      Var <$> sourceLocal x

    TC.TCCall f ts as ->
      case ts of
        [] ->
          do ff <- fromFName f
             es <- mapM fromArg as
             pure (callF ff es)

        _ -> panic "fromExpr" [ "Unexpected type arguments" ]


    TC.TCUniOp op v ->
      do e <- fromExpr v
         pure case op of
                TC.Not -> eNot e
                TC.Neg -> neg e
                TC.Concat -> eConcat e
                TC.BitwiseComplement -> bitNot e

    TC.TCBinOp op v1 v2 _ ->
      do e1 <- fromExpr v1
         e2 <- fromExpr v2
         pure case op of
                TC.Add -> add e1 e2
                TC.Sub -> sub e1 e2
                TC.Mul -> mul e1 e2
                TC.Div -> eDiv e1 e2
                TC.Mod -> eMod e1 e2

                TC.Lt     -> e1 `lt` e2
                TC.Leq    -> e1 `leq` e2
                TC.Eq     -> e1 `eq` e2
                TC.NotEq  -> e1 `notEq` e2

                TC.Cat          -> cat e1 e2
                TC.LCat         -> lCat e1 e2
                TC.LShift       -> lShift e1 e2
                TC.RShift       -> rShift e1 e2
                TC.BitwiseAnd   -> bitAnd e1 e2
                TC.BitwiseOr    -> bitOr e1 e2
                TC.BitwiseXor   -> bitXor e1 e2

                TC.ArrayStream  -> arrayStream e1 e2


    TC.TCTriOp op v1 v2 v3 _ ->
      do e1 <- fromExpr v1
         e2 <- fromExpr v2
         e3 <- fromExpr v3
         pure case op of
                TC.RangeUp    -> rangeUp e1 e2 e3
                TC.RangeDown  -> rangeDown e1 e2 e3

    TC.TCFor lp -> doLoop lp


doLoop :: TC.Loop a TC.Value -> M Expr
doLoop lp =
  do keyVar <- traverse fromName (TC.loopKName lp)
     elVar  <- fromName (TC.loopElName lp)
     colE   <- fromExpr (TC.loopCol lp)

     colT   <- fromTypeM $ TC.typeOf (TC.loopCol lp)

     let doBody = withSourceLocal elVar
                $ maybe id withSourceLocal keyVar
                $ fromExpr $ TC.loopBody lp

     case TC.loopFlav lp of

       TC.Fold x s ->
         do sVar     <- fromName x
            initS    <- fromExpr s
            g        <- withSourceLocal sVar doBody
            let free = freeVars g
            ty       <- fromTypeM $ TC.loopType lp
            foldLoop
              colT ty (Set.toList free)
              (snd sVar) initS
              (snd <$> keyVar) (snd elVar) colE
              \_ -> g

       TC.LoopMap ->
         fromTypeM (TC.loopType lp) >>= \resT ->
         case resT of

           TArray elTy ->
             do sVar <- newLocal (TBuilder elTy)
                g <- doBody
                let free = freeVars g
                let ty = TBuilder elTy
                step1 <- foldLoop
                           colT ty (Set.toList free)
                           sVar (newBuilder elTy)
                           (snd <$> keyVar) (snd elVar) colE
                           \_ -> consBuilder g (Var sVar)

                pure (finishBuilder step1)

           ty@(TMap tk tv) ->
             do sVar <- newLocal ty
                g <- doBody
                let free = freeVars g
                foldLoop colT ty (Set.toList free)
                  sVar (mapEmpty tk tv)
                  (snd <$> keyVar) (snd elVar) colE
                  \i -> mapInsert (Var sVar) (iteratorKey i) g


           _ -> panic "doLoopG/MapLoop" ["Unexpected result type"]



foldLoop ::
  Type               {- ^ Collection type -} ->
  Type               {- ^ Result type -} ->
  [Name]             {- ^ Free vars -} ->
  Name               {- ^ State var name -} ->
  Expr               {- ^ Initial state -} ->
  Maybe Name         {- ^ Key var, if any -} ->
  Name               {- ^ Element var -} ->
  Expr               {- ^ Collection to fold -} ->
  (Expr -> Expr)     {- ^ Loop body, parameterized by iterator -} ->
  M Expr
foldLoop colT ty vs0 sVar initS keyVar elVar colE g =
  do let vs  = vs0 \\ (maybeToList keyVar ++ [sVar,elVar])
         es  = map Var vs
         maybeAddKey e = case keyVar of
                           Nothing -> id
                           Just k  -> PureLet k e

     f <- newFName' Nothing ty
     i <- newLocal (TIterator colT)
     defFunF f (sVar : i : vs)
         $ eIf
             (iteratorDone (Var i))
             (Var sVar)
         $   PureLet elVar (iteratorVal (Var i))
         $   maybeAddKey   (iteratorNext (Var i))
         $   callF f (g (Var i) : iteratorNext (Var i) : es)

     pure $ callF f (initS : newIterator colE : es)




--------------------------------------------------------------------------------

data TEnv = TEnv
  { tNumVars :: Map TC.TVar TParam
  , tVars    :: Map TC.TVar TParam
  , tCons    :: Map TC.TCTyName TName
  }

lkpTCon :: TEnv -> TC.TCTyName -> TName
lkpTCon env x =
  case Map.lookup x (tCons env) of
    Just y -> y
    Nothing -> panic "lkpTCon" ["Undefined type constructor: " ++ show x ]

fromGTypeM :: TC.Type -> M Type
fromGTypeM ty =
  do te <- getTEnv
     let ?tenv = te
     pure (fromGType ty)

fromTypeM :: TC.Type -> M Type
fromTypeM ty =
  do te <- getTEnv
     let ?tenv = te
     pure (fromType ty)

userTypeM :: TC.Type -> M UserType
userTypeM ty =
  do te <- getTEnv
     let ?tenv = te
     pure (userType ty)


fromGType :: (?tenv :: TEnv) => TC.Type -> Type
fromGType ty =
  case ty of
    TC.Type (TC.TGrammar t) -> fromType t
    _ -> panic "fromGType" [ "Not a grammar" ]


fromType :: (?tenv :: TEnv) => TC.Type -> Type
fromType ty =
  case ty of
    TC.Type ty1 ->
      case ty1 of
        TC.TGrammar {} -> panic "fromType'" [ "Unexpected TGrammar" ]
        TC.TFun {}     -> panic "fromType;" [ "Unexpected function" ]
        TC.TStream     -> TStream
        TC.TByteClass  -> panic "fromtType" [ "Unexpected ByteClass" ]
        TC.TNum {}     -> panic "fromType'" [ "Unexpected TNum" ]
        TC.TUInt t     -> TUInt (fromNumType t)
        TC.TSInt t     -> TSInt (fromNumType t)
        TC.TInteger    -> TInteger
        TC.TBool       -> TBool
        TC.TUnit       -> TUnit
        TC.TArray t    -> TArray (fromType t)
        TC.TMaybe t    -> TMaybe (fromType t)
        TC.TMap k v    -> TMap (fromType k) (fromType v)

    TC.TCon {} -> TUser (userType ty)
    TC.TVar x ->
      case Map.lookup x (tVars ?tenv) of
        Just t -> TParam t
        _ -> error "fromType'" [ "Unexpected type variable: " ++ show x ]

fromNumType :: (?tenv :: TEnv) => TC.Type -> SizeType
fromNumType ty =
  case ty of

    TC.TVar x ->
      case Map.lookup x (tNumVars ?tenv) of
        Just i -> TSizeParam i
        Nothing -> panic "fromNumType" [ "Missing type variable" ]

    TC.Type (TC.TNum x) -> TSize x
    _ -> panic "fromNumType" [ "Not a numeric type" ]

userType :: (?tenv :: TEnv) => TC.Type -> UserType
userType ty =
  case ty of
    TC.TCon c tys -> go [] [] tys
      where
      go ts nts todo =
        case todo of
          [] -> UserType { utName = lkpTCon ?tenv c
                         , utNumArgs = reverse nts
                         , utTyArgs  = reverse ts
                         }

          t : more ->
            case TC.kindOf t of
              TC.KValue  -> go (fromType t : ts) nts more
              TC.KNumber -> go ts (fromNumType t : nts) more
              k -> error "userType" ["Unexpecte paramtere kind: " ++ show k]

    _ -> panic "userType" ["Not a type constructor"]





--------------------------------------------------------------------------------
-- Type declaraionts

fromTCTyDeclRec :: Rec TC.TCTyDecl -> M (Rec TDecl)
fromTCTyDeclRec r =
  case r of
    MutRec ds -> MutRec <$> mapM fromTCTyDecl ds
    NonRec d  -> NonRec <$> fromTCTyDecl d

fromTCTyDecl :: TC.TCTyDecl -> M TDecl
fromTCTyDecl td =
  do te <- getTEnv
     let (ixs,txs) = partitionEithers (map tparam (TC.tctyParams td))
         is = [ TP n | n <- zipWith const [0..] ixs ]
         isN = length is
         ts = [ TP (isN + n) | n <- zipWith const [0..] txs ]
         f  = let ?tenv = te { tVars    = Map.fromList (zip txs ts)
                             , tNumVars = Map.fromList (zip ixs is)
                             }
              in fromTCTyDef (TC.tctyDef td)
     pure TDecl { tName = lkpTCon te (TC.tctyName td)
                , tTParamKNumber = is
                , tTParamKValue  = ts
                , tDef           = f
                }
  where
  tparam x = case TC.tvarKind x of
               TC.KValue -> Right x
               TC.KNumber -> Left x
               k -> panic "fromTCTyDecl" [ "Unexpected kind param: " ++ show k ]


fromTCTyDef :: (?tenv :: TEnv) => TC.TCTyDef -> TDef
fromTCTyDef tdef =
  case tdef of
    TC.TCTyStruct fs -> TStruct (map field fs)
    TC.TCTyUnion fs  -> TUnion  (map field fs)
  where
  field (l,t) = (l, fromType t)




--------------------------------------------------------------------------------
-- Names

-- | Generate a new local binder
fromName :: TC.TCName TC.Value -> M (TC.TCName TC.Value, Name)
fromName x =
  do let lab = case TC.nameScope (TC.tcName x) of
                 TC.Local i -> i
                 _ -> panic "fromName" ["Not a local name"]
     n <- newName (Just lab) =<< fromTypeM (TC.typeOf x)
     pure (x,n)

-- | Resolve a top-level name
fromFName :: TC.TCName TC.Value -> M FName
fromFName x = topName (TC.tcName x)

-- | Resolve a top-level name
fromCName :: TC.TCName TC.Class -> M FName
fromCName x = topName (TC.tcName x)

-- | Resolve a top-level name
fromGName :: TC.TCName TC.Grammar -> M FName
fromGName x = topName (TC.tcName x)


-- | Add translated name
fromFDefName :: TC.Name -> TC.Type -> M ()
fromFDefName x t =
  do let lab = case TC.nameScope x of
                 TC.ModScope _ i -> i
                 _ -> panic "fromGDefName" ["Not a top-level name"]
     addTopName x =<< newFName' (Just lab) =<< fromTypeM t


-- | Add translated name
fromCDefName :: TC.Name -> M ()
fromCDefName x =
  do let lab = case TC.nameScope x of
                 TC.ModScope _ i -> i
                 _ -> panic "fromCDefName" ["Not a top-level name"]
     addTopName x =<< newFName' (Just lab) TBool


-- | Add translated name
fromGDefName :: TC.Name -> TC.Type -> M ()
fromGDefName x t =
  do let lab = case TC.nameScope x of
                 TC.ModScope _ i -> i
                 _ -> panic "fromGDefName" ["Not a top-level name"]
     addTopName x =<< newFName' (Just lab) =<< fromGTypeM t


fromArg :: TC.Arg a -> M Expr
fromArg a =
  case a of
    TC.ValArg e -> fromExpr e
    _           -> panic "fromArg" [ "Unexpected function argument" ]

fromParam :: TC.Param -> M (TC.TCName TC.Value, Name)
fromParam p =
  case p of
    TC.ValParam x -> fromName x
    TC.ClassParam _ -> panic "fromParam" [ "Unexpected class parameter" ]
    TC.GrammarParam _ -> panic "fromParam" [ "Unexpected grammar parameter" ]


fromManybeBound :: TC.ManyBounds (TC.TC a TC.Value) -> M (TC.ManyBounds Expr)
fromManybeBound bnds =
  case bnds of
    TC.Exactly e   -> TC.Exactly <$> fromExpr e
    TC.Between a b -> TC.Between <$> traverse fromExpr a
                                 <*> traverse fromExpr b





--------------------------------------------------------------------------------
-- Utilities

tByte :: Type
tByte = TUInt (TSize 8)

resTy :: WithSem -> Type -> Type
resTy sem ty = case sem of
                 NoSem  -> TUnit
                 YesSem -> ty

result :: WithSem -> Expr -> Expr
result sem val = case sem of
                   NoSem -> unit
                   YesSem -> val

orOp :: Commit -> Grammar -> Grammar -> Grammar
orOp cmt = case cmt of
             Commit    -> OrBiased
             Backtrack -> OrUnbiased

fromMb :: WithSem -> Type -> Expr -> M Grammar
fromMb sem t e =
  case sem of
    NoSem -> pure $ If (eIsJust e) (Pure unit) (nope TUnit)
    YesSem ->
      do x <- newLocal (TMaybe t)
         let xe = Var x
         pure $ Let x e
              $ If (eIsJust xe) (Pure (eFromJust xe)) (nope t)
  where
  nope ty = sysErr ty "unexpected `nothing`"

--------------------------------------------------------------------------------
-- Translation monad

newtype M a = M (R -> S -> (a,S))

data R = R
  { sourceLocals :: Map (TC.TCName TC.Value) Name
  , curMod       :: MName
  }

data S = S
  { localName :: Int
  , globName  :: Int
  , tname     :: Int
  , newFFuns  :: [Fun Expr]
  , newGFuns  :: [Fun Grammar]

    -- These don't change during the translation,
    -- we only modify them once at the beginning of a module.
    -- It is conveninet to keep them in the state, if we want to process
    -- multiple modules at once.
  , topNames  :: Map TC.Name FName
  , topTNames :: Map TC.TCTyName TName
  }


instance Functor M where
  fmap = liftM

instance Applicative M where
  pure a = M \_ s -> (a,s)
  (<*>)  = ap

instance Monad M where
  M m >>= f = M \r s -> let (a,s1) = m r s
                            M m1 = f a
                        in m1 r s1


runM :: Map TC.TCTyName TName -> Map TC.Name FName -> M a ->
        (a, (Map TC.TCTyName TName, Map TC.Name FName))
runM topT topN (M m) = (a, (topTNames s, topNames s))
  where
  (a,s) = m r0 s0

  r0 = R { sourceLocals = Map.empty
         , curMod       = MName "(no module)"
         }

  s0 = S { localName  = 0
         , globName   = 0
         , tname      = 0
         , newFFuns   = []
         , newGFuns   = []
         , topNames   = topN
         , topTNames  = topT
         }

--------------------------------------------------------------------------------
-- The current module
withModuleName :: TC.ModuleName -> M a -> M a
withModuleName n (M m) = M \r s -> m r { curMod = MName n } s

getCurModule :: M MName
getCurModule = M \r s -> (curMod r, s)




--------------------------------------------------------------------------------
-- Type Names

newTName :: TC.TCTyName -> M ()
newTName nm = M \r s ->
  let n = tname s
      (l,anon) = case nm of
                   TC.TCTy a -> (a, Nothing)
                   TC.TCTyAnon a i -> (a, Just i)
      x = TName { tnameId = n
                , tnameText = case TC.nameScope l of
                                TC.ModScope _ txt -> txt
                                _ -> panic "newTName" [ "Not a ModScope" ]
                , tnameAnon = anon
                , tnameMod = curMod r
                }
  in ((), s { tname = tname s + 1
            , topTNames = Map.insert nm x (topTNames s)
            })

-- | Type environemnt during translation of declaraionts.
-- There should be not type variables.
getTEnv :: M TEnv
getTEnv = M \_ s -> (TEnv { tVars     = Map.empty
                          , tNumVars  = Map.empty
                          , tCons     = topTNames s
                          }, s)



--------------------------------------------------------------------------------
-- Local names


newName :: Maybe Text -> Type -> M Name
newName mb t = M \_ s ->
  let n = localName s
      x = Name { nameId = n, nameType = t, nameText = mb }
  in (x, s { localName = localName s + 1 })


-- | Make up a new local name
newLocal :: Type -> M Name
newLocal = newName Nothing


-- | Add a local varialble from the source (i.e., not newly generate)
withSourceLocal :: (TC.TCName TC.Value, Name) -> M a -> M a
withSourceLocal (x,n) (M m) =
  M \r s -> m r { sourceLocals = Map.insert x n (sourceLocals r) } s

-- | Add mulitple local variable from the source.
withSourceLocals :: [(TC.TCName TC.Value,Name)] -> M a -> M a
withSourceLocals xs m = foldr withSourceLocal m xs

-- | Resolve a local name.
sourceLocal :: TC.TCName TC.Value -> M Name
sourceLocal x = M \r s ->
  case Map.lookup x (sourceLocals r) of
    Just v  -> (v,s)
    Nothing -> panic "sourceLocal" ["Missing source local: " ++ show x ]



--------------------------------------------------------------------------------
-- Resolving top-level names

-- | Resolve a top-level name
topName :: TC.Name -> M FName
topName x = M \_ s ->
  case Map.lookup x (topNames s) of
    Just v -> (v,s)
    Nothing -> error "topNames" ["Missing top name: " ++ show x ]

scopedIdent :: TC.ScopedIdent -> M FName
scopedIdent n = M \_ s ->
  case [ r | (x,r) <- Map.toList (topNames s), TC.nameScope x == n ] of
    [ f ] -> (f,s)
    _ -> error "scopedIdent" ["Missing entry: " ++ show n ]

addTopName :: TC.Name -> FName -> M ()
addTopName x f = M \_ s -> ((), s { topNames = Map.insert x f (topNames s) })

newGName :: Type -> M FName
newGName = newFName' Nothing

newFName' :: Maybe Text -> Type -> M FName
newFName' mb ty = M \r s ->
  let n = globName s
      x = FName { fnameId = n
                , fnameType = ty
                , fnameText = mb
                , fnameMod = curMod r
                }
  in (x, s { globName = globName s + 1 })





--------------------------------------------------------------------------------
-- Definint new functions

defFunG :: FName -> [Name] -> Grammar -> M ()
defFunG  f xs e =
  M \_ s -> ((), s { newGFuns = Fun { fName = f, fParams = xs, fDef = Def e }
                              : newGFuns s })

defFunF :: FName -> [Name] -> Expr -> M ()
defFunF f xs e =
  M \_ s -> ((), s { newFFuns = Fun { fName = f, fParams = xs, fDef = Def e }
                              : newFFuns s })


removeNewFuns :: M ([Fun Expr], [Fun Grammar])
removeNewFuns = M \_ s -> ( (newFFuns s, newGFuns s)
                          , s { newFFuns = [], newGFuns = [] }
                          )



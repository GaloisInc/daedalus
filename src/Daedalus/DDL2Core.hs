{-# Language BlockArguments #-}
{-# Language GADTs #-}
{-# Language ImplicitParams #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Daedalus.DDL2Core where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(maybeToList)
import Data.List((\\))
import Data.Either(partitionEithers)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import MonadLib

import Daedalus.PP hiding (cat)
import Daedalus.Panic(panic)
import Daedalus.GUID(invalidGUID)

import Daedalus.Pass(PassM)
import qualified Daedalus.Type.AST as TC
import Daedalus.Type.AST (Commit(..), WithSem(..))
import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.Type(typeOf,sizeType)


--------------------------------------------------------------------------------


fromModule :: TC.TCModule a -> M Module
fromModule mo =
  fromDecls (TC.tcModuleName mo) (TC.tcModuleTypes mo) (TC.tcModuleDecls mo)

fromDecls ::
  TC.ModuleName ->
  [ Rec TC.TCTyDecl ] ->
  [ Rec (TC.TCDecl a) ] ->
  M Module
fromDecls mo tdecls decls =
  withModuleName mo $
  do mapM_ newTNameRec tdecls
     tds <- mapM fromTCTyDeclRec tdecls

     let ds = concatMap recToList decls
     mapM_ addDeclName ds
     (dffs,dbfs,dgfs) <- splitSomeFuns <$> mapM fromDecl ds
     (effs,egfs) <- removeNewFuns

     m <- getCurModule
     pure
       Module { mName   = m
              , mImports = []
              , mTypes  = tds
              , mFFuns  = effs ++ dffs
              , mBFuns  = dbfs
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

data SomeFun = FE (Fun Expr) | FB (Fun ByteSet) | FG (Fun Grammar)

splitSomeFuns :: [SomeFun] -> ([Fun Expr], [Fun ByteSet], [Fun Grammar])
splitSomeFuns fs = ( [ x | FE x <- fs ]
                   , [ x | FB x <- fs ]
                   , [ x | FG x <- fs ]
                   )

fromDecl :: TC.TCDecl a -> M SomeFun
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
                  pure $ FE Fun { fName = f, fParams = xs, fDef = External }

                TC.Defined v ->
                   do e <- fromExpr v
                      pure $ FE Fun { fName = f, fParams = xs, fDef = Def e }

           TC.AClass ->
             case tcDeclDef of
               TC.ExternDecl _ ->
                 pure $ FB Fun { fName = f, fParams = xs, fDef = External }

               TC.Defined v ->
                 do e <- fromClass v
                    pure $ FB Fun { fName = f, fParams = xs, fDef = Def e }

           TC.AGrammar ->
             case tcDeclDef of

               TC.ExternDecl _ ->
                 pure $ FG Fun { fName = f, fParams = xs, fDef = External }

               TC.Defined v ->
                 do e <- fromGrammar v
                    pure $ FG Fun { fName = f, fParams = xs, fDef = Def e }


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
      pure (Match (fromSem sem) (MatchByte SetAny))

    TC.TCMatch sem c ->
      do p <- fromClass c
         pure (Match (fromSem sem) (MatchByte p))

    TC.TCGuard e ->
      do v <- fromExpr e
         pure $ gIf v (Pure unit) (sysErr TUnit "guard failed")

    TC.TCMatchBytes sem e ->
      do e' <- fromExpr e
         pure (Match (fromSem sem) (MatchBytes e'))

    TC.TCChoice cmt opts ty ->
      case opts of
        [] -> fromTypeM ty >>= \t -> pure (sysErr t "empty choice")
        _  -> foldr1 (orOp cmt) <$> mapM fromGrammar opts


    TC.TCOptional cmt g ->
      do ty <- fromGTypeM (TC.typeOf g)
         x <- newLocal ty
         ge <- fromGrammar g
         let lhs = Do x ge
                 $    Pure (just (Var x))
         pure (orOp cmt lhs (Pure (nothing ty)))


    TC.TCEnd ->
      do x <- newLocal TStream
         pure $ Do x GetStream
              $    gIf (isEmptyStream (Var x))
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
           NoSem -> pure $ gIf (lenE `leq` streamLen strE)
                              (Pure unit)
                              (sysErr TUnit "unexpected end of input")


           YesSem -> do len <- newLocal sizeType
                        str <- newLocal TStream
                        pure $ Let len lenE
                             $ Let str strE
                             $ gIf (Var len `leq` streamLen (Var str))
                                  (Pure $ eTake (Var len) (Var str))
                                  (sysErr TUnit "unexpected end of input")

    TC.TCStreamOff sem n s ->
      do lenE <- fromExpr n
         strE <- fromExpr s
         case sem of
           NoSem -> pure $ gIf (lenE `leq` streamLen strE)
                              (Pure unit)
                              (sysErr TUnit "unexpected end of input")
           YesSem ->
             do len <- newLocal sizeType
                str <- newLocal TStream
                pure $ Let len lenE
                     $ Let str strE
                     $ gIf (Var len `leq` streamLen (Var str))
                          (Pure $ eDrop (Var len) (Var str))
                          (sysErr TUnit "unexpected end of input")


    TC.TCMany sem cmt bnd g ->
      do ge <- fromGrammar g
         ty   <- fromGTypeM (TC.typeOf g)
         cbnd <- fromManybeBound bnd
         let vs = Set.toList (freeVars ge)

         case cbnd of
           TC.Exactly e ->
              do x <- newLocal sizeType
                 let xe = Var x
                     vs1 = x : vs

                 Let x e <$>
                   case sem of
                     NoSem  -> checkAtLeast Nothing xe =<<
                                            pSkipAtMost cmt vs1 (Just xe) ge
                     YesSem -> checkAtLeast (Just ty) xe
                                       =<< pParseAtMost cmt ty vs1 xe ge

           TC.Between Nothing Nothing ->
              case sem of
                NoSem  -> pSkipMany cmt vs ge
                YesSem -> pParseMany cmt ty vs ge

           TC.Between Nothing (Just e) ->
              case sem of
                NoSem  -> pSkipAtMost cmt vs (Just e) ge
                YesSem -> pParseAtMost cmt ty vs e ge

           TC.Between (Just e) Nothing ->
              case sem of
                NoSem  -> checkAtLeast Nothing e =<< pSkipAtMost cmt vs Nothing ge
                YesSem -> checkAtLeast (Just ty) e =<< pParseMany cmt ty vs ge

           TC.Between (Just lb) (Just ub) ->
              case sem of
                NoSem  -> checkAtLeast Nothing lb =<< pSkipAtMost cmt vs (Just ub) ge
                YesSem -> checkAtLeast (Just ty) lb =<< pParseAtMost cmt ty vs ub ge

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
                NoSem  -> gIf (mapMember mpE kE)
                             (sysErr TUnit "duplicate key in map")
                             (Pure unit)
                YesSem ->
                  gIf (mapMember mpE kE)
                     (sysErr ty "duplicate key in map")
                     (Pure (mapInsert mpE kE vE))

    TC.TCArrayIndex sem arr ix ->
      do aE <- fromExpr arr
         iE <- fromExpr ix
         case sem of
           NoSem -> pure $ gIf (arrayLen aE `leq` iE)
                              (sysErr TUnit "array index out of bounds")
                              (Pure unit)
           YesSem ->
             do ty0 <- fromTypeM (TC.typeOf arr)
                let ty = case ty0 of
                           TArray t -> t
                           _ -> panic "fromGrammar"
                                   [ "TCArrayIndex: not an array" ]
                a <- newLocal sizeType
                i <- newLocal sizeType
                pure $ Let a aE
                     $ Let i iE
                     $ gIf (arrayLen (Var a) `leq` Var i)
                          (sysErr ty "array index out of bounds")
                          (Pure (arrayIndex (Var a) (Var i)))

    TC.TCVar x -> panic "fromGrammar"
                  [ "Unexpected grammar variable: " ++ show x ]

    TC.TCCoerceCheck sem _t1 t2 v ->
      do e   <- fromExpr v
         tgt <- fromTypeM t2
         fromMb sem tgt (coerceMaybeTo tgt e)

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


    TC.TCCase e as dflt ->
      do t  <- fromGTypeM (TC.typeOf gram)
         ms <- mapM (doAlt fromGrammar) as
         mbase <- case dflt of
                    Nothing -> pure Failure
                    Just d  -> Success Nothing <$> fromGrammar d
         let match = foldr biasedOr mbase ms
         matchToGrammar t match <$> fromExpr e



fromSem :: WithSem -> Sem
fromSem sem = case sem of
                NoSem -> SemNo
                YesSem -> SemYes

--------------------------------------------------------------------------------
-- Pattern Matching

data PMatch k =
    Success (Maybe Name) k
  | Failure
  | IfPat Pattern (Maybe Type) (PMatch k) (PMatch k)
    -- ^ If then pattern succeeds, use the first match otherwise try the second.
    -- The `maybe type` indicates if we have a nested patern (Just) or not.
    -- If we have a nested pattern, the "then" 'Match' will examine it.
    -- The type is for the nested value

dumpMatch :: Show a => PMatch a -> Doc
dumpMatch match =
  case match of
    Success _ k -> text (show k)
    Failure -> "FAIL"
    IfPat p _ m1 m2 ->
      "if" <+> pp p $$ nest 2 ("then" <+> dumpMatch m1)
                    $$ "else" <+> dumpMatch m2

patMatch :: TC.TCPat -> (src -> M tgt) -> src -> M (PMatch tgt)
patMatch pat leaf k =
  case pat of
     TC.TCConPat _ l p   -> nested (PCon l) p
     TC.TCNumPat _ x     -> terminal (PNum x)
     TC.TCBoolPat b      -> terminal (PBool b)
     TC.TCJustPat p      -> nested PJust p
     TC.TCNothingPat {}  -> terminal PNothing
     TC.TCWildPat {}     -> success
     TC.TCVarPat x ->
       do x' <- fromName x
          g  <- withSourceLocal x' (leaf k)
          pure (Success (Just (snd x')) g)
  where
  failure     = pure Failure
  terminal p  = IfPat p Nothing <$> success <*> failure
  nested p q  = IfPat p <$> (Just <$> fromTypeM (TC.typeOf q))
                         <*> patMatch q leaf k
                         <*> failure

  success =
    do g <- leaf k
       pure (Success Nothing g)


-- XXX: for now we duplicate alternatives
doAlt :: (TC.TC a k -> M tgt) -> TC.TCAlt a k -> M (PMatch tgt)
doAlt eval (TC.TCAlt ps k) =
  do ms <- sequence [ patMatch pat eval k | pat <- ps ]
     pure (foldr1 matchOr ms)



-- union of two patterns
matchOr :: PMatch k -> PMatch k -> PMatch k
matchOr m1 m2 =
  case m1 of
    Success {} -> m1
    Failure    -> m2
    IfPat p1 pt pNest pOr ->
      case m2 of
        Success {} -> m2
        Failure    -> m1
        IfPat q1 qt qNest qOr ->
          case compare p1 q1 of
            EQ -> IfPat p1 pt (matchOr pNest qNest) (matchOr pOr qOr)
            LT -> IfPat p1 pt pNest (matchOr pOr m2)
            GT -> IfPat q1 qt qNest (matchOr m1 qOr)


-- match left, if that fails, match right
biasedOr :: PMatch k -> PMatch k -> PMatch k
biasedOr m1 m2 =
  case m1 of
    Success {} -> m1
    Failure    -> m2
    IfPat p1 pt pNest pOr ->
      case m2 of
        Failure -> m1
        -- NOTE: duplicates `m2`
        Success {} -> IfPat p1 pt (biasedOr pNest m2) (biasedOr pOr m2)
        IfPat q1 qt qNest qOr ->
          case compare p1 q1 of
            EQ -> IfPat p1 pt (biasedOr pNest qNest) (biasedOr pOr qOr)
            LT -> IfPat p1 pt pNest (biasedOr pOr m2)
            GT -> IfPat q1 qt qNest (biasedOr m1 qOr)


matchToGrammar :: Type -> PMatch Grammar -> Expr -> Grammar
matchToGrammar t match e =
  case match of
    Failure -> pFail
    Success mb g ->
      case mb of
        Nothing -> g
        Just x  -> Let x e g
    IfPat {} -> gCase e $ completeAlts pFail
                        $ matchToAlts (matchToGrammar t) match e
  where
  pFail = sysErr t "Pattern match failure"

matchToByteSet :: PMatch ByteSet -> Expr -> ByteSet
matchToByteSet match e =
  case match of
    Failure -> SetComplement SetAny
    Success mb k ->
      case mb of
        Nothing -> k
        Just x -> SetLet x e k
    IfPat {} -> bCase e (matchToAlts matchToByteSet match e)

matchToExpr :: PMatch Expr -> Expr -> Expr
matchToExpr match e =
  case match of
    Failure -> panic "matchToExpr" [ "empty case" ]
    Success mb k ->
      case mb of
       Nothing -> k
       Just x  -> PureLet x e k
    IfPat {} -> eCase e (matchToAlts matchToExpr match e)

matchToAlts ::
  (PMatch k -> Expr -> k) ->
  PMatch k ->
  Expr -> [(Pattern,k)]
matchToAlts mExpr match e =
  case match of
    Failure -> []
    Success {} -> [(PAny, mExpr match e)]
    IfPat p nestP yes no -> (p, mExpr yes nested) : matchToAlts mExpr no e
      where
      nested = case nestP of
                 Nothing -> e
                 Just t  ->
                   case p of
                     PCon l   -> fromUnion t l e
                     PJust    -> eFromJust e
                     PBool {} -> bad
                     PNothing -> bad
                     PNum {}  -> bad
                     PAny     -> bad
      bad = panic "matchToAlts" ["Unexpected nested pattern"]

completeAlts :: k -> [(Pattern,k)] -> [(Pattern,k)]
completeAlts d ps0 =
  case ps0 of
    [] -> [(PAny,d)]
    this@(p,k) : more ->
      case p of
        PAny          -> [(p,k)]
        PBool b       -> this : go [PBool (not b)] more
        PNothing      -> this : go [PJust] more
        PJust         -> this : go [PNothing] more
        PNum {}       -> ps0 ++ [(PAny,d)]
        PCon {}       -> ps0 ++ [(PAny,d)] -- XXX: could check that we have all
  where
  go need ps =
    case ps of
      [] -> [ (p,d) | p <- need ]
      (p,k) : more
        | (as,_:bs) <- break (== p) need -> (p,k) : go (as ++ bs) more
        | otherwise                      ->         go need       more

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
         $ gIf (iteratorDone (Var i))
              (Pure (Var sVar))
         $    Let elVar (iteratorVal (Var i))
         $    maybeAddKey (iteratorKey (Var i))
         $    Do nextS (g (Var i))
         $       Call f (Var nextS : iteratorNext (Var i) : es)
     pure $ Call f (initS : newIterator colE : es)


maybeSkip :: Commit -> Grammar -> Grammar -> Grammar -> M Grammar
maybeSkip cmt p yes no =
  do r <- newLocal TBool
     pure case cmt of
            Commit ->
              Do r (OrBiased (Do_ p (Pure (boolL True))) (Pure (boolL False)))
               $ GCase $ Case (Var r)
                           [ (PBool True, yes)
                           , (PBool False, no)
                           ]
            _ -> orOp cmt (Do_ p yes) no


maybeParse ::
  Commit -> Type -> Grammar -> (Expr -> Grammar) -> Grammar -> M Grammar
maybeParse cmt ty p yes no =
  do r   <- newLocal ty
     rMb <- newLocal (TMaybe ty)
     pure case cmt of
            Commit ->
               Do rMb (OrBiased (Do r p (Pure (just (Var r))))
                                (Pure (nothing ty)))
                $ GCase
                $ Case (Var rMb)
                    [ (PJust,    yes (eFromJust (Var rMb)))
                    , (PNothing, no)
                    ]
            _ ->
             orOp cmt (Do r p (yes (Var r))) no




pSkipMany :: Commit -> [Name] -> Grammar -> M Grammar
pSkipMany cmt vs p =
  do f <- newGName TUnit
     let es = map Var vs
     skipBody <- maybeSkip cmt p (Call f es) (Pure unit)
     defFunG f vs skipBody
     pure (Call f es)

pParseMany :: Commit -> Type -> [Name] -> Grammar -> M Grammar
pParseMany cmt ty vs p =
  do f <- newGName (TBuilder ty)
     let es = map Var vs
     x <- newLocal (TBuilder ty)
     let xe = Var x
     body <- maybeParse cmt ty p
                              (\a -> Call f (consBuilder a xe : es)) (Pure xe)
     defFunG f (x:vs) body
     z <- newLocal (TBuilder ty)
     pure $ Do z (Call f (newBuilder ty : es))
                 (Pure $ finishBuilder $ Var z)


pSkipAtMost :: Commit -> [Name] -> Maybe Expr -> Grammar -> M Grammar
pSkipAtMost cmt vs mbTgt p =
  do f <- newGName sizeType
     let es = map Var vs
     x <- newLocal sizeType
     let xe = Var x
     skipBody <- maybeSkip cmt p (Call f (add xe (intL 1 sizeType) : es))
                                 (Pure xe)
     defFunG f (x:vs)
        case mbTgt of
          Nothing  -> skipBody
          Just tgt -> gIf (xe `lt` tgt) skipBody (Pure xe)
     pure (Call f (intL 0 sizeType: es))


pParseAtMost :: Commit -> Type -> [Name] -> Expr -> Grammar -> M Grammar
pParseAtMost cmt ty vs tgt p =
  do f <- newGName (TArray ty)
     let es = map Var vs
     x <- newLocal sizeType
     b <- newLocal (TBuilder ty)
     let xe = Var x
         be = Var b

     body <- maybeParse cmt ty p
                (\a -> Call f (add xe (intL 1 sizeType)
                              : consBuilder a be
                              : es))
                (Pure (finishBuilder be))

     defFunG f (x : b : vs) (gIf (xe `lt` tgt) body (Pure (finishBuilder be)))
     pure $ Call f (intL 0 sizeType : newBuilder ty : es)


-- | Use to check the lower bounds for `many`
checkAtLeast :: Maybe Type -> Expr -> Grammar -> M Grammar
checkAtLeast mb tgt g =
  case mb of
    Nothing ->
      do x <- newLocal sizeType
         pure $ Do x g $ gIf (Var x `lt` tgt)
                            (nope TUnit)
                            (Pure unit)
    Just ty ->
      do x <- newLocal (TArray ty)
         pure $ Do x g $ gIf (arrayLen (Var x) `lt` tgt) (nope (TArray ty))
                                                        (Pure (Var x))

  where
  nope ty = sysErr ty "insufficient element occurances"


--------------------------------------------------------------------------------


fromClass :: TC.TC a TC.Class -> M ByteSet
fromClass cla =
  case TC.texprValue cla of
    TC.TCSetAny          -> pure SetAny
    TC.TCSetSingle v     -> SetSingle <$> fromExpr v
    TC.TCSetComplement c -> SetComplement <$> fromClass c
    TC.TCSetUnion cs     -> do es <- mapM fromClass cs
                               pure case es of
                                      [] -> SetComplement SetAny
                                      _  -> foldr1 SetUnion es

    TC.TCSetOneOf xs
      | BS.null xs -> pure (SetComplement SetAny)
      | otherwise  -> pure (foldr1 SetUnion (map fromByte (BS.unpack xs)))
        where fromByte b = SetSingle (byteL b)

    TC.TCSetDiff c1 c2 ->
      do e1 <- fromClass c1
         e2 <- fromClass c2
         pure (SetIntersection e1 (SetComplement e2))

    TC.TCSetRange v1 v2  -> SetRange <$> fromExpr v1 <*> fromExpr v2

    TC.TCCall f ts as ->
      case ts of
        [] ->
          do fc <- fromCName f
             es <- mapM fromArg as
             pure (SetCall fc es)

        _ -> panic "fromClass" ["Unexptect type parameters"]

    TC.TCCase e as dflt ->
      do ms <- mapM (doAlt fromClass) as
         match <- case dflt of
                    Nothing -> pure (foldr1 biasedOr ms)
                    Just d  ->
                      do base <- Success Nothing <$> fromClass d
                         pure (foldr biasedOr base ms)
         matchToByteSet match <$> fromExpr e

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

    TC.TCLiteral (TC.LNumber n) t ->
      intL n <$> fromTypeM t

    TC.TCLiteral (TC.LBool b) _ ->
      pure (boolL b)

    TC.TCNothing t ->
      nothing <$> fromTypeM t

    TC.TCJust e ->
      just <$> fromExpr e

    TC.TCLiteral (TC.LByte x) _ ->
      pure (intL (toInteger x) tByte)

    TC.TCUnit ->
      pure unit

    TC.TCStruct fs t ->
      Struct <$> userTypeM t <*> mapM field fs

        where field (l,v) = do e <- fromExpr v
                               pure (l,e)

    TC.TCLiteral (TC.LBytes bs) _ ->
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

    TC.TCVar x -> sourceLocal x

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

                -- XXX: Logic And and OR

    TC.TCTriOp op v1 v2 v3 _ ->
      do e1 <- fromExpr v1
         e2 <- fromExpr v2
         e3 <- fromExpr v3
         pure case op of
                TC.RangeUp    -> rangeUp e1 e2 e3
                TC.RangeDown  -> rangeDown e1 e2 e3

    TC.TCFor lp -> doLoop lp

    TC.TCCase e as dflt ->
      do ms <- mapM (doAlt fromExpr) as
         match <- case dflt of
                    Nothing -> pure (foldr1 biasedOr ms)
                    Just d  ->
                      do base <- Success Nothing <$> fromExpr d
                         pure (foldr biasedOr base ms)
         matchToExpr match <$> fromExpr e




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
         $   maybeAddKey   (iteratorKey (Var i))
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
  do let lab = case TC.nameScopedIdent (TC.tcName x) of
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
  do let lab = case TC.nameScopedIdent x of
                 TC.ModScope _ i -> i
                 _ -> panic "fromGDefName" ["Not a top-level name"]
     addTopName x =<< newFName' (Just lab) =<< fromTypeM t


-- | Add translated name
fromCDefName :: TC.Name -> M ()
fromCDefName x =
  do let lab = case TC.nameScopedIdent x of
                 TC.ModScope _ i -> i
                 _ -> panic "fromCDefName" ["Not a top-level name"]
     addTopName x =<< newFName' (Just lab) TBool


-- | Add translated name
fromGDefName :: TC.Name -> TC.Type -> M ()
fromGDefName x t =
  do let lab = case TC.nameScopedIdent x of
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

fromParamE :: TC.Param -> Expr -> (TC.TCName TC.Value, Expr)
fromParamE p e =
  case p of
    TC.ValParam x -> (x,e)
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
    NoSem -> pure $ gCase e [ (PNothing, nope TUnit)
                            , (PJust, Pure unit)
                            ]
    YesSem ->
      do x <- newLocal (TMaybe t)
         let xe = Var x
         pure $ Let x e
              $ gCase xe
                  [ (PNothing, nope t)
                  , (PJust, Pure (eFromJust xe))
                  ]
  where
  nope ty = sysErr ty "unexpected `nothing`"

--------------------------------------------------------------------------------
-- Translation monad

newtype M a = M (ReaderT R (StateT S PassM) a)
  deriving (Functor,Applicative,Monad)

data R = R
  { sourceLocals :: Map (TC.TCName TC.Value) Expr
  , curMod       :: MName
  }

data S = S
  { newFFuns  :: [Fun Expr]
  , newGFuns  :: [Fun Grammar]


    -- These don't change during the translation,
    -- we only modify them once at the beginning of a module.
    -- It is conveninet to keep them in the state, if we want to process
    -- multiple modules at once.
  , topNames  :: Map TC.Name FName
  , topTNames :: Map TC.TCTyName TName
  }


runToCore :: Map TC.TCTyName TName -> Map TC.Name FName -> M a ->
        PassM (a, (Map TC.TCTyName TName, Map TC.Name FName))
runToCore topT topN (M m) =
  do (a,s) <- runStateT s0 $ runReaderT r0 m
     pure (a, (topTNames s, topNames s))
  where
  r0 = R { sourceLocals = Map.empty
         , curMod       = MName "(no module)"
         }

  s0 = S { newFFuns   = []
         , newGFuns   = []
         , topNames   = topN
         , topTNames  = topT
         }

--------------------------------------------------------------------------------
-- The current module
withModuleName :: TC.ModuleName -> M a -> M a
withModuleName n (M m) = M (mapReader (\r -> r { curMod = MName n }) m)

getCurModule :: M MName
getCurModule = M (curMod <$> ask)




--------------------------------------------------------------------------------
-- Type Names

-- | Generate new names for these type declarations.
newTNameRec :: Rec TC.TCTyDecl -> M ()
newTNameRec rec =
  case rec of
    NonRec d  -> doOne False d
    MutRec ds -> mapM_ (doOne True) ds
  where
  doOne r d =
    let flavor = case TC.tctyDef d of
                   TC.TCTyStruct {} -> TFlavStruct
                   TC.TCTyUnion cs
                     | all ((== TC.tUnit) . snd) cs -> TFlavEnum (map fst cs)
                     | otherwise                    -> TFlavUnion (map fst cs)
    in newTName r flavor (TC.tctyName d)

newTName :: Bool -> TFlav -> TC.TCTyName -> M ()
newTName isRec flavor nm = M
  do r <- ask
     let (l,anon) = case nm of
                      TC.TCTy a -> (a, Nothing)
                      TC.TCTyAnon a i -> (a, Just i)
     x <- freshTName
            TName { tnameId = invalidGUID
                  , tnameText = case TC.nameScopedIdent l of
                                  TC.ModScope _ txt -> txt
                                  _ -> panic "newTName" [ "Not a ModScope" ]
                  , tnameAnon = anon
                  , tnameMod = curMod r
                  , tnameRec = isRec
                  , tnameFlav = flavor
                  }
     sets_ \s -> s { topTNames = Map.insert nm x (topTNames s) }


-- | Type environemnt during translation of declaraionts.
-- There should be not type variables.
getTEnv :: M TEnv
getTEnv = M
  do s <- get
     pure TEnv { tVars     = Map.empty
               , tNumVars  = Map.empty
               , tCons     = topTNames s
               }



--------------------------------------------------------------------------------
-- Local names


newName :: Maybe Text -> Type -> M Name
newName mb t =
  M $ freshName Name { nameId = invalidGUID, nameType = t, nameText = mb }

-- | Make up a new local name
newLocal :: Type -> M Name
newLocal = newName Nothing


-- | Add a local varialble from the source (i.e., not newly generate)
withSourceLocal :: (TC.TCName TC.Value, Name) -> M a -> M a
withSourceLocal (x,n) (M m) = M (mapReader upd m)
  where upd r = r { sourceLocals = Map.insert x (Var n) (sourceLocals r) }

-- | Add a local varialble from the source (i.e., not newly generate)
withSourceLocalE :: (TC.TCName TC.Value, Expr) -> M a -> M a
withSourceLocalE (x,n) (M m) = M (mapReader upd m)
  where upd r = r { sourceLocals = Map.insert x n (sourceLocals r) }


-- | Add mulitple local variable from the source.
withSourceLocals :: [(TC.TCName TC.Value,Name)] -> M a -> M a
withSourceLocals xs m = foldr withSourceLocal m xs

-- | Add mulitple local variable from the source.
withSourceLocalsE :: [(TC.TCName TC.Value,Expr)] -> M a -> M a
withSourceLocalsE xs m = foldr withSourceLocalE m xs



-- | Resolve a local name.
sourceLocal :: TC.TCName TC.Value -> M Expr
sourceLocal x = M
  do r <- ask
     case Map.lookup x (sourceLocals r) of
       Just v  -> pure v
       Nothing -> panic "sourceLocal" ["Missing source local: " ++ show x ]



--------------------------------------------------------------------------------
-- Resolving top-level names

-- | Resolve a top-level name
topName :: TC.Name -> M FName
topName x = M $ sets \s ->
  case Map.lookup x (topNames s) of
    Just v -> (v,s)
    Nothing -> error "topNames" ["Missing top name: " ++ show x ]

scopedIdent :: TC.ScopedIdent -> M FName
scopedIdent n = M $ sets \s ->
  case [ r | (x,r) <- Map.toList (topNames s), TC.nameScopedIdent x == n ] of
    [ f ] -> (f,s)
    _ -> error "scopedIdent" ["Missing entry: " ++ show n ]

addTopName :: TC.Name -> FName -> M ()
addTopName x f = M $ sets_ \s -> s { topNames = Map.insert x f (topNames s) }

newGName :: Type -> M FName
newGName = newFName' Nothing

newFName' :: Maybe Text -> Type -> M FName
newFName' mb ty = M
  do r <- ask
     freshFName
       FName { fnameId = invalidGUID
             , fnameType = ty
             , fnameText = mb
             , fnameMod = curMod r
             }




--------------------------------------------------------------------------------
-- Definint new functions

defFunG :: FName -> [Name] -> Grammar -> M ()
defFunG  f xs e =
  M $ sets_ \s -> s { newGFuns = Fun { fName = f, fParams = xs, fDef = Def e }
                               : newGFuns s }

defFunF :: FName -> [Name] -> Expr -> M ()
defFunF f xs e =
  M $ sets_ \s -> s { newFFuns = Fun { fName = f, fParams = xs, fDef = Def e }
                               : newFFuns s }


removeNewFuns :: M ([Fun Expr], [Fun Grammar])
removeNewFuns =
  M $ sets \s -> ( (newFFuns s, newGFuns s)
                 , s { newFFuns = [], newGFuns = [] }
                 )



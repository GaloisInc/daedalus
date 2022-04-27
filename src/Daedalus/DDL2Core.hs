{-# Language BlockArguments #-}
{-# Language GADTs #-}
{-# Language ImplicitParams #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ViewPatterns #-}
{-# Language ImplicitParams #-}
{-# Language ConstraintKinds #-}
module Daedalus.DDL2Core where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(maybeToList,isJust)
import Data.List((\\))
import Data.Either(partitionEithers)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import MonadLib

import Daedalus.PP hiding (cat)
import Daedalus.Panic(panic)
import qualified Daedalus.BDD as BDD
import Daedalus.Rec(forgetRecs)
import Daedalus.GUID(invalidGUID)

import Daedalus.Pass(PassM)
import qualified Daedalus.Type.AST as TC
import Daedalus.Type.AST (Commit(..), WithSem(..))

import Daedalus.Core

import Daedalus.Core.Free
import Daedalus.Core.Type(typeOf,sizeType)
import Panic (HasCallStack)


--------------------------------------------------------------------------------

fromModule :: TC.TCModule TC.SourceRange -> M Module
fromModule mo =
  let ?ents = Set.fromList (TC.tcEntries mo)
  in fromDecls (TC.tcModuleName mo) (TC.tcModuleTypes mo) (TC.tcModuleDecls mo)

type UsesTypes = (?tyMap :: Map TName TDecl)
type Ents      = (?ents  :: Set TC.Name)

fromDecls ::
  Ents =>
  TC.ModuleName ->
  [ Rec TC.TCTyDecl ] ->
  [ Rec (TC.TCDecl TC.SourceRange) ] ->
  M Module
fromDecls mo tdecls decls =
  withModuleName mo $
  do mapM_ newTNameRec tdecls
     tds <- mapM fromTCTyDeclRec tdecls

     let ds = concatMap recToList decls
     let ?tyMap = Map.fromList [ (tName d, d) | d <- forgetRecs tds ]
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

srcRangeAnnot :: TC.SourceRange -> Annot
srcRangeAnnot = SrcRange

exprAnnot :: TC.TC TC.SourceRange a -> [Annot]
exprAnnot e = [ srcRangeAnnot (TC.texprAnnot e) ]

declAnnot :: TC.TCDecl TC.SourceRange -> [Annot]
declAnnot d = [ srcRangeAnnot (TC.tcDeclAnnot d) ]


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

fromDecl :: (Ents,UsesTypes) => TC.TCDecl TC.SourceRange -> M SomeFun
fromDecl decl@TC.TCDecl { .. }
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
                  pure $ FE Fun { fName = f, fParams = xs, fDef = External
                                , fIsEntry = tcDeclName `Set.member` ?ents
                                , fAnnot = declAnnot decl }

                TC.Defined v ->
                   do e <- fromExpr v
                      pure $ FE Fun { fName = f, fParams = xs, fDef = Def e
                                    , fIsEntry = tcDeclName `Set.member` ?ents
                                    , fAnnot = declAnnot decl }
           TC.AClass ->
             case tcDeclDef of
               TC.ExternDecl _ ->
                 pure $ FB Fun { fName = f, fParams = xs, fDef = External
                               , fIsEntry = tcDeclName `Set.member` ?ents
                               , fAnnot = declAnnot decl }

               TC.Defined v ->
                 do e <- fromClass v
                    pure $ FB Fun { fName = f, fParams = xs, fDef = Def e
                                  , fIsEntry = tcDeclName `Set.member` ?ents
                                  , fAnnot = declAnnot decl }

           TC.AGrammar ->
             case tcDeclDef of

               TC.ExternDecl _ ->
                 pure $ FG Fun { fName = f, fParams = xs, fDef = External
                               , fIsEntry = tcDeclName `Set.member` ?ents
                               , fAnnot = declAnnot decl }

               TC.Defined v ->
                 do e <- fromGrammar v
                    pure $ FG Fun { fName = f, fParams = xs, fDef = Def e
                                  , fIsEntry = tcDeclName `Set.member` ?ents
                                  , fAnnot = declAnnot decl }


  | otherwise =
    panic "fromDeclG" [ "Type parmaeters/constraints" ]



fromGrammar :: UsesTypes => TC.TC TC.SourceRange TC.Grammar -> M Grammar
fromGrammar gram =
  let annot = exprAnnot gram
  in
  gAnnotate annot <$>
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


    TC.TCEnd -> pure (Match SemNo MatchEnd)

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
           NoSem -> doIf (lenE `leq` streamLen strE)
                        (Pure unit)
                        (sysErr TUnit "unexpected end of input")


           YesSem -> do len <- newLocal sizeType
                        str <- newLocal TStream
                        Let len lenE . Let str strE
                          <$> doIf (Var len `leq` streamLen (Var str))
                                  (Pure $ eTake (Var len) (Var str))
                                  (sysErr TStream "unexpected end of input")

    TC.TCStreamOff sem n s ->
      do lenE <- fromExpr n
         strE <- fromExpr s
         case sem of
           NoSem -> doIf (lenE `leq` streamLen strE)
                        (Pure unit)
                        (sysErr TUnit "unexpected end of input")
           YesSem ->
             do len <- newLocal sizeType
                str <- newLocal TStream
                Let len lenE . Let str strE
                  <$> doIf (Var len `leq` streamLen (Var str))
                          (Pure $ eDrop (Var len) (Var str))
                          (sysErr TStream "unexpected end of input")


    TC.TCMany sem cmt bnd g ->
      do ge <- fromGrammar g
         ty   <- fromGTypeM (TC.typeOf g)
         cbnd <- fromManybeBound bnd
         let vs = Set.toList (freeVars ge `Set.union` foldMap freeVars cbnd)

         case cbnd of
           TC.Exactly e ->
             case sem of
               NoSem  -> pSkipExactlyMany annot cmt vs e ge
               YesSem -> pParseExactlyMany annot cmt ty vs e ge
                         >>= finishMany ty

           TC.Between Nothing Nothing ->
              case sem of
                NoSem  -> pSkipMany annot cmt vs ge
                YesSem -> pParseMany annot cmt ty vs (newBuilder ty) ge
                          >>= finishMany ty

           TC.Between Nothing (Just e) ->
              case sem of
                NoSem  -> pSkipAtMost annot cmt vs e ge
                YesSem -> pParseAtMost annot cmt ty vs e (newBuilder ty) ge
                          >>= finishMany ty

           TC.Between (Just e) Nothing ->
              case sem of
                NoSem  -> do
                  p1 <- pSkipExactlyMany annot cmt vs e ge
                  p2 <- pSkipMany annot cmt vs ge
                  pure $ Do_ p1 p2
                YesSem -> do
                  b <- newLocal (TBuilder ty)
                  p1 <- pParseExactlyMany annot cmt ty vs e ge
                  p2 <- pParseMany annot cmt ty vs (Var b) ge
                  finishMany ty (Do b p1 p2)

           TC.Between (Just lb) (Just ub) ->
             -- FIXME: bind lb and ub?
             doIf (ub `lt` lb)
                 (sysErr (TArray ty) "Empty bounds")
                 =<< case sem of
             NoSem  -> do
               p1 <- pSkipExactlyMany annot cmt vs lb ge
               p2 <- pSkipAtMost annot cmt vs (ub `sub` lb) ge
               pure $ Do_ p1 p2
             YesSem -> do
               b <- newLocal (TBuilder ty)
               p1 <- pParseExactlyMany annot cmt ty vs lb ge
               p2 <- pParseAtMost annot cmt ty vs (ub `sub` lb) (Var b) ge
               finishMany ty (Do b p1 p2)

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
         case sem of
           NoSem  -> doIf (mapMember mpE kE)
                         (sysErr TUnit "duplicate key in map")
                         (Pure unit)
           YesSem -> doIf (mapMember mpE kE)
                         (sysErr ty "duplicate key in map")
                         (Pure (mapInsert mpE kE vE))

    TC.TCArrayIndex sem arr ix ->
      do aE <- fromExpr arr
         iE <- fromExpr ix
         case sem of
           NoSem -> doIf (arrayLen aE `leq` iE)
                        (sysErr TUnit "array index out of bounds")
                        (Pure unit)
           YesSem ->
             do ty0 <- fromTypeM (TC.typeOf arr)
                let ty = case ty0 of
                           TArray t -> t
                           _ -> panic "fromGrammar"
                                   [ "TCArrayIndex: not an array" ]
                a <- newLocal ty0
                i <- newLocal sizeType
                Let a aE . Let i iE
                  <$> doIf (arrayLen (Var a) `leq` Var i)
                          (sysErr ty "array index out of bounds")
                          (Pure (arrayIndex (Var a) (Var i)))

    TC.TCVar x -> panic "fromGrammar"
                  [ "Unexpected grammar variable: " ++ show x ]

    TC.TCCoerceCheck sem _t1 t2 v ->
      fromExpr v >>= \e ->
      withVar e      \x ->
      do tgt <- fromTypeM t2
         let e' = Pure case sem of
                         YesSem -> coerceTo tgt (Var x)
                         NoSem -> unit
         chk <- needsCoerceCheck tgt x
         case chk of
           Nothing  -> pure e'
           Just c   -> doIf c e'
                        (sysErr tgt "value does not fit in target type")

    TC.TCFor l -> doLoopG annot l

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

    TC.TCIf e e1 e2 ->
      join (doIf <$> fromExpr e <*> fromGrammar e1 <*> fromGrammar e2)

    TC.TCCase e as dflt ->
      do t  <- fromGTypeM (TC.typeOf gram)
         ms <- mapM (doAlt fromGrammar) as
         mbase <- case dflt of
                    Nothing -> pure Failure
                    Just d  -> Success Nothing <$> fromGrammar d
         let match = foldr biasedOr mbase ms
         matchToGrammar t match =<< fromExpr e

fromSem :: WithSem -> Sem
fromSem sem = case sem of
                NoSem -> SemNo
                YesSem -> SemYes


needsCoerceCheck :: UsesTypes => Type -> Name -> M (Maybe Expr)
needsCoerceCheck toTy x =
  case (toTy, fromTy) of

    -- equal types need no checking
    _ | toTy == fromTy -> pure Nothing

    -- coerciong to a float
    (TFloat, _) ->
      case fromTy of

        TDouble -> Just <$> backForthFromFloatToFloat

        TInteger -> Just <$> backForthFromIntToFloat

        (isUInt -> Just w)
          | w <= 24   -> pure Nothing
          | otherwise -> Just <$> backForthFromIntToFloat

        (isSInt -> Just w)
          | w <= 25   -> pure Nothing
          | otherwise -> Just <$> backForthFromIntToFloat

        _ -> bad

    -- coerciong to a double
    (TDouble, _) ->
      case fromTy of

        TFloat -> pure Nothing

        TInteger -> Just <$> backForthFromIntToFloat

        (isUInt -> Just w)
          | w <= 53   -> pure Nothing
          | otherwise -> Just <$> backForthFromIntToFloat

        (isSInt -> Just w)
          | w <= 54   -> pure Nothing
          | otherwise -> Just <$> backForthFromIntToFloat

        _ -> bad

    (TInteger, TFloat)  -> Just <$> backForthFromFloatToInt
    (TUInt {}, TFloat)  -> Just <$> backForthFromFloatToInt
    (TSInt {}, TFloat)  -> Just <$> backForthFromFloatToInt
    (TInteger, TDouble) -> Just <$> backForthFromFloatToInt
    (TUInt {}, TDouble) -> Just <$> backForthFromFloatToInt
    (TSInt {}, TDouble) -> Just <$> backForthFromFloatToInt




    -- int -> uint 8
    -- int -> sint 8
    (isBits -> Just _, TInteger) ->
      Just <$> eAnd lowerBoundCheck upperBoundCheck

    -- Cases for two numeric types, that don't match the above
    (isBits -> Just (toSigned, nTo), isBits -> Just (fromSigned, nFrom))


      -- uint 8 -> uint 9
      -- sint 8 -> sint 9
      -- uint 8 -> sint 9
      | nFrom < nTo && (toSigned == fromSigned || not fromSigned) ->
        pure Nothing

      -- sint 8 -> uint 8
      -- sint 8 -> uint 9
      -- check: 0 <= e
      | nFrom <= nTo && not toSigned -> pure (Just lowerBoundCheck)
        -- signed -> unsigned

      -- uint 8 -> sint 8
      -- e <= 127
      | nFrom == nTo && toSigned -> pure (Just upperBoundCheck)
        -- unsigned -> signed

      -- uint 9 -> sint 8, check: e <= 127
      -- uint 9 -> uint 8, check: e <= 255
      | nTo < nFrom && not fromSigned -> pure (Just upperBoundCheck)

      -- sint 9 -> uint 8
      -- check: 0 <= e
      | nTo == nFrom + 1 && fromSigned && not toSigned ->
        pure (Just lowerBoundCheck)

      -- sint 9 -> sint 8: -128 <= e <= 127
      -- sint 10 -> uint 8 0    <= e <= 255
      | nTo < nFrom -> Just <$> eAnd lowerBoundCheck upperBoundCheck

    -- bitdata -> uint
    (isUInt -> Just _, TUser ut)
      | tnameBD (utName ut) -> pure Nothing -- TC ensures widths match

    (TUser ut, isUInt -> Just _)
      | Just decl <- Map.lookup (utName ut) ?tyMap ->
        bitdataValidator (tDef decl) e

    _ -> bad
  where
    bad = panic "needsCoerceCheck"
            [ "Unexpected coercsion:"
            , showPP fromTy, showPP toTy
            ]

    e = Var x
    fromTy = typeOf e
    Just (s, n) = isBits toTy -- lazy
    upperBoundCheck = e `leq` intL (2 ^ (if s then n - 1 else n) - 1) fromTy
    lowerBoundCheck = intL (if s then - (2 ^ (n - 1)) else 0) fromTy `leq` e

    backForthFromFloatToFloat =
      do let p1 = eIsNaN e
         let p2 = eq e (coerceTo fromTy (coerceTo toTy e))
         eOr p1 p2

    backForthFromIntToFloat =
      withVar (coerceTo toTy e) \y ->
        do let p1 = eNot (eIsInfinite (Var y))
           let p2 = eq e (coerceTo fromTy (Var y))
           eAnd p1 p2

    backForthFromFloatToInt =
      do p1 <- eNot <$> eOr (eIsNaN e) (eIsInfinite e)
         let p2 = eq e (coerceTo fromTy (coerceTo toTy e))
         eAnd p1 p2



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
     TC.TCNumPat _ x _   -> terminal (PNum x)
     TC.TCStrPat xs      -> terminal (PBytes xs)
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


matchToGrammar :: Type -> PMatch Grammar -> Expr -> M Grammar
matchToGrammar t match e = 
  case match of
    Failure -> pure pFail
    Success mb g ->
      case mb of
        Nothing -> pure g
        Just x  -> pure (Let x e g)
    IfPat {} ->
      doCase e
        (fmap (completeAlts pFail) . matchToAlts (matchToGrammar t) match)
  where
  pFail = sysErr t "Pattern match failure"

matchToByteSet :: PMatch ByteSet -> Expr -> M ByteSet
matchToByteSet match e =
  case match of
    Failure -> pure (SetComplement SetAny)
    Success mb k ->
      case mb of
        Nothing -> pure k
        Just x -> pure (SetLet x e k)
    IfPat {} -> doCase e (matchToAlts matchToByteSet match)

matchToExpr :: PMatch Expr -> Expr -> M Expr
matchToExpr match e =
  case match of
    Failure -> panic "matchToExpr" [ "empty case" ]
    Success mb k ->
      case mb of
       Nothing -> pure k
       Just x  -> pure (PureLet x e k)
    IfPat {} -> doCase e (matchToAlts matchToExpr match)

matchToAlts ::
  (PMatch k -> Expr -> M k) ->
  PMatch k ->
  Name -> M [(Pattern,k)]
matchToAlts mExpr match x =
  case match of
    Failure -> pure []
    Success {} -> do
      me <- mExpr match (Var x)
      pure [(PAny, me)]
                     
    IfPat p nestP yes no -> do
      me <- mExpr yes nested
      (:) (p, me) <$> matchToAlts mExpr no x
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
                     PBytes {} -> bad
                     PAny     -> bad
      bad = panic "matchToAlts" ["Unexpected nested pattern"]
      e   = Var x
      
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
        PNum {}       -> this : completeAlts d more
        PBytes {}     -> this : completeAlts d more
        PCon {}       -> this : completeAlts d more -- XXX: could check that we have all
  where
  go need ps =
    case ps of
      [] -> [ (p,d) | p <- need ]
      (p,k) : more
        | (as,_:bs) <- break (== p) need -> (p,k) : go (as ++ bs) more
        | otherwise                      ->         go need       more

--------------------------------------------------------------------------------
-- Loops

doLoopCol ::
  TC.LoopCollection TC.SourceRange ->
  M ( Maybe (TC.TCName TC.Value, Name)
    , (TC.TCName TC.Value, Name)
    , Expr
    , Type
    )
doLoopCol col =
  do keyVar <- traverse fromName (TC.lcKName col)
     elVar  <- fromName (TC.lcElName col)
     colE   <- fromExpr (TC.lcCol col)
     colT   <- fromTypeM $ TC.typeOf (TC.lcCol col)
     pure (keyVar, elVar, colE, colT)




doLoopG ::
  UsesTypes => [Annot] -> TC.Loop TC.SourceRange TC.Grammar -> M Grammar
doLoopG ann lp =
  do let doBody elVar keyVar = withSourceLocal elVar
                             $ maybe id withSourceLocal keyVar
                             $ fromGrammar $ TC.loopBody lp

     case TC.loopFlav lp of

       TC.LoopMany cmt x s ->
         do v@(_,sVar)  <- fromName x
            initS <- fromExpr s
            g     <- withSourceLocal v (fromGrammar (TC.loopBody lp))
            ty    <- fromGTypeM (TC.loopType lp)
            let free = Set.toList (Set.delete sVar (freeVars g))
                es   = map Var free

            f     <- newGName ty

            let def = do lhs <- do r1 <- newLocal ty
                                   pure (Do r1 g (Pure (just (Var r1))))
                         r2 <- newLocal (TMaybe ty)
                         pure $ Do r2 (orOp cmt lhs (Pure (nothing ty)))
                              $ GCase
                              $ Case r2
                                  [ (PJust, Call f (eFromJust (Var r2) : es))
                                  , (PNothing, Pure (Var sVar))
                                  ]

            defFunG ann f (sVar : free) =<< def
            pure (Call f (initS : es))



       TC.Fold x s col ->
         do (keyVar,elVar,colE,colT) <- doLoopCol col

            sVar     <- fromName x
            initS    <- fromExpr s
            g        <- withSourceLocal sVar (doBody elVar keyVar)
            let free = freeVars g
            ty       <- fromGTypeM $ TC.loopType lp
            foldLoopG
              ann
              colT ty (Set.toList free)
              (snd sVar) initS
              (snd <$> keyVar) (snd elVar) colE
              \_ -> g

       TC.LoopMap col ->
         fromGTypeM (TC.loopType lp) >>= \resT ->
         doLoopCol col >>= \(keyVar,elVar,colE,colT) ->
         case resT of

           TArray elTy ->
             do sVar <- newLocal (TBuilder elTy)
                newEl <- newLocal elTy
                g <- doBody elVar keyVar
                let free = freeVars g
                let ty = TBuilder elTy
                step1 <- foldLoopG
                           ann
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
                 g <- doBody elVar keyVar
                 let free = freeVars g
                 foldLoopG ann colT ty (Set.toList free)
                   sVar (mapEmpty tk tv)
                   (snd <$> keyVar) (snd elVar) colE
                   \i -> Do newEl g
                          $ Pure (mapInsert (Var sVar)
                                            (iteratorKey i)
                                            (Var newEl))

           _ -> panic "doLoopG/MapLoop" ["Unexpected result type"]


foldLoopG ::
  [Annot]            {- ^ Locaiton information -} ->
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
foldLoopG ann colT ty vs0 sVar initS keyVar elVar colE g =
  do let vs  = vs0 \\ (maybeToList keyVar ++ [sVar,elVar])
         es  = map Var vs
         maybeAddKey e = case keyVar of
                           Nothing -> id
                           Just k  -> Let k e

     f <- newGName ty
     i <- newLocal (TIterator colT)
     nextS <- newLocal ty
     defFunG ann f (sVar : i : vs)
       =<< doIf (iteratorDone (Var i))
               (Pure (Var sVar))
               (Let elVar (iteratorVal (Var i))
                $    maybeAddKey (iteratorKey (Var i))
                $    Do nextS (g (Var i))
                $       Call f (Var nextS : iteratorNext (Var i) : es))
     pure $ Call f (initS : newIterator colE : es)


maybeSkip :: Commit -> Grammar -> Grammar -> Grammar -> M Grammar
maybeSkip cmt p yes no =
  do r <- newLocal TBool
     pure case cmt of
            Commit ->
              Do r (OrBiased (Do_ p (Pure (boolL True))) (Pure (boolL False)))
               $ coreIf r yes no
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
                $ Case rMb
                    [ (PJust,    yes (eFromJust (Var rMb)))
                    , (PNothing, no)
                    ]
            _ ->
             orOp cmt (Do r p (yes (Var r))) no

pSkipMany :: [Annot] -> Commit -> [Name] -> Grammar -> M Grammar
pSkipMany ann cmt vs p =
  do f <- newGName TUnit
     let es = map Var vs
     skipBody <- maybeSkip cmt p (Call f es) (Pure unit)
     defFunG ann f vs skipBody
     pure (Call f es)

pParseMany ::
  [Annot] -> Commit -> Type -> [Name] -> Expr -> Grammar -> M Grammar
pParseMany ann cmt ty vs be p =
  do f <- newGName (TBuilder ty)
     let es = map Var vs
     x <- newLocal (TBuilder ty)
     let xe = Var x
     body <- maybeParse cmt ty p
                              (\a -> Call f (consBuilder a xe : es)) (Pure xe)
     defFunG ann f (x:vs) body
     pure $ Call f (be : es)

pSkipExactlyMany :: [Annot] -> Commit -> [Name] -> Expr -> Grammar -> M Grammar
pSkipExactlyMany ann _cmt vs tgt p =
  do f <- newGName TUnit
     let es = map Var vs
     x <- newLocal sizeType
     let xe = Var x

     let body = Do_ (OrBiased p (sysErr TUnit "insufficient element occurances"))
                    (Call f (add xe (intL 1 sizeType) : es))

     defFunG ann f (x : vs) =<< doIf (xe `lt` tgt) body (Pure unit)
     pure $ Call f (intL 0 sizeType : es)

-- | Produces a function which returns a builder

-- FIXME: we should evaluate tgt outside of the function if it is
-- complex to avoid recomputing it.
pParseExactlyMany ::
  [Annot] -> Commit -> Type -> [Name] -> Expr -> Grammar -> M Grammar
pParseExactlyMany ann _cmt ty vs tgt p =
  do f <- newGName (TBuilder ty)
     let es = map Var vs
     x <- newLocal sizeType
     b <- newLocal (TBuilder ty)
     r <- newLocal ty

     let xe = Var x
         be = Var b
         re = Var r

     -- FIXME: We don't need to worry about commit here(?) as we 
     -- always take exactly tgt many iterations
     let body = Do r (OrBiased p (sysErr ty "insufficient element occurances"))
                     (Call f (add xe (intL 1 sizeType)
                              : consBuilder re be
                              : es))

     defFunG ann f (x : b : vs) =<< doIf (xe `lt` tgt) body (Pure be)
     pure $ Call f (intL 0 sizeType : newBuilder ty : es)

pSkipAtMost :: [Annot] -> Commit -> [Name] -> Expr -> Grammar -> M Grammar
pSkipAtMost ann cmt vs tgt p =
  do f <- newGName sizeType
     let es = map Var vs
     x <- newLocal sizeType
     let xe = Var x
     skipBody <- maybeSkip cmt p (Call f (add xe (intL 1 sizeType) : es))
                                 (Pure xe)
     defFunG ann f (x:vs) =<< doIf (xe `lt` tgt) skipBody (Pure xe)
     pure (Call f (intL 0 sizeType : es))

pParseAtMost ::
  [Annot] -> Commit -> Type -> [Name] -> Expr -> Expr -> Grammar -> M Grammar
pParseAtMost ann cmt ty vs tgt be p =
  do f <- newGName (TBuilder ty)
     let es = map Var vs
     x <- newLocal sizeType
     bv <- newLocal (TBuilder ty)
     let xe = Var x
         bve = Var bv

     body <- maybeParse cmt ty p
                (\a -> Call f (add xe (intL 1 sizeType)
                              : consBuilder a bve
                              : es))
                (Pure bve)

     defFunG ann f (x : bv : vs) =<< doIf (xe `lt` tgt) body (Pure bve)
     pure $ Call f (intL 0 sizeType : be : es)

finishMany :: Type -> Grammar -> M Grammar
finishMany ty p = do
  b <- newLocal (TBuilder ty)
  pure (Do b p (Pure (finishBuilder (Var b))))

--------------------------------------------------------------------------------


fromClass :: TC.TC TC.SourceRange TC.Class -> M ByteSet
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


    TC.TCIf e e1 e2 ->
      join (doIf <$> fromExpr e <*> fromClass e1 <*> fromClass e2)

    TC.TCCase e as dflt ->
      do ms <- mapM (doAlt fromClass) as
         match <- case dflt of
                    Nothing -> pure (foldr1 biasedOr ms)
                    Just d  ->
                      do base <- Success Nothing <$> fromClass d
                         pure (foldr biasedOr base ms)
         matchToByteSet match =<< fromExpr e

    TC.TCFor {} -> panic "fromClass" ["Unexpected loop"]
    TC.TCVar {} -> panic "fromClass" ["Unexpected var"]




--------------------------------------------------------------------------------


fromExpr :: TC.TC TC.SourceRange TC.Value -> M Expr
fromExpr expr =
  case TC.texprValue expr of
    TC.TCLet x e1 e2 ->
      do x'  <- fromName x
         e1' <- fromExpr e1
         e2' <- withSourceLocal x' (fromExpr e2)
         pure (PureLet (snd x') e1' e2')

    TC.TCMapEmpty t ->
      fromTypeM t >>= \resT ->
      case resT of
        TMap k v -> pure (mapEmpty k v)
        _ -> panic "fromExpr" ["MapEmpty not a map"]

    TC.TCArrayLength v ->
      arrayLen <$> fromExpr v

    TC.TCCoerce _t1 t2 v ->
      coerceTo <$> fromTypeM t2 <*> fromExpr v

    TC.TCLiteral l t ->
      case l of
        TC.LNumber n _ -> intL n   <$> fromTypeM t
        TC.LFloating n -> floatL n <$> fromTypeM t
        TC.LPi         -> floatL pi <$> fromTypeM t
        TC.LBool b     -> pure (boolL b)
        TC.LByte x _   -> pure (intL (toInteger x) tByte)
        TC.LBytes bs   -> pure (byteArrayL bs)

    TC.TCNothing t ->
      nothing <$> fromTypeM t
    
    TC.TCBuilder t ->
      newBuilder <$> fromTypeM t

    TC.TCJust e ->
      just <$> fromExpr e

    TC.TCStruct fs t ->
      case t of
        TC.Type TC.TUnit -> pure unit
        _ -> Struct <$> userTypeM t <*> mapM field fs

        where field (l,v) = do e <- fromExpr v
                               pure (l,e)

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
         doIf e1 e2 e3

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

                TC.WordToFloat    -> wordToFloat e
                TC.WordToDouble   -> wordToDouble e
                TC.IsNaN          -> eIsNaN e
                TC.IsInfinite     -> eIsInfinite e
                TC.IsDenormalized -> eIsDenormalized e
                TC.IsNegativeZero -> eIsNegativeZero e
                TC.BytesOfStream  -> bytesOfStream e
                TC.BuilderBuild   -> finishBuilder e

    TC.TCBinOp op v1 v2 _ ->
      do e1 <- fromExpr v1
         e2 <- fromExpr v2
         case op of
           TC.Add -> pure $ add e1 e2
           TC.Sub -> pure $ sub e1 e2
           TC.Mul -> pure $ mul e1 e2
           TC.Div -> pure $ eDiv e1 e2
           TC.Mod -> pure $ eMod e1 e2

           TC.Lt     -> pure $ e1 `lt` e2
           TC.Leq    -> pure $ e1 `leq` e2
           TC.Eq     -> pure $ e1 `eq` e2
           TC.NotEq  -> pure $ e1 `notEq` e2

           TC.Cat          -> pure $ cat e1 e2
           TC.LCat         -> pure $ lCat e1 e2
           TC.LShift       -> pure $ lShift e1 e2
           TC.RShift       -> pure $ rShift e1 e2
           TC.BitwiseAnd   -> pure $ bitAnd e1 e2
           TC.BitwiseOr    -> pure $ bitOr e1 e2
           TC.BitwiseXor   -> pure $ bitXor e1 e2

           TC.ArrayStream  -> pure $ arrayStream e1 e2
           TC.LogicAnd     -> eAnd e1 e2
           TC.LogicOr      -> eOr  e1 e2
           TC.LookupMap    -> pure $ mapLookup e2 e1
           TC.BuilderEmit  -> pure $ consBuilder e2 e1

    TC.TCTriOp op v1 v2 v3 _ ->
      do e1 <- fromExpr v1
         e2 <- fromExpr v2
         e3 <- fromExpr v3
         pure case op of
                TC.RangeUp     -> rangeUp e1 e2 e3
                TC.RangeDown   -> rangeDown e1 e2 e3
                TC.MapDoInsert -> mapInsert e3 e1 e2 -- note: map is 1st here

    TC.TCFor lp -> doLoop (exprAnnot expr) lp

    TC.TCCase e as dflt ->
      do ms <- mapM (doAlt fromExpr) as
         match <- case dflt of
                    Nothing -> pure (foldr1 biasedOr ms)
                    Just d  ->
                      do base <- Success Nothing <$> fromExpr d
                         pure (foldr biasedOr base ms)
         matchToExpr match =<< fromExpr e




doLoop :: [Annot] -> TC.Loop TC.SourceRange TC.Value -> M Expr
doLoop ann lp =
  do let doBody elVar keyVar = withSourceLocal elVar
                             $ maybe id withSourceLocal keyVar
                             $ fromExpr $ TC.loopBody lp

     case TC.loopFlav lp of

       TC.Fold x s col ->
         do (keyVar,elVar,colE,colT) <- doLoopCol col
            sVar     <- fromName x
            initS    <- fromExpr s
            g        <- withSourceLocal sVar (doBody elVar keyVar)
            let free = freeVars g
            ty       <- fromTypeM $ TC.loopType lp
            foldLoop
              ann
              colT ty (Set.toList free)
              (snd sVar) initS
              (snd <$> keyVar) (snd elVar) colE
              \_ -> g

       TC.LoopMap col ->
         fromTypeM (TC.loopType lp) >>= \resT ->
         doLoopCol col >>= \(keyVar,elVar,colE,colT) ->
         case resT of

           TArray elTy ->
             do sVar <- newLocal (TBuilder elTy)
                g <- doBody elVar keyVar
                let free = freeVars g
                let ty = TBuilder elTy
                step1 <- foldLoop
                           ann
                           colT ty (Set.toList free)
                           sVar (newBuilder elTy)
                           (snd <$> keyVar) (snd elVar) colE
                           \_ -> consBuilder g (Var sVar)

                pure (finishBuilder step1)

           ty@(TMap tk tv) ->
             do sVar <- newLocal ty
                g <- doBody elVar keyVar
                let free = freeVars g
                foldLoop ann colT ty (Set.toList free)
                  sVar (mapEmpty tk tv)
                  (snd <$> keyVar) (snd elVar) colE
                  \i -> mapInsert (Var sVar) (iteratorKey i) g


           _ -> panic "doLoopG/MapLoop" ["Unexpected result type"]



foldLoop ::
  [Annot]            {- ^ Location information -} ->
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
foldLoop ann colT ty vs0 sVar initS keyVar elVar colE g =
  do let vs  = vs0 \\ (maybeToList keyVar ++ [sVar,elVar])
         es  = map Var vs
         maybeAddKey e = case keyVar of
                           Nothing -> id
                           Just k  -> PureLet k e

     f <- newFName' Nothing ty
     i <- newLocal (TIterator colT)
     defFunF ann f (sVar : i : vs)
       =<< doIf (iteratorDone (Var i))
               (Var sVar)
               (PureLet elVar (iteratorVal (Var i))
                $ maybeAddKey   (iteratorKey (Var i))
                $ callF f (g (Var i) : iteratorNext (Var i) : es))

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

fromGTypeM :: HasCallStack => TC.Type -> M Type
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

fromGType :: (?tenv :: TEnv, HasCallStack) => TC.Type -> Type
fromGType ty =
  case ty of
    TC.Type (TC.TGrammar t) -> fromType t
    _ -> panic "fromGType" [ "Not a grammar", showPP ty ]

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
        TC.TFloat      -> TFloat
        TC.TDouble     -> TDouble
        TC.TUnit       -> TUnit
        TC.TArray t    -> TArray (fromType t)
        TC.TMaybe t    -> TMaybe (fromType t)
        TC.TMap k v    -> TMap (fromType k) (fromType v)
        TC.TBuilder t  -> TBuilder (fromType t)

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

    _ -> panic "userType" ["Not a type constructor", showPP ty ]





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
              in fromTCTyDef (TC.tctyBD td) (TC.tctyDef td)
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

bitdataValidator :: TDef -> Expr -> M (Maybe Expr)
bitdataValidator tdef = \e' ->
  case tdef of
    TBitdata univ _ | not (BDD.willAlwaysMatch univ) ->
      Just <$>
      case e' of
        Ap0 (IntL n _) -> pure (boolL (BDD.willMatch univ n))
        Var x          -> doTests x
        _              -> withVar e' doTests
        where
        ty = typeOf e'

        appMask :: Integer -> Expr -> Expr
        appMask mask e = if mask == (2 ^ BDD.width univ) - 1
                           then e
                           else bitAnd (intL mask ty) e

        tests = BDD.groupTestsByMask (BDD.patTests univ)

        doTests x = foldr (doTest x) (pure (boolL False)) (Map.toList tests)

        doTest :: Name -> (Integer,[Integer]) -> M Expr -> M Expr
        doTest e (mask,cases) orElseM =
          do orElse <- orElseM
             withVar (appMask mask (Var e)) \x ->
                pure $ coreCase x
                     $ [ (PNum c, boolL True) | c <- cases ] ++
                       [ (PAny, orElse) ]

    _ -> pure Nothing


fromBitdata :: (?tenv :: TEnv) => TC.TCTyDef -> BitdataDef
fromBitdata def =
  case def of
    TC.TCTyStruct mbCon _ ->
      case mbCon of
        Just con -> BDStruct [ cvt f | f <- TC.bdFields con ]
        Nothing -> panic "fromBitdata" [ "Not bitdata", showPP def ]
    TC.TCTyUnion fs -> BDUnion [ (l,fromType t) | (l,(t,_)) <- fs ]
  where
  cvt f = BDField { bdOffset = TC.bdOffset f
                  , bdWidth  = TC.bdWidth f
                  , bdFieldType =
                    case TC.bdFieldType f of
                      TC.BDWild     -> BDWild
                      TC.BDTag n    -> BDTag n
                      TC.BDData l t -> BDData l (fromType t)
                  }

fromTCTyDef :: (?tenv :: TEnv) => Maybe BDD.Pat -> TC.TCTyDef -> TDef
fromTCTyDef bd tdef =
  case bd of
    Nothing ->
      case tdef of
        TC.TCTyStruct _ fs -> TStruct (map fieldS fs)
        TC.TCTyUnion fs  -> TUnion  (map field fs)
    Just univ -> TBitdata univ (fromBitdata tdef)
  where
  fieldS (l,t)     = (l, fromType t)
  field (l,(t, _)) = (l, fromType t)




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
                 _ -> panic "fromFDefName" ["Not a top-level name"]
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


fromArg :: TC.Arg TC.SourceRange -> M Expr
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





fromManybeBound ::
  TC.ManyBounds (TC.TC TC.SourceRange TC.Value) -> M (TC.ManyBounds Expr)
fromManybeBound bnds =
  case bnds of
    TC.Exactly e   -> TC.Exactly <$> fromExpr e
    TC.Between a b -> TC.Between <$> traverse fromExpr a
                                 <*> traverse fromExpr b





--------------------------------------------------------------------------------
-- Utilities

withVar :: CoreSyn t => Expr -> (Name -> M t) -> M t
withVar e k =
  case e of
    Var x -> k x
    _     -> do x <- newLocal (typeOf e)
                coreLet x e <$> k x

doCase :: CoreSyn t => Expr -> (Name -> M [(Pattern, t)]) -> M t
doCase e mkAlts =
  withVar e \x ->
  do alts <- mkAlts x
     pure (coreCase x alts)

doIf :: CoreSyn t => Expr -> t -> t -> M t
doIf e g1 g2 = withVar e \x -> pure (coreIf x g1 g2)

eOr, eAnd :: Expr -> Expr -> M Expr
eOr x y  = doIf x (boolL True) y
eAnd x y = doIf x y (boolL False)

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
    NoSem -> doCase e (const (pure [ (PNothing, nope TUnit)
                                   , (PJust, Pure unit)
                                   ]))
    YesSem ->
      doCase e (\x -> pure [ (PNothing, nope t)
                          , (PJust, Pure (eFromJust (Var x)))
                          ])
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
    let bd = isJust (TC.tctyBD d)
        flavor = case TC.tctyDef d of
                   TC.TCTyStruct {} -> TFlavStruct
                   TC.TCTyUnion cs
                     | all (\(_, (t, _)) -> t == TC.tUnit) cs -> TFlavEnum (map fst cs)
                     | otherwise -> TFlavUnion (map fst cs)
    in newTName r bd flavor (TC.tctyName d)

newTName :: Bool -> Bool -> TFlav -> TC.TCTyName -> M ()
newTName isRec isBD flavor nm = M
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
                  , tnameBD = isBD
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

defFunG :: [Annot] -> FName -> [Name] -> Grammar -> M ()
defFunG ann f xs e =
  M $ sets_ \s -> s { newGFuns = Fun { fName = f, fParams = xs, fDef = Def e
                                     , fIsEntry = False
                                     , fAnnot = ann }
                               : newGFuns s }

defFunF :: [Annot] -> FName -> [Name] -> Expr -> M ()
defFunF ann f xs e =
  M $ sets_ \s -> s { newFFuns = Fun { fName = f, fParams = xs, fDef = Def e
                                     , fIsEntry = False, fAnnot = ann }
                               : newFFuns s }


removeNewFuns :: M ([Fun Expr], [Fun Grammar])
removeNewFuns =
  M $ sets \s -> ( (newFFuns s, newGFuns s)
                 , s { newFFuns = [], newGFuns = [] }
                 )



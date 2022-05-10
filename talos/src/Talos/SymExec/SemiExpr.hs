{-# LANGUAGE GADTs, PatternGuards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

-- -----------------------------------------------------------------------------
-- Semi symbolic/concrete evaluation

module Talos.SymExec.SemiExpr ( runSemiSolverM
                              , symExecToSemiExec
                              , semiExecByteSet
                              , semiExecExpr
                              , semiExecCase
                              , SymbolicCaseResult(..)
                              , SemiSolverM
                              , SemiSExpr
                              , vUnit
                              , vSExpr
                              , Typed(..)
                              ) where

-- import Data.Map (Map)
import           Control.Monad.Reader
import           Data.Foldable                   (find)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (isJust, isNothing)
import qualified Data.Vector                     as Vector

import           SimpleSMT                       (SExpr)
import qualified SimpleSMT                       as S

import           Daedalus.Core                   hiding (freshName)
import qualified Daedalus.Core.Semantics.Env     as I
import           Daedalus.Core.Semantics.Expr    (evalOp0, evalOp1, evalOp2,
                                                  evalOp3, evalOpN, matches,
                                                  partial)
import           Daedalus.Core.Type
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic
import qualified Daedalus.Value.Type             as V

-- import Talos.Strategy.Monad
import qualified Talos.SymExec.Expr as SE
import           Talos.SymExec.SemiValue (SemiValue (..))
import qualified Talos.SymExec.SemiValue as SV
import           Talos.SymExec.SolverT
import           Talos.SymExec.StdLib
import           Talos.SymExec.Type


-- FIXME: move

type SemiSExpr = SemiValue (Typed SExpr)

-- -----------------------------------------------------------------------------
-- Values

vUnit :: SemiSExpr
vUnit = VValue V.vUnit

vSExpr :: Type -> SExpr -> SemiSExpr
vSExpr ty = VOther . Typed ty

pattern VBool :: Bool -> SemiValue a
pattern VBool b = VValue (V.VBool b)


-- -----------------------------------------------------------------------------
-- Byte sets
-- 
-- At the moment we just symbolically execute

semiExecByteSet :: (HasGUID m, Monad m, MonadIO m) => ByteSet -> SemiSExpr -> SemiSolverM m SemiSExpr
semiExecByteSet = symExecByteSet

-- -----------------------------------------------------------------------------
-- Fallin back to (fully) symbolic execution

symExecToSemiExec :: (HasGUID m, Monad m, MonadIO m) => SE.SymExecM m a -> SemiSolverM m a
symExecToSemiExec = withReaderT envf
  where
    envf env = envToSymEnv (typeDefs env) (localBoundNames env)

symExec :: (HasGUID m, Monad m, MonadIO m) => Expr -> SemiSolverM m SemiSExpr
symExec e = vSExpr (typeOf e) <$> symExecToSemiExec (SE.symExecExpr e)

symExecByteSet :: (HasGUID m, Monad m, MonadIO m) => ByteSet -> SemiSExpr -> SemiSolverM m SemiSExpr
symExecByteSet bs b = do
  tys <- asks typeDefs
  vSExpr TBool <$> symExecToSemiExec (SE.symExecByteSet (semiSExprToSExpr tys (TUInt (TSize 8)) b) bs)

envToSymEnv :: Map TName TDecl -> Map Name SemiSExpr ->  Map Name SExpr
envToSymEnv tenv = Map.mapWithKey (semiSExprToSExpr tenv . nameType)

semiSExprToSExpr :: Map TName TDecl -> Type -> SemiSExpr -> SExpr
semiSExprToSExpr tys ty sv =
  case sv of
    VValue v -> valueToSExpr tys ty v
    VOther s -> typedThing s
    -- FIXME: copied from valueToSExpr, unify.
    VUnionElem l v' | Just (ut, ty') <- typeAtLabel tys ty l
      -> S.fun (labelToField (utName ut) l) [go ty' v']

    -- FIXME: copied from valueToSExpr, unify.
    VStruct els
      | TUser ut <- ty
      , Just TDecl { tDef = TStruct flds } <- Map.lookup (utName ut) tys
      -> S.fun (typeNameToCtor (utName ut)) (zipWith goStruct els flds)

    VSequence _ vs ->
      let elTy = case ty of
            TArray elTy' -> elTy'
            TBuilder elTy' -> elTy'
            _ -> panic "Expecting a sequence-like structure" [ showPP ty, show vs ]
      in sArrayLit (symExecTy elTy) (typeDefault elTy) (map (go elTy) vs)
    VMaybe mv | TMaybe ty' <- ty -> case mv of
                  Nothing -> sNothing (symExecTy ty')
                  Just v' -> sJust (symExecTy ty') (go ty' v')

    VMap ms | TMap kt vt <- ty ->
      -- FIXME: breaks abstraction of maps
      sFromList (tTuple (symExecTy kt) (symExecTy vt))
                [ sTuple (go kt k) (go vt v) | (k, v) <- ms ]

    VIterator vs -> case ty of
      -- FIXME: this breaks abstractions
      TIterator (TArray elTy) ->
        let emptyA = sEmptyL (symExecTy elTy) (typeDefault elTy)
            els    = [ (go sizeType k, go elTy v) | (k, v) <- vs ]
            arr    = foldr (\(i, v) arr' -> S.store arr' i v) emptyA els
        in case els of
          []             -> sArrayIterNew emptyA
          ((fstI, _) : _) -> S.fun "mk-ArrayIter" [arr, fstI]
          
      TIterator (TMap   _kt' _vt') -> panic "Unimplemented" []
      _ -> panic "Malformed iterator type" []
      
    _ -> panic "Malformed value" [show sv, showPP ty]
  where
    go = semiSExprToSExpr tys
    goStruct (l, e) (l', ty') | l == l' = go ty' e
    goStruct (l, _) (l', _) = panic "Mis-matched labels" [showPP l, showPP l']

semiExecName :: (HasGUID m, Monad m) => Name -> SemiSolverM m SemiSExpr
semiExecName n = do
  m_local <- asks (Map.lookup n . localBoundNames)
  case m_local of
    Nothing -> lift (vSExpr (typeOf n) <$> getName n)
    Just r  -> pure r

bindNameIn :: Monad m => Name -> SemiSExpr -> SemiSolverM m a -> SemiSolverM m a
bindNameIn n v =
  local (\e -> e { localBoundNames = Map.insert n v (localBoundNames e) })

-- Stolen from Synthesis
-- projectEnvFor :: FreeVars t => t -> I.Env -> SynthEnv -> Maybe I.Env
-- projectEnvFor tm env0 se = doMerge <$> Map.traverseMaybeWithKey go (synthValueEnv se)
--   where
--     frees = freeVars tm

--     doMerge m = env0 { I.vEnv = Map.union m (I.vEnv env0) } 

--     go k v | k `Set.member` frees = Just <$> projectInterpValue v
--     go _ _                        = Just Nothing

-- projectEnvForM :: FreeVars t => t -> SynthesisM I.Env
-- projectEnvForM tm = do
--   env0 <- getIEnv
--   m_e <- SynthesisM $ asks (projectEnvFor tm env0)
--   case m_e of
--     Just e  -> pure e
--     Nothing -> panic "Captured stream value" []

-- Stolen from Daedalus.Core.Semantics.Expr
matches' :: SemiSExpr -> Pattern ->  Maybe Bool
matches' (VValue v) pat = Just (matches pat v)
matches' v pat =
  case pat of
    PBool {} -> Nothing

    PNothing | VMaybe mb <- v -> Just (isNothing mb)
    PNothing -> Nothing

    PJust    | VMaybe mb <- v -> Just (isJust mb)
    PJust    -> Nothing

    PCon l | VUnionElem l' _ <- v -> Just (l == l')
    PCon {} -> Nothing

    PAny   -> Just True
    PNum _ -> Nothing -- only works on values

data SymbolicCaseResult a = TooSymbolic | NoMatch | DidMatch Int a

semiExecCase :: (HasGUID m, Monad m, MonadIO m, PP a) =>
                Case a -> SemiSolverM m (SymbolicCaseResult a)
semiExecCase c@(Case y pats) = do
  ve <- semiExecName y
  -- we need to be careful not to match 'Any' if the others can't
  -- be determined.
  let ms = zip (zip pats [0..]) <$> mapM (matches' ve . fst) pats
  pure $ case ms of
    -- can't determine match, just return a sexpr
    Nothing -> TooSymbolic
    Just ps
      | Just (((_, r), ix), _) <- find snd ps -> DidMatch ix r
      | otherwise -> NoMatch

-- -----------------------------------------------------------------------------
-- Monad

data SemiSolverEnv = SemiSolverEnv
  { localBoundNames :: Map Name SemiSExpr
  -- for concretely evaluating functions, only const/pure fun env
  -- should be used.
  , interpEnv       :: I.Env
  , funDefs         :: Map FName (Fun Expr)
  }

typeDefs :: SemiSolverEnv -> Map TName TDecl
typeDefs = I.tEnv . interpEnv

type SemiSolverM m = ReaderT SemiSolverEnv (SolverT m)

runSemiSolverM :: (HasGUID m, Monad m, MonadIO m) =>
                  Map FName (Fun Expr) ->
                  Map Name SemiSExpr ->
                  I.Env ->
                  SemiSolverM m a -> SolverT m a
runSemiSolverM funs lenv env m =
  runReaderT m (SemiSolverEnv lenv env funs)


semiExecExpr :: (HasGUID m, Monad m, MonadIO m) => Expr -> SemiSolverM m SemiSExpr
semiExecExpr expr =
  case expr of
    Var n          -> semiExecName n
    PureLet n e e' -> do
      ve  <- go e
      bindNameIn n ve (go e') -- Maybe we should generate a let?

    Struct _ut ctors -> do
      let (ls, es) = unzip ctors
      sves <- mapM go es
      pure (VStruct (zip ls sves))

    ECase cs -> do
      m_e <- semiExecCase cs
      case m_e of
        -- can't determine match, just return a sexpr
        TooSymbolic   -> symExec expr
        DidMatch _ e' -> go e'
        -- Shouldn't happen in pure code
        NoMatch -> panic "No match" []
        
    Ap0 op       -> pure (VValue $ partial (evalOp0 op))
    Ap1 op e     -> semiExecOp1 op rty (typeOf e) =<< go e
    Ap2 op e1 e2 -> join (semiExecOp2 op rty (typeOf e1) (typeOf e2) <$> go e1 <*> go e2)
    Ap3 op e1 e2 e3 -> join (semiExecOp3 op rty (typeOf e1) <$> go e1 <*> go e2 <*> go e3)
    ApN opN vs     -> semiExecOpN opN rty =<< mapM go vs
  where
    go = semiExecExpr
    rty = typeOf expr
    
-- Might be able to just use the value instead of requiring t
semiExecOp1 :: (Monad m, HasGUID m) => Op1 -> Type -> Type -> SemiSExpr -> SemiSolverM m SemiSExpr
semiExecOp1 op _rty ty (VValue v) =
  do env <- asks interpEnv
     pure $ VValue (evalOp1 env op ty v)
-- These operations are lazy in the value, so we can produce concrete
-- values even though we have symbolic arguments
semiExecOp1 EJust _rty _ty sv = pure (VMaybe (Just sv))
semiExecOp1 (InUnion _ut l)  _rty _ty sv = pure (VUnionElem l sv)
-- Otherwise, we just produce a sexpr
semiExecOp1 op rty  ty (VOther s) = lift (vSExpr rty <$> SE.symExecOp1 op ty (typedThing s))
semiExecOp1 op rty ty sv =
  -- We only care about operations over the compound semivalues (i.e., concrete values are handled above)
  case op of
    IsEmptyStream -> unimplemented
    Head          -> unimplemented
    StreamOffset  -> unimplemented
    StreamLen     -> unimplemented
    ArrayLen | Just svs <- SV.toList sv -> pure $ VValue (V.vSize (toInteger (length svs)))
    Concat   | Just svs <- SV.toList sv
             , Just svss <- mapM SV.toList svs -> pure (SV.fromList False (mconcat svss))
    -- Just flip the 'isbuilder' flag
    FinishBuilder | VSequence _ vs <- sv -> pure (VSequence False vs)
    NewIterator
      | TArray {} <- ty,
        Just svs <- SV.toList sv -> pure $ VIterator (zip (map (VValue . V.vSize) [0..]) svs)
    NewIterator -> unimplemented -- maps
    IteratorDone | VIterator els <- sv -> pure (VBool (null els))
    IteratorKey  | VIterator ((k, _) : _) <- sv  -> pure k
    IteratorVal  | VIterator ((_, el) : _) <- sv -> pure el
    IteratorNext
      | VIterator (_ : els) <- sv -> pure (VIterator els)
      | VIterator [] <- sv -> panic "empty iterator" []

    -- EJust -> pure (VMaybe (Just sv))
    FromJust
      | VMaybe (Just sv') <- sv -> pure sv'
      | VMaybe Nothing    <- sv -> panic "FromJust: Nothing" []

    SelStruct _ty l
      | VStruct flds <- sv
      , Just v <- lookup l flds -> pure v

    -- InUnion _ut l                     -> pure (VUnionElem l sv)
    FromUnion _ty l
      | VUnionElem l' sv' <- sv, l == l' -> pure sv'
      | VUnionElem {} <- sv -> panic "Incorrect union tag" []

    -- symbolic
    _ -> do
      tys <- asks typeDefs
      vSExpr rty <$> lift (SE.symExecOp1 op ty (semiSExprToSExpr tys ty sv))
  where
    unimplemented = panic "semiEvalOp1: Unimplemented" [showPP op]


typeToElType :: Type -> Maybe Type
typeToElType ty = 
  case ty of
    TBuilder elTy -> Just elTy
    TArray   elTy -> Just elTy
    _ -> Nothing

-- Short circuiting op
-- scBinOp :: MonadReader SemiSolverEnv m =>
--   (SExpr -> SExpr -> SExpr) ->
--   (SemiSExpr -> SemiSExpr) ->
--   (SemiSExpr -> SemiSExpr) ->
--   SemiSExpr -> SemiSExpr -> m SemiSExpr
-- scBinOp op tc fc x y =
--   case (x, y) of
--     (VBool True, _)  -> pure (tc y)
--     (VBool False, _) -> pure (fc y)
--     (_, VBool True)  -> pure (tc x)
--     (_, VBool False) -> pure (fc x)
--     _ -> do
--       tys <- asks typeDefs
--       pure (vSExpr TBool (op (semiSExprToSExpr tys TBool x)
--                              (semiSExprToSExpr tys TBool y)))

-- bAnd, bOr :: MonadReader SemiSolverEnv m => SemiSExpr -> SemiSExpr -> m SemiSExpr
-- bAnd = scBinOp S.and id (const (VBool False))
-- bOr  = scBinOp S.or (const (VBool True)) id

bOpMany :: MonadReader SemiSolverEnv m => Bool -> [SemiSExpr] -> m SemiSExpr
bOpMany opUnit els 
  | any (isBool absorb) els = pure (VBool absorb)
  | otherwise = do
      tys <- asks typeDefs
      -- strip out units and
      let nonUnits = [ semiSExprToSExpr tys TBool sv | sv <- els, not (isBool opUnit sv) ]
      if null nonUnits
        then pure (VBool opUnit)
        else pure (vSExpr TBool (op nonUnits))
  where
    isBool b (VBool b') = b == b'
    isBool _ _ = False
    op = if opUnit then S.andMany else S.orMany
    absorb = not opUnit

bAndMany, bOrMany :: MonadReader SemiSolverEnv m => [SemiSExpr] -> m SemiSExpr
bAndMany = bOpMany True
bOrMany  = bOpMany False

semiExecEq, semiExecNEq :: (Monad m, HasGUID m) => Type -> SemiSExpr -> SemiSExpr ->
          SemiSolverM m SemiSExpr
semiExecEq  = semiExecEqNeq True
semiExecNEq = semiExecEqNeq False

semiExecEqNeq :: (Monad m, HasGUID m) => Bool -> Type -> SemiSExpr -> SemiSExpr ->
                 SemiSolverM m SemiSExpr
semiExecEqNeq iseq ty sv1 sv2 =
  case (sv1, sv2) of
    (VValue v1, VValue v2) -> pure $ VBool (v1 `eqcmp` v2)
    (VOther v1, VOther v2) ->
      lift (vSExpr TBool <$> SE.symExecOp2 op ty (typedThing v1)
                                                 (typedThing v2))

    (VUnionElem l sv1', VUnionElem l' sv2') ->
      if l == l' then do
        tys <- asks typeDefs
        case typeAtLabel tys ty l of
          Just (_, ty') -> semiExecEqNeq iseq ty' sv1' sv2'
          _             -> panic "Missing label" [showPP l]
      else pure (VBool (not iseq))

    (VStruct flds1, VStruct flds2) -> do
      tys <- asks typeDefs
      opMany =<< zipWithM (go tys) flds1 flds2
      
    (VMaybe sv1', VMaybe sv2')
      | TMaybe ty' <- ty -> case (sv1', sv2') of
          (Nothing, Nothing)       -> pure (VBool iseq)
          (Just sv1'', Just sv2'') -> semiExecEqNeq iseq ty' sv1'' sv2''
          _ -> pure (VBool (not iseq))
    _ | Just svs1 <- SV.toList sv1
      , Just svs2 <- SV.toList sv2
      , Just elTy <- typeToElType ty ->
        if length svs1 /= length svs2
        then pure (VBool (not iseq))
        else opMany =<< zipWithM (semiExecEqNeq iseq elTy) svs1 svs2
    _ -> do
      tys <- asks typeDefs
      vSExpr TBool <$> lift (SE.symExecOp2 op TBool
                             (semiSExprToSExpr tys ty sv1)
                             (semiSExprToSExpr tys ty sv2))
  where
    (op, eqcmp, opMany) =
      if iseq
      then (Eq   , (==), bAndMany)
      else (NotEq, (/=), bOrMany)
    
    go tys (l, sv1') (l', sv2') =
      if l == l'
      then case typeAtLabel tys ty l of
             Just (_, ty') -> semiExecEqNeq iseq ty' sv1' sv2'
             _             -> panic "Missing label" [showPP l]
      else panic "Label mismatch" [showPP l, showPP l']
    
semiExecOp2 :: (Monad m, HasGUID m) => Op2 -> Type -> Type -> Type ->
               SemiSExpr -> SemiSExpr -> SemiSolverM m SemiSExpr
semiExecOp2 op _rty _ty _ty' (VValue v1) (VValue v2) = pure $ VValue (evalOp2 op v1 v2)
semiExecOp2 op rty   ty _ty' (VOther v1) (VOther v2) =
  lift (vSExpr rty <$> SE.symExecOp2 op ty (typedThing v1) (typedThing v2))
semiExecOp2 op rty ty1 ty2 sv1 sv2 =
  case op of
    IsPrefix -> unimplemented
    Drop     -> unimplemented
    Take     -> unimplemented

    Eq       -> semiExecEq ty1 sv1 sv2
    NotEq    -> semiExecNEq ty1 sv1 sv2
    
    -- sv1 is arr, sv2 is ix
    ArrayIndex
      | Just svs <- SV.toList sv1
      , VValue v <- sv2, Just ix <- V.valueToIntSize v
        -> pure (svs !! ix)
    Emit | Just svs <- SV.toList sv1 -> pure (VSequence True (svs ++ [sv2]))

    -- sv1 is map, sv2 is key
    MapLookup -> mapOp (VValue $ V.VMaybe Nothing) (sNothing . symExecTy)
                       (VMaybe . Just)             sJust 

    MapMember -> mapOp (VValue $ V.VBool False)        (const (S.bool False))
                       (const (VValue $ V.VBool True)) (\_ _ -> S.bool True)

    ArrayStream -> unimplemented

    _ -> def
  where
    def = do
      tys <- asks typeDefs
      vSExpr rty <$> lift (SE.symExecOp2 op ty1
                            (semiSExprToSExpr tys ty1 sv1)
                            (semiSExprToSExpr tys ty2 sv2))

    unimplemented = panic "semiEvalOp2: Unimplemented" [showPP op]

    -- sv1 is map, sv2 is key
    mapOp missing smissingf found sfound
      | Just els <- SV.toMappings sv1, VValue kv <- sv2
      , Just res <- mapLookupV missing found kv els
        = pure res
      -- Expand into if-then-else.
      | Just els <- SV.toMappings sv1
      , TMap kt vt <- ty1 = do
          tys <- asks typeDefs
          let symkv = semiSExprToSExpr tys kt sv2
              mk    = sfound (symExecTy vt) . semiSExprToSExpr tys vt
          pure (vSExpr rty (foldr (mapLookupS tys mk kt symkv sv2)
                             (smissingf vt)
                             els))
      | otherwise = def

    mapLookupV z _f  _kv  [] = Just z
    mapLookupV z f  kv ((VValue kv', el) : rest) =
      if kv == kv' then Just (f el) else mapLookupV z f kv rest
    mapLookupV _ _ _ _ = Nothing

    mapLookupS tys f kTy symkv skv (skv', sel) rest =
      case (skv, skv') of
        (VValue kv, VValue kv')
          -> if kv == kv' then f sel else rest
        _ -> S.ite (S.eq symkv (semiSExprToSExpr tys kTy skv')) (f sel) rest


semiExecOp3 :: (Monad m, HasGUID m) => Op3 -> Type -> Type ->
               SemiSExpr -> SemiSExpr -> SemiSExpr ->
               SemiSolverM m SemiSExpr
semiExecOp3 op _rty _ty (VValue v1) (VValue v2) (VValue v3) = pure $ VValue (evalOp3 op v1 v2 v3)
semiExecOp3 op rty   ty (VOther v1) (VOther v2) (VOther v3) =
  lift (vSExpr rty <$> SE.symExecOp3 op ty (typedThing v1) (typedThing v2) (typedThing v3))
semiExecOp3 MapInsert _rty _ty sv k v =
  case sv of
    VMap ms -> pure (VMap ((k, v) : ms))
    VValue (V.VMap m) ->
      -- FIXME: this picks an ordering
      let ms = [ (VValue k', VValue v') | (k', v') <- Map.toList m ]
      in pure (VMap ((k, v) : ms))
    _ -> panic "Unimplemented" [showPP MapInsert, show sv]

semiExecOp3 op        _    _   _         _ _ = panic "Unimplemented" [showPP op]

semiExecOpN :: (Monad m, HasGUID m, MonadIO m) => OpN -> Type -> [SemiSExpr] ->
               SemiSolverM m SemiSExpr
semiExecOpN op rty vs
  | Just vs' <- mapM unValue vs = do
      env <- asks interpEnv
      pure (VValue (evalOpN op vs' env))
  | Just vs' <- mapM unOther vs = lift (vSExpr rty <$> SE.symExecOpN op vs')
  where
    unValue (VValue v) = Just v
    unValue _ = Nothing

    unOther (VOther v) = Just (typedThing v)
    unOther _ = Nothing

-- unfold body of function.
semiExecOpN (CallF fn) _rty vs = do
  fdefs <- asks funDefs
  let (ps, e) = case Map.lookup fn fdefs of
        Just fdef | Def d <- fDef fdef -> (fParams fdef, d)
        _   -> panic "Missing function " [showPP fn]

  foldr (uncurry bindNameIn) (semiExecExpr e) (zip ps vs)

semiExecOpN op _rty _vs = panic "Unimplemented" [showPP op]

-- -----------------------------------------------------------------------------
-- Value -> SExpr

-- FIXME: move
-- Probably an error if they don't match?
typeAtLabel :: Map TName TDecl -> Type -> Label -> Maybe (UserType, Type)
typeAtLabel tys (TUser ut) l
  | Just TDecl { tDef = TUnion flds }  <- tdecl = (,) ut <$> lookup l flds
  | Just TDecl { tDef = TStruct flds } <- tdecl = (,) ut <$> lookup l flds
  where
    tdecl = Map.lookup (utName ut) tys
typeAtLabel _ _ _ = Nothing

valueToSExpr :: Map TName TDecl -> Type -> V.Value -> SExpr
valueToSExpr tys ty v =
  case v of
    V.VUInt n i ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    V.VSInt n i -> -- FIXME: correct?
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    V.VInteger i -> S.int i
    V.VBool b -> S.bool b
    V.VUnionElem l v' 
      | Just (ut, ty') <- typeAtLabel tys ty l
      -> S.fun (labelToField (utName ut) l) [go ty' v']

    -- FIXME: assumes the order is the same
    V.VStruct els
      | TUser ut <- ty
      , Just TDecl { tDef = TStruct flds } <- Map.lookup (utName ut) tys
      -> S.fun (typeNameToCtor (utName ut)) (zipWith goStruct els flds)

    V.VArray vs | TArray elty <- ty ->
      let sVals     = map (go elty) (Vector.toList vs)
          emptyArr = sArrayL (sEmptyL (symExecTy elty) (typeDefault elty)) -- FIXME, a bit gross?
          arr      = foldr (\(i, b) arr' -> S.store arr' (sSize i) b) emptyArr (zip [0..] sVals)
      in sArrayWithLength (symExecTy elty) arr (sSize (fromIntegral $ Vector.length vs))

    V.VMaybe mv | TMaybe ty' <- ty ->
      case mv of
        Nothing -> sNothing (symExecTy ty')
        Just v' -> sJust (symExecTy ty') (go ty' v')

    V.VMap m | TMap kt vt <- ty ->
      -- FIXME: breaks abstraction of maps
      sFromList (tTuple (symExecTy kt) (symExecTy vt))
                [ sTuple (go kt k) (go vt v') | (k, v') <- Map.toList m ]


    V.VStream {}                       -> unimplemented
    V.VBuilder vs
      | TBuilder elTy <- ty -> sFromList (symExecTy elTy) (reverse (map (go elTy) vs))

    -- FIXME: copied from semiSExprToSExpr
    V.VIterator vs -> case ty of
      -- FIXME: this breaks abstractions
      TIterator (TArray elTy) ->
        let emptyA = sEmptyL (symExecTy elTy) (typeDefault elTy)
            els    = [ (go sizeType k, go elTy v') | (k, v') <- vs ]
            arr    = foldr (\(i, v') arr' -> S.store arr' i v') emptyA els
        in case els of
          []             -> sArrayIterNew emptyA
          ((fstI, _) : _) -> S.fun "mk-ArrayIter" [arr, fstI]
      TIterator (TMap   _kt' _vt') -> panic "Unimplemented" []
      _ -> panic "Malformed iterator type" []

    _ -> unexpectedTy
  where
    unexpectedTy = panic "Unexpected type" [showPP v, showPP ty]

    go = valueToSExpr tys
    goStruct (l, e) (l', ty') | l == l' = go ty' e
    goStruct (l, _) (l', _) = panic "Mis-matched labels" [showPP l, showPP l']

    unimplemented = panic "Unimplemented" [showPP v]

-- -- Says whether a variable occurs in a SExpr, taking into account let binders.
-- freeInSExpr :: String -> SExpr -> Bool
-- freeInSExpr n = getAny . go
--   where
--     go (S.Atom a) = Any (a == n)
--     go (S.List [S.Atom "let", S.List es, e]) =
--       let (Any stop, occ) = goL es
--       in if stop then occ else occ <> go e

--     go (S.List es) = foldMap go es

--     goL (S.List [S.Atom a, be] : es) = (Any (a == n), go be) <> goL es
--     goL [] = (mempty, mempty)
--     goL _ = panic "Malformed let binding" []

-- freeInSemiSExpr :: String -> SemiSExpr -> Bool
-- freeInSemiSExpr n = getAny . foldMap (Any . freeInSExpr n)

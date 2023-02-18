{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeApplications #-}
{-# Language RankNTypes #-}
{-# Language TypeOperators #-}

module Talos.Strategy.What4.Exprs(
    toWhat4Expr
  , evalByteSet
  , valueToConcrete
  , groundToValue
) where

import           Control.Monad.IO.Class
import qualified Data.ByteString                 as BS

import qualified Data.BitVector.Sized as BVS
import qualified Data.Map                        as Map
import           Data.Word (Word8)

import           Data.Parameterized.NatRepr
import qualified Data.Parameterized.Context      as Ctx
import           Data.Parameterized.Some
import qualified Data.Text                       as T
import qualified What4.Interface                 as W4
import qualified What4.Concrete                  as W4

import           Daedalus.Core                   hiding (streamOffset)
import qualified Daedalus.Core.Type              as I
import qualified Daedalus.Core.Basics            as I
import qualified Daedalus.Value                  as I
import           Daedalus.Panic
import           Daedalus.PP
import           Talos.Strategy.Monad

import           Talos.Strategy.What4.SymM
import           Talos.Strategy.What4.Types
import Control.Monad
import qualified What4.Expr.GroundEval as W4



nameToSymbol :: Name -> W4.SolverSymbol
nameToSymbol nm = case nameText nm of
  Just nm' -> W4.safeSymbol (T.unpack nm')
  Nothing -> W4.emptySymbol

nameToRepr :: Name -> W4SolverT sym m (Some (W4.BaseTypeRepr))
nameToRepr nm = typeToRepr (nameType nm)

nameToVar :: Name -> W4SolverT sym m (Some (W4.BoundVar sym))
nameToVar nm = withSym $ \sym -> do
  Some repr <- nameToRepr nm
  Some <$> (liftIO $ W4.freshBoundVar sym (nameToSymbol nm) repr)

withBoundVars ::
  [Name] -> 
  W4SolverT sym m a ->
  W4SolverT sym m (Some (Ctx.Assignment (W4.BoundVar sym)), a)
withBoundVars [] f = do
  a <- f
  return (Some Ctx.empty, a)
withBoundVars (nm : nms) f = withSym $ \sym -> do
  Some bv <- nameToVar nm
  bindVarIn nm (W4.varExpr sym bv) $ do
    (Some bvs, a) <- withBoundVars nms f
    return $ (Some (bvs Ctx.:> bv), a)



mkSymFn :: Fun Expr -> W4SolverT sym m (SomeSymFn sym)
mkSymFn fn = withSym $ \sym -> do
  let ret_raw = fnameType (fName fn)
  Some ret <- typeToRepr ret_raw
  case fDef fn of
    Def e -> do
      (Some args, body) <- withBoundVars (fParams fn) $ toWhat4Expr ret_raw ret e
      symFn <- liftIO $ W4.definedFn sym nm args body W4.UnfoldConcrete
      return $ SomeSymFn symFn
    External -> do
      Some args <- Ctx.fromList <$> mapM (\x -> nameToRepr x) (fParams fn) 
      symFn <- liftIO $ W4.freshTotalUninterpFn sym nm args ret
      return $ SomeSymFn symFn
  where
    nm = W4.safeSymbol (T.unpack (fnameText (fName fn)))

lookupFn ::
  FName -> 
  W4SolverT sym m (SomeSymFn sym, Fun Expr)
lookupFn nm = withFNameCache nm $ do
  defs <- getFunDefs
  case Map.lookup nm defs of
    Just def -> do
      symFn <- mkSymFn def
      return (symFn, def)
    Nothing -> panic "lookupFn: missing function definition" [showPP nm]

{-
lookupFn :: 
  FName -> 
  W4StratT sym m (SomeSymFn sym, Fun Expr)
lookupFn nm = do
  ref <- asks fnCache
  cache <- liftIO $ IO.readIORef ref
  case Map.lookup nm cache of
    Just fn -> return fn
    Nothing -> do
      defs <- getFunDefs
      case Map.lookup nm defs of
        Just def -> do
          symFn <- mkSymFn def
          liftIO $ IO.modifyIORef' ref (Map.insert nm (symFn, def))
          return (symFn, def)
        Nothing -> panic "lookupFn: missing function definition" [showPP nm]
-}

data SymBV sym w = SymBV (W4.SymExpr sym (W4.BaseBVType w))

byteToBV :: W4.IsSymExprBuilder sym => sym -> Word8 -> IO (SymBV sym 8)
byteToBV sym w8 = SymBV <$> W4.bvLit sym (knownNat @8) (BVS.mkBV (knownNat @8) (fromIntegral w8))

-- FIXME: endianness?
bsToBV :: W4.IsSymExprBuilder sym => sym -> [Word8] -> IO (Some (SymBV sym))
bsToBV _sym [] = panic "Empty ByteString" []
bsToBV sym [w8] = Some <$> byteToBV sym w8
bsToBV sym (w8 : ws) = do
  SymBV w8_bv <- byteToBV sym w8
  Some (SymBV bv) <- bsToBV sym ws
  W4.BaseBVRepr{} <- return $ W4.exprType w8_bv
  W4.BaseBVRepr{} <- return $ W4.exprType bv
  (Some .  SymBV) <$> W4.bvConcat sym w8_bv bv

-- Core translation

toWhat4Expr :: I.Type -> W4.BaseTypeRepr tp -> Expr -> W4SolverT sym m (W4.SymExpr sym tp)
toWhat4Expr t_raw t e = withSym $ \sym -> case (t, e) of
  -- Core
  (_, Var nm) -> do
    Some e' <- getVar nm
    case testEquality t (W4.exprType e') of
      Just Refl -> return e'
      Nothing -> panic "Unexpected variable type" [showPP nm, show t]
  (_, PureLet nm e1 e2) -> do
    (Some t1) <- typeToRepr (nameType nm)
    e1Sym <- toWhat4Expr (nameType nm) t1 e1
    bindVarIn nm e1Sym $ toWhat4Expr t_raw t e2
  (_, ECase c) -> do
    fallthrough <- liftIO $ W4.freshConstant sym W4.emptySymbol t
    evalCase c (\x y z -> matchesPat x y z) (\e_ -> toWhat4Expr t_raw t e_) fallthrough
  -- FIXME: missing Struct
  -- Ap0
  (W4.BaseStructRepr Ctx.Empty, Ap0 Unit) -> liftIO $ W4.mkStruct sym Ctx.empty
  (W4.BaseIntegerRepr, Ap0 (IntL i _)) -> liftIO $ W4.intLit sym i
  (W4.BaseBoolRepr, Ap0 (BoolL b)) -> case b of
    True -> return $ W4.truePred sym
    False -> return $ W4.falsePred sym
  (W4.BaseBVRepr w, Ap0 (ByteArrayL bs)) -> do
    Some (SymBV bv) <- liftIO $ bsToBV sym (BS.unpack bs)
    W4.BaseBVRepr w' <- return $ W4.exprType bv
    case testEquality w w' of
      Just Refl -> return bv
      Nothing -> panic "Mismatched bitvector size" [showPP e]
  ((W4.BaseArrayRepr (Ctx.Empty Ctx.:> rkey) rvalue), Ap0 (MapEmpty tfrom to)) -> do
    Some rkey' <- typeToRepr tfrom
    Some rvalue' <- typeToRepr to
    case (testEquality rkey rkey', testEquality rvalue rvalue' ) of
      (Just Refl, Just Refl) -> liftIO $ W4.freshConstant sym W4.emptySymbol t
      _ -> panic "Unexpected map type" [showPP e]
  (BaseMaybeRepr tp, Ap0 (ENothing t')) -> do
    Some tp' <- typeToRepr t'
    case testEquality tp tp' of
      Just Refl -> do
        fallthrough <- liftIO $ W4.freshConstant sym W4.emptySymbol tp
        liftIO $ mkBaseMaybe sym fallthrough (W4.falsePred sym) 
      Nothing -> panic "Unexpected maybe type" [showPP e]
  -- Ap1
  (BaseUnionRepr repr, Ap1 (InUnion ut lbl) e1) | [] <- utTyArgs ut, [] <- utNumArgs ut -> do
    flds <- unionFields (TUser ut)
    (Some idx, t_e1) <- getFieldIndex flds  (Ctx.size repr) lbl
    let e1_repr = repr Ctx.! idx
    e1' <- toWhat4Expr t_e1 e1_repr e1
    liftIO $ mkBaseUnion sym repr e1' idx
  -- FIXME: todo
  -- Ap2
  (W4.BaseBoolRepr, Ap2 relOp e1 e2) -> do
    (Some inner_repr) <- typeToRepr (I.typeOf e1) 
    e1' <- toWhat4Expr (I.typeOf e1) inner_repr e1
    e2' <- toWhat4Expr (I.typeOf e2) inner_repr e2
    case relOp of
      Eq -> liftIO $ W4.isEq sym e1' e2'
      NotEq -> liftIO $ (W4.isEq sym e1' e2' >>= W4.notPred sym)
      -- FIXME: should use fixed-width bitvectors instead of integers?
      Lt | W4.BaseIntegerRepr <- inner_repr -> liftIO $ W4.intLt sym e1' e2'
      Leq | W4.BaseIntegerRepr <- inner_repr -> liftIO $ W4.intLe sym e1' e2'
      Lt | W4.BaseBVRepr{} <- inner_repr, TUInt{} <- I.typeOf e1, TUInt{} <- I.typeOf e2 -> liftIO $ W4.bvUlt sym e1' e2'
      Lt | W4.BaseBVRepr{} <- inner_repr, TSInt{} <- I.typeOf e1, TSInt{} <- I.typeOf e2 -> liftIO $ W4.bvUlt sym e1' e2'
      _ -> panic "Unsupported comparison" [showPP e]
  (W4.BaseIntegerRepr, Ap2 intOp e1 e2) -> do
    e1' <- toWhat4Expr (I.typeOf e1) W4.BaseIntegerRepr e1
    e2' <- toWhat4Expr (I.typeOf e2) W4.BaseIntegerRepr e2
    case intOp of
      Add -> liftIO $ W4.intAdd sym e1' e2'
      Mul -> liftIO $ W4.intMul sym e1' e2'
      _ -> panic "Unsupported integer operation" [showPP e]
  (W4.BaseBVRepr w, Ap2 bvOp e1 e2) -> do
    e1' <- toWhat4Expr (I.typeOf e1) (W4.BaseBVRepr w) e1
    e2' <- toWhat4Expr (I.typeOf e2) (W4.BaseBVRepr w) e2
    case bvOp of
      Add -> liftIO $ W4.bvAdd sym e1' e2'
      Mul -> liftIO $ W4.bvMul sym e1' e2'
      _ -> panic "Unsupported bitvector operation" [showPP e]
  (_, ApN (CallF nm) args) -> do
    (SomeSymFn fn, fn_raw) <- lookupFn nm
    let argTs = map nameType (fParams fn_raw)
    case testEquality (W4.fnReturnType fn) t of
      Just Refl | length argTs == length args -> do
        let args_typs = zip args argTs
        args' <- toWhat4ExprList (W4.fnArgTypes fn) args_typs
        liftIO $ W4.applySymFn sym fn args'
      _ -> panic "Mismatched function return type" [showPP e]
  _ -> panic "Unsupported type" [showPP e]

evalByteSet :: ByteSet -> W4.SymExpr sym (W4.BaseBVType 8) -> W4SolverT sym m (W4.Pred sym)
evalByteSet bs bv = withSym $ \sym -> do
  let mk_bv e = toWhat4Expr (I.TUInt (I.TSize 8)) (W4.BaseBVRepr (knownNat @8)) e
  case bs of
    SetAny -> return $ W4.truePred sym
    SetSingle e -> do
      e_sym <- mk_bv e
      liftIO $ W4.isEq sym e_sym bv
    SetRange lo hi -> do
      lo_sym <- mk_bv lo
      hi_sym <- mk_bv hi
      lo_p  <- liftIO $ W4.bvUle sym lo_sym bv
      hi_p  <- liftIO $ W4.bvUle sym bv hi_sym
      liftIO $ W4.andPred sym lo_p hi_p
    SetComplement bs' -> do
      p <- evalByteSet bs' bv
      liftIO $ W4.notPred sym p
    SetUnion bs1 bs2 -> do
      p1 <- evalByteSet bs1 bv
      p2 <- evalByteSet bs2 bv
      liftIO $ W4.orPred sym p1 p2
    SetIntersection bs1 bs2 -> do
      p1 <- evalByteSet bs1 bv
      p2 <- evalByteSet bs2 bv
      liftIO $ W4.andPred sym p1 p2
    SetLet x e b -> do
      (Some t1) <- typeToRepr (nameType x)
      e1Sym <- toWhat4Expr (nameType x) t1 e
      bindVarIn x e1Sym $ evalByteSet b bv
    SetCall f args -> do
      (SomeSymFn fn, e) <- lookupFn f
      let argTs = map I.typeOf args
      case testEquality (W4.fnReturnType fn) W4.BaseBoolRepr of
        Just Refl | length argTs == length args -> do
          let args_typs = zip args argTs
          args' <- toWhat4ExprList (W4.fnArgTypes fn) args_typs
          liftIO $ W4.applySymFn sym fn args'
        _ -> panic "Mismatched function return type" [showPP e]
    SetCase b ->
      evalCase b (\x y z -> matchesPat x y z) (\bs_ -> evalByteSet bs_ bv) (W4.falsePred sym)

evalCase ::
  Case a ->
  (forall var_tp. I.Type -> Pattern -> W4.SymExpr sym var_tp -> W4SolverT_ sym m (W4.Pred sym)) ->
  (a -> W4SolverT_ sym m (W4.SymExpr sym tp)) ->
  W4.SymExpr sym tp ->
  W4SolverT sym m (W4.SymExpr sym tp)
evalCase c eval_pat eval_case fallthrough = withSym $ \sym -> do
  let var = I.caseVar c
  Some e' <- getVar var
  foldM (\x (pat,body_raw) -> do
    matches <- eval_pat (nameType var) pat e'
    body <- eval_case body_raw
    liftIO $ W4.baseTypeIte sym matches body x) fallthrough (I.casePats c)

-- byteSetToWhat4 :: ByteSet -> W4SolverT sym m (W4.SymFn sym (Ctx.EmptyCtx Ctx.::> W4.BaseBVType 8) W4.BaseBoolType)
-- liftIO $ W4.definedFn sym W4.emptySymbol (Ctx.empty Ctx.:> bv_var) body W4.UnfoldConcrete

unionFields :: I.Type -> W4SolverT sym m ([(Label,Type)])
unionFields t@(TUser ut) | [] <- utTyArgs ut, [] <- utNumArgs ut = do
  tdefs <- getTypeDefs
  case Map.lookup (utName ut) tdefs of
    Just tdecl -> case tDef tdecl of
      TUnion flds -> return flds
      _ -> panic "unionFields: unexpected type" [showPP t]
    _ -> panic "unionFields: unexpected type" [showPP t]
unionFields t = panic "unionFields: unexpected type" [showPP t]

toWhat4ExprList ::
  Ctx.Assignment W4.BaseTypeRepr tps -> 
  [(Expr, Type)] -> 
  W4SolverT sym m (Ctx.Assignment (W4.SymExpr sym) tps)
toWhat4ExprList Ctx.Empty [] = return Ctx.Empty
toWhat4ExprList (tps Ctx.:> tp) ((e, t_raw) : exprs) = do
  e' <- toWhat4Expr t_raw tp e
  exprs' <- toWhat4ExprList tps exprs
  return $ exprs' Ctx.:> e'
toWhat4ExprList _ _ = panic "toWhat4ExprList: mismatch" []

matchesPat ::
  I.Type ->
  Pattern -> 
  W4.SymExpr sym tp -> 
  W4SolverT sym m (W4.Pred sym)
matchesPat tp_raw pat e = withSym $ \sym -> do
  case (W4.exprType e, pat) of
    (W4.BaseBoolRepr, PBool True) -> return e
    (W4.BaseBoolRepr, PBool False) -> liftIO $ W4.notPred sym e
    (BaseMaybeRepr{}, PNothing) -> do
      (_, p) <- liftIO $ getBaseMaybe sym e
      liftIO $ W4.notPred sym p
    (BaseMaybeRepr{}, PJust) -> do
      (_, p) <- liftIO $ getBaseMaybe sym e
      return p
    (W4.BaseIntegerRepr, PNum i) -> do
      i_expr <- liftIO $ W4.intLit sym i
      liftIO $ W4.isEq sym e i_expr
    (W4.BaseBVRepr w, PBytes bs) -> do
      Some (SymBV bv) <- liftIO $ bsToBV sym (BS.unpack bs)
      W4.BaseBVRepr w' <- return $ W4.exprType bv
      case testEquality w w' of
        Just Refl -> liftIO $ W4.isEq sym e bv
        Nothing -> return $ W4.falsePred sym
    (BaseUnionRepr reprs, PCon l) -> do
      flds <- unionFields tp_raw
      (Some idx, _) <- getFieldIndex flds (Ctx.size reprs) l
      snd <$> (liftIO $ getBaseUnion sym e idx)
    (_, PAny) -> return $ W4.truePred sym
    _ -> panic "Unsupported pattern/expression combination" []

groundToValue :: W4.BaseTypeRepr tp -> W4.GroundValue tp -> W4SolverT sym m (I.Value)
groundToValue tp gv = case tp of
  W4.BaseIntegerRepr -> return $ I.VInteger gv
  _ -> panic "Unsupported ground value" [show tp]

-- concrete values
valueToConcrete :: W4.BaseTypeRepr tp -> I.Value -> Maybe (W4.ConcreteVal tp)
valueToConcrete t v_outer = case (v_outer, t) of
  (I.VUInt _ i, W4.BaseBVRepr nr) -> do
    (W4.ConcreteBV nr) <$> BVS.mkBVUnsigned nr i
  (I.VSInt _ i, W4.BaseBVRepr nr) -> do
    (W4.ConcreteBV nr) <$> BVS.mkBVSigned nr i
  (I.VInteger i, W4.BaseIntegerRepr) -> do
    return $ W4.ConcreteInteger i
  (I.VBool b, W4.BaseBoolRepr) -> do
    return $ W4.ConcreteBool b
  (I.VMaybe v,BaseMaybeRepr repr) -> do
    case v of
      Nothing -> do
        default_ <- defaultConcrete repr
        return $ W4.ConcreteStruct (Ctx.Empty Ctx.:> W4.ConcreteBool False Ctx.:> default_)
      Just v' -> do
        v'' <- valueToConcrete repr v'
        return $ W4.ConcreteStruct (Ctx.Empty Ctx.:> W4.ConcreteBool True Ctx.:> v'')
  (I.VMap m, W4.BaseArrayRepr (Ctx.Empty Ctx.:> krepr) vrepr)-> do
      default_ <- defaultConcrete vrepr
      let (ks,vs) = unzip (Map.toList m)
      ks' <- mapM (\v' -> Ctx.singleton <$> valueToConcrete krepr v') ks
      vs' <- mapM (\v' -> valueToConcrete vrepr v') vs
      let m' = Map.fromList (zip ks' vs')
      return $ W4.ConcreteArray (Ctx.Empty Ctx.:> krepr) default_ m'
  _ -> Nothing


defaultConcrete :: W4.BaseTypeRepr tp -> Maybe (W4.ConcreteVal tp)
defaultConcrete repr = case repr of
  W4.BaseIntegerRepr -> Just $ W4.ConcreteInteger 0
  W4.BaseBoolRepr -> Just $ W4.ConcreteBool False
  W4.BaseBVRepr nr -> Just $ W4.ConcreteBV nr (BVS.mkBV nr 0)
  _ -> Nothing
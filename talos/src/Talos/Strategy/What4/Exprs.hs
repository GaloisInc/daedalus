{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeApplications #-}

module Talos.Strategy.What4.Exprs(
    toWhat4Expr
  , valueToConcrete
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



nameToSymbol :: Name -> W4.SolverSymbol
nameToSymbol nm = case nameText nm of
  Just nm' -> W4.safeSymbol (T.unpack nm')
  Nothing -> W4.emptySymbol

nameToRepr :: SymM sym m => Name -> m (Some (W4.BaseTypeRepr))
nameToRepr nm = typeToRepr (nameType nm)

nameToVar :: SymM sym m => Name -> m (Some (W4.BoundVar sym))
nameToVar nm = withSym $ \sym -> do
  Some repr <- nameToRepr nm
  Some <$> (liftIO $ W4.freshBoundVar sym (nameToSymbol nm) repr)

withBoundVars ::
  SymM sym m => 
  [Name] -> 
  m a ->
  m (Some (Ctx.Assignment (W4.BoundVar sym)), a)
withBoundVars [] f = do
  a <- f
  return (Some Ctx.empty, a)
withBoundVars (nm : nms) f = withSym $ \sym -> do
  Some bv <- nameToVar nm
  bindVarIn nm (W4.varExpr sym bv) $ do
    (Some bvs, a) <- withBoundVars nms f
    return $ (Some (bvs Ctx.:> bv), a)



mkSymFn :: SymM sym m => Fun Expr -> m (SomeSymFn sym)
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
  SymM sym m => 
  FName -> 
  m (SomeSymFn sym, Fun Expr)
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

toWhat4Expr :: SymM sym m => I.Type -> W4.BaseTypeRepr tp -> Expr -> m (W4.SymExpr sym tp)
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
    let var = I.caseVar c
    Some e' <- getVar var
    cases <- mapM (\x -> matchesPat t t_raw e' x) (I.casePats c)
    fallthrough <- liftIO $ W4.freshConstant sym W4.emptySymbol t
    liftIO $ foldM (\x (p,body) -> W4.baseTypeIte sym p body x) fallthrough cases
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

unionFields :: SymM sym m => I.Type -> m ([(Label,Type)])
unionFields t@(TUser ut) | [] <- utTyArgs ut, [] <- utNumArgs ut = do
  tdefs <- getTypeDefs
  case Map.lookup (utName ut) tdefs of
    Just tdecl -> case tDef tdecl of
      TUnion flds -> return flds
      _ -> panic "unionFields: unexpected type" [showPP t]
    _ -> panic "unionFields: unexpected type" [showPP t]
unionFields t = panic "unionFields: unexpected type" [showPP t]

toWhat4ExprList ::
  SymM sym m =>
  Ctx.Assignment W4.BaseTypeRepr tps -> 
  [(Expr, Type)] -> 
  m (Ctx.Assignment (W4.SymExpr sym) tps)
toWhat4ExprList Ctx.Empty [] = return Ctx.Empty
toWhat4ExprList (tps Ctx.:> tp) ((e, t_raw) : exprs) = do
  e' <- toWhat4Expr t_raw tp e
  exprs' <- toWhat4ExprList tps exprs
  return $ exprs' Ctx.:> e'
toWhat4ExprList _ _ = panic "toWhat4ExprList: mismatch" []

matchesPat ::
  SymM sym m =>
  W4.BaseTypeRepr tp_body ->
  I.Type ->
  W4.SymExpr sym tp -> 
  (Pattern, Expr) -> 
  m (W4.Pred sym, W4.SymExpr sym tp_body)
matchesPat tp_body tp_raw e (pat, body) = withSym $ \sym -> do
  bodyE <- toWhat4Expr tp_raw tp_body body
  p <- case (W4.exprType e, pat) of
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
  return (p, bodyE)

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
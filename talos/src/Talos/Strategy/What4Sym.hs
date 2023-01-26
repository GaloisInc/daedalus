{-# Language OverloadedStrings #-}
{-# Language GADTs #-}
{-# Language GeneralisedNewtypeDeriving #-}
{-# Language RankNTypes #-}
{-# Language PatternSynonyms #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language KindSignatures #-}
{-# Language ScopedTypeVariables #-}
{-# Language ViewPatterns #-}

-- FIXME: much of this file is similar to Synthesis, maybe factor out commonalities
module Talos.Strategy.What4Sym (randDFS, randRestart, randMaybeT, mkStrategyFun) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString                 as BS
import           Data.Functor.Identity           (Identity (Identity))
import qualified Data.Map                        as Map

import qualified What4.Interface                 as W4
import qualified What4.Concrete                 as W4
import           Data.Parameterized.NatRepr
import qualified Data.Parameterized.Context      as Ctx
import qualified Data.BitVector.Sized as BV

import           Daedalus.Core                   hiding (streamOffset)
import qualified Daedalus.Core.Semantics.Env     as I
import qualified Daedalus.Core.Semantics.Expr    as I
import qualified Daedalus.Core.Semantics.Grammar as I
import qualified Daedalus.Core.Type              as I
import qualified Daedalus.Core.Basics              as I
import qualified Daedalus.Value                  as I
import           Daedalus.Panic
import           Daedalus.PP

import           Talos.Analysis.Exported         (ExpCallNode (..), ExpSlice)
import           Talos.Analysis.Slice
import           Talos.Strategy.DFST
import           Talos.Strategy.Monad
import           Talos.SymExec.Path
import Data.Parameterized.Some
import Data.Word (Word8)
import qualified Data.BitVector.Sized as BVS
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as T
import qualified Data.IORef as IO

-- ----------------------------------------------------------------------------------------
-- Backtracking random strats

-- FIXME: maybe unify these into a single parameterised strat 'rand backtrack=dfs ...'
randDFS :: Strategy
randDFS = 
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = \ptag sl -> trivialStratGen . lift $
                                   runDFST (go ptag sl) (return . Just) (return Nothing)
           }
    name  = "rand-dfs"
    descr = "Simple depth-first random generation"
    
    go :: ProvenanceTag -> ExpSlice -> DFST (Maybe SelectedPath) StrategyM SelectedPath
    go ptag sl = mkStrategyFun ptag sl

-- ----------------------------------------------------------------------------------------
-- Restarting strat (restart-on-failure)

randRestart :: Strategy
randRestart = 
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = randRestartStrat
           }
    name  = "rand-restart"
    descr = "Restart on failure with random selection"

restartBound :: Int
restartBound = 1000

randRestartStrat :: ProvenanceTag -> ExpSlice -> StratGen
randRestartStrat ptag sl = trivialStratGen . lift $ go restartBound
  where
    go 0 = pure Nothing
    go n = do
      m_p <- once
      case m_p of
        Just {} -> pure m_p
        Nothing -> go (n - 1)
    
    once :: StrategyM (Maybe SelectedPath)
    once = runRestartT (mkStrategyFun ptag sl) (return . Just) (return Nothing)

-- ----------------------------------------------------------------------------------------
-- Local backtracking, restart

randMaybeT :: Strategy
randMaybeT = 
  Strategy { stratName  = name
           , stratDescr = descr
           , stratParse = pure inst
           }
  where
    inst = StrategyInstance
           { siName = name
           , siDescr = descr
           , siFun   = randMaybeStrat
           }
    name  = "rand-restart-local-bt"
    descr = "Backtrack locally on failure, restart on (global) failure with random selection"

randMaybeStrat :: ProvenanceTag -> ExpSlice -> StratGen
randMaybeStrat ptag sl = trivialStratGen . lift $ go restartBound
  where
    go 0 = pure Nothing
    go n = do
      m_p <- once
      case m_p of
        Just {} -> pure m_p
        Nothing -> go (n - 1)
    
    once :: StrategyM (Maybe SelectedPath)
    once = runMaybeT (mkStrategyFun ptag sl)
  
-- ----------------------------------------------------------------------------------------

-- Maybe type encoded as base type

type BaseMaybeType tp = W4.BaseStructType (Ctx.EmptyCtx Ctx.::> W4.BaseBoolType Ctx.::> tp)

getBaseMaybe :: 
  W4.IsSymExprBuilder sym =>
  sym -> 
  W4.SymExpr sym (BaseMaybeType tp) -> 
  IO (W4.Pred sym, W4.SymExpr sym tp)
getBaseMaybe sym e = do
  p <- W4.structField sym e Ctx.i1of2
  e' <- W4.structField sym e Ctx.i2of2
  return (p, e')

mkBaseMaybe ::
  W4.IsSymExprBuilder sym =>
  sym ->
  W4.Pred sym ->
  W4.SymExpr sym tp ->
  IO (W4.SymExpr sym (BaseMaybeType tp))  
mkBaseMaybe sym p e = W4.mkStruct sym (Ctx.Empty Ctx.:> p Ctx.:> e)

pattern BaseMaybeRepr :: () => (tp_outer ~ BaseMaybeType tp_inner) => W4.BaseTypeRepr tp_inner -> W4.BaseTypeRepr tp_outer
pattern BaseMaybeRepr x <- W4.BaseStructRepr (Ctx.Empty Ctx.:> W4.BaseBoolRepr Ctx.:> x)

type LabelType = W4.BaseStringType W4.Unicode

labelRepr :: W4.BaseTypeRepr LabelType
labelRepr = W4.BaseStringRepr W4.UnicodeRepr

type StringTupleType tp = W4.BaseStructType (Ctx.EmptyCtx Ctx.::> LabelType Ctx.::> tp)
type StringTuple sym tp = W4.SymExpr sym (StringTupleType tp)

pattern StringTupleRepr :: () => (tp_outer ~ StringTupleType tp) => W4.BaseTypeRepr tp -> W4.BaseTypeRepr tp_outer
pattern StringTupleRepr tp <- ((\x -> asStringTupleProof x) -> Just (StringTupleReprProof tp))

stringTupleRepr :: W4.BaseTypeRepr tp -> W4.BaseTypeRepr (StringTupleType tp)
stringTupleRepr repr = W4.BaseStructRepr (Ctx.Empty Ctx.:> W4.BaseStringRepr W4.UnicodeRepr Ctx.:> repr)

data StringTupleReprProof (tp :: W4.BaseType) where
  StringTupleReprProof :: W4.BaseTypeRepr tp -> StringTupleReprProof (StringTupleType tp)

asStringTupleProof :: W4.BaseTypeRepr tp -> Maybe (StringTupleReprProof tp)
asStringTupleProof tp = case tp of
  W4.BaseStructRepr (Ctx.Empty Ctx.:> W4.BaseStringRepr W4.UnicodeRepr Ctx.:> tp2) -> Just (StringTupleReprProof tp2)
  _ -> Nothing

type family StringTupleCtx (tps :: Ctx.Ctx W4.BaseType) :: Ctx.Ctx W4.BaseType
type instance StringTupleCtx (tps Ctx.::> tp) = StringTupleCtx tps Ctx.::> StringTupleType tp
type instance StringTupleCtx Ctx.EmptyCtx = Ctx.EmptyCtx

stringTupleCtxRepr :: Ctx.Assignment W4.BaseTypeRepr tps -> Ctx.Assignment W4.BaseTypeRepr (StringTupleCtx tps)
stringTupleCtxRepr (ctx Ctx.:> tp) = stringTupleCtxRepr ctx Ctx.:> stringTupleRepr tp


data StringTupleCtxReprProof (tps :: Ctx.Ctx W4.BaseType) where
  StringTupleCtxReprProof :: Ctx.Assignment W4.BaseTypeRepr tps -> StringTupleCtxReprProof (StringTupleCtx tps)

asStringTupleCtxProof :: Ctx.Assignment W4.BaseTypeRepr tps -> Maybe (StringTupleCtxReprProof tps)
asStringTupleCtxProof (ctx Ctx.:> tp) = do
  StringTupleReprProof tp' <- asStringTupleProof tp
  StringTupleCtxReprProof ctx' <- asStringTupleCtxProof ctx
  return $ StringTupleCtxReprProof (ctx' Ctx.:> tp') 
asStringTupleCtxProof Ctx.Empty = return $ StringTupleCtxReprProof Ctx.Empty

pattern StringTupleCtxRepr :: () => (tp_outer ~ StringTupleCtx tps) => Ctx.Assignment W4.BaseTypeRepr tps -> Ctx.Assignment W4.BaseTypeRepr tp_outer
pattern StringTupleCtxRepr tps <- ((\x -> asStringTupleCtxProof x) -> Just (StringTupleCtxReprProof tps))

-- Required to prove that StringTuples preserves length
stringTupleSize ::
  Ctx.Size (StringTupleCtx tps) -> Ctx.Size tps
stringTupleSize sz = unsafeCoerce sz

-- Union type encoded as base type
-- Each component type is tagged with a string and the final value
-- indicates which type is indicated
type BaseUnionType tps = W4.BaseStructType (StringTupleCtx tps Ctx.::> LabelType)
type BaseUnion sym tps = W4.SymExpr sym (BaseUnionType tps)
type BaseUnionRepr tps = W4.BaseTypeRepr (BaseUnionType tps)

pattern BaseUnionRepr :: () => (tp_outer ~ BaseUnionType tps) => Ctx.Assignment W4.BaseTypeRepr tps -> W4.BaseTypeRepr tp_outer
pattern BaseUnionRepr tps <- ((\x -> asBaseUnion x) -> Just (BaseUnionReprProof tps))

baseUnionRepr :: Ctx.Assignment W4.BaseTypeRepr tps -> BaseUnionRepr tps
baseUnionRepr ctx = W4.BaseStructRepr (stringTupleCtxRepr ctx Ctx.:> labelRepr)


data BaseUnionReprProof (tp :: W4.BaseType) where
  BaseUnionReprProof :: Ctx.Assignment W4.BaseTypeRepr tps -> BaseUnionReprProof (BaseUnionType tps)

asBaseUnion :: W4.BaseTypeRepr tp -> Maybe (BaseUnionReprProof tp)
asBaseUnion (W4.BaseStructRepr (vs Ctx.:> W4.BaseStringRepr W4.UnicodeRepr)) = do
  (StringTupleCtxReprProof t) <- asStringTupleCtxProof vs
  return $ BaseUnionReprProof t
asBaseUnion _ = Nothing

----
-- Record is just a labeled struct
type BaseRecordType tps = W4.BaseStructType (StringTupleCtx tps)
type BaseRecord sym tps = W4.SymExpr sym (BaseRecordType tps)
type BaseRecordRepr tps = W4.BaseTypeRepr (BaseRecordType tps)

baseRecordRepr :: Ctx.Assignment W4.BaseTypeRepr tps -> BaseRecordRepr tps
baseRecordRepr ctx = W4.BaseStructRepr (stringTupleCtxRepr ctx)

data BaseRecordReprProof (tp :: W4.BaseType) where
  BaseRecordReprProof :: Ctx.Assignment W4.BaseTypeRepr tps -> BaseRecordReprProof (BaseRecordType tps)

asBaseRecord:: W4.BaseTypeRepr tp -> Maybe (BaseRecordReprProof tp)
asBaseRecord (W4.BaseStructRepr vs) = do
  (StringTupleCtxReprProof t) <- asStringTupleCtxProof vs
  return $ BaseRecordReprProof t
asBaseRecord _ = Nothing

pattern BaseRecordRepr :: () => (tp_outer ~ BaseRecordType tps) => Ctx.Assignment W4.BaseTypeRepr tps -> W4.BaseTypeRepr tp_outer
pattern BaseRecordRepr tps <- ((\x -> asBaseRecord x) -> Just (BaseRecordReprProof tps))

data LabeledExpr sym tp = LabeledExpr (W4.SymExpr sym LabelType) (W4.SymExpr sym tp)  

mkStringTuple ::
  W4.IsSymExprBuilder sym =>
  sym ->
  LabeledExpr sym tp ->
  IO (StringTuple sym tp)
mkStringTuple sym (LabeledExpr lbl e) = W4.mkStruct sym (Ctx.empty Ctx.:> lbl Ctx.:> e)

mkStringTuples ::
  W4.IsSymExprBuilder sym =>
  sym ->
  Ctx.Assignment (LabeledExpr sym) tps ->
  IO (Ctx.Assignment (W4.SymExpr sym) (StringTupleCtx tps))
mkStringTuples sym (exprs Ctx.:> e) = do
  tuple <- mkStringTuple sym e
  tuples <- mkStringTuples sym exprs
  return $ tuples Ctx.:> tuple
mkStringTuples _sym Ctx.Empty = return Ctx.Empty

mkBaseUnion ::
  W4.IsSymExprBuilder sym =>
  sym ->
  W4.SymExpr sym LabelType ->
  Ctx.Assignment (LabeledExpr sym) tps ->
  IO (W4.SymExpr sym (BaseUnionType tps))
mkBaseUnion sym lbl vls = do
  tuples <- mkStringTuples sym vls
  W4.mkStruct sym (tuples Ctx.:> lbl)

getStringTuple ::
  W4.IsSymExprBuilder sym =>
  sym ->
  W4.SymExpr sym (StringTupleType tp) ->
  IO (LabeledExpr sym tp)
getStringTuple sym e = do
  lbl <- W4.structField sym e Ctx.i1of2
  v <- W4.structField sym e Ctx.i2of2
  return $ LabeledExpr lbl v

getStringTuples' ::
  W4.IsSymExprBuilder sym =>
  sym ->
  Ctx.Size tps ->
  Ctx.Assignment (W4.SymExpr sym) (StringTupleCtx tps) ->
  IO (Ctx.Assignment (LabeledExpr sym) tps)  
getStringTuples' sym sz a = case Ctx.viewSize sz of
  Ctx.ZeroSize -> return Ctx.Empty
  Ctx.IncSize sz' | (tuples Ctx.:> tuple) <- a -> do
    tuple' <- getStringTuple sym tuple
    tuples' <- getStringTuples' sym sz' tuples
    return $ tuples' Ctx.:> tuple'

getStringTuples ::
  forall tps sym.
  W4.IsSymExprBuilder sym =>
  sym ->
  Ctx.Assignment (W4.SymExpr sym) (StringTupleCtx tps) ->
  IO (Ctx.Assignment (LabeledExpr sym) tps)  
getStringTuples sym a = getStringTuples' sym (stringTupleSize (Ctx.size a)) a


getBaseUnion ::
  forall tps sym.
  W4.IsSymExprBuilder sym =>
  sym ->
  W4.SymExpr sym (BaseUnionType tps) ->
  IO (Ctx.Assignment (LabeledExpr sym) tps, W4.SymExpr sym LabelType)
getBaseUnion sym e = do
  W4.BaseStructRepr repr <- return $ W4.exprType e
  (tuples Ctx.:> lbl) <- Ctx.traverseWithIndex (\idx _ -> W4.structField sym e idx) repr
  tuples' <- getStringTuples @tps sym tuples
  return $ (tuples', lbl)

-- Main functions

data NameEnv sym = NameEnv (Map.Map Name (Some (W4.SymExpr sym)))

data SomeSymFn sym = forall args ret. SomeSymFn (W4.SymFn sym args ret)

type FnEnv sym = Map.Map FName (SomeSymFn sym)

data W4StratEnv sym = W4.IsSymExprBuilder sym => W4StratEnv { sym_ :: sym, varEnv :: I.Env, nameEnv :: NameEnv sym, fnCache :: IO.IORef (FnEnv sym) }

newtype W4StratT_ sym m a = W4StratT { unW4StratM :: ReaderT (W4StratEnv sym) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (W4StratEnv sym), MonadTrans, LiftStrategyM)

instance Monad m => MonadFail (W4StratT_ sym m) where
  fail msg = panic "W4StratT Failure" [msg]

type W4StratT sym m a = (W4.IsSymExprBuilder sym, Monad m, MonadIO m, LiftStrategyM m) => W4StratT_ sym m a

withSym :: Monad m => (W4.IsSymExprBuilder sym => sym -> W4StratT_ sym m a) -> W4StratT_ sym m a
withSym f = do
  W4StratEnv{} <- ask
  sym <- asks sym_
  f sym

bindVarIn :: Name -> W4.SymExpr sym tp -> W4StratT_ sym m a -> W4StratT sym m a
bindVarIn _ _ _ = error ""


getVar :: Name -> W4StratT sym m (Some (W4.SymExpr sym))
getVar nm = do
  NameEnv env <- asks nameEnv
  case Map.lookup nm env of
    Just se -> return se
    Nothing -> panic "Unbound variable" [showPP nm]

nameToSymbol :: Name -> W4.SolverSymbol
nameToSymbol nm = case nameText nm of
  Just nm' -> W4.safeSymbol (T.unpack nm')
  Nothing -> W4.emptySymbol

nameToRepr :: Name -> W4StratT sym m (Some (W4.BaseTypeRepr))
nameToRepr nm = typeToRepr (nameType nm)

nameToVar :: Name -> W4StratT sym m (Some (W4.BoundVar sym))
nameToVar nm = withSym $ \sym -> do
  Some repr <- nameToRepr nm
  Some <$> (liftIO $ W4.freshBoundVar sym (nameToSymbol nm) repr)

withBoundVars :: 
  [Name] -> 
  W4StratT sym m a ->
  W4StratT sym m (Some (Ctx.Assignment (W4.BoundVar sym)), a)
withBoundVars [] f = do
  a <- f
  return (Some Ctx.empty, a)
withBoundVars (nm : nms) f = withSym $ \sym -> do
  Some bv <- nameToVar nm
  bindVarIn nm (W4.varExpr sym bv) $ do
    (Some bvs, a) <- withBoundVars nms f
    return $ (Some (bvs Ctx.:> bv), a)

namesToVars :: [Name] -> W4StratT sym m (Some (Ctx.Assignment (W4.BoundVar sym)))
namesToVars [] = return $ Some Ctx.empty
namesToVars (nm : nms) = do
  Some bv <- nameToVar nm
  Some bvs <- namesToVars nms
  return $ Some $ bvs Ctx.:> bv

mkSymFn :: Fun Expr -> W4StratT sym m (SomeSymFn sym)
mkSymFn fn = withSym $ \sym -> do
  Some ret <- typeToRepr (fnameType (fName fn))
  case fDef fn of
    Def e -> do
      (Some args, body) <- withBoundVars (fParams fn) $ toWhat4Expr ret e
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
  W4StratT sym m (SomeSymFn sym)
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
          liftIO $ IO.modifyIORef' ref (Map.insert nm symFn)
          return symFn
        Nothing -> panic "lookupFn: missing function definition" [showPP nm]

toWhat4Expr :: W4.BaseTypeRepr tp -> Expr -> W4StratT sym m (W4.SymExpr sym tp)
toWhat4Expr t e = withSym $ \sym -> case (t, e) of
  -- Core
  (_, Var nm) -> do
    NameEnv env <- asks nameEnv
    case Map.lookup nm env of
      Just (Some e') -> case testEquality t (W4.exprType e') of
        Just Refl -> return e'
        Nothing -> panic "Unexpected variable type" [showPP nm, show t]
      Nothing -> panic "Unbound variable" [showPP nm]
  (_, PureLet nm e1 e2) -> do
    (Some t1) <- typeToRepr (I.typeOf e1)
    e1Sym <- toWhat4Expr t1 e1
    bindVarIn nm e1Sym $ toWhat4Expr t e2
  (_, ECase c) -> do
    let var = I.caseVar c
    Some e' <- getVar var
    cases <- mapM (\x -> matchesPat t e' x) (I.casePats c)
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
        liftIO $ mkBaseMaybe sym (W4.falsePred sym) fallthrough
      Nothing -> panic "Unexpected maybe type" [showPP e]
  -- Ap1
  -- FIXME: todo
  -- Ap2
  (W4.BaseBoolRepr, Ap2 relOp e1 e2) -> do
    (Some inner_repr) <- typeToRepr (I.typeOf e1) 
    e1' <- toWhat4Expr inner_repr e1
    e2' <- toWhat4Expr inner_repr e2
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
    e1' <- toWhat4Expr W4.BaseIntegerRepr e1
    e2' <- toWhat4Expr W4.BaseIntegerRepr e2
    case intOp of
      Add -> liftIO $ W4.intAdd sym e1' e2'
      Mul -> liftIO $ W4.intMul sym e1' e2'
      _ -> panic "Unsupported integer operation" [showPP e]
  (W4.BaseBVRepr w, Ap2 bvOp e1 e2) -> do
    e1' <- toWhat4Expr (W4.BaseBVRepr w) e1
    e2' <- toWhat4Expr (W4.BaseBVRepr w) e2
    case bvOp of
      Add -> liftIO $ W4.bvAdd sym e1' e2'
      Mul -> liftIO $ W4.bvMul sym e1' e2'
      _ -> panic "Unsupported bitvector operation" [showPP e]
  (_, ApN (CallF nm) args) -> do
    SomeSymFn fn <- lookupFn nm
    case testEquality (W4.fnReturnType fn) t of
      Just Refl -> do
        args' <- toWhat4ExprList (W4.fnArgTypes fn) args
        liftIO $ W4.applySymFn sym fn args'
      Nothing -> panic "Mismatched function return type" [showPP e]
  _ -> panic "Unsupported type" [showPP e]

toWhat4ExprList :: 
  Ctx.Assignment W4.BaseTypeRepr tps -> 
  [Expr] -> 
  W4StratT sym m (Ctx.Assignment (W4.SymExpr sym) tps)
toWhat4ExprList Ctx.Empty [] = return Ctx.Empty
toWhat4ExprList (tps Ctx.:> tp) (e : exprs) = do
  e' <- toWhat4Expr tp e
  exprs' <- toWhat4ExprList tps exprs
  return $ exprs' Ctx.:> e'
toWhat4ExprList _ _ = panic "toWhat4ExprList: mismatch" []

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

{-
    let symbol = case nameText var of
          Just txt -> W4.safeSymbol (show txt)
          Nothing -> W4.emptySymbol
-}
matchesPat :: 
  W4.BaseTypeRepr tp_body ->
  W4.SymExpr sym tp -> 
  (Pattern, Expr) -> 
  W4StratT sym m (W4.Pred sym, W4.SymExpr sym tp_body)
matchesPat tp_body e (pat, body) = withSym $ \sym -> do
  bodyE <- toWhat4Expr tp_body body
  p <- case (W4.exprType e, pat) of
    (W4.BaseBoolRepr, PBool True) -> return e
    (W4.BaseBoolRepr, PBool False) -> liftIO $ W4.notPred sym e
    (BaseMaybeRepr{}, PNothing) -> do
      (p, _) <- liftIO $ getBaseMaybe sym e
      liftIO $ W4.notPred sym p
    (BaseMaybeRepr{}, PJust) -> do
      (p, _) <- liftIO $ getBaseMaybe sym e
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
    (BaseUnionRepr (_ :: Ctx.Assignment W4.BaseTypeRepr tps), PCon l) -> do
      lbl_pat <- liftIO $ W4.stringLit sym (W4.UnicodeLiteral l)
      (_, lbl_expr) <- liftIO $ getBaseUnion @tps sym e
      liftIO $ W4.isEq sym lbl_pat lbl_expr
    (_, PAny) -> return $ W4.truePred sym
    _ -> panic "Unsupported pattern/expression combination" []
  return (p, bodyE)

    

liftReaderT :: ReaderT I.Env m a -> W4StratT sym m a
liftReaderT f = do
  venv <- asks varEnv
  lift $ runReaderT f venv

sizeToRepr :: I.SizeType -> Maybe (Some (W4.NatRepr))
sizeToRepr (I.TSize n) = W4.someNat n
sizeToRepr (I.TSizeParam{}) = Nothing

liftMaybe :: Maybe a -> W4StratT sym m a
liftMaybe (Just a) = return a
liftMaybe Nothing = panic "liftMaybe" []

-- FIXME: I don't really understand how the parameterized types work
-- here. Are they fully monomorphized out at this point?
lookupTName :: TName -> W4StratT sym m  (Some (W4.BaseTypeRepr))
lookupTName nm = do
  tdefs <- getTypeDefs
  case Map.lookup nm tdefs of
    Just tdecl -> case tDef tdecl of
      TUnion flds -> return $ error ""
  return $ error ""

typeToRepr ::
  forall sym m.
  I.Type -> 
  W4StratT sym m  (Some (W4.BaseTypeRepr))
typeToRepr v = go v
  where
    go :: I.Type -> W4StratT sym m  (Some (W4.BaseTypeRepr))
    go (I.TUInt n) = do
      Some nr <- liftMaybe $ sizeToRepr n
      W4.LeqProof <- liftMaybe $ W4.isPosNat nr
      return $ Some (W4.BaseBVRepr nr)
    go (I.TSInt n) = do
      Some nr <- liftMaybe $ sizeToRepr n
      W4.LeqProof <- liftMaybe $ W4.isPosNat nr
      return $ Some (W4.BaseBVRepr nr)
    go (TInteger) = return (Some W4.BaseIntegerRepr)
    go (TBool) = return (Some W4.BaseBoolRepr)
    go TUnit = return (Some (W4.BaseStructRepr Ctx.Empty))
    go (TMaybe t) = do
      Some repr <- go t
      return $ (Some (W4.BaseStructRepr (Ctx.Empty Ctx.:> W4.BaseBoolRepr Ctx.:> repr)))
    go (TMap tkey tvalue) = do
      Some rkey <- go tkey
      Some rvalue <- go tvalue
      return $ (Some (W4.BaseArrayRepr (Ctx.Empty Ctx.:> rkey) rvalue))
    go _ = panic "typeToRepr: unsupported type" [showPP v]



valueToConcrete :: W4.BaseTypeRepr tp -> I.Value -> Maybe (W4.ConcreteVal tp)
valueToConcrete t v_outer = case (v_outer, t) of
  (I.VUInt _ i, W4.BaseBVRepr nr) -> do
    (W4.ConcreteBV nr) <$> BV.mkBVUnsigned nr i
  (I.VSInt _ i, W4.BaseBVRepr nr) -> do
    (W4.ConcreteBV nr) <$> BV.mkBVSigned nr i
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
  W4.BaseBVRepr nr -> Just $ W4.ConcreteBV nr (BV.mkBV nr 0)
  _ -> Nothing

  

{-
valueToSymExpr :: I.Type -> I.Value -> W4StratT sym m (Some (W4.SymExpr sym))
valueToSymExpr t v = withSym $ \sym -> case v of
  I.VUInt _ i | Just (Some (W4.BaseBVRepr nr)) <- typeToRepr t -> do
    Just bv <- return $ BV.mkBVUnsigned nr i
    Some <$> (liftIO $ W4.bvLit sym nr bv)
  I.VSInt _ i | Just (Some (W4.BaseBVRepr nr)) <- typeToRepr t -> do
    Just bv <- return $ BV.mkBVSigned nr i
    Some <$> (liftIO $ W4.bvLit sym nr bv)
  I.VInteger i | Just (Some (W4.BaseIntegerRepr)) <- typeToRepr t -> do
    Some <$> (liftIO $ W4.intLit sym i)
  I.VBool b | Just (Some (W4.BaseBoolRepr)) <- typeToRepr t -> do
    case b of
      True -> return $ Some $ W4.truePred sym
      False -> return $ Some $ W4.falsePred sym
  I.VMaybe v | Just (Some (W4.BaseStructRepr (Ctx.Empty Ctx.:> W4.BaseBoolRepr Ctx.:> repr))) <- typeToRepr t -> 
    case v of
      Nothing -> liftIO $ do
        default_ <- W4.baseDefaultValue sym repr
        Some <$> (W4.mkStruct sym (Ctx.Empty Ctx.:> W4.falsePred sym Ctx.:> default_))
      Just v' -> do
        I.TMaybe t' <- return $ t
        Some vSym <- valueToSymExpr t' v'
        Some <$> (liftIO $ W4.mkStruct sym (Ctx.Empty Ctx.:> W4.truePred sym Ctx.:> vSym))
  I.VMap m | Just (Some (W4.BaseArrayRepr (Ctx.Empty Ctx.:> krepr) vrepr)) <- typeToRepr t -> do
    I.TMap vt kt <- return $ t
    case krepr of
      W4.BaseIntegerRepr -> do
        mapM (\(v,k) -> do
          valueToSymExpr vtr v
-}




-- A family of backtracking strategies indexed by a MonadPlus, so MaybeT StrategyM should give DFS
mkStrategyFun :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> ExpSlice -> m SelectedPath
mkStrategyFun ptag sl = do
  env0 <- getIEnv -- for pure function implementations
  snd <$> runReaderT (stratSlice ptag sl) env0 

stratSlice :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> ExpSlice
           -> ReaderT I.Env m (I.Value, SelectedPath)
stratSlice ptag = go
  where
    go sl = 
      case sl of
        SHole -> pure (uncPath I.vUnit)
        SPure e -> uncPath <$> synthesiseExpr e

        SDo x lsl rsl -> do
          (v, lpath)  <- go lsl
          onSlice (SelectedDo lpath) <$> bindIn x v (go rsl)

        SMatch bset -> do
          env <- ask
          -- Run the predicate over all bytes.
          -- FIXME: Too brute force? We could probably be smarter
          let bs = filter (I.evalByteSet bset env) [0 .. 255]
          guard (bs /= [])
          b <- choose bs -- select a byte from the set, backtracking
          -- liftStrategy (liftIO $ putStrLn ("Chose byte " ++ show b))
          pure (I.vUInt 8 (fromIntegral b)
               , SelectedBytes ptag (BS.singleton b))
          
        SChoice sls -> do
          (i, sl') <- choose (enumerate sls) -- select a choice, backtracking
          -- liftStrategy (liftIO $ putStrLn ("Chose choice " ++ show i))
          onSlice (SelectedChoice . PathIndex i) <$> go sl'

        SCall cn -> stratCallNode ptag cn

        SCase _ c -> do
          env <- ask
          I.evalCase (\(_i, sl') _env -> onSlice (SelectedCase . Identity) <$> go sl' ) mzero (enumerate c) env

        -- FIXME: For now we just keep picking until we get something which satisfies the predicate; this can obviously be improved upon ...
        SInverse n ifn p -> do
          let tryOne = do
                v <- synthesiseExpr =<< typeToRandomInhabitant (I.typeOf n)
                bindIn n v $ do
                  b <- I.valueToBool <$> synthesiseExpr p
                  guard b
                  bs <- synthesiseExpr ifn
                  pure (v, SelectedBytes ptag (I.valueToByteString bs))
              tryMany = tryOne <|> tryMany -- FIXME: this might run forever.
          tryMany 

    uncPath :: I.Value -> (I.Value, SelectedPath)
    uncPath v = (v, SelectedHole)
    onSlice f = \(a, sl') -> (a, f sl')

    -- unimplemented = panic "Unimplemented" []

-- Synthesise for each call

-- Adding field sensitivity means that there can be multiple result
-- slices, with non-overlapping projections.  As a result, we need to
-- merge the resulting slices.
--
-- Merging all slices could introduce spurious internal backtracking,
-- although it is not clear whether that is an issue or not.

stratCallNode :: (MonadPlus m, LiftStrategyM m) => ProvenanceTag -> ExpCallNode -> 
                 ReaderT I.Env m (I.Value, SelectedPath)
stratCallNode ptag cn = do
  env <- ask
  let env' = env { I.vEnv = Map.compose (I.vEnv env) (ecnParamMap cn) }
  sl <- getSlice (ecnSliceId cn)
  (v, res) <- local (const env') (stratSlice ptag sl)
  pure (v, SelectedCall (ecnIdx cn) res)

-- ----------------------------------------------------------------------------------------
-- Strategy helpers

-- Backtracking choice + random permutation
choose :: (MonadPlus m, LiftStrategyM m) => [a] -> m a
choose bs = do
  bs' <- randPermute bs
  foldr mplus mzero (map pure bs')

-- ----------------------------------------------------------------------------------------
-- Environment helpers

bindIn :: Monad m => Name -> I.Value -> ReaderT I.Env m a -> ReaderT I.Env m a
bindIn x v m = local upd m
  where
    upd e = e { I.vEnv = Map.insert x v (I.vEnv e) }

synthesiseExpr :: Monad m => Expr -> ReaderT I.Env m I.Value
synthesiseExpr e = I.eval e <$> ask

-- ----------------------------------------------------------------------------------------
-- Utils

enumerate :: Traversable t => t a -> t (Int, a)
enumerate t = evalState (traverse go t) 0
  where
    go a = state (\i -> ((i, a), i + 1))
    
-- =============================================================================
-- Restart monad transformer
--
-- This is similar to the list monad, but it wraps another monad and
-- hence has to be a bit careful about what to do when --- if we use
-- ListT, we get effects from all the alternatives, which could be
-- expensive.  This is similar to ContT, but we also keep around a
-- failure continuation.
--

-- The dfsCont takes the return value, and also an updated failure
-- continuation, as we may want to backtrack into a completed
-- computation.
data RestartTContext r m a =
  RestartTContext { randCont   :: a -> m r
                  , randEscape :: m r
                  }

newtype RestartT r m a = RestartT { getRestartT :: RestartTContext r m a -> m r }

runRestartT :: RestartT r m a -> (a -> m r) -> m r -> m r
runRestartT m cont fl = getRestartT m (RestartTContext (\v -> cont v) fl)

instance Functor (RestartT r m) where
  fmap f (RestartT m) = RestartT $ \ctxt -> m (ctxt { randCont = randCont ctxt . f })

instance Applicative (RestartT r m) where
  pure v              = RestartT $ \ctxt -> randCont ctxt v
  (<*>)               = ap

instance Monad (RestartT r m) where
  RestartT m >>= f = RestartT $ \ctxt ->
    let cont v = getRestartT (f v) ctxt
    in m (ctxt { randCont = cont })

-- | We want
--
-- (a `mplus` b) >>= f == (a >>= f) `mplus` (b >>= f)
--
-- i.e., we give f the result of a, but if that fails, we run f on b's
-- result.

instance Alternative (RestartT r m) where
  m1 <|> _m2 = m1 
  empty = RestartT randEscape

instance MonadPlus (RestartT r m) where -- default body (Alternative)
                     
instance MonadTrans (RestartT r) where
  lift m = RestartT $ \ctxt -> m >>= \v -> randCont ctxt v
  
instance LiftStrategyM m => LiftStrategyM (RestartT r m) where
  liftStrategy m = lift (liftStrategy m)
    





        
        
        

          


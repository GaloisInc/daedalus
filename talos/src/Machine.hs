{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Machine where

import Data.List
import Data.Unique
import qualified Data.Sequence as Seq
import Data.Word

import qualified Daedalus.AST as K
import Daedalus.Interp hiding (Env)
import Daedalus.Interp.Value
import Daedalus.PP hiding (empty, mode)
import Daedalus.Type.AST hiding (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import RTS.ParserAPI hiding (Fail)

import Debug.Trace

data Byte = Symbolic Unique | Concrete Word8

instance Show Byte where
  show (Symbolic i) = "(Symbolic " ++ show (hashUnique i) ++ ")"
  show (Concrete w) = "(Concrete " ++ show w ++ ")"

data SymbolicInput = Region (Seq.Seq Byte) | SymbolicInput :|:  SymbolicInput
  deriving Show

data Parser a
  = Pure Value
  deriving Show
  ---- | Bind (Parser a) (Fun a K.Grammar)
  ---- | Then (Parser a) (Parser a)
  ---- | Choose
  ---- | Fail
  ---- | Guard

data ClassVal' =
  CVAny | CVSingle Word8 | CVSetRange Word8 Word8 | CVComplement ClassVal' | CVUnion [ClassVal']
  deriving Show

matches :: ClassVal' -> Word8 -> Bool
matches CVAny _ = True
matches (CVSingle b) c = b == c
matches (CVSetRange from to) c = from <= c && c <= to
matches (CVComplement cv) c = not (matches cv c)
matches (CVUnion cvs) c = any (flip matches c) cvs


data Env a = Env
  { ruleEnv :: Map Name (Fun a K.Grammar),
    funEnv :: Map Name (Fun a K.Value),
    clsFun :: Map Name (Fun a K.Class),
    valEnv :: Map Name Value,
    clsEnv :: Map Name ClassVal',
    gmrEnv :: Map Name (Parser a),
    -- | Bindings for polymorphic type argumens
    tyEnv :: Map TVar TVal
  }
instance Show (Env a) where
  show _ = "<env>"

env0 :: forall a. Env a
env0 =
  Env Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

data SomeVal a
  = VVal Value
  | VClass ClassVal'
  | VGrm (Parser a)

instance Show (SomeVal a) where
  show (VVal v) = show (pp v)
  show (VClass _v) = "classval"
  show (VGrm v) = show v

data SomeFun a
  = FVal (Fun a K.Value)
  | FClass (Fun a K.Class)
  | FGrm (Fun a K.Grammar)

data Fun a k = Fun
  { funTypes :: [String],
    arguments :: [Name],
    environment :: Env a,
    body :: TC a k
  }

instance Show (Fun a k) where
  show f = "(λ" ++ intercalate "," (map (show . pp) (arguments f)) ++ " → " ++ show (pp (body f)) ++ ")"

data PParser a = PParser {pArgs :: [Name], pBody :: TC a K.Grammar}

data ValueOf a (c :: K.Ctx) :: * where
  ValValue :: Value -> ValueOf a K.Value
  ValGrammar :: Parser a -> ValueOf a K.Grammar
  ValClass :: ClassVal' -> ValueOf a K.Class

instance Show (ValueOf a k) where
  show (ValValue v) = show v
  show (ValGrammar v) = show v
  show (ValClass _v) = "classval"

--instance ShowVal a k where


data Cont store a k where
  KDone :: Cont store a K.Grammar
  KJust :: Cont store a K.Value -> Cont store a K.Value
  KStruct :: Cont store a K.Value -> [(Label, Value)] -> Label -> [(Label, TC a K.Value)] -> Cont store a K.Value
  KUniOp :: Cont store a K.Value -> UniOp -> Cont store a K.Value
  KBinOp1 :: Cont store a K.Value -> BinOp -> TC a K.Value -> Cont store a K.Value
  KBinOp2 :: Cont store a K.Value -> BinOp -> Value -> Cont store a K.Value
  KTriOp1 :: Cont store a K.Value -> TriOp -> TC a K.Value -> TC a K.Value -> Cont store a K.Value
  KTriOp2 :: Cont store a K.Value -> TriOp -> Value -> TC a K.Value -> Cont store a K.Value
  KTriOp3 :: Cont store a K.Value -> TriOp -> Value -> Value -> Cont store a K.Value
  KIf :: Cont store a K.Value -> TC a K.Value -> TC a K.Value -> Cont store a K.Value
  KPure :: Cont store a K.Grammar -> Cont store a K.Value
  KDo :: Cont store a K.Grammar -> Maybe (TCName K.Value) -> TC a K.Grammar -> Cont store a K.Grammar
  KGuard :: Cont store a K.Grammar -> Cont store a K.Value
  KMatch :: Cont store a K.Grammar -> Cont store a K.Class
  KChoose :: Cont store a K.Grammar -> Env a -> store -> K.Commit -> [TC a K.Grammar] -> Cont store a K.Grammar
  KManyExactly :: Cont store a K.Grammar -> WithSem -> TC a K.Grammar -> Cont store a K.Value
  KManyBounds1 :: Cont store a K.Grammar -> WithSem -> Maybe (TC a K.Value) -> TC a K.Grammar -> Cont store a K.Value
  KManyBounds2 :: Cont store a K.Grammar -> WithSem -> Maybe Value -> TC a K.Grammar -> Cont store a K.Value
  KSetSingle :: Cont store a K.Class -> Cont store a K.Value
  KSetRange1 :: Cont store a K.Class -> TC a K.Value -> Cont store a K.Value
  KSetRange2 :: Cont store a K.Class -> Value -> Cont store a K.Value
  KCall :: KnownCtx k => Cont store a k -> TCName k -> [SomeVal a] -> [Arg a] -> Cont store a k'
  KSavedEnv :: Cont store a k -> Env a -> Cont store a k

deriving instance (Show store, Show a) => Show (Cont store a k)

data Store = Store { storeInput :: SymbolicInput } deriving Show

store0 :: Store
store0 = Store $ Region mempty

data MCont store a
  = MKDone
  | MKThen (MCont store a) (Parser a)
  | MKBind (MCont store a) (Fun a K.Grammar)

data MachineState store a
  = forall k. KnownCtx k => MachineState (TC a k) (Env a) store (Cont store a k)
  | forall k. (Valuable a k) => ValueState (ValueOf a k) (Env a) store (Cont store a k)

instance (Show a, Show store) => Show (MachineState store a) where
  show x =
    case x of
     MachineState c _e _s k -> "(MachineState " ++ show (pp c) ++ " " ++ show k ++ ")"
     ValueState v _e _s k -> "(ValueState " ++ show v ++ " " ++ show k ++ ")"

initState :: store -> TC a Grammar -> MachineState store a
initState initStore g = MachineState g env0 initStore KDone

data AwaitingEffect store a = AwaitingEffect (Env a) (Cont store a K.Grammar)

data Eff a (k :: K.Ctx) where
  Match :: ClassVal' -> Eff a K.Grammar
  Fail :: Eff a K.Grammar
  Guard :: Bool -> Eff a K.Grammar
  Many :: ManyBounds Integer -> TC a K.Grammar -> Eff a K.Grammar

deriving instance Show a => (Show (Eff a k))

data Step store a where
  Step :: MachineState store a -> Step store a
  Halt :: Value -> Step store a
  Crash :: CrashReason -> Step store a
  Effect :: Valuable a k => Eff a k -> Env a -> store -> Cont store a k -> Step store a

deriving instance (Show a, Show store) => Show (Step store a)

type CrashReason = String

-- data CrashReason = TypeMismatch String String

class KnownCtx (k :: K.Ctx) where
  ctx :: K.Context k

instance KnownCtx 'K.Grammar where
  ctx = AGrammar

instance KnownCtx 'K.Value where
  ctx = AValue

instance KnownCtx 'K.Class where
  ctx = AClass

lookupFun :: forall a k. KnownCtx k => TCName k -> Env a -> Maybe (Fun a k)
lookupFun x e =
  case ctx @k of
    AGrammar | Just r <- Map.lookup (tcName x) (ruleEnv e) -> Just r
    AValue   | Just r <- Map.lookup (tcName x) (funEnv e) -> Just r
    AClass   | Just r <- Map.lookup (tcName x) (clsFun e) -> Just r
    _ -> Nothing

stepM :: KnownCtx k => TC a k -> Env a -> store -> Cont store a k -> Step store a
stepM c e s k = Step $ MachineState c e s k

class KnownCtx k => Valuable a k where
  type ValuableType a k
  valuable :: ValuableType a k -> ValueOf a k
  invaluable :: ValueOf a k -> ValuableType a k

instance Valuable a K.Grammar where
  type ValuableType a K.Grammar = Parser a
  valuable v = (ValGrammar v)
  invaluable (ValGrammar v) = v
instance Valuable a K.Value where
  type ValuableType a K.Value = Value
  valuable v = (ValValue v)
  invaluable (ValValue v) = v
instance Valuable a K.Class where
  type ValuableType a K.Class = ClassVal'
  valuable v = (ValClass v)
  invaluable (ValClass v) = v

stepV :: forall a store k . (KnownCtx k, Valuable a k) => ValuableType a k -> Env a -> store -> Cont store a k -> Step store a
stepV v e s k = Step $ ValueState (valuable v) e s k


someVal :: forall a k. ValueOf a k -> SomeVal a
someVal (ValValue v) = VVal v
someVal (ValGrammar v) = VGrm v
someVal (ValClass v) = VClass v


-- TODO:
-- - is store needed?
-- - Add choose, match bytes

run ::
  Monad m =>
  (forall k . Valuable a k =>
   Eff a k -> Env a -> store -> Cont store a k ->
   m (Either (TC a k) (ValueOf a k), Env a, store, Cont store a k)) {- ^ How to interpret effects -} ->
  MachineState store a ->
  m (Step store a)
run eff st =
  case step st of
    Step st' -> run eff st'
    Halt pa -> pure $ Halt pa
    Crash reason -> pure $ Crash reason
    Effect f e s k  ->
      do (res, e', s', k') <- eff f e s k
         case res of
           Left c -> run eff $ MachineState c e' s' k'
           Right v -> run eff $ ValueState v e' s' k'


step :: forall a store . MachineState store a -> Step store a
step (MachineState c e s k) =
  case texprValue c of
    (TCVar x :: TCF a k) ->
      case ctx @k of
        AValue ->
          case Map.lookup (tcName x) (valEnv e) of
            Nothing -> Crash $ "Unknown name " ++ show (pp x)
            Just v -> stepV v e s k
        AGrammar ->
          case Map.lookup (tcName x) (gmrEnv e) of
            Nothing -> Crash $ "Unknown name " ++ show (pp x)
            Just v -> stepV v e s k
        AClass -> error "TODO"
    TCBinOp op c1 c2 _ ->
      stepM c1 e s (KBinOp1 k op c2)
    TCPure c' -> stepM c' e s $ KPure k
    TCMatch _ws cls -> stepM cls e s $ KMatch k
    TCDo mn c' next -> stepM c' e s $ KDo k mn next
    TCGuard c' -> stepM c' e s $ KGuard k
    TCChoice mode choices _ ->
      case choices of
        [] -> Effect Fail e s k
        (ch : chs) -> stepM ch e s $ KChoose k e s mode chs
    TCMany sem _ (Exactly howMany) c' ->
      stepM howMany e s $ KManyExactly k sem c'
    TCMany sem _ (Between low hi) c' ->
      case low of
        Nothing ->
          case hi of
            Nothing -> Effect (Many (Between Nothing Nothing) c') e s k
            Just hc -> stepM hc e s $ KManyBounds2 k sem Nothing c'
        Just lc -> stepM lc e s $ KManyBounds1 k sem hi c'
    TCJust c' -> stepM c' e s $ KJust k
    TCStruct [] _ty -> stepV (VStruct []) e s k
    TCStruct ((lbl, c') : initializers) _ty -> stepM c' e s $ KStruct k [] lbl initializers
    TCCall y _ts es ->
      case lookupFun y e of
        Just r ->
          case es of
            [] -> invoke e s k r []
            (a : as) ->
              case a of
                ValArg c' -> stepM c' e s $ KCall k y [] as
                ClassArg c' -> stepM c' e s $ KCall k y [] as
                GrammarArg c' -> stepM c' e s $ KCall k y [] as
        Nothing -> Crash $ "Unknown value function " ++ show (pp y)
    TCUnit -> stepV vUnit e s k
    TCLiteral (LByte w) _ -> stepV (mkUInt 8 (fromIntegral w)) e s k
    TCSetSingle ch -> stepM ch e s (KSetSingle k)
    TCSetRange ch1 ch2 -> stepM ch1 e s (KSetRange1 k ch2)
    -- TCSetComplement e -> _
    -- TCSetUnion es -> _
    -- TCSetOneOf bs -> _
    other -> error $ "No case for " ++ show (pp other)
step (ValueState v e s k) =
  case k of
    KDone ->
      case invaluable v of
        Pure v' -> Halt v'
    KIf k' t f ->
      case v of
        ValValue (VBool b) ->
          let c' = if b then t else f
           in stepM c' e s k'
        other -> Crash $ "Not a Boolean value: " ++ show (pp (invaluable other))
    KJust k' -> stepV (VMaybe (Just (invaluable v))) e s k'
    KPure k' -> stepV (Pure $ invaluable v) e s k'
    KDo k' Nothing next ->
      stepM next e s k'
    KDo k' (Just x) next ->
      let Pure v' = invaluable v in
      stepM next (e { valEnv = Map.insert (tcName x) v' $ valEnv e }) s k'
    KMatch k' ->
      Effect (Match (invaluable v)) e s k'
    KGuard k' ->
      case invaluable v of
        VBool b -> Effect (Guard b) e s k'
        _ -> Crash "not a bool in guard"
    KStruct k' done lbl [] ->
      stepV (VStruct (reverse ((lbl, (invaluable v)) : done))) e s k'
    KStruct k' done lbl ((lbl', c') : todo) ->
      stepM c' e s $ KStruct k' ((lbl, (invaluable v)) : done) lbl' todo
    KUniOp k' op -> stepV (evalUniOp op (invaluable v)) e s k'
    KBinOp1 k' op c2 -> stepM c2 e s $ KBinOp2 k' op (invaluable v)
    KBinOp2 k' op v1 ->
      stepV (evalBinOp op v1 (invaluable v)) e s k'
    KTriOp1 k' op c2 c3 -> stepM c2 e s $ KTriOp2 k' op (invaluable v) c3
    KTriOp2 k' op v1 c3 ->
      stepM c3 e s $ KTriOp3 k' op v1 (invaluable v)
    KTriOp3 k' op v1 v2 ->
      let ValValue v3 = v in
        stepV (evalTriOp op v1 v2 v3) e s k'
    KSetSingle k' -> stepV (CVSingle $ valueToByte $ invaluable v) e s k'
    KSetRange1 k' ch2 -> stepM ch2 e s (KSetRange2 k' $ invaluable v)
    KSetRange2 k' v1 -> stepV (CVSetRange (valueToByte v1) (valueToByte $ invaluable v)) e s k'
    KCall k' f done [] ->
      case lookupFun f e of
        Just r -> invoke e s k' r (reverse done)
        Nothing -> Crash $ "Unknown value function " ++ show (pp f)
    KCall k' f done (next : todo) ->
      case next of
        ValArg c' -> stepM c' e s $ KCall k' f (someVal v : done) todo
        ClassArg c' -> stepM c' e s $ KCall k' f (someVal v : done) todo
        GrammarArg c' -> stepM c' e s $ KCall k' f (someVal v : done) todo
    KSavedEnv k' e' -> stepV (invaluable v) e' s k'


invoke :: KnownCtx k => Env a -> store -> Cont store a k -> Fun a k -> [SomeVal a] -> Step store a
invoke e s k (Fun _ argNames newEnv fbody) args =
  case zipSame argNames args of
    Nothing -> Crash "Wrong number of arguments"
    Just bindings ->
      stepM fbody (addBindings bindings newEnv) s (KSavedEnv k e)
  where
    zipSame :: [a] -> [b] -> Maybe [(a, b)]
    zipSame [] [] = Just []
    zipSame (_ : _) [] = Nothing
    zipSame [] (_ : _) = Nothing
    zipSame (x : xs) (y : ys) = ((x, y) :) <$> zipSame xs ys

    addBindings :: [(Name, SomeVal a)] -> Env a -> Env a
    addBindings [] env = env
    addBindings ((x, v) : xvs) env = addBinding x v $ addBindings xvs env

    addBinding x (VVal v) env = env {valEnv = Map.insert x v $ valEnv env}
    addBinding x (VClass v) env = env {clsEnv = Map.insert x v $ clsEnv env}
    addBinding x (VGrm v) env = env {gmrEnv = Map.insert x v $ gmrEnv env}

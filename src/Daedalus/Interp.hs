{-# LANGUAGE DeriveFunctor, OverloadedStrings, TupleSections #-}
{-# LANGUAGE GADTs, RecordWildCards, BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- An interpreter for the typed AST
module Daedalus.Interp
  ( interp, interpFile
  , compile, Env, interpCompiled
  , Value(..)
  , ParseError(..)
  , Result(..)
  , Input(..)
  -- For synthesis
  , compilePureExpr
  , compilePredicateExpr
  , addValMaybe
  , addVal
  , vUnit
  ) where

import Control.Monad (replicateM,foldM,replicateM_,void)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Data.Bits(shiftL,shiftR,(.|.),(.&.),xor)

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Functor.Identity(Identity,runIdentity)

import qualified Data.Vector as Vector

import Daedalus.SourceRange
import Daedalus.PP hiding (empty)
import Daedalus.Panic

import qualified Daedalus.AST as K
import Daedalus.Type.AST hiding (Value)
import Daedalus.Interp.Value


import RTS.ParserAPI
import RTS.ParserT as P
import qualified RTS.ParserAPI as RTS
import RTS.Vector(vecFromRep,vecToRep)
import qualified RTS.Vector as RTS

type Parser = P.ParserT Identity

-- We can use VUInt instead of mkUInt here b/c we are coming from Word8
byteStringToValue :: ByteString -> Value
byteStringToValue = VArray . Vector.fromList . map (VUInt 8 . fromIntegral) . BS.unpack 

vUnit :: Value
vUnit = VStruct []

-- A rule can take either parsers or values as an argument, e.g.
--
-- F X y = many[y] X

data SomeVal      = VVal Value | VClass ClassVal | VGrm (Parser Value)
data SomeFun      = FVal (Fun Value)
                  | FClass (Fun ClassVal)
                  | FGrm (Fun (Parser Value))
newtype Fun a     = Fun ([TVal] -> [SomeVal] -> a)

instance Show (Fun a) where
  show _ = "FunDecl"

-- -----------------------------------------------------------------------------
-- Interpreting as a Haskell function

data Env = Env
  { ruleEnv :: Map Name (Fun (Parser Value))
  , funEnv  :: Map Name (Fun Value)
  , clsFun  :: Map Name (Fun ClassVal)

  , valEnv  :: Map Name Value
  , clsEnv  :: Map Name ClassVal
  , gmrEnv  :: Map Name (Parser Value)

  , tyEnv   :: Map TVar TVal
    -- ^ Bindings for polymorphic type argumens
  }

type Prims = Map Name SomeFun

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty
               Map.empty Map.empty Map.empty
               Map.empty

addVal :: TCName K.Value -> Value -> Env -> Env
addVal x v env = env { valEnv = Map.insert (tcName x) v (valEnv env) }

addValMaybe :: Maybe (TCName K.Value) -> Value -> Env -> Env
addValMaybe Nothing  _ e = e
addValMaybe (Just x) v e = addVal x v e


evalUniOp :: UniOp -> Value -> Value
evalUniOp op v =
  case op of
    Not -> VBool $ not $ valueToBool v
    Neg ->
      case v of
        VInteger i -> VInteger (negate i)
        VUInt n i  -> mkUInt n (negate i)
        VSInt n i  -> mkSInt n (negate i)
        _ -> panic "evalUniOp" [ "Neg on a non-numeric value" ]


    Concat -> VArray $ Vector.concat
                     $ Vector.toList
                     $ Vector.map valueToVector
                     $ valueToVector v

    BitwiseComplement ->
      case v of
        VUInt n x -> VUInt n (2 ^ n - x - 1)
        r         -> panic "expecting a uint" [show (pp r)]

    ArrayStream -> VStream Input { inputBytes = bs, inputOffset = 0 }
      where bs = valueToByteString v


evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp op v1 v2 =
  case op of
    Add     -> numBin $ \mk a b -> mk (a + b)
    Sub     -> numBin $ \mk a b -> mk (a - b)
    Mul     -> numBin $ \mk a b -> mk (a * b)
    Div     -> numBin $ \mk a b -> mk (a `div` b)
    Mod     -> numBin $ \mk a b -> mk (a `mod` b)
    Lt      -> numBin $ \_  a b -> VBool (a < b)
    Leq     -> numBin $ \_  a b -> VBool (a <= b)
    Eq      -> VBool (v1 == v2)
    NotEq   -> VBool (v1 /= v2)
    -- It is OK to use Haskell's derived Eq here as the DDL type checker
    -- guarantess that the values have the same shape, so false negatives
    -- (e.g. integer == struct) are ruled out statically.

    Cat -> case (v1,v2) of
             (VUInt m x, VUInt n y) ->
                mkUInt (m + n) ((x `shiftL` fromIntegral n) .|. y)
             _ -> error "BUG: invalid bit concatenation"

    LCat ->
      case v2 of
        VUInt w y ->
          let mk f i = f ((i `shiftL` fromIntegral w) .|. y)
              --- XXX: fromIntegral is a bit wrong
          in
          case v1 of
            VInteger x -> mk VInteger  x
            VUInt n x  -> mk (VUInt n) x
            VSInt n x  -> mk (VSInt n) x
            _          -> error "BUG: 1st argument to (<#) must be numeric"
        _ -> error "BUG: 2nd argument of (<#) should be UInt"

    LShift -> shiftOp shiftL
    RShift -> shiftOp shiftR
    BitwiseAnd -> bitwiseOp (.&.)
    BitwiseOr  -> bitwiseOp (.|.)
    BitwiseXor -> bitwiseOp xor

  where
  bitwiseOp f =
    case (v1, v2) of
      (VUInt n x, VUInt _ y)  -> VUInt n (f x y)
      _ -> panic "argument to binop must be an unsigned int"
                  [show (pp v1), show (pp v2)]

  shiftOp sh =
    case v2 of
      VInteger w ->
        --- XXX: fromIntegral is a bit wrong
        let mk f i = f (sh i (fromIntegral w))
        in
        case v1 of
          VInteger x -> mk VInteger  x
          VUInt n x  -> mk (VUInt n) x
          VSInt n x  -> mk (VSInt n) x
          _          -> error "BUG: 1st argument to (<#) must be numeric"
      _ -> error "BUG: 2nd argument to shuft must be an integer"

  numBin f =
    case (v1,v2) of
      (VInteger x, VInteger y) -> f VInteger   x y
      (VUInt m x,  VUInt _ y)  -> f (mkUInt m) x y
      (VSInt m x,  VSInt _ y)  -> f (mkSInt m) x y
      _ -> error ("BUG: invalid binaryo operation: " ++ show op)


evalTriOp :: TriOp -> Value -> Value -> Value -> Value
evalTriOp op e1 e2 e3 =
  case op of
    RangeUp -> rangeOp
    RangeDown -> rangeOp
  where
  rangeOp
    | step <= 0 = error "rangeUp: invalid step"
    | otherwise = VArray
                $ Vector.fromList
                $ map tag
                $ takeWhile notDone
                $ iterate upd start
      where
      upd   = case op of
                RangeUp   -> (+ step)
                RangeDown -> subtract step
      notDone = case op of
                  RangeUp -> (< end)
                  RangeDown -> (> end)
      start = valueToInteger e1
      end   = valueToInteger e2
      step  = valueToInteger e3
      tag = case e1 of
              VUInt n _   -> VUInt n
              VSInt n _   -> VSInt n
              VInteger _  -> VInteger
              _ -> error "rangeUp: Unexpected numeric type"


--------------------------------------------------------------------------------
-- Generic utilities for evaluating loops

data LoopEval col b = LoopEval
  { unboxCol  :: Value -> col
  , loopNoKey :: (Value -> Value          -> LoopV b) -> Value -> col -> LoopV b
  , loopKey   :: (Value -> Value -> Value -> LoopV b) -> Value -> col -> LoopV b

  , mapNoKey  :: (Value          -> LoopV b) -> col -> LoopV b
  , mapKey    :: (Value -> Value -> LoopV b) -> col -> LoopV b
  }

class EvalLoopBody b where
  type LoopV b
  evalLoopBody :: HasRange a => Env -> TC a b -> LoopV b

instance EvalLoopBody K.Value where
  type LoopV K.Value = Value
  evalLoopBody = compilePureExpr

instance EvalLoopBody K.Grammar where
  type LoopV K.Grammar = Parser Value
  evalLoopBody = compileExpr

doLoop :: (HasRange a, EvalLoopBody b) =>
          Env -> Loop a b -> LoopEval col b -> LoopV b
doLoop env lp ev =
  case loopKName lp of
    Nothing ->
      case loopFlav lp of
        LoopMap -> mapNoKey ev step colV
          where
          step elV = bodyVal (addVal (loopElName lp) elV)

        Fold x s -> loopNoKey ev step initVal colV
          where
          initVal     = compilePureExpr env s
          step sV elV = bodyVal ( addVal x               sV
                                . addVal (loopElName lp) elV
                                )

    Just k ->
      case loopFlav lp of
        LoopMap -> mapKey ev step colV
          where
          step kV elV = bodyVal ( addVal k kV
                                . addVal (loopElName lp) elV
                                )

        Fold x s -> loopKey ev step initVal colV
          where
          initVal        = compilePureExpr env s
          step sV kV elV = bodyVal ( addVal x sV
                                   . addVal k kV
                                   . addVal (loopElName lp) elV
                                   )

  where
  colV           = unboxCol ev (compilePureExpr env (loopCol lp))
  bodyVal extEnv = evalLoopBody (extEnv env) (loopBody lp)
--------------------------------------------------------------------------------



evalFor :: HasRange a => Env -> Loop a K.Value -> Value
evalFor env lp =

  case evalType env (typeOf (loopCol lp)) of

    TVArray -> doLoop env lp LoopEval
      { unboxCol  = valueToVector
      , mapNoKey  = \f -> VArray . Vector.map f
      , mapKey    = \f -> VArray . Vector.imap (stepKeyMap f)
      , loopNoKey = \f s -> Vector.foldl' f s
      , loopKey   = \f s -> Vector.ifoldl' (stepKey f) s
      }
      where
      stepKey f    = \sV kV elV -> f sV (VInteger (toInteger kV)) elV
      stepKeyMap f = \   kV elV -> f    (VInteger (toInteger kV)) elV

    TVMap -> doLoop env lp LoopEval
      { unboxCol  = valueToMap
      , loopNoKey = \f s -> Map.foldl' f s
      , loopKey   = \f s -> Map.foldlWithKey' f s
      , mapNoKey  = \f -> VMap . Map.map f
      , mapKey    = \f -> VMap . Map.mapWithKey f
      }

    t -> panic "evalPureFor" [ "Unexpected collection in `for`", show (pp t) ]



evalForM :: HasRange a => Env -> Loop a K.Grammar -> Parser Value
evalForM env lp =

  case evalType env (typeOf (loopCol lp)) of

    TVArray -> doLoop env lp LoopEval
      { unboxCol  = valueToVector
      , loopNoKey = \f s -> Vector.foldM'           f  s
      , loopKey   = \f s -> Vector.ifoldM' (stepKey f) s
      , mapNoKey  = \f   -> fmap VArray . Vector.mapM f
      , mapKey    = \f   -> fmap VArray . Vector.imapM (stepKeyMap f)
      }
      where
      stepKey f    = \sV kV elV -> f sV (VInteger (toInteger kV)) elV
      stepKeyMap f = \   kV elV -> f    (VInteger (toInteger kV)) elV

    TVMap -> doLoop env lp LoopEval
      { unboxCol  = valueToMap
      , loopNoKey = \f s -> foldM (stepNoKey f) s . Map.toList
      , loopKey   = \f s -> foldM (stepKey   f) s . Map.toList
      , mapNoKey  = \f -> fmap VMap . traverse f
      , mapKey    = \f -> fmap VMap . Map.traverseWithKey f
      }
      where
      stepNoKey f = \sV (_  ,elV)-> f sV    elV
      stepKey f   = \sV (kV,elV) -> f sV kV elV

    t -> panic "evalForM" [ "Unexpected collection in `for`", show (pp t) ]



-- Handles expr with kind KValue
compilePureExpr :: HasRange a => Env -> TC a K.Value -> Value
compilePureExpr env = go
  where
    go expr =
      case texprValue expr of

        TCNumber n t   ->
          let bad = panic "compilePureExpr"
                            [ "unexpected numeric literal"
                            , "Type: " ++ show (pp t)
                            ]
          in
          case evalType env t of
            TVInteger -> VInteger n
            TVUInt s  -> mkUInt s n
            TVSInt s  -> mkSInt s n
            TVNum {}  -> panic "compilePureExpr" ["Kind error"]
            TVArray   -> bad
            TVMap     -> bad
            TVOther   -> bad

        TCBool   b     -> VBool b
        TCNothing _    -> VMaybe Nothing
        TCJust e       -> VMaybe (Just (go e))

        TCByte   w     -> mkUInt 8 (fromIntegral w)
        TCUnit         -> VStruct [] -- XXX
        TCStruct fs _  -> VStruct $ map (\(n, e) -> (n, go e)) fs
        TCByteArray bs -> byteStringToValue bs 
        TCArray     es _ -> VArray (Vector.fromList $ map go es)
        TCIn lbl e _   -> VUnionElem lbl (go e)
        TCVar x        -> case Map.lookup (tcName x) (valEnv env) of
                            Nothing -> error ("BUG: unknown value variable " ++ show x)
                            Just v  -> v

        TCUniOp op e1      -> evalUniOp op (go e1)
        TCBinOp op e1 e2 _ -> evalBinOp op (go e1) (go e2)
        TCTriOp op e1 e2 e3 _ -> evalTriOp op (go e1) (go e2) (go e3)


        TCFor lp -> evalFor env lp

        TCIf be te fe  -> go (if valueToBool (go be) then te else fe)

        TCSelStruct e n _ ->
          case lookup n (valueToStruct (go e)) of
            Just v  -> v
            Nothing ->
              panic "BUG: missing field or not a struct" [ show (pp n) ]

        TCCall x ts es  ->
          case Map.lookup (tcName x) (funEnv env) of
            Just r  -> invoke r env ts es
            Nothing -> error $ "BUG: unknown grammar function " ++ show x

        TCCoerce _ t2 e -> fst (doCoerceTo (evalType env t2) (go e))

        TCMapEmpty _ -> VMap Map.empty
        TCArrayLength e -> VInteger (fromIntegral (Vector.length (valueToVector (go e))))

invoke :: HasRange ann => Fun a -> Env -> [Type] -> [Arg ann] -> a
invoke (Fun f) env ts as = f ts1 (map valArg as)
  where
  ts1 = map (evalType env) ts
  valArg a = case a of
               ValArg e -> VVal (compilePureExpr env e)
               ClassArg e -> VClass (compilePredicateExpr env e)
               GrammarArg e -> VGrm (compileExpr env e)

evalType :: Env -> Type -> TVal
evalType env ty =
  case ty of
    TVar x -> lkpTy x
    TCon {} -> TVOther
    Type t0 ->
      case t0 of
        TGrammar _ -> TVOther
        TStream    -> TVOther
        TByteClass -> TVOther
        TNum n     -> TVNum (fromIntegral n) -- wrong for very large sizes.
        TUInt t    -> TVUInt (tvInt t)
        TSInt t    -> TVSInt (tvInt t)
        TInteger   -> TVInteger
        TMap {}    -> TVMap
        TArray {}  -> TVArray
        TBool      -> TVOther
        TUnit      -> TVOther
        TMaybe {}  -> TVOther

  where
  lkpTy x = case Map.lookup x (tyEnv env) of
              Just tv -> tv
              Nothing -> panic "evalType"
                            [ "undefined type vairalbe"
                            , show (pp x)
                            ]

  tvInt t = case evalType env t of
              TVNum n -> n
              it      -> panic "evalType.tvInt" [ "Expected a number"
                                                , "Got: " ++ show (pp it)
                                                ]


compilePredicateExpr :: HasRange a => Env -> TC a K.Class -> ClassVal
compilePredicateExpr env = go
  where
    go expr =
      let cv p = ClassVal p (show (pp expr))
      in
      case texprValue expr of
        TCVar x ->
          case Map.lookup (tcName x) (clsEnv env) of
            Just p -> p
            Nothing -> error ("BUG: undefined class " ++ show x)

        TCFor {} -> panic "compilePredicateExpr" [ "TCFor" ]
        TCCall f ts as ->
          case Map.lookup (tcName f) (clsFun env) of
            Just p -> invoke p env ts as
            Nothing -> error ("BUG: undefined clas function " ++ show f)
        TCSetAny -> cv \_ -> True
        TCSetSingle e ->
          let v = valueToByte (compilePureExpr env e)
          in cv \b -> v == b
        TCSetComplement e -> RTS.bcComplement (go e)
        TCSetUnion es -> foldr RTS.bcUnion RTS.bcNone (map go es)

        TCSetOneOf bs -> cv \b -> BS.any (== b) bs
        TCSetDiff e1 e2 -> RTS.bcDiff (go e1) (go e2)

        TCSetRange e e' ->
          let l = compilePureExpr env e
              u = compilePureExpr env e'
          in cv \b -> valueToByte l <= b && b <= valueToByte u


mbSkip :: WithSem -> Value -> Value
mbSkip s v = case s of
               NoSem  -> VStruct []
               YesSem -> v

compileExpr :: forall a. HasRange a => Env -> TC a K.Grammar -> Parser Value
compileExpr env = go
  where
    go :: TC a K.Grammar -> Parser Value
    go expr =
      let erng = prettySourceRangeLong (range expr)
          alt c = case c of
                    Commit   -> (<||)
                    Backtrack -> (|||)
      in
      case texprValue expr of
        TCFail mbL mbM _ ->
          case (mbLoc,mbMsg) of
            (Nothing,Nothing)  -> pError FromSystem erng "Parse error"
            (Nothing,Just msg) -> pError FromUser erng msg
            (Just l, Nothing)  -> pErrorAt FromUser [erng] l "Parse error"
            (Just l, Just msg) -> pErrorAt FromUser [erng] l msg
          where
          mbLoc = fromInteger . valueToInteger . compilePureExpr env <$> mbL
          mbMsg = BS8.unpack . valueToByteString . compilePureExpr env <$> mbM

        TCPure e -> pure $! compilePureExpr env e

        TCDo m_var e e' ->
          do v <- go e
             compileExpr (addValMaybe m_var v env) e'

        TCGetByte s ->
          do r <- pByte erng
             pure $! mbSkip s (mkUInt 8 (fromIntegral r))

        TCMatch s e ->
          do b <- pMatch1 erng (compilePredicateExpr env e)
             return $! mbSkip s (mkUInt 8 (fromIntegral b))

        TCGuard e ->
          if valueToBool (compilePureExpr env e)
             then pure vUnit
             else pError FromSystem erng "guard failed"

        TCEnd -> pEnd erng >> pure (VStruct [])
        TCOffset -> VInteger . fromIntegral <$> pOffset

        TCCurrentStream -> VStream <$> pPeek

        TCSetStream s -> do pSetInput (valueToStream (compilePureExpr env s))
                            pure (VStruct [])

        TCStreamLen sem n s ->
          let vn = fromIntegral (valueToInteger (compilePureExpr env n)) -- XXX
              vs = valueToStream  (compilePureExpr env s)
              bs = inputBytes vs
          in if vn <= BS.length bs
                then pure $ mbSkip sem
                          $ VStream vs { inputBytes = BS.take vn bs }
                else pError FromSystem erng
                        ("Not enough bytes: need " ++ show vn
                                      ++ ", have " ++ show (BS.length bs))

        TCStreamOff sem n s ->
          let vn = fromIntegral (valueToInteger (compilePureExpr env n)) -- XXX
              vs = valueToStream  (compilePureExpr env s)
              bs = inputBytes vs
          in if vn <= BS.length bs
                then pure $ mbSkip sem
                          $ VStream vs { inputBytes = BS.drop vn bs
                                       , inputOffset = inputOffset vs + vn }
                else pError FromSystem erng
                         ("Offset out of bounds: offset" ++ show vn
                                      ++ ", have " ++ show (BS.length bs))




        TCLabel l p -> pEnter (Text.unpack l) (go p)

        TCMapInsert s ke ve me ->
          do let kv = compilePureExpr env ke
                 vv = compilePureExpr env ve
                 m  = valueToMap (compilePureExpr env me)
             if kv `Map.member` m
               then pError FromSystem erng ("duplicate key " ++ show (pp kv))
               else pure $! mbSkip s (VMap (Map.insert kv vv m))

        TCMapLookup s ke me -> do
          let kv = compilePureExpr env ke
              m  = valueToMap (compilePureExpr env me)
          case Map.lookup kv m of
            Just v  -> pure $! mbSkip s v
            Nothing -> pError FromSystem erng ("missing key " ++ show (pp kv))

        TCArrayIndex s e ix -> do
          let v   = valueToVector  (compilePureExpr env e)
              ixv = valueToInteger (compilePureExpr env ix)
          case v Vector.!? (fromInteger ixv) of
            Just v'  -> pure $! mbSkip s v'
            Nothing -> pError FromSystem erng
                                  ("index out of bounds " ++ show (pp ixv))

        TCMatchBytes s e  ->
          do let v  = compilePureExpr env e
             _ <- pMatch erng (vecFromRep (valueToByteString v))
             pure $! mbSkip s v

        TCChoice c es _  -> foldr (alt c)
                              (pError FromSystem erng "no choice") (map go es)

        TCOptional c e   ->
             alt c (VMaybe . Just <$> go e) (pure (VMaybe Nothing))



        TCMany s _ (Exactly e) e' ->
          do let v = fromIntegral (valueToInteger (compilePureExpr env e))
                 p = go e'
             case s of
               YesSem -> do vs <- replicateM v p
                            pure (VArray (Vector.fromList vs))
               NoSem  -> do replicateM_ v p
                            pure (VStruct [])

        TCMany s cmt (Between m_le m_ue) e ->
          let code   = go e
              code'  = void code
              m_l    = (valueToInteger . compilePureExpr env) <$> m_le
              m_u    = (valueToInteger . compilePureExpr env) <$> m_ue

              vec :: RTS.Vector Value -> Value
              vec xs  = VArray (vecToRep xs)
              unit _  = VStruct []

          in
          case (m_l, m_u) of

            (Nothing, Nothing) ->
              case s of
                YesSem -> vec  <$> RTS.pMany     (alt cmt) code
                NoSem  -> unit <$> RTS.pSkipMany (alt cmt) code'

            (Nothing, Just ub) ->
              case s of
                YesSem -> vec  <$> RTS.pManyUpTo (alt cmt) ub code
                NoSem  -> unit <$> RTS.pSkipManyUpTo (alt cmt) ub code'

            (Just lb,Nothing) ->
              case s of
                YesSem -> vec <$> RTS.pMinLength erng lb
                                                  (RTS.pMany (alt cmt) code)

                NoSem  -> unit <$> RTS.pSkipAtLeast (alt cmt) lb code'

            (Just lb, Just ub) ->
              case s of
                YesSem -> vec <$> RTS.pMinLength erng lb
                                          (RTS.pManyUpTo (alt cmt) ub code)
                NoSem  -> unit <$>
                              RTS.pSkipWithBounds erng (alt cmt) lb ub code'


        TCCall x ts es  ->
          case Map.lookup (tcName x) (ruleEnv env) of
            Nothing -> error $ "BUG: unknown grammar function " ++ show x
            Just r  -> let lab = text erng <.> colon <+> pp x
                       in pEnter (show lab) (invoke r env ts es)

        TCSelUnion s e sel _ ->
          case valueToUnion (compilePureExpr env e) of
            (lbl,va) | lbl == sel -> pure $! mbSkip s va
                     | otherwise ->
                       pError FromSystem erng
                          ("union is not `" ++ Text.unpack sel ++ "`")

        TCSelJust s e _ ->
          case valueToMaybe (compilePureExpr env e) of
            Just v  -> pure $! mbSkip s v
            Nothing -> pError FromSystem erng "semantic value is not `just`"


        TCVar x ->
          case Map.lookup (tcName x) (gmrEnv env) of
            Just v  -> v
            Nothing -> error $ "BUG: unknown grammar variable " ++ show x

        TCCoerceCheck  s _ t e ->
          case doCoerceTo (evalType env t) (compilePureExpr env e) of
            (v, NotLossy) -> pure $! mbSkip s v
            (_, Lossy) ->
                pError FromSystem erng "value does not fit in target type"

        TCFor lp -> evalForM env  lp

        TCErrorMode m p -> pErrorMode m' (compileExpr env p)
          where m' = case m of
                       Commit    -> Abort
                       Backtrack -> Fail


-- Decl has already been added to Env if required
compileDecl :: HasRange a => Prims -> Env -> TCDecl a -> (Name, SomeFun)
compileDecl prims env TCDecl { .. } =
  ( tcDeclName
  , case tcDeclDef of
      ExternDecl _ -> case Map.lookup tcDeclName prims of
                        Just yes -> yes
                        Nothing ->
                         panic "compileDecl"
                           [ "No implementation for primitive: " ++
                                                    show (pp tcDeclName) ]
      Defined d ->
        case tcDeclCtxt of
              AGrammar ->
                FGrm $ Fun \targs args -> compileExpr (newEnv targs args) d

              AValue ->
                FVal $ Fun \targs args -> compilePureExpr (newEnv targs args) d

              AClass ->
                FClass $ Fun \targs args ->
                              compilePredicateExpr (newEnv targs args) d
  )

  where
  addArg (p,a) Env { .. } =
    case (p,a) of
      (ValParam x, VVal v) ->
         Env { valEnv = Map.insert (tcName x) v valEnv, .. }

      (GrammarParam x, VGrm pa) ->
        Env { gmrEnv = Map.insert (tcName x) pa gmrEnv, .. }

      (ClassParam x, VClass v) -> 
        Env { clsEnv = Map.insert (tcName x) v clsEnv, .. }

      _ -> error "BUG: type error in function call"

  addTyArg (x,t) Env { .. } = Env { tyEnv = Map.insert x t tyEnv, .. }

  newEnv targs args
    | length targs /= length tcDeclTyParams =
      error ("BUG: not enough type arguments for " ++ show (pp tcDeclName))
    | length args /= length tcDeclParams =
      error ("BUG: not enough args for " ++ show tcDeclName)
    | otherwise =
      let withT = foldr addTyArg env (zip tcDeclTyParams targs)
      in foldr addArg withT (zip tcDeclParams args)



-- decls are mutually recursive (maybe)
compileDecls :: HasRange a => Prims -> Env -> [TCDecl a] -> Env
compileDecls prims env decls = env'
  where
    addDecl (x,d) e =
      case d of
        FGrm f -> e { ruleEnv = Map.insert x f (ruleEnv e) }
        FVal f -> e { funEnv  = Map.insert x f (funEnv e) }
        FClass f -> e { clsFun = Map.insert x f (clsFun e) }

    -- Tying the knot so that we get mutual recursion
    env' = foldr addDecl env (map (compileDecl prims env') decls)

compile :: HasRange a =>
          [ (Name, ([Value] -> Parser Value)) ]
        -> [TCModule a] -> Env
compile builtins prog = foldl (compileDecls prims) emptyEnv allRules
  where
    prims = Map.fromList [ (i, mkRule f) | (i, f) <- builtins ]
    someValToValue sv =
      case sv of
        VVal v -> v
        _      -> panic "expecting a VVal" []

    mkRule f = FGrm $ Fun $ \_ svals -> f (map someValToValue svals)

interpCompiled :: ByteString -> Env -> ScopedIdent -> [Value] -> Result Value
interpCompiled bytes env startName args = 
  case [ rl | (x, Fun rl) <- Map.toList (ruleEnv env)
            , nameScope x == startName] of
    (rl : _)        -> runIdentity $
                       P.runParserT (rl [] (map VVal args))
                                    Input { inputBytes = bytes
                                          , inputOffset = 0
                                          }
    []              -> error ("Unknown start rule: " ++ show startName)


interp :: HasRange a => [ (Name, ([Value] -> Parser Value)) ]
       -> ByteString -> [TCModule a] -> ScopedIdent -> Result Value
interp builtins bytes prog startName =
  interpCompiled bytes env startName []
  where
    env = compile builtins prog

interpFile :: HasRange a => FilePath -> [TCModule a] -> ScopedIdent ->
                                              IO (ByteString, Result Value)
interpFile input prog startName = do
  bytes <- if input == "-" then BS.getContents
                           else BS.readFile input
  return (bytes, interp builtins bytes prog startName)
  where
  builtins = [ ]





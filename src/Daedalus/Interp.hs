{-# LANGUAGE DeriveFunctor, OverloadedStrings, TupleSections #-}
{-# LANGUAGE GADTs, RecordWildCards, BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- An interpreter for the typed AST
module Daedalus.Interp
  ( interp, interpFile
  , compile, Env, interpCompiled
  , evalType
  , emptyEnv
  , Value(..)
  , ParseError(..)
  , Result(..)
  , Input(..)
  -- For synthesis
  , compilePureExpr
  , compilePredicateExpr
  , addValMaybe
  , addVal
  , evalUniOp
  , evalBinOp
  , evalTriOp
  , setVals
  , vUnit
  ) where


import Control.Monad (replicateM,foldM,replicateM_,void,guard,msum,forM)

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Data.Text.Encoding(encodeUtf8)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Daedalus.SourceRange
import Daedalus.PP hiding (empty)
import Daedalus.Panic

import Daedalus.Value

import qualified Daedalus.AST as K
import Daedalus.Type.AST hiding (Value)
import Daedalus.Rec (forgetRecs)


import RTS.ParserAPI
import RTS.Input
import RTS.Parser as P
import qualified RTS.ParserAPI as RTS
import RTS.Vector(vecFromRep,vecToRep)
import RTS.Numeric(UInt(..))
import qualified RTS.Vector as RTS

-- A rule can take either parsers or values as an argument, e.g.
--
-- F X y = many[y] X

data SomeVal      = VVal Value | VClass ClassVal | VGrm (PParser Value)
data SomeFun      = FVal (Fun Value)
                  | FClass (Fun ClassVal)
                  | FGrm (Fun (Parser Value))
newtype Fun a     = Fun ([TValue] -> [SomeVal] -> a)


instance Show (Fun a) where
  show _ = "FunDecl"

type PParser a = [SomeVal] -> Parser a

-- -----------------------------------------------------------------------------
-- Interpreting as a Haskell function

data Env = Env
  { ruleEnv :: Map Name (Fun (Parser Value))
  , funEnv  :: Map Name (Fun Value)
  , clsFun  :: Map Name (Fun ClassVal)

  , valEnv  :: Map Name Value
  , clsEnv  :: Map Name ClassVal
  , gmrEnv  :: Map Name (PParser Value)

  , tyEnv   :: Map TVar TValue
    -- ^ Bindings for polymorphic type argumens
  , tyDecls :: Map TCTyName TCTyDecl
    -- ^ Used for bitdata (for coercion)
  }

type Prims = Map Name SomeFun

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty
               Map.empty Map.empty Map.empty
               Map.empty Map.empty

setVals :: Map Name Value -> Env -> Env
setVals vs env = env { valEnv = vs }

addVal :: TCName K.Value -> Value -> Env -> Env
addVal x v env = env { valEnv = Map.insert (tcName x) v (valEnv env) }

addValMaybe :: Maybe (TCName K.Value) -> Value -> Env -> Env
addValMaybe Nothing  _ e = e
addValMaybe (Just x) v e = addVal x v e

--------------------------------------------------------------------------------

partial :: Partial Value -> Value
partial val =
  case val of
    Left err -> error err
    Right a  -> a

partial2 :: (Value -> Value -> Partial Value) -> Value -> Value -> Value
partial2 f = \x y -> partial (f x y)

partial3 :: (Value -> Value -> Value -> Partial Value) ->
            Value -> Value -> Value -> Value
partial3 f = \x y z -> partial (f x y z)

evalUniOp :: UniOp -> Value -> Value
evalUniOp op =
  case op of
    Not               -> vNot
    Neg               -> partial . vNeg
    Concat            -> vArrayConcat
    BitwiseComplement -> vComplement



evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp op =
  case op of
    Add         -> partial2 vAdd
    Sub         -> partial2 vSub
    Mul         -> partial2 vMul
    Div         -> partial2 vDiv
    Mod         -> partial2 vMod

    Lt          -> vLt
    Leq         -> vLeq
    Eq          -> vEq
    NotEq       -> vNeq

    Cat         -> vCat
    LCat        -> partial2 vLCat
    LShift      -> partial2 vShiftL
    RShift      -> partial2 vShiftR
    BitwiseAnd  -> vBitAnd
    BitwiseOr   -> vBitOr
    BitwiseXor  -> vBitXor

    ArrayStream -> vStreamFromArray

    LogicAnd    -> panic "evalBinOp" ["LogicAnd"]
    LogicOr     -> panic "evalBinOp" ["LogicOr"]




evalTriOp :: TriOp -> Value -> Value -> Value -> Value
evalTriOp op =
  case op of
    RangeUp   -> partial3 vRangeUp
    RangeDown -> partial3 vRangeDown


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
      stepKey f    = \sV kV elV -> f sV (vSize (toInteger kV)) elV
      stepKeyMap f = \   kV elV -> f    (vSize (toInteger kV)) elV

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
      stepKey f    = \sV kV elV -> f sV (vSize (toInteger kV)) elV
      stepKeyMap f = \   kV elV -> f    (vSize (toInteger kV)) elV

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

        TCLiteral (LNumber n) t   ->
          let bad = panic "compilePureExpr"
                            [ "unexpected numeric literal"
                            , "Type: " ++ show (pp t)
                            ]
          in
          case evalType env t of
            TVInteger -> VInteger n
            TVUInt s  -> vUInt s n
            TVSInt s  -> partial (vSInt s n)
            TVNum {}  -> panic "compilePureExpr" ["Kind error"]
            TVArray   -> bad
            TVMap     -> bad
            TVOther   -> bad

        TCLiteral (LBool b)   _ -> VBool b
        TCLiteral (LByte w)   _ -> vByte w
        TCLiteral (LBytes bs) _ -> vByteString bs

        TCNothing _    -> VMaybe Nothing
        TCJust e       -> VMaybe (Just (go e))

        TCUnit         -> vUnit
        TCStruct fs _  -> vStruct (map (\(n, e) -> (n, go e)) fs)
        TCArray     es _ -> VArray (Vector.fromList $ map go es)
        TCIn lbl e _   -> VUnionElem lbl (go e)
        TCVar x        -> case Map.lookup (tcName x) (valEnv env) of
                            Nothing -> error ("BUG: unknown value variable " ++ show (pp x))
                            Just v  -> v

        TCUniOp op e1      -> evalUniOp op (go e1)
        TCBinOp op e1 e2 _ -> evalBinOp op (go e1) (go e2)
        TCTriOp op e1 e2 e3 _ -> evalTriOp op (go e1) (go e2) (go e3)


        TCFor lp -> evalFor env lp

        TCIf be te fe  -> go (if valueToBool (go be) then te else fe)

        TCSelStruct e n _ -> vStructLookup (go e) n

        TCCall x ts es  ->
          case Map.lookup (tcName x) (funEnv env) of
            Just r  -> invoke r env ts es []
            Nothing -> error $ "BUG: unknown grammar function " ++ show (pp x)

        -- XXX: move to generic
        TCCoerce _ t@(TCon {}) e ->
          case evalBitData env e t of
            Just v  -> v
            Nothing -> panic "TCCoerceCheck" [ "Unexpeted coercion failre" ]

        TCCoerce _ t2 e -> fst (vCoerceTo (evalType env t2) (go e))

        TCMapEmpty _    -> VMap Map.empty
        TCArrayLength e -> vArrayLength (go e)

        TCCase e alts def ->
          evalCase
            compilePureExpr
            (error "Pattern match failure")
            env e alts def


evalCase ::
  HasRange a =>
  (Env -> TC a k -> val) ->
  val ->
  Env ->
  TC a K.Value ->
  NonEmpty (TCAlt a k) ->
  Maybe (TC a k) ->
  val
evalCase eval ifFail env e alts def =
  let v = compilePureExpr env e
  in case msum (NE.map (tryAlt eval env v) alts) of
       Just res -> res
       Nothing ->
         case def of
           Just d  -> eval env d
           Nothing -> ifFail

tryAlt :: (Env -> TC a k -> val) -> Env -> Value -> TCAlt a k -> Maybe val
tryAlt eval env v (TCAlt ps e) =
  do binds <- matchPatOneOf ps v
     let newEnv = foldr (uncurry addVal) env binds
     pure (eval newEnv e)

evalBitData :: HasRange a =>
  Env ->
  TC a K.Value ->
  Type ->
  Maybe Value
evalBitData env e ty = go (valueToIntegral (compilePureExpr env e)) ty
  where
    go bits ty' =
      case ty' of
        Type (TUInt (Type (TNum w))) -> Just $ vUInt  (fromIntegral w) bits
        TCon n [] | Just tdecl <- Map.lookup n (tyDecls env) -> goCon bits tdecl
        _ -> panic "evalBitData"
                   [ "Cannot coerce to type"
                   , show (pp ty')
                   ]

    goCon bits tdecl =
      case tctyDef tdecl of
        TCTyStruct flds -> vStruct <$> mapM (goS bits) flds
        TCTyUnion  flds -> msum (map (goU bits) flds)

    goS _bits (_, (_, Nothing)) =
      panic "evalBitData" [ "Missing bitdata struct meta data"
                          , show (pp ty)
                          ]

    goS bits (fld, (ty', Just sm)) =
      (,) fld <$> go ((bits `shiftR` fromIntegral (tcbdsLowBit sm)) `mod` 2 ^  (tcbdsWidth sm)) ty'

    goU _bits (_, (_, Nothing)) =
      panic "evalBitData" [ "Missing bitdata union meta data"
                          , show (pp ty)
                          ]
    goU bits (fld, (ty', Just sm))
      | bits .&. tcbduMask sm == tcbduBits sm = VUnionElem fld <$> go bits ty'
      | otherwise                             = Nothing

matchPatOneOf :: [TCPat] -> Value -> Maybe [(TCName K.Value,Value)]
matchPatOneOf ps v = msum [ matchPat p v | p <- ps ]

matchPat :: TCPat -> Value -> Maybe [(TCName K.Value,Value)]
matchPat pat =
  case pat of
    TCConPat _ l p    -> \v -> case valueToUnion v of
                                 (l1,v1) | l == l1 -> matchPat p v1
                                 _ -> Nothing
    TCNumPat _ i      -> \v -> do guard (valueToIntegral v == i)
                                  pure []
    TCBoolPat b       -> \v -> do guard (valueToBool v == b)
                                  pure []
    TCJustPat p       -> \v -> case valueToMaybe v of
                                 Nothing -> Nothing
                                 Just v1 -> matchPat p v1
    TCNothingPat {}   -> \v -> case valueToMaybe v of
                                 Nothing -> Just []
                                 Just _  -> Nothing
    TCVarPat x        -> \v -> Just [(x,v)]
    TCWildPat {}      -> \_ -> Just []


invoke :: HasRange ann => Fun a -> Env -> [Type] -> [Arg ann] -> [SomeVal] -> a
invoke (Fun f) env ts as cloAs = f ts1 (map valArg as ++ cloAs)
  where
  ts1 = map (evalType env) ts
  valArg a = case a of
               ValArg e -> VVal (compilePureExpr env e)
               ClassArg e -> VClass (compilePredicateExpr env e)
               GrammarArg e -> VGrm (compilePExpr env e)

evalType :: Env -> Type -> TValue
evalType env ty =
  case ty of
    TVar x -> lkpTy x
    TCon {} -> TVOther
    Type t0 ->
      case t0 of
        TGrammar _ -> TVOther
        TFun _ _   -> TVOther
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
            Just p -> invoke p env ts as []
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

        TCIf e e1 e2 ->
          if valueToBool (compilePureExpr env e)
             then compilePredicateExpr env e1
             else compilePredicateExpr env e2

        TCCase e alts def ->
          evalCase
            compilePredicateExpr
            (ClassVal (\_ -> False) "Pattern match failure")
            env e alts def

mbSkip :: WithSem -> Value -> Value
mbSkip s v = case s of
               NoSem  -> vUnit
               YesSem -> v

compileExpr :: forall a. HasRange a => Env -> TC a K.Grammar -> Parser Value
compileExpr env expr = compilePExpr env expr []

compilePExpr :: forall a. HasRange a => Env -> TC a K.Grammar -> PParser Value
compilePExpr env expr0 args = go expr0
  where
    go :: TC a K.Grammar -> Parser Value
    go expr =
      let erng = prettySourceRangeLong (range expr)
          alt c = case c of
                    Commit   -> (<||)
                    Backtrack -> (|||)
      in
      case texprValue expr of
        TCFail mbM _ ->
          case mbMsg of
            Nothing  -> pError FromSystem erng "Parse error"
            Just msg -> pError FromUser erng msg
          where
          mbMsg = BS8.unpack . valueToByteString . compilePureExpr env <$> mbM

        TCPure e -> pure $! compilePureExpr env e

        TCDo m_var e e' ->
          do v <- go e
             compileExpr (addValMaybe m_var v env) e'

        TCGetByte s ->
          do r <- pByte erng
             pure $! mbSkip s (vByte r)

        TCMatch s e ->
          do b <- pMatch1 erng (compilePredicateExpr env e)
             return $! mbSkip s (vByte b)

        TCGuard e ->
          if valueToBool (compilePureExpr env e)
             then pure vUnit
             else pError FromSystem erng "guard failed"

        TCEnd -> pEnd erng >> pure vUnit
        TCOffset -> vStreamOffset . VStream <$> pPeek

        TCCurrentStream -> VStream <$> pPeek

        TCSetStream s -> do pSetInput (valueToStream (compilePureExpr env s))
                            pure vUnit

        TCStreamLen sem n s ->
          case vStreamTake vn vs of
            Right v -> pure $ mbSkip sem v
            Left _  -> pError FromSystem erng
                             ("Not enough bytes: need " ++
                              show (valueToSize vn)
                              ++ ", have " ++
                              show (inputLength (valueToStream vs)))
          where
          vn = compilePureExpr env n
          vs = compilePureExpr env s

        TCStreamOff sem n s ->
          case vStreamDrop vn vs of
            Right v -> pure $ mbSkip sem v
            Left _ -> pError FromSystem erng
                             ("Offset out of bounds: offset " ++
                               show (valueToSize vn)
                             ++ ", have " ++
                             show (inputLength (valueToStream vs)))
          where
          vn = compilePureExpr env n
          vs = compilePureExpr env s



        TCLabel l p -> pEnter (Text.unpack l) (go p)

        TCMapInsert s ke ve me ->
          case vMapInsert kv vv mv of
            Right a -> pure $! mbSkip s a
            Left _  -> pError FromSystem erng ("duplicate key " ++ show (pp kv))
          where
          kv = compilePureExpr env ke
          vv = compilePureExpr env ve
          mv = compilePureExpr env me

        TCMapLookup s ke me ->
          case vMapLookup kv mv of
            Right a -> pure $! mbSkip s a
            Left _  -> pError FromSystem erng ("missing key " ++ show (pp kv))
          where
          kv = compilePureExpr env ke
          mv = compilePureExpr env me

        TCArrayIndex s e ix ->
          case vArrayIndex v ixv of
            Right a  -> pure $! mbSkip s a
            Left _   -> pError FromSystem erng
                            ("index out of bounds " ++ showPP ixv)
          where
          v   = compilePureExpr env e
          ixv = compilePureExpr env ix

        TCMatchBytes s e  ->
          do let v  = compilePureExpr env e
             _ <- pMatch erng (vecFromRep (valueToByteString v))
             pure $! mbSkip s v

        TCChoice c es _  ->
          case es of
            [] -> pError FromSystem erng "empty choice"
            _  -> foldr1 (alt c) (map go es)

        TCOptional c e   ->
             alt c (VMaybe . Just <$> go e) (pure (VMaybe Nothing))

        TCMany s _ (Exactly e) e' ->
          case valueToIntSize (compilePureExpr env e) of
            Nothing -> pError FromSystem erng "Limit of `Many` is too large"
            Just v ->
              case s of
                YesSem -> vArray <$> replicateM v p
                NoSem  -> replicateM_ v p >> pure vUnit
             where p = go e'

        TCMany s cmt (Between m_le m_ue) e ->
          do let checkBound mb =
                   forM mb \b ->
                     case valueToIntSize (compilePureExpr env b) of
                       Just a  -> pure (UInt (fromIntegral a))
                       Nothing -> pError FromSystem erng
                                                "Limit of `Many` is too large"

             let code   = go e
                 code'  = void code

             m_l <- checkBound m_le
             m_u <- checkBound m_ue

             let vec :: RTS.Vector Value -> Value
                 vec xs  = VArray (vecToRep xs)
                 unit _  = vUnit

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


        TCCall x ts es -> pEnter (show lab) (invoke rule env ts es args)
          where
          f   = tcName x
          lab = text erng <.> colon <+> pp x

          rule
            | isLocalName f =
                case Map.lookup f (gmrEnv env) of
                  Just r  -> Fun (\_ -> r)
                  Nothing -> bad "local"
            | otherwise =
                case Map.lookup f (ruleEnv env) of
                  Just r  -> r
                  Nothing -> bad "top-level"

          bad z = panic "compileExpr"
                  [ "Unknown " ++ z ++ " function " ++ show (backticks (pp x)) ]

        TCVar x ->
          case Map.lookup (tcName x) (gmrEnv env) of
            Just v  -> v args
            Nothing -> error $ "BUG: unknown grammar variable " ++ show (pp x)

        -- BitData
        -- XXX: should go in generic value
        TCCoerceCheck  s _ t@(TCon {}) e ->
          case evalBitData env e t of
            Just v  -> pure $! mbSkip s v
            Nothing -> pError FromSystem erng "value does not fit in target type"

        -- XXX:  actually identty coercions between *any* type should work...
        TCCoerceCheck  s t@(TCon {}) _ e ->
          panic "compileExpr"
            [ "XXX: Coercion from bitdata to word is not yet implemented." ]

        TCCoerceCheck  s _ t e ->
          case vCoerceTo (evalType env t) (compilePureExpr env e) of
            (v, exact) ->
              if exact
                then pure $! mbSkip s v
                else pError FromSystem erng "value does not fit in target type"

        TCFor lp -> evalForM env  lp

        TCErrorMode m p -> pErrorMode m' (compileExpr env p)
          where m' = case m of
                       Commit    -> Abort
                       Backtrack -> Fail

        TCIf e e1 e2 ->
          if valueToBool (compilePureExpr env e) then go e1 else go e2

        TCCase e alts def ->
          evalCase
            compileExpr
            (pError FromSystem erng "pattern match failure")
            env e alts def

tracePrim :: [SomeVal] -> Parser Value
tracePrim vs =
  case vs of
    [ VVal v ] ->
        do pTrace (vecFromRep (valueToByteString v))
           pure vUnit
    _ -> panic "tracePrim" [ "Invalid call to the trace primitive" ]


-- Decl has already been added to Env if required
compileDecl :: HasRange a => Prims -> Env -> TCDecl a -> (Name, SomeFun)
compileDecl prims env TCDecl { .. } =
  ( tcDeclName
  , case tcDeclDef of

      ExternDecl _ ->
        case Map.lookup tcDeclName prims of
          Just yes -> yes
          Nothing
            | ("Debug","Trace") <- K.nameScopeAsModScope tcDeclName ->
              FGrm (Fun \_ -> tracePrim)

            | otherwise -> panic "compileDecl"
              [ "No implementation for primitive: " ++ show (pp tcDeclName) ]

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

      _ -> panic "compileDecl"
              [ "type error in function call"
              , "declaration: " ++ showPP tcDeclName
              ]


  addTyArg (x,t) Env { .. } = Env { tyEnv = Map.insert x t tyEnv, .. }

  newEnv targs args
    | length targs /= length tcDeclTyParams =
      error ("BUG: not enough type arguments for " ++ show (pp tcDeclName)
              ++ ". This usually indicates some expression in the program became polymorphic, "
              ++ "which can be fixed by adding more type annotations.")
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
compile builtins prog = foldl (compileDecls prims) env0 allRules
  where
    prims = Map.fromList [ (i, mkRule f) | (i, f) <- builtins ]
    someValToValue sv =
      case sv of
        VVal v -> v
        _      -> panic "expecting a VVal" []

    mkRule f = FGrm $ Fun $ \_ svals -> f (map someValToValue svals)

    allRules   = map (forgetRecs . tcModuleDecls) prog
    allTyDecls = concatMap (forgetRecs . tcModuleTypes) prog

    env0     = emptyEnv { tyDecls = Map.fromList [ (tctyName d, d) | d <- allTyDecls ] }

interpCompiled :: ByteString -> ByteString -> Env -> ScopedIdent -> [Value] -> Result Value
interpCompiled name bytes env startName args =
  case [ rl | (x, Fun rl) <- Map.toList (ruleEnv env)
            , nameScopedIdent x == startName] of
    (rl : _)        -> P.runParser (rl [] (map VVal args))
                       (newInput name bytes)
    []              -> error ("Unknown start rule: " ++ show startName ++ ". Known rules: "
                               ++ show [ nameScopedIdent x | (x, _) <- Map.toList (ruleEnv env) ] )

interp :: HasRange a => [ (Name, ([Value] -> Parser Value)) ] ->
          ByteString -> ByteString -> [TCModule a] -> ScopedIdent ->
          Result Value
interp builtins nm bytes prog startName =
  interpCompiled nm bytes env startName []
  where
    env = compile builtins prog

interpFile :: HasRange a => Maybe FilePath -> [TCModule a] -> ScopedIdent ->
                                              IO (ByteString, Result Value)
interpFile input prog startName = do
  (nm,bytes) <- case input of
                  Nothing  -> pure ("(empty)", BS.empty)
                  Just "-" -> do bs <- BS.getContents
                                 pure ("(stdin)", bs)
                  Just f   -> do bs <- BS.readFile f
                                 pure (encodeUtf8 (Text.pack f), bs)
  return (bytes, interp builtins nm bytes prog startName)
  where
  builtins = [ ]

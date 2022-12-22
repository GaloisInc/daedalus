{-# LANGUAGE DeriveFunctor, OverloadedStrings, TupleSections #-}
{-# LANGUAGE GADTs, RecordWildCards, BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
-- An interpreter for the typed AST
module Daedalus.Interp
  ( interp, interpFile
  , compile, Env, interpCompiled
  , evalType
  , emptyEnv
  , Value(..)
  , ParseError, ParseErrorG(..)
  , Result, ResultG(..)
  , Input(..)
  , InterpError(..)
  , interpError
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
  , parseErrorTrieToJSON
  ) where

import GHC.Float(double2Float)
import Control.Monad (replicateM,replicateM_,void,guard,msum,forM)

import Data.Bits (shiftR,shiftL,(.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding(encodeUtf8)
import Data.List(foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Daedalus.SourceRange
import Daedalus.PP hiding (empty)
import Daedalus.Panic
import qualified Daedalus.BDD as BDD

import Daedalus.Value

import qualified Daedalus.AST as K
import Daedalus.Type.AST hiding (Value, BDCon(..), BDField(..))
import qualified Daedalus.Type.AST as AST
import Daedalus.Rec (forgetRecs)


import RTS.Parser as RTS
import RTS.ParserAPI as RTS
import RTS.Input
import RTS.Vector(vecFromRep,vecToRep)
import RTS.Numeric(UInt(..))
import RTS.ParseError (ParseErrorG, ParseErrorSource(..))
import qualified RTS.ParseError as RTS
import qualified RTS.Vector as RTS

import Daedalus.Interp.Error
import Daedalus.Interp.DebugAnnot
import Daedalus.Interp.Env
import Daedalus.Interp.Loop
import Daedalus.Interp.ErrorTrie



--------------------------------------------------------------------------------
-- Operators
--------------------------------------------------------------------------------


evalLiteral :: Env -> Type -> Literal -> Value
evalLiteral env t l =
  case l of
    LNumber n _ ->
      case tval of
        TVInteger     -> VInteger n
        TVUInt s      -> vUInt s n
        TVSInt s      -> partial (vSInt s n)
        TVFloat       -> vFloat (fromIntegral n)
        TVDouble      -> vDouble (fromIntegral n)
        TVNum {}      -> panic "compilePureExpr" ["Kind error"]
        TVBDStruct {} -> bad
        TVBDUnion {}  -> bad
        TVArray       -> bad
        TVMap         -> bad
        TVOther       -> bad

    LBool b           -> VBool b
    LByte w _         -> vByte w
    LBytes bs         -> vByteString bs
    LFloating d       ->
      case tval of
        TVFloat       -> vFloat (double2Float d)
        TVDouble      -> vDouble d
        TVInteger {}  -> bad
        TVUInt {}     -> bad
        TVSInt {}     -> bad
        TVNum {}      -> bad
        TVArray       -> bad
        TVMap         -> bad
        TVBDStruct {} -> bad
        TVBDUnion {}  -> bad
        TVOther       -> bad

    LPi ->
      case tval of
        TVFloat       -> vFloatPi
        TVDouble      -> vDoublePi
        TVInteger {}  -> bad
        TVUInt {}     -> bad
        TVSInt {}     -> bad
        TVNum {}      -> bad
        TVArray       -> bad
        TVMap         -> bad
        TVBDStruct {} -> bad
        TVBDUnion {}  -> bad
        TVOther       -> bad

  where
  bad  = panic "evalLiteral" [ "unexpected literal", "Type: " ++ show (pp t) ]
  tval = evalType env t



evalUniOp :: UniOp -> Value -> Value
evalUniOp op =
  case op of
    Not               -> vNot
    Neg               -> partial . vNeg
    ArrayLength       -> vArrayLength
    Concat            -> vArrayConcat
    BitwiseComplement -> vComplement
    WordToFloat       -> vWordToFloat
    WordToDouble      -> vWordToDouble
    IsNaN             -> vIsNaN
    IsInfinite        -> vIsInfinite
    IsDenormalized    -> vIsDenormalized
    IsNegativeZero    -> vIsNegativeZero
    BytesOfStream     -> vBytesOfStream
    BuilderBuild      -> vFinishBuilder



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
    LookupMap   -> vMapLookup

    BuilderEmit        -> vEmit
    BuilderEmitArray   -> vEmitArray
    BuilderEmitBuilder -> vEmitBuilder
    LogicAnd    -> panic "evalBinOp" ["LogicAnd"]
    LogicOr     -> panic "evalBinOp" ["LogicOr"]


evalTriOp :: TriOp -> Value -> Value -> Value -> Value
evalTriOp op =
  case op of
    RangeUp     -> partial3 vRangeUp
    RangeDown   -> partial3 vRangeDown
    MapDoInsert -> vMapInsert
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Loops
--------------------------------------------------------------------------------

class EvalLoopBody k where
  evalLoopBody  :: HasRange a => Env -> TC a k -> ResultFor k
  arrayLoop     :: LoopEval (Vector.Vector Value) k
  mapLoop       :: LoopEval (Map Value Value) k

instance EvalLoopBody K.Value where
  evalLoopBody = compilePureExpr
  arrayLoop    = loopOverArray
  mapLoop      = loopOverMap

instance EvalLoopBody K.Grammar where
  evalLoopBody = compileExpr
  arrayLoop    = loopOverArrayM
  mapLoop      = loopOverMapM

loopOver ::
  EvalLoopBody k => f k -> Env -> LoopCollection a ->
  (forall col. LoopEval col k -> ResultFor k) -> ResultFor k
loopOver _ env col k =
  case evalType env (typeOf (lcCol col)) of
    TVArray -> k arrayLoop
    TVMap   -> k mapLoop
    t       -> panic "loopOver" [ "Unexpected loop type", show (pp t) ]



doLoop :: (HasRange a, EvalLoopBody k) => Env -> Loop a k -> ResultFor k
doLoop env lp =
  case loopFlav lp of

    LoopMap c ->

      loopOver lp env c \ev ->
        let colV = unboxCol ev (compilePureExpr env (lcCol c))
        in
        case lcKName c of
          Nothing -> mapNoKey ev step colV
            where
            step elV = bodyVal (addVal (lcElName c) elV)

          Just k -> mapKey ev step colV
            where
            step kV elV = bodyVal ( addVal k kV
                                  . addVal (lcElName c) elV
                                  )

    Fold x s c ->
      loopOver lp env c \ev ->
        let colV  = unboxCol ev (compilePureExpr env (lcCol c))
        in
        case lcKName c of
          Nothing -> loopNoKey ev step initVal colV
            where
            initVal     = compilePureExpr env s
            step sV elV = bodyVal ( addVal x             sV
                                  . addVal (lcElName c) elV
                                  )

          Just k -> loopKey ev step initVal colV
            where
            initVal        = compilePureExpr env s
            step sV kV elV = bodyVal ( addVal x sV
                                     . addVal k kV
                                     . addVal (lcElName c) elV
                                     )

    LoopMany c x s -> loop initVal
      where
      initVal = compilePureExpr env s
      loop :: Value -> Parser Value
      loop v  = do mb <- alt (Just <$> bodyVal (addVal x v)) (pure Nothing)
                   case mb of
                     Nothing -> pure v
                     Just v1 -> loop v1
      alt     = case c of
                  Commit    -> (<||)
                  Backtrack -> (|||)


  where
  bodyVal extEnv = evalLoopBody (extEnv env) (loopBody lp)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Case expressions
--------------------------------------------------------------------------------

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

matchPatOneOf :: [TCPat] -> Value -> Maybe [(TCName K.Value,Value)]
matchPatOneOf ps v = msum [ matchPat p v | p <- ps ]

matchPat :: TCPat -> Value -> Maybe [(TCName K.Value,Value)]
matchPat pat =
  case pat of
    TCConPat _ l p    -> \v -> case unTrace v of
                                 VUnionElem l1 v1
                                   | l == l1 -> matchPat p v1
                                 VBDUnion t x
                                   | bduMatches t l x ->
                                     matchPat p (bduGet t l x)
                                 _ -> Nothing
    TCNumPat _ i _    -> \v -> do guard (valueToIntegral v == i)
                                  pure []
    TCStrPat bs       -> \v -> do guard (valueToByteString v == bs)
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
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Calling things
--------------------------------------------------------------------------------

invoke :: HasRange ann => Fun a -> Env -> [Type] -> [Arg ann] -> [SomeVal] -> a
invoke (Fun f) env ts as cloAs = f ts1 (map valArg as ++ cloAs)
  where
  ts1 = map (evalType env) ts
  valArg a = case a of
               ValArg e -> VVal (compilePureExpr env e)
               ClassArg e -> VClass (compilePredicateExpr env e)
               GrammarArg e -> VGrm (compilePExpr env e)



--------------------------------------------------------------------------------
-- Evaluating Types
--------------------------------------------------------------------------------

evalType :: Env -> Type -> TValue
evalType env ty =
  case ty of
    TVar x -> lkpTy x
    TCon c []
      | Just decl <- Map.lookup c (tyDecls env)
      , let name = Text.pack (show (pp (tctyName decl)))
      , Just u <- tctyBD decl -> evalBitdataType env name u (tctyDef decl)
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
        TFloat     -> TVFloat
        TDouble    -> TVDouble
        TUnit      -> TVOther
        TMaybe {}  -> TVOther
        TBuilder {}-> TVOther

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

-- XXX: This reavaluates types over and over againg.  It might be
-- better to evalute bitdata types in the environment once instead,
-- and store them in the environemtn.
evalBitdataType :: Env -> Text -> BDD.Pat -> TCTyDef -> TValue
evalBitdataType env name u def =
  case def of

    TCTyStruct ~(Just bd) _ ->
      let fs = AST.bdFields bd
      in
      TVBDStruct BDStruct
        { bdName = name
        , bdWidth  = BDD.width u
        , bdGetField =
            let mp =
                  Map.fromList
                    [ (l, inField (AST.bdOffset f) ty)
                    | f <- fs
                    , AST.BDData l ty <- [AST.bdFieldType f]
                    ]
            in \l -> case Map.lookup l mp of
                       Just v  -> v
                       Nothing -> panic "evalBitdataType"
                                    [ "Missing field: " ++ showPP l ]
        , bdStruct   = \fvs -> foldl' (outField fvs) 0 (AST.bdFields bd)
        , bdValid    = BDD.willMatch u
        , bdFields   = [ l | AST.BDData l _ <- map AST.bdFieldType fs ]
        }

    TCTyUnion fs ->
      let mp = Map.fromList [ (l, (evalType env t,p)) | (l,(t,Just p)) <- fs ]
      in
      TVBDUnion BDUnion
        { bduName  = name
        , bduWidth = BDD.width u
        , bduValid = BDD.willMatch u
        , bduGet   =
          \l -> case Map.lookup l mp of
                  Just (t,_) -> vFromBits t
                  Nothing -> panic ("bduGet@" ++ Text.unpack name)
                                   ["Unknown constructor: " ++ showPP l]
        , bduMatches =
          \l -> BDD.willMatch
                case Map.lookup l mp of
                  Just (_,p) -> p
                  Nothing -> panic ("bduMatches@" ++ Text.unpack name)
                                   ["Unknown constructor: " ++ showPP l]
        , bduCases = map fst fs
        }


  where
  inField off t = \i -> vFromBits (evalType env t) (i `shiftR` off)
  outField fs w f =
    shiftL w (AST.bdWidth f) .|.
    case AST.bdFieldType f of
      AST.BDWild  -> 0
      AST.BDTag n -> n
      AST.BDData l _ ->
        vToBits
        case lookup l fs of
          Just fv -> fv
          Nothing -> panic "outField" ["Missing field value", showPP l ]
--------------------------------------------------------------------------------



-- Handles expr with kind KValue
compilePureExpr :: HasRange a => Env -> TC a K.Value -> Value
compilePureExpr env = go
  where
    go expr =
      case texprValue expr of

        TCLiteral l t  -> evalLiteral env t l
        TCNothing _    -> VMaybe Nothing
        TCBuilder _    -> VBuilder []
        TCJust e       -> VMaybe (Just (go e))

        TCStruct fs t  ->
          let vs = [ (n,go e) | (n,e) <- fs ]
          in case evalType env t of
               TVBDStruct bd -> VBDStruct bd (bdStruct bd vs)
               _             -> vStruct vs
        TCArray     es _ -> VArray (Vector.fromList $ map go es)
        TCIn lbl e t ->
          case evalType env t of
            TVBDUnion bd -> VBDUnion bd (vToBits (go e))
            _ -> VUnionElem lbl (go e)
        TCVar x        -> case Map.lookup (tcName x) (valEnv env) of
                            Nothing -> panic "compilePureExpr"
                                          [ "unknown value variable"
                                          , show (pp x)
                                          ]
                            Just v  -> v

        TCUniOp op e1      -> evalUniOp op (go e1)
        TCBinOp op e1 e2 _ -> evalBinOp op (go e1) (go e2)
        TCTriOp op e1 e2 e3 _ -> evalTriOp op (go e1) (go e2) (go e3)

        TCLet x e1 e2 ->
          compilePureExpr (addVal x (compilePureExpr env e1) env) e2

        TCFor lp -> doLoop env lp

        TCIf be te fe  -> go (if valueToBool (go be) then te else fe)

        TCSelStruct e n _ -> vStructLookup (go e) n

        TCCall x ts es  ->
          case Map.lookup (tcName x) (funEnv env) of
            Just r  -> invoke r env ts es []
            Nothing -> panic "compilePureExpr"
                         [ "unknown grammar function"
                         , show (pp x)
                         ]

        TCCoerce _ t2 e -> partial (fst (vCoerceTo (evalType env t2) (go e)))

        TCMapEmpty _    -> VMap Map.empty

        TCCase e alts def ->
          evalCase
            compilePureExpr
            (interpError (PatternMatchFailure (describeAlts alts)))
            env e alts def



compilePredicateExpr :: HasRange a => Env -> TC a K.Class -> ClassVal
compilePredicateExpr env = go
  where
    go expr =
      case texprValue expr of
        TCVar x ->
          case Map.lookup (tcName x) (clsEnv env) of
            Just p -> p
            Nothing -> panic "compilePredicateExpr"
                          [ "undefined class", show (pp x) ]

        TCFor {} -> panic "compilePredicateExpr" [ "TCFor" ]
        TCCall f ts as ->
          case Map.lookup (tcName f) (clsFun env) of
            Just p -> invoke p env ts as []
            Nothing -> panic "compilePredicateExpr"
                        [ "undefined class function", show (pp f) ]
        TCSetAny -> RTS.bcAny
        TCSetSingle e ->
          RTS.bcSingle (UInt (valueToByte (compilePureExpr env e)))
        TCSetComplement e -> RTS.bcComplement (go e)
        TCSetUnion es -> foldr RTS.bcUnion RTS.bcNone (map go es)

        TCSetOneOf bs -> RTS.bcByteString bs
        TCSetDiff e1 e2 -> RTS.bcDiff (go e1) (go e2)

        TCSetRange e e' ->
          let l = UInt (valueToByte (compilePureExpr env e))
              u = UInt (valueToByte (compilePureExpr env e'))
          in RTS.bcRange l u

        TCIf e e1 e2 ->
          if valueToBool (compilePureExpr env e)
             then compilePredicateExpr env e1
             else compilePredicateExpr env e2

        TCCase e alts def ->
          evalCase
            compilePredicateExpr
            (ClassVal (\_ -> False) (describeAlts alts))
            env e alts def

mbSkip :: WithSem -> Value -> Value
mbSkip s v = case s of
               NoSem  -> vUnit
               YesSem -> v

compileExpr :: forall a. HasRange a => Env -> TC a K.Grammar -> Parser Value
compileExpr env expr = compilePExpr env expr []

compileSourceRange :: AST.SourceRange -> RTS.SourceRange
compileSourceRange rng =
  RTS.SourceRange { RTS.srcFrom = toRtsPos (sourceFrom rng)
                  , RTS.srcTo   = toRtsPos (sourceTo rng)
                  }
  where
  toRtsPos s = RTS.SourcePos { RTS.srcName = Text.unpack (sourceFile s)
                             , RTS.srcLine = sourceLine s
                             , RTS.srcCol  = sourceColumn s
                             }


compilePExpr :: forall a. HasRange a => Env -> TC a K.Grammar -> PParser Value
compilePExpr env expr0 args =
  do (v,t) <- traceScope (go expr0)
     pure (vTraced v t)
  where
    addScope :: Parser x -> Parser x
    addScope = pScope env

    go :: TC a K.Grammar -> Parser Value
    go expr =
      let erng = compileSourceRange (range expr)
          alt c = case c of
                    Commit   -> (<||)
                    Backtrack -> (|||)
      in
      case texprValue expr of
        TCFail mbM _ ->
          addScope
          case mbMsg of
            Nothing  -> pError FromSystem erng "Parse error"
            Just msg -> pError FromUser erng msg
          where
          mbMsg = BS8.unpack . valueToByteString . compilePureExpr env <$> mbM

        TCPure e -> pure $! compilePureExpr env e

        TCDo m_var e e' ->
          do v <- go e
             let env' = addValMaybe m_var v env
             compileExpr env' e'

        TCMatch s e ->
          do b <- addScope (pMatch1 erng (compilePredicateExpr env e))
             return $! mbSkip s (vByte b)

        TCEnd -> addScope (pEnd erng) >> pure vUnit
        TCOffset -> vStreamOffset . VStream <$> pPeek

        TCCurrentStream -> VStream <$> pPeek

        TCSetStream s -> do pSetInput (valueToStream (compilePureExpr env s))
                            pure vUnit

        TCStreamLen sem n s ->
          case vStreamTake vn vs of
            Right v -> pure $ mbSkip sem v
            Left _  -> addScope
                     $ pError FromSystem erng
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
            Left _ -> addScope
                    $ pError FromSystem erng
                             ("Offset out of bounds: offset " ++
                               show (valueToSize vn)
                             ++ ", have " ++
                             show (inputLength (valueToStream vs)))
          where
          vn = compilePureExpr env n
          vs = compilePureExpr env s



        TCLabel l p -> pEnter (TextAnnot l) (go p)

        TCMapInsert s ke ve me ->
          case vMapLookup kv mv of
            VMaybe (Just {}) ->
              addScope $
              pError FromSystem erng ("duplicate key " ++ show (pp kv))
            _ -> pure $! mbSkip s (vMapInsert kv vv mv)
          where
          kv = compilePureExpr env ke
          vv = compilePureExpr env ve
          mv = compilePureExpr env me

        TCMapLookup s ke me ->
          case vMapLookup kv mv of
            VMaybe (Just a) -> pure $! mbSkip s a
            _ -> addScope $ pError FromSystem erng ("missing key " ++ show (pp kv))
          where
          kv = compilePureExpr env ke
          mv = compilePureExpr env me

        TCArrayIndex s e ix ->
          case vArrayIndex v ixv of
            Right a  -> pure $! mbSkip s a
            Left _   -> addScope $ pError FromSystem erng
                            ("index out of bounds " ++ showPP ixv)
          where
          v   = compilePureExpr env e
          ixv = compilePureExpr env ix

        TCMatchBytes s e  ->
          do let v  = compilePureExpr env e
             _ <- addScope $ pMatch erng (vecFromRep (valueToByteString v))
             pure $! mbSkip s v

        TCChoice c es _  ->
          case es of
            [] -> addScope $ pError FromSystem erng "empty choice"
            _  -> foldr1 (alt c) (map go es)

        TCOptional c e   ->
             alt c (VMaybe . Just <$> go e) (pure (VMaybe Nothing))

        TCMany s _ (Exactly e) e' ->
          case valueToIntSize (compilePureExpr env e) of
            Nothing -> addScope
                    $ pError FromSystem erng "Limit of `Many` is too large"
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
                       Nothing -> addScope $ pError FromSystem erng
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
                 addScope
                 case s of
                   YesSem -> vec  <$> RTS.pManyUpTo (alt cmt) ub code
                   NoSem  -> unit <$> RTS.pSkipManyUpTo (alt cmt) ub code'

               (Just lb,Nothing) ->
                 addScope
                 case s of
                   YesSem -> vec <$> RTS.pMinLength erng lb
                                                     (RTS.pMany (alt cmt) code)

                   NoSem  -> unit <$> RTS.pSkipAtLeast (alt cmt) lb code'

               (Just lb, Just ub) ->
                 addScope
                 case s of
                   YesSem -> vec <$> RTS.pMinLength erng lb
                                             (RTS.pManyUpTo (alt cmt) ub code)
                   NoSem  -> unit <$>
                                 RTS.pSkipWithBounds erng (alt cmt) lb ub code'


        TCCall x ts es -> addScope
                        $ do i <- pPeek
                             pEnter (CallAnnot (CallSite f erng i))
                                    (invoke rule env ts es args)
          where
          f   = tcName x

          rule
            | isLocalName f =
                case Map.lookup f (gmrEnv env) of
                  Just r  -> Fun (\_ -> r)
                  Nothing -> bad "local" (Map.keys $ gmrEnv env)
            | otherwise =
                case Map.lookup f (ruleEnv env) of
                  Just r  -> r
                  Nothing -> bad "top-level" (Map.keys $ ruleEnv env)

          bad z ks = panic "compileExpr"
                     [ "Unknown " ++ z ++ " function " ++ show (backticks (pp x))
                     , "Known functions: " ++ show ks ]

        TCVar x ->
          case Map.lookup (tcName x) (gmrEnv env) of
            Just v  -> v args
            Nothing -> panic "compilePExpr" [ "unknown grammar variable"
                                            , show (pp x) ]

        TCCoerceCheck  s _ t e ->
          case vCoerceTo (evalType env t) (compilePureExpr env e) of
            (v, exact) ->
              if exact
                then pure $! mbSkip s (partial v)
                     -- XXX: should it still be an error if the value is
                     -- skipped?

                else addScope
                   $ pError FromSystem erng "value does not fit in target type"

        TCFor lp -> doLoop env  lp

        TCErrorMode m p -> pErrorMode m' (compileExpr env p)
          where m' = case m of
                       Commit    -> Abort
                       Backtrack -> Fail

        TCIf e e1 e2 ->
          if valueToBool (compilePureExpr env e) then go e1 else go e2

        TCCase e alts def ->
          evalCase
            compileExpr
            (addScope (pError FromSystem erng (describeAlts alts)))
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

            | otherwise ->
                interpError (MissingExternal (nameScopedIdent tcDeclName))

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
      panic "compileDecl" [ "not enough type arguments for"
                          , show (pp tcDeclName)
                          ]
    | length args /= length tcDeclParams =
      panic "compileDecl" [ "not enough args for", show tcDeclName ]
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

compile ::
  HasRange a =>
  ScopedIdent ->
  [ (Name, ([Value] -> Parser Value)) ] ->
  [TCModule a] ->
  Env
compile start builtins prog =
  case Map.lookup start ruleMap of
    Just ([],[]) -> foldl (compileDecls prims) env0 allRules
    Just _ -> interpError (InvalidStartRule start)
    Nothing -> interpError (UnknownStartRule start)
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
    ruleMap =
      Map.fromList [ ( nameScopedIdent (tcDeclName x)
                     , (tcDeclTyParams x, tcDeclParams x)
                     ) | rs <- allRules, x <- rs ]

interpCompiled ::
  ByteString -> ByteString -> Env -> ScopedIdent -> [Value] -> Result Value
interpCompiled name bytes env startName args =
  case [ rl | (x, Fun rl) <- Map.toList (ruleEnv env)
            , nameScopedIdent x == startName] of
    rl : _  -> runParser (rl [] (map VVal args)) (newInput name bytes)
    [] -> panic "interpCompiled" [ "Missing statr rule", show (pp startName) ]

interp :: HasRange a => [ (Name, ([Value] -> Parser Value)) ] ->
          ByteString -> ByteString -> [TCModule a] -> ScopedIdent ->
          Result Value
interp builtins nm bytes prog startName =
  interpCompiled nm bytes env startName []
  where
    env = compile startName builtins prog

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






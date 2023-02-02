module Daedalus.Interp.Operators where

import GHC.Float(double2Float)

import Daedalus.PP
import Daedalus.Panic(panic)

import Daedalus.Value

import Daedalus.Interp.Env
import Daedalus.Interp.Type
import Daedalus.Interp.Error
import Daedalus.Type.AST hiding (Value, BDCon(..), BDField(..))


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


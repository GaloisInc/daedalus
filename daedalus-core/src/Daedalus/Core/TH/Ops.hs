{-# Language TemplateHaskell, TypeApplications #-}
module Daedalus.Core.TH.Ops where

import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List(sort)
import Data.Maybe(fromJust)
import GHC.Records(getField)

import qualified Daedalus.RTS        as RTS
import qualified Daedalus.RTS.Vector as RTS
import qualified Daedalus.RTS.Map    as RTS

import Daedalus.Panic(panic)

import qualified Daedalus.TH as TH
import Daedalus.Core.Basics
import Daedalus.Core.Expr as Core
import Daedalus.Core.TH.Names
import Daedalus.Core.TH.Type

compileOp0 :: Op0 -> TH.ExpQ
compileOp0 op =
  case op of
    Unit            -> [| () |]
    IntL i t ->
      case t of
        TUInt {}    -> [| RTS.UInt i  :: $(compileMonoType t) |]
        TSInt {}    -> [| RTS.SInt i  :: $(compileMonoType t) |]
        TInteger    -> [| i           :: $(compileMonoType t) |]
        TFloat      -> [| i           :: $(compileMonoType t) |]
        TDouble     -> [| i           :: $(compileMonoType t) |]
        _           -> panic "compileOp0" ["Unexpected type"]

    FloatL d t      -> [| d           :: $(compileMonoType t) |]
    BoolL b         -> [| b |]

    -- In GHC 9 bytestrings are liftable
    ByteArrayL bs   -> [| RTS.vecFromRep $lit :: $(compileMonoType tByteArray) |]
      where lit = [| $(TH.litE (TH.stringL (BS8.unpack bs))) :: BS.ByteString |]

    NewBuilder t    -> [| RTS.emptyBuilder :: $(compileMonoType (TBuilder t)) |]
    MapEmpty t1 t2  -> [| mempty           :: $(compileMonoType (TMap t1 t2)) |]
    ENothing t      -> [| Nothing          :: $(compileMonoType (TMaybe t)) |]

compileOp1 :: Op1 -> Type -> TH.ExpQ -> TH.ExpQ
compileOp1 op1 argT e =
  case op1 of
    CoerceTo t        -> [| RTS.convert $e :: $(compileMonoType t) |]

    IsEmptyStream     -> [| RTS.inputEmpty $e |]

    Head              -> [| RTS.uint8 (RTS.inputHead $e) |]

    StreamOffset      -> [| RTS.UInt (fromIntegral (RTS.inputOffset $e))
                              :: $(compileMonoType (tWord 64)) |]

    BytesOfStream     -> [| RTS.vecFromRep (RTS.inputBytes $e)
                              :: $(compileMonoType tByteArray) |]

    -- We could compile this ourselves in some way, but we leave to GHC for now
    OneOf bs          -> TH.caseE e (map alt opts ++ [dflt])
      where
      opts  = sort (BS.unpack bs)
      alt b = let pat = TH.litP (TH.integerL (fromIntegral b))
              in TH.match pat (TH.normalB [| True |]) []
      dflt  = TH.match [p| _ |](TH.normalB [| False |]) []

    Neg               -> [| RTS.neg $e |]

    BitNot            -> [| RTS.bitCompl $e |]

    Not               -> [| not $e |]

    ArrayLen          -> [| RTS.length $e |]

    Concat            -> [| RTS.concat $e |]

    FinishBuilder     -> [| RTS.finishBuilder $e |]

    NewIterator       -> [| RTS.newIterator $e |]

    IteratorDone      -> [| RTS.iteratorDone $e |]

    IteratorKey       -> [| RTS.iteratorKey $e |]

    IteratorVal       -> [| RTS.iteratorVal $e |]

    IteratorNext      -> [| RTS.iteratorNext $e |]

    EJust             -> [| Just $e |]

    FromJust          -> [| fromJust $e |]

    SelStruct t l     -> [| getField @($lab) $e :: $(compileMonoType t) |]
      where lab = TH.litT (TH.strTyLit (Text.unpack l))

    InUnion ut l ->
      let nm = utName ut
          con = TH.conE (unionConName nm l)
      in if tnameBD nm
           then [| RTS.convert $e :: $(TH.conT (dataName nm)) |]
           else case argT of
                  TUnit -> con
                  _     -> TH.appE con e

    FromUnion t l     -> [| getField @($lab) $e :: $(compileMonoType t) |]
      where lab = TH.litT (TH.strTyLit (Text.unpack l))

    WordToFloat       -> [| RTS.wordToFloat $e  |]
    WordToDouble      -> [| RTS.wordToDouble $e |]
    IsNaN             -> [| isNaN $e |]
    IsInfinite        -> [| isInfinite $e |]
    IsDenormalized    -> [| isDenormalized $e |]
    IsNegativeZero    -> [| isNegativeZero $e |]


compileOp2 :: Op2 -> TH.ExpQ -> TH.ExpQ -> TH.ExpQ
compileOp2 op e1 e2 =
  case op of
    IsPrefix    -> [| RTS.vecToRep $e1 `BS.isPrefixOf` RTS.inputBytes $e2 |]

    Drop        -> [| RTS.inputDrop $e1 $e2 |]
    DropMaybe   -> [| RTS.advanceBy $e1 $e2 |]
    Take        -> [| RTS.inputTake $e1 $e2 |]

    Eq          -> [| $e1 == $e2 |]
    NotEq       -> [| $e1 /= $e2 |]
    Leq         -> [| $e1 <= $e2 |]
    Lt          -> [| $e1 <  $e2 |]

    Add         -> [| RTS.add $e1 $e2 |]
    Sub         -> [| RTS.sub $e1 $e2 |]
    Mul         -> [| RTS.mul $e1 $e2 |]
    Div         -> [| RTS.div $e1 $e2 |]
    Mod         -> [| RTS.mod $e1 $e2 |]

    BitAnd      -> [| RTS.bitAnd $e1 $e2 |]
    BitOr       -> [| RTS.bitOr  $e1 $e2 |]
    BitXor      -> [| RTS.bitXor $e1 $e2 |]
    Cat         -> [| RTS.cat    $e1 $e2 |]
    LCat        -> [| RTS.lcat   $e1 $e2 |]
    LShift      -> [| RTS.shiftl $e1 $e2 |]
    RShift      -> [| RTS.shiftr $e1 $e2 |]

    ArrayIndex  -> [| $e1 RTS.! $e2 |]

    Emit        -> [| RTS.pushBack $e1 $e2 |]
    EmitArray   -> [| RTS.pushBackVector $e1 $e2 |]
    EmitBuilder -> [| RTS.pushBackBuilder $e1 $e2 |]

    -- The map is the first argument.
    MapLookup   -> [| RTS.lookup $e2 $e1 |]
    MapMember   -> [| RTS.member $e2 $e1 |]

    ArrayStream -> [| RTS.arrayStream $e1 $e2 |]

compileOp3 :: Op3 -> TH.ExpQ -> TH.ExpQ -> TH.ExpQ -> TH.ExpQ
compileOp3 op e1 e2 e3 =
  case op of
    RangeUp    -> [| RTS.rangeUp $e1 $e2 $e3 |]
    RangeDown  -> [| RTS.rangeDown $e1 $e2 $e3 |]

    -- The map is the first argument
    MapInsert  -> [| RTS.insert $e2 $e3 $e1 |]

compileOpN :: (FName -> [TH.ExpQ] -> TH.ExpQ) -> OpN -> [TH.ExpQ] -> TH.ExpQ
compileOpN call op es =
  case op of
    ArrayL t -> [| RTS.fromList $(TH.listE es)
                      :: $(compileMonoType (TArray t)) |]
    CallF f  -> call f es




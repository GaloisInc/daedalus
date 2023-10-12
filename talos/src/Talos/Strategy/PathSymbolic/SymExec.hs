{-# LANGUAGE RankNTypes #-}
{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-- Symbolically execute Core terms, but only for the subset required by MuxValue.hs

module Talos.Strategy.PathSymbolic.SymExec
  ( symExecOp0
  , symExecOp1
  , symExecOp2
  , symExecTy
  , symExecValue
  ) where

import qualified Data.ByteString     as BS
import qualified SimpleSMT           as S
import           SimpleSMT           (SExpr)

import           Daedalus.Core       hiding (freshName, tByte)
import           Daedalus.Panic
import           Daedalus.PP
import           Daedalus.Value.Type as V

-- -----------------------------------------------------------------------------
-- OpN

symExecOp0 ::  Op0 -> SExpr
symExecOp0 op = 
  case op of
    Unit -> unexpected
    IntL i TInteger -> S.int i
    IntL i (isBits -> Just (_, n)) ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    IntL _ _ -> unexpected

    BoolL b -> S.bool b
    ByteArrayL _bs -> unexpected
    NewBuilder _ty -> unexpected
    MapEmpty _kt _vt  -> unexpected
    ENothing _ty   -> unexpected
    FloatL {} -> unexpected
  where
    unexpected = panic "Unexpected" [showPP op]

symExecOp1 :: Op1 -> Type -> SExpr -> SExpr
symExecOp1 op ty = 
  case op of
    CoerceTo tyTo    -> symExecCoerce ty tyTo
    IsEmptyStream    -> unexpected
    Head             -> unexpected
    StreamOffset     -> unexpected
    OneOf bs         -> \v -> S.orMany (map (S.eq v . S.bvHex 8 . fromIntegral) (BS.unpack bs))
    Neg | Just _ <- isBits ty -> S.bvNeg
        | TInteger <- ty      -> S.neg
        | otherwise           -> unexpected
    BitNot | Just _ <- isBits ty -> S.bvNot
           | otherwise           -> unexpected
    Not | TBool <- ty  -> S.not
        | otherwise    -> unexpected
    
    ArrayLen -> unexpected
    Concat   -> unexpected
    FinishBuilder -> unexpected
    NewIterator   -> unexpected
    IteratorDone  -> unexpected
    IteratorKey   -> unexpected
    IteratorVal   -> unexpected
    IteratorNext  -> unexpected
    EJust         -> unexpected
    FromJust      -> unexpected
    SelStruct {}  -> unexpected
    InUnion {}    -> unexpected
    FromUnion {}  -> unexpected
    BytesOfStream {} -> unexpected
    
    WordToFloat {}  -> unsupported
    WordToDouble {} -> unsupported
    IsNaN           -> unsupported
    IsInfinite      -> unsupported
    IsDenormalized  -> unsupported
    IsNegativeZero  -> unsupported
  where
    unsupported = panic "Unsupported" [showPP op]
    unexpected = panic "Unexpected" [showPP op]

symExecOp2 :: Op2 -> Type -> SExpr -> SExpr -> SExpr
symExecOp2 bop ty =
  case bop of
    -- Stream ops
    IsPrefix  -> unexpected
    Drop      -> unexpected
    DropMaybe -> unexpected
    Take      -> unexpected
    
    Eq     -> S.eq -- handled above
    NotEq  -> \x y -> S.distinct [x, y]
    Lt  -> bvOrIntSigned S.bvSLt S.bvULt S.lt
    Leq -> bvOrIntSigned S.bvSLeq S.bvULeq S.leq
    
    Add -> bvOrInt S.bvAdd S.add    
    Sub -> bvOrInt S.bvSub S.sub
    Mul -> bvOrInt S.bvMul S.mul
    Div -> bvOrIntSigned S.bvSDiv S.bvUDiv S.div
    Mod -> bvOrIntSigned S.bvSRem S.bvURem S.mod

    BitAnd -> S.bvAnd
    BitOr  -> S.bvOr
    BitXor -> S.bvXOr

    Cat    -> S.concat
    LCat   -> unsupported
    LShift | Just (_, nBits) <- isBits ty -> \x y -> S.bvShl x (S.List [ S.fam "int2bv" [nBits], y] )
           | otherwise -> unexpected
    RShift | Just (signed, nBits) <- isBits ty
             -> \x y -> (if signed then S.bvAShr else S.bvLShr)
                        x (S.List [ S.fam "int2bv" [nBits], y] )
           | otherwise -> unexpected
           
    ArrayIndex  -> unexpected
    Emit        -> unexpected
    EmitArray   -> unexpected
    EmitBuilder -> unexpected
    
    MapLookup   -> unexpected
    MapMember   -> unexpected

    ArrayStream -> unexpected
  where
    bvOrInt bv = bvOrIntSigned bv bv 
    
    bvOrIntSigned bvSigned bvUnsigned intf
      | Just (True, _nBits)  <- isBits ty  = bvSigned
      | Just (False, _nBits) <- isBits ty = bvUnsigned
      | TInteger <- ty             = intf
      | otherwise                  = unexpected
    
    unsupported = panic "Unsupported" [showPP bop]
    unexpected = panic "Unexpected" [showPP bop]

-- symExecOp3 :: Op3 -> Type -> SExpr -> SExpr -> SExpr -> SExpr
-- symExecOp3 op = panic "Unsupported" [showPP op]
                        
-- -----------------------------------------------------------------------------
-- Coercion

symExecCoerce :: Type -> Type -> SExpr -> SExpr
symExecCoerce fromT toT v | fromT == toT = v

-- from Integers
-- FIXME: sign?
symExecCoerce TInteger (isBits -> Just (_, n)) v = do
   S.app (S.fam "int2bv" [n]) [v]

-- From UInts
symExecCoerce (isUInt -> Just _) TInteger v =
   S.fun "bv2int" [v]
symExecCoerce (isUInt -> Just n) (isBits -> Just (_signed, m)) v
  | n == m    = v -- included for completeness
  | n < m     = S.zeroExtend (m - n) v
  | otherwise = S.extract v (m - 1) 0

    -- From SInts
symExecCoerce (isSInt -> Just _) _toT _v  =
  panic "symExecCoerce from SInt is unimplemented" []

symExecCoerce fromT toT _v  =
  panic "Shouldn't happen (symExecCoerce non-reflexive/non-numericb)"
        [show (pp fromT) ++ " to " ++ show (pp toT)]

-- -----------------------------------------------------------------------------
-- Expressions

-- symExecExpr :: SymExec Expr
-- symExecExpr expr = undefined

  -- case expr of
  --   Var n       -> symExecName n
  --   PureLet n e e' -> do
  --     (n', r) <- bindNameFreshIn n (symExecExpr e')
  --     mklet n' <$> symExecExpr e <*> pure r
  
  --   Struct ut ctors ->
  --     S.fun (typeNameToCtor (utName ut)) <$> mapM (symExecExpr . snd) ctors
      
  --   ECase c -> symExecCase symExecExpr c
    
  --   ELoop _lm -> panic "symExec on loops is unimplemented" []
  
  --   Ap0 op      -> pure (symExecOp0 op)
  --   Ap1 op e    -> lift . symExecOp1 op (typeOf e) =<< symExecExpr e
  --   Ap2 op e e' -> do
  --     se  <- symExecExpr e
  --     se' <- symExecExpr e'
  --     lift (symExecOp2 op (typeOf e) se se')
  --   Ap3 op e1 e2 e3 -> do
  --     se1 <- symExecExpr e1
  --     se2 <- symExecExpr e2
  --     se3 <- symExecExpr e3
      
  --     lift (symExecOp3 op (typeOf e1) se1 se2 se3)
    
  --   ApN op args -> lift .  symExecOpN op =<< mapM symExecExpr args


-- ----------------------------------------------------------------------------------------
-- Types

symExecTy :: Type -> SExpr
symExecTy ty = 
  case ty of
    -- TUInt (TSize 8) ->  -- prettier in the output
    TUInt (TSize n) -> S.tBits n
    TUInt {}        -> panic "Non-constant bit size" [ showPP ty ]
    TSInt (TSize n) -> S.tBits n
    TSInt {}        -> panic "Non-constant bit size" [ showPP ty ]
    TInteger        -> S.tInt
    TBool           -> S.tBool
    TFloat          -> panic "Saw a float" []
    TDouble         -> panic "Saw a double" []
    _               -> unexpected
  where unexpected = panic "Unexpected type" [showPP ty]

-- ----------------------------------------------------------------------------------------
-- Interpreter Values
--
-- This should match symExecTy


symExecValue :: V.Value -> SExpr
symExecValue v =
  case v of
    V.VUInt n i -> symExecOp0 (IntL i (TUInt (TSize (fromIntegral n))))
    V.VSInt n i -> symExecOp0 (IntL i (TSInt (TSize (fromIntegral n))))
    V.VInteger i -> symExecOp0 (IntL i TInteger)
    V.VBool b    -> symExecOp0 (BoolL b)
    _            -> unexpected
  where unexpected = panic "Unexpected value" [showPP v]
    

{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}

-- Symbolically execute TC terms

module Talos.SymExec.TC where

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Data.ByteString as BS

import SimpleSMT (SExpr, Solver)
import qualified SimpleSMT as S

import Daedalus.Panic
import Daedalus.PP
import Daedalus.Type.AST hiding (tByte, tUnit, tMaybe, tMap)

import Talos.Lib
import Talos.SymExec.Monad


symExecTCTyName :: TCTyName -> String
symExecTCTyName = show . pp

labelToField :: TCTyName -> Label -> String
labelToField n l = symExecTCTyName n ++ "-" ++ show (pp l)

typeNameToCtor :: TCTyName -> String
typeNameToCtor n = "mk-" ++ symExecTCTyName n

-- | Construct SMT solver datatype declaration based on DaeDaLus type
-- declarations.
symExecTyDecl :: Solver -> TCTyDecl -> IO ()
symExecTyDecl s TCTyDecl { tctyName = name, tctyParams = ps, tctyDef = td } =
  S.declareDatatype s n tvs sflds
  where
    tvs = map (\x -> "t" ++ show (tvarId x)) ps
    env = Map.fromList (zipWith (\p tv -> (p, S.const tv)) ps tvs)
    n = symExecTCTyName name
    sflds = case td of
              TCTyStruct flds -> [(typeNameToCtor name, map mkOneS flds) ]
              TCTyUnion flds  -> map mkOneU flds
    mkOneS (l, t) = (lblToFld l, symExecTy' env t)
    mkOneU (l, t) = (lblToFld l, [ ("get-" ++ lblToFld l, symExecTy' env t) ])
    lblToFld = labelToField name

symExecTy' :: Map TVar SExpr -> Type -> SExpr
symExecTy' env ty = go ty
  where
    go ty' = 
      case ty' of
        TVar x | Just t <- env Map.!? x -> t
        TVar x -> error ("Unknown type variable " ++ show (pp x))
     
        TCon tname args -> S.fun (symExecTCTyName tname) (map go args)
        Type t ->
          case t of
            TGrammar {} -> error "Shouldn't happen (symExecTy: TGrammar)"
            TStream     -> error "Unimplemented (symExecTy: TStream)"
            TByteClass  -> error "Unimplemented (symExecTy: TByteClass)" -- Shouldn't happen?
            TNum {}     -> error "Shouldn't happen (symExecTy: TNum)"
            TFun {}     -> panic "Shouldn't happen" [showPP ty']
            TUInt (Type (TNum n))
                        -> S.tBits n
            TUInt {}    -> error "Shouldn't happen (symExecTy: TUInt)"
            TSInt (Type (TNum n))
                        -> S.tBits n
            TSInt {}    -> error "Shouldn't happen (symExecTy: TSInt)"
            TInteger    -> S.tInt
            TBool       -> S.tBool
            TUnit       -> tUnit
            TArray t'   -> tListWithLength (go t') -- S.tArray S.tInt (symExecTy t)
            TMaybe t'   -> tMaybe (go t')
            TMap kt vt  -> tMap (go kt) (go vt)

symExecTy :: Type -> SExpr
symExecTy = symExecTy' mempty


tcNameToSMTName :: TCName k -> String
tcNameToSMTName tcN = show (pp (nameScopedIdent n) <> "@" <> pp (nameID n))
  where
    n = tcName tcN

symExecTCName :: TCName Value -> SExpr
symExecTCName =  S.const . tcNameToSMTName


-- -----------------------------------------------------------------------------
-- Symbolic execution of terms

-- -- This causes GHC to simpl loop
-- {-# NOINLINE mbPure #-}
-- mbPure :: WithSem -> SExpr -> SParserM
-- mbPure NoSem _ = spure sUnit
-- mbPure _     v = spure v

symExecBinop :: BinOp -> Type -> SExpr -> SExpr -> SExpr
symExecBinop bop (isBits -> Just (signed, _n)) =
  case bop of
    Add    -> S.bvAdd
    Sub    -> S.bvSub
    Mul    -> S.bvMul
    Div    -> if signed then S.bvSDiv else S.bvUDiv
    Mod    -> if signed then S.bvSRem else S.bvURem
    Lt     -> if signed then S.bvSLt else S.bvULt
    Leq    -> if signed then S.bvSLeq else S.bvULeq
    Eq     -> S.eq
    NotEq  -> \x y -> S.distinct [x, y]
    Cat    -> S.concat
    LCat   -> error "Unimplemented"
    LShift -> S.bvShl
    RShift -> if signed then S.bvAShr else S.bvLShr
    BitwiseAnd -> S.bvAnd
    BitwiseOr  -> S.bvOr
    BitwiseXor -> S.bvXOr
    LogicAnd   -> unimplemented
    LogicOr    -> unimplemented
    ArrayStream -> unimplemented
  where unimplemented = panic "Unimplemented" []

symExecBinop bop t | isInteger t =
  case bop of
    Add    -> S.add
    Sub    -> S.sub
    Mul    -> S.mul
    Div    -> S.div
    Mod    -> S.mod
    Lt     -> S.lt
    Leq    -> S.leq
    Eq     -> S.eq
    NotEq  -> \x y -> S.distinct [x, y]
    Cat    -> unimplemented
    LCat   -> unimplemented
    LShift -> unimplemented
    RShift -> unimplemented
    BitwiseAnd -> unimplemented
    BitwiseOr  -> unimplemented
    BitwiseXor -> unimplemented
    LogicAnd   -> unimplemented
    LogicOr    -> unimplemented
    ArrayStream -> unimplemented
  where unimplemented = panic "Unimplemented" []
            
symExecBinop _bop t  = error ("Shouldn't happend: symExecBinop got a " ++ show (pp t))

symExecCoerce :: Type -> Type -> SExpr -> SExpr
symExecCoerce fromT toT v | fromT == toT = v

-- from Integers
-- FIXME: sign?
symExecCoerce fromT (isBits -> Just (_, n)) v | isInteger fromT = do
   S.app (S.fam "int2bv" [n]) [v]

-- From UInts
symExecCoerce (isUInt -> Just _) toT v | isInteger toT =
   S.fun "bv2int" [v]
symExecCoerce (isUInt -> Just n) (isBits -> Just (_signed, m)) v
  | n == m    = v -- included for completeness
  | n < m     = S.zeroExtend (m - n) v
  | otherwise = S.extract v (m - 1) 0

    -- From SInts
symExecCoerce (isSInt -> Just _) _toT _v  =
  error "symExecCoerce from SInt is unimplemented"

symExecCoerce fromT toT _v  =
  error ("Shouldn't happen (symExecCoerce non-reflexive/non-numericb) "
         ++ show (pp fromT) ++ " to " ++ show (pp toT))

symExecV :: TC a Value -> SExpr
symExecV tc =
  case texprValue tc of
    -- Maps
    TCMapEmpty    {} -> unimplemented

    -- Array operations
    TCArrayLength {} -> unimplemented -- sLength $ symExecV v

    -- coercion
    TCCoerce fromT toT v ->
      symExecCoerce fromT toT $ symExecV v

    -- Value constructors
    TCLiteral (LNumber i) t | isInteger t    -> S.int i
    TCLiteral (LNumber i) (isBits -> Just (_, n)) ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    TCLiteral (LNumber {}) _ -> panic "Shouldn't happen: symExecV/TCNumber" []

    TCLiteral (LBool b) _ -> S.bool b
    TCNothing ty -> sNothing (symExecTy ty)
    TCJust     v -> sJust $ symExecV v
    TCLiteral (LByte b) _ -> sByte b

    TCUnit         -> sUnit
    TCStruct ctors (TCon tname _targs) ->
      S.fun (typeNameToCtor tname) $ map (symExecV . snd) ctors
      -- FIXME: we assume order in ctors matches the order in the type.
    TCStruct _ctors _ty -> error "BUG: non-tcon in TCStruct case for symExecV"

    TCLiteral (LBytes bs) _ -> sFromList tByte (map sByte (BS.unpack bs))
    TCArray  vs ty -> sFromList (symExecTy ty) $ map (symExecV) vs

    TCIn lbl v ty@(TCon tname _targs) ->
      S.app (S.as (S.const (labelToField tname lbl)) (symExecTy ty)) . (: []) $ symExecV v
    TCIn _lbl _v _ty -> error "BUG: non-tcon in TCIn case for symExecV"

    TCTriOp     {} -> unimplemented

    TCBinOp op e1 e2 _resT ->
      symExecBinop op (typeOf e1) (symExecV e1) (symExecV e2)
    TCUniOp     {} -> unimplemented
    TCFor {}       -> unimplemented -- symExecVLoop e loop

    TCSelStruct {} -> unimplemented
    TCIf b l r -> do
      S.ite (symExecV b) (symExecV l) (symExecV r)

    TCVar n -> symExecTCName n -- lookupEnv e n
    TCCall {} -> unimplemented -- S.fun (ruleName (tcName fname)) <$> mapM (symExecArg e) args
    TCCase {} -> unimplemented
  where
    unimplemented = error ("SymExecV: Unimplemented: " ++ show (pp tc))

symExecP :: TC a Class -> SExpr -> SExpr
symExecP tc b =
  case texprValue tc of
    -- a set of bytes (aka a byte predicate)
    TCSetAny          -> S.bool True
    TCSetSingle  v    -> S.eq b (symExecV v)
    TCSetComplement c -> S.not (symExecP c b)
    TCSetUnion cs     ->
      S.orMany $ map (\tc' -> symExecP tc' b) cs
    TCSetOneOf bytes  ->
      S.orMany (map (S.eq b . sByte) (BS.unpack bytes))
    TCSetDiff c c'    -> S.and (symExecP c b) (S.not (symExecP c' b))
    TCSetRange l h    -> S.and (S.bvULeq (symExecV l) b)
                               (S.bvULeq b (symExecV h))
    TCFor  {} -> unimplemented
    TCVar  {} -> error "Shoudn't happen: symExecP/TCVar"
    TCCall {} -> unimplemented -- S.fun (ruleName (tcName fname)) <$> ((++) <$> mapM (symExecArg e) args <*> pure [b])
    TCCase {} -> unimplemented
  where
    unimplemented = error ("SymExecP: Unimplemented: " ++ show (pp tc))

-- symExecArg :: Env -> Arg a -> SParserM
-- symExecArg e (ValArg v) = symExecV v
-- symExecArg _ _          = error "Shoudn't happen: symExecArg nonValue"

symExecG :: SExpr -> SExpr -> TC a Grammar -> SExpr
symExecG rel res tc
  | not (isSimpleTC tc) = error "Saw non-simple node in symExecG"
  | otherwise = 
    case texprValue tc of
      -- We (probably) don't care about the bytes here, but this gives smaller models(?)
      TCPure v       -> S.and (S.fun "is-nil" [rel])
                              (S.eq  res (symExecV v))
      TCGetByte {}    -> S.fun "getByteP" [rel, res]
      TCMatch NoSem p -> -- FIXME, this should really not happen (we shouldn't see it)
        S.fun "exists" [ S.List [ S.List [ S.const resN, tByte ]]
                       , S.and (S.fun "getByteP" [rel, S.const resN]) (symExecP p (S.const resN)) ]
      TCMatch _s p    -> S.and (S.fun "getByteP" [rel, res]) (symExecP p res)
      
      TCMatchBytes _ws v ->
        S.and (S.eq rel res) (S.eq res (symExecV v))

      -- Convert a value of tyFrom into tyTo.  Currently very limited
      TCCoerceCheck ws (Type tyFrom) (Type tyTo) e
        | TUInt _ <- tyFrom, TInteger <- tyTo ->
            S.and (S.fun "is-nil" [rel])
                  (if ws == YesSem then (S.fun "bv2int" [symExecV e]) else S.bool True)
      TCCoerceCheck _ws tyFrom tyTo _e ->
        panic "Unsupported types" [showPP tyFrom, showPP tyTo]
        
      _ -> panic "BUG: unexpected term in symExecG" [show (pp tc)]

  where
    resN = "$tres"
      
--------------------------------------------------------------------------------
-- Simple nodes
--
-- A simple node is one where the synthesis can completely determine
-- the value and bytes, e.g. Match, UInt8, etc.  Simple nodes are
-- annotated with a corresponding BytesVar which is used to track
-- bytes assigned earlier.

isSimpleTC :: TC a k -> Bool
isSimpleTC tc =
  case texprValue tc of
    TCMatchBytes {}  -> True
    TCPure {}        -> True
    TCGetByte {}     -> True
    TCMatch {}       -> True
    TCCoerceCheck {} -> True
    _               -> False -- includes unsupported operations as well

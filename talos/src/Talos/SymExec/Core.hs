{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}

-- Symbolically execute Core terms

module Talos.SymExec.Core where

import Control.Monad.IO.Class (liftIO)
import Data.Map(Map)
import qualified Data.Map as Map

import qualified Data.ByteString as BS

import SimpleSMT (SExpr)
import qualified SimpleSMT as S

import Daedalus.Panic
import Daedalus.PP

import Daedalus.Core
import Daedalus.Core.Type

import Talos.Lib
import Talos.SymExec.StdLib
import Talos.SymExec.Monad

--------------------------------------------------------------------------------
-- Types
  
symExecTName :: TName -> String
symExecTName n = show (pp (tnameText n) <> "@" <> pp (tnameId n))

labelToField :: TName -> Label -> String
labelToField n l = symExecTName n ++ "-" ++ show (pp l)

typeNameToCtor :: TName -> String
typeNameToCtor n = "mk-" ++ symExecTName n

-- | Construct SMT solver datatype declaration based on DaeDaLus type
-- declarations.
symExecTDecl :: TDecl -> SymExecM ()
symExecTDecl td@(TDecl { tTParamKNumber = _ : _ }) =
  panic "Unsupported type decl" [showPP td]

symExecTDecl TDecl { tName = name, tTParamKNumber = [], tTParamKValue = ps, tDef = td } =
  withSolver $ \s -> liftIO $ S.declareDatatype s n tvs sflds
  where
    tvs = map showPP ps
    env = Map.fromList (zipWith (\p tv -> (p, S.const tv)) ps tvs)
    n = symExecTName name
    sflds = case td of
              TStruct flds -> [(typeNameToCtor name, map mkOneS flds) ]
              TUnion flds  -> map mkOneU flds
    mkOneS (l, t) = (lblToFld l, symExecTy' env t)
    mkOneU (l, t) = (lblToFld l, [ ("get-" ++ lblToFld l, symExecTy' env t) ])
    lblToFld = labelToField name

symExecTy' :: Map TParam SExpr -> Type -> SExpr
symExecTy' env ty = go ty
  where
    go ty' =
      let unimplemented = panic "Unsupported type" [showPP ty']
      in case ty' of
        TStream         -> unimplemented
        TUInt (TSize n) -> S.tBits n
        TUInt {}        -> panic "Non-constant bit size" [ showPP ty' ]
        TSInt (TSize n) -> S.tBits n
        TSInt {}        -> panic "Non-constant bit size" [ showPP ty' ]
        TInteger        -> S.tInt
        TBool           -> S.tBool
        TUnit           -> tUnit
        TArray t'       -> tArrayWithLength (go t')
        TMaybe t'       -> tMaybe (go t')
        TMap kt vt      -> tMap (go kt) (go vt)
        TBuilder {}     -> unimplemented
        TIterator {}    -> unimplemented    
        TUser (UserType { utName = n, utNumArgs = [], utTyArgs = args }) -> 
          S.fun (symExecTName n) (map go args)
        TUser {}        -> unimplemented
        TParam x | Just t <- env Map.!? x -> t
        TParam x -> panic "Unknown type variable" [showPP x]

symExecTy :: Type -> SExpr
symExecTy = symExecTy' mempty

--------------------------------------------------------------------------------
-- Values

nameToSMTName :: Name -> String
nameToSMTName n = show (maybe "_" pp (nameText n) <> "@" <> pp (nameId n))

fnameToSMTName :: FName -> String
fnameToSMTName n = show (maybe "_" pp (fnameText n) <> "@" <> pp (fnameId n))

symExecName :: Name -> SExpr
symExecName =  S.const . nameToSMTName

symExecFName :: FName -> SExpr
symExecFName =  S.const . fnameToSMTName

-- -----------------------------------------------------------------------------
-- Symbolic execution of terms

-- -- This causes GHC to simpl loop
-- {-# NOINLINE mbPure #-}
-- mbPure :: WithSem -> SExpr -> SParserM
-- mbPure NoSem _ = spure sUnit
-- mbPure _     v = spure v

-- -----------------------------------------------------------------------------
-- OpN

symExecOp0 :: Op0 -> SExpr
symExecOp0 op =
  case op of
    Unit -> sUnit
    IntL i TInteger -> S.int i
    IntL i (isBits -> Just (_, n)) ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i

    BoolL b -> S.bool b
    ByteArrayL {} -> unimplemented --  sFromList tByte (map sByte (BS.unpack bs))    
    NewBuilder {} -> unimplemented
    MapEmpty {}   -> unimplemented
    ENothing ty   -> sNothing (symExecTy ty)
    _ -> unimplemented
  where
    unimplemented = panic "Unimplemented" [showPP op]

symExecOp1 :: Op1 -> Type -> SExpr -> SExpr
symExecOp1 op ty =
  case op of
    CoerceTo tyTo    -> symExecCoerce ty tyTo
    CoerceMaybeTo {} -> unimplemented
    IsEmptyStream    -> unimplemented
    Head             -> unimplemented
    StreamOffset     -> unimplemented
    StreamLen        -> unimplemented
    OneOf bs         -> \v -> S.orMany (map (S.eq v . sByte) (BS.unpack bs))
    Neg | Just _ <- isBits ty -> S.bvNeg
        | TInteger <- ty      -> S.neg
    BitNot | Just _ <- isBits ty -> S.bvNot
    Not | TBool <- ty  -> S.not
    ArrayLen -> unimplemented
    Concat   -> unimplemented -- concat an array of arrays
    FinishBuilder -> unimplemented
    NewIterator -> unimplemented
    IteratorDone -> unimplemented
    IteratorKey  -> unimplemented
    IteratorVal  -> unimplemented
    IteratorNext -> unimplemented
    EJust        -> sJust
    FromJust        -> fun "fromJust"
    SelStruct (TUser ut) l -> fun (labelToField (utName ut) l)
    -- FIXME: we probably need (_ as Foo) ...
    InUnion ut l    -> 
      S.app (S.as (S.const (labelToField (utName ut) l)) (symExecTy ty)) . (: []) 
    
    FromUnion (TUser ut) l -> fun (labelToField (utName ut) l)
    _ -> unimplemented -- shouldn't really happen, we should cover everything above

  where
    unimplemented = panic "Unimplemented" [showPP op]
    fun f = \v -> S.fun f [v]

symExecOp2 :: Op2 -> Type -> SExpr -> SExpr -> SExpr
symExecOp2 bop (isBits -> Just (signed, nBits)) =
  case bop of
    -- Stream ops
    IsPrefix -> unimplemented
    Drop     -> unimplemented
    Take     -> unimplemented
    
    Eq     -> S.eq
    NotEq  -> \x y -> S.distinct [x, y]
    Leq    -> if signed then S.bvSLeq else S.bvULeq
    Lt     -> if signed then S.bvSLt else S.bvULt
    
    Add    -> S.bvAdd
    Sub    -> S.bvSub
    Mul    -> S.bvMul
    Div    -> if signed then S.bvSDiv else S.bvUDiv
    Mod    -> if signed then S.bvSRem else S.bvURem

    BitAnd -> S.bvAnd
    BitOr  -> S.bvOr
    BitXor -> S.bvXOr

    Cat    -> S.concat
    LCat   -> unimplemented
    LShift -> \x y -> S.bvShl x (S.List [ S.fam "int2bv" [nBits], y] )
    RShift -> \x y -> (if signed then S.bvAShr else S.bvLShr)
                       x (S.List [ S.fam "int2bv" [nBits], y] )

    ArrayIndex  -> unimplemented
    ConsBuilder -> unimplemented
    MapLookup   -> unimplemented
    MapMember   -> unimplemented

    ArrayStream -> unimplemented
  where unimplemented = panic "Unimplemented" []

symExecOp2 bop TInteger =
  case bop of
    -- Stream ops
    IsPrefix -> unimplemented
    Drop     -> unimplemented
    Take     -> unimplemented

    Eq     -> S.eq
    NotEq  -> \x y -> S.distinct [x, y]
    Leq    -> S.leq
    Lt     -> S.lt
        
    Add    -> S.add
    Sub    -> S.sub
    Mul    -> S.mul
    Div    -> S.div
    Mod    -> S.mod

    BitAnd -> unimplemented
    BitOr  -> unimplemented
    BitXor -> unimplemented

    Cat    -> unimplemented
    LCat   -> unimplemented
    LShift -> unimplemented
    RShift -> unimplemented

    ArrayIndex  -> unimplemented
    ConsBuilder -> unimplemented
    MapLookup   -> unimplemented
    MapMember   -> unimplemented

    ArrayStream -> unimplemented
  where unimplemented = panic "Unimplemented" []
            
symExecOp2 bop _t = panic "Unsupported operation" [showPP bop]


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
  error "symExecCoerce from SInt is unimplemented"

symExecCoerce fromT toT _v  =
  error ("Shouldn't happen (symExecCoerce non-reflexive/non-numericb) "
         ++ show (pp fromT) ++ " to " ++ show (pp toT))

-- -----------------------------------------------------------------------------
-- Expressions

symExecV :: Expr -> SExpr
symExecV tc =
  case tc of
    Var n       -> symExecName n
    PureLet n e e' -> mklet (nameToSMTName n) (symExecV e) (symExecV e')
    Struct ut ctors ->
      S.fun (typeNameToCtor (utName ut)) $ map (symExecV . snd) ctors
    ECase {} -> unimplemented
    
    Ap0 op      -> symExecOp0 op 
    Ap1 op e    -> symExecOp1 op (typeOf e) (symExecV e)
    Ap2 op e e' -> symExecOp2 op (typeOf e) (symExecV e) (symExecV e')
    Ap3 {}      -> unimplemented
    ApN (CallF _fn) _args -> unimplemented -- FIXME
    ApN {} -> unimplemented
  where
    unimplemented = panic "SymExecV: Unimplemented" [showPP tc]

symExecByteSet :: ByteSet -> SExpr -> SExpr
symExecByteSet bs b = go bs
  where
    go bs' =
      case bs' of
        SetAny          -> S.bool True
        SetSingle  v    -> S.eq b (symExecV v)
        SetRange l h    -> S.and (S.bvULeq (symExecV l) b)
                                 (S.bvULeq b (symExecV h))
    
        SetComplement c -> S.not (go c)
        SetUnion l r    -> S.or (go l) (go r)
        SetIntersection l r -> S.and (go l) (go r)

        SetLet n e bs'' -> mklet (nameToSMTName n) (symExecV e) (go bs'')
        SetCall {}    -> unimplemented
        SetCase {}    -> unimplemented

    unimplemented = panic "SymExecP: Unimplemented inside" [showPP bs]

-- symExecArg :: Env -> Arg a -> SParserM
-- symExecArg e (ValArg v) = symExecV v
-- symExecArg _ _          = error "Shoudn't happen: symExecArg nonValue"

-- symExecG :: SExpr -> SExpr -> TC a Grammar -> SExpr
-- symExecG rel res tc
--   | not (isSimpleTC tc) = error "Saw non-simple node in symExecG"
--   | otherwise = 
--     case texprValue tc of
--       -- We (probably) don't care about the bytes here, but this gives smaller models(?)
--       TCPure v       -> S.and (S.fun "is-nil" [rel])
--                               (S.eq  res (symExecV v))
--       TCGetByte {}    -> S.fun "getByteP" [rel, res]
--       TCMatch NoSem p -> -- FIXME, this should really not happen (we shouldn't see it)
--         S.fun "exists" [ S.List [ S.List [ S.const resN, tByte ]]
--                        , S.and (S.fun "getByteP" [rel, S.const resN]) (symExecP p (S.const resN)) ]
--       TCMatch _s p    -> S.and (S.fun "getByteP" [rel, res]) (symExecP p res)
      
--       TCMatchBytes _ws v ->
--         S.and (S.eq rel res) (S.eq res (symExecV v))

--       -- Convert a value of tyFrom into tyTo.  Currently very limited
--       TCCoerceCheck ws tFrom@(Type tyFrom) tTo@(Type tyTo) e
--         | TUInt _ <- tyFrom, TInteger <- tyTo -> mbCoerce ws tFrom tTo e
--       TCCoerceCheck ws tFrom@(isBits -> Just (_signed1, nFrom)) tTo@(isBits -> Just (_signed2, nTo)) e
--         | nFrom <= nTo -> mbCoerce ws tFrom tTo e

--       TCCoerceCheck _ws tyFrom tyTo _e ->
--         panic "Unsupported types" [showPP tyFrom, showPP tyTo]
        
--       _ -> panic "BUG: unexpected term in symExecG" [show (pp tc)]

--   where
--     resN = "$tres"
--     mbCoerce ws tyFrom tyTo e =
--       S.and (S.fun "is-nil" [rel])
--             (if ws == YesSem then (S.eq res (symExecCoerce tyFrom tyTo (symExecV e))) else S.bool True)

      

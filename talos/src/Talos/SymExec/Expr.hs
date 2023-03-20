{-# LANGUAGE RankNTypes #-}
{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Symbolically execute Core terms

module Talos.SymExec.Expr where

-- import Data.Map (Map)
import           Control.Monad.Reader
import qualified Data.ByteString                 as BS
import           Data.Map                        (Map)
import qualified Data.Map                        as Map

import           SimpleSMT                       (SExpr)
import qualified SimpleSMT                       as S

import           Daedalus.Core                   hiding (tByte)
import           Daedalus.Core.Type
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic

-- import Talos.Strategy.Monad
import           Talos.SymExec.SolverT hiding (freshName)
import           Talos.SymExec.StdLib
import           Talos.SymExec.Type

-- -----------------------------------------------------------------------------
-- Class

-- class SymExec a where
--   -- monadio for debugging
--   symExec :: (Monad m, HasGUID m, MonadIO m) => a -> SolverT m SExpr

type SymExecM m a = ReaderT (Map Name SExpr) (SolverT m) a

type SymExec a = forall m. (Monad m, HasGUID m, MonadIO m) => a -> SymExecM m SExpr

-- -----------------------------------------------------------------------------
-- Names

-- Lookup previously bound names
symExecName :: SymExec Name
symExecName n = do
  m_sx <- asks (Map.lookup n)
  case m_sx of
    Just r  -> pure r
    Nothing -> panic "Missing name" [showPP n]

-- This is probably overkill for lets, but it is safer (we probably don't need to freshName)
bindNameFreshIn :: (Monad m, HasGUID m) => Name -> SymExecM m a -> SymExecM m (String, a)
bindNameFreshIn n m = do
  n' <- lift (freshName n)
  let ns = nameToSMTName n'
  (,) ns <$> local (Map.insert n (S.const ns)) m 
      
-- -----------------------------------------------------------------------------
-- Symbolic execution of terms

-- -- This causes GHC to simpl loop
-- {-# NOINLINE mbPure #-}
-- mbPure :: WithSem -> SExpr -> SParserM
-- mbPure NoSem _ = spure sUnit
-- mbPure _     v = spure v

-- -----------------------------------------------------------------------------
-- OpN

symExecOp0 ::  Op0 -> SExpr
symExecOp0 op =
  case op of
    Unit -> sUnit
    IntL i TInteger -> S.int i
    IntL i (isBits -> Just (_, n)) ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i

    BoolL b -> S.bool b
    ByteArrayL bs ->
      let sBytes   = map sByte (BS.unpack bs)
          emptyArr = S.app (S.as (S.const "const") (S.tArray tSize tByte)) [sByte 0]
          arr      = foldr (\(i, b) arr' -> S.store arr' (sSize i) b) emptyArr (zip [0..] sBytes)
      in sArrayWithLength tByte arr (sSize (fromIntegral $ BS.length bs))
      
    NewBuilder ty -> sEmptyL (symExecTy ty) (typeDefault ty)
    MapEmpty kt vt   -> sMapEmpty (symExecTy kt) (symExecTy vt)
    ENothing ty   -> sNothing (symExecTy ty)
    _ -> unimplemented
  where
    unimplemented = panic "Unimplemented" [showPP op]

symExecOp1 :: (Monad m, HasGUID m) => Op1 -> Type -> SExpr -> SolverT m SExpr
symExecOp1 op ty =
  pure . case op of
    CoerceTo tyTo    -> symExecCoerce ty tyTo
    IsEmptyStream    -> unimplemented
    Head             -> unimplemented
    StreamOffset     -> unimplemented
    StreamLen        -> unimplemented
    OneOf bs         -> \v -> S.orMany (map (S.eq v . sByte) (BS.unpack bs))
    Neg | Just _ <- isBits ty -> S.bvNeg
        | TInteger <- ty      -> S.neg
    BitNot | Just _ <- isBits ty -> S.bvNot
    Not | TBool <- ty  -> S.not
    ArrayLen | TArray elTy <- ty -> sArrayLen (symExecTy elTy)
    Concat   -> unimplemented -- concat an array of arrays
    FinishBuilder -> id -- builders and arrays are identical
    NewIterator  | TArray {} <- ty -> sArrayIterNew
    IteratorDone | TIterator (TArray elTy) <- ty -> sArrayIterDone (symExecTy elTy)
    IteratorKey  | TIterator (TArray {}) <- ty -> sArrayIterKey
    IteratorVal  | TIterator (TArray {}) <- ty -> sArrayIterVal
    IteratorNext | TIterator (TArray {}) <- ty -> sArrayIterNext
    EJust         -> sJust (symExecTy ty)
    FromJust      -> fun "fromJust"
    SelStruct _ l | TUser ut <- ty -> fun (labelToField (utName ut) l)
    -- FIXME: we probably need (_ as Foo) ...
    InUnion ut l    -> fun (labelToField (utName ut) l)
      
      -- S.app (S.as (S.const (labelToField (utName ut) l)) (symExecTy ty)) . (: []) 
    
    FromUnion _ l | TUser ut <- ty -> fun ("get-" ++ labelToField (utName ut) l)
    
    _ -> unimplemented -- shouldn't really happen, we should cover everything above

  where
    unimplemented :: a
    unimplemented = panic "Unimplemented" [showPP op]
    fun f = \v -> S.fun f [v]

symExecOp2 :: (Monad m, HasGUID m) => Op2 -> Type -> SExpr -> SExpr -> SolverT m SExpr

-- Generic ops
symExecOp2 Emit (TBuilder elTy) = \v1 v2 -> pure $ sPushBack (symExecTy elTy) v2 v1
symExecOp2 EmitArray   _ = panic "EmitArray is unimplemented" []
symExecOp2 EmitBuilder _ = panic "EmitBuilder is unimplemented" []
symExecOp2 Eq          _elTy = \v1 v2 -> pure $ S.eq v1 v2
symExecOp2 NotEq       _elTy = \x y -> pure $ S.distinct [x, y]
symExecOp2 MapLookup   (TMap kt vt) = \x y -> do
  fnm <- getPolyFun (PMapLookup (symExecTy kt) (symExecTy vt))
  pure $ S.app fnm [x, y]
symExecOp2 MapLookup   ty = panic "Unexpected type" [showPP ty]
symExecOp2 MapMember   (TMap kt vt) = \x y -> do
  fnm <- getPolyFun (PMapMember (symExecTy kt) (symExecTy vt))
  pure $ S.app fnm [x, y]
symExecOp2 MapMember   ty = panic "Unexpected type" [showPP ty]

symExecOp2 bop (isBits -> Just (signed, nBits)) =
  case bop of
    -- Stream ops
    IsPrefix -> unimplemented
    Drop     -> unimplemented
    Take     -> unimplemented
    
    Eq     -> pure2 S.eq
    NotEq  -> pure2 $ \x y -> S.distinct [x, y]
    Leq    -> pure2 $ if signed then S.bvSLeq else S.bvULeq
    Lt     -> pure2 $ if signed then S.bvSLt else S.bvULt
    
    Add    -> pure2 $ S.bvAdd
    Sub    -> pure2 $ S.bvSub
    Mul    -> pure2 $ S.bvMul
    Div    -> pure2 $ if signed then S.bvSDiv else S.bvUDiv
    Mod    -> pure2 $ if signed then S.bvSRem else S.bvURem

    BitAnd -> pure2 $ S.bvAnd
    BitOr  -> pure2 $ S.bvOr
    BitXor -> pure2 $ S.bvXOr

    Cat    -> pure2 $ S.concat
    LCat   -> unimplemented
    LShift -> pure2 $ \x y -> S.bvShl x (S.List [ S.fam "int2bv" [nBits], y] )
    RShift -> pure2 $ \x y -> (if signed then S.bvAShr else S.bvLShr)
                              x (S.List [ S.fam "int2bv" [nBits], y] )

    ArrayIndex  -> unimplemented
    Emit        -> unimplemented
    EmitArray   -> unimplemented
    EmitBuilder -> unimplemented
    
    MapLookup   -> unimplemented -- handled above
    MapMember   -> unimplemented -- handled above

    ArrayStream -> unimplemented
  where unimplemented = panic "Unimplemented" [showPP bop]
        pure2 f = \x y -> pure (f x y)
        
symExecOp2 bop TInteger =
  case bop of
    -- Stream ops
    IsPrefix -> unimplemented
    Drop     -> unimplemented
    Take     -> unimplemented

    Eq     -> pure2 $ S.eq
    NotEq  -> pure2 $ \x y -> S.distinct [x, y]
    Leq    -> pure2 $ S.leq
    Lt     -> pure2 $ S.lt
        
    Add    -> pure2 $ S.add
    Sub    -> pure2 $ S.sub
    Mul    -> pure2 $ S.mul
    Div    -> pure2 $ S.div
    Mod    -> pure2 $ S.mod

    BitAnd -> unimplemented
    BitOr  -> unimplemented
    BitXor -> unimplemented

    Cat    -> unimplemented
    LCat   -> unimplemented
    LShift -> unimplemented
    RShift -> unimplemented

    ArrayIndex  -> unimplemented
    Emit        -> unimplemented
    EmitArray   -> unimplemented
    EmitBuilder -> unimplemented
    
    MapLookup   -> unimplemented -- handled above
    MapMember   -> unimplemented -- handled above

    ArrayStream -> unimplemented
  where unimplemented = panic "Unimplemented" []
        pure2 f = \x y -> pure (f x y)
    
symExecOp2 bop _t = panic "Unsupported operation" [showPP bop]

symExecOp3 :: (Monad m, HasGUID m) => Op3 -> Type -> SExpr -> SExpr -> SExpr -> SolverT m SExpr
symExecOp3 MapInsert (TMap kt vt) m k v = do
  fnm <- getPolyFun (PMapInsert (symExecTy kt) (symExecTy vt))
  pure $ S.app fnm [m, k, v]
symExecOp3 op _ _ _ _ = panic "Unimpleented" [showPP op]

symExecOpN :: (Monad m, HasGUID m) => OpN -> [SExpr] -> SolverT m SExpr
symExecOpN (CallF fn) args = pure (S.fun (fnameToSMTName fn) args)
symExecOpN op _ = panic "Unimplemented" [showPP op]
                        
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

patternToPredicate :: Type -> Pattern -> SExpr -> SExpr
patternToPredicate ty p se = case p of
  PBytes _bs -> panic "FIXME" []
  PBool b    -> if b then se else S.not se
  PNothing   -> S.fun "is-Nothing" [se]
  PJust      -> S.fun "is-Just" [se]
  PNum n     -> S.eq se (mkLit n)
  PCon l     -> S.fun ("is-" ++ labelToField tyName l) [se]
  PAny       -> S.bool True
  where
    mkLit n = symExecOp0 (IntL n ty)

    tyName = case ty of
      TUser ut -> utName ut
      _        -> panic "Not a user type" [showPP ty]

-- Compile patterns to a list of predicates, one for each alternative
symExecCaseAlts :: (Monad m, HasGUID m, MonadIO m) => Case a -> SymExecM m [(SExpr, a)]
symExecCaseAlts (Case y alts) = do
  se <- symExecName y
  pure [ (patternToPredicate ty p se, a) | (p, a) <- alts ]
  where
    ty = typeOf y

symExecCase :: SymExec a -> SymExec (Case a)
symExecCase symE c = do
  alts <- symExecCaseAlts c
  let fallThru = symE (snd (last alts))
  foldr (\(p, a) rest -> S.ite p <$> symE a <*> rest) fallThru (init alts)

-- FIXME: this gives more readable output, but is a bit verbose.

-- symExecCase :: SymExec a -> SymExec (Case a)
-- symExecCase symE (Case e alts) =
--   case typeOf e of
--     -- FIXME: maybe we should normalise these somehow
--     TBool -> let mk = S.ite <$> symExecExpr e in
--       case alts of
--         (PBool True, tc) : (PBool False, fc) : _ -> mk <*> symE tc <*> symE fc
--         (PBool False, fc) : (PBool True, tc) : _ -> mk <*> symE tc <*> symE fc
--         (PBool True, tc) : (PAny, fc) : _        -> mk <*> symE tc <*> symE fc
--         (PBool False, fc) : (PAny, tc) : _       -> mk <*> symE tc <*> symE fc
--         _ -> panic "Unknown Bool case" []
          
--     TMaybe {} -> do
--       let mk nce jce = do
--             nc <- symE nce
--             jc <- symE jce
--             mkMatch <$> symExecExpr e <*> pure [ (S.const "Nothing", nc)
--                                                , (S.fun "Just" [S.const "_"], jc)
--                                                ]
--       case alts of
--         (PNothing, nc) : (PJust, jc) : _ -> mk nc jc
--         (PJust, jc) : (PNothing, nc) : _ -> mk nc jc
--         (PNothing, nc) : (PAny, jc) : _  -> mk nc jc
--         (PJust, jc) : (PAny, nc) : _     -> mk nc jc
--         _ -> panic "Unknown Maybe case" []
        
--     -- We translate a case into a smt case
--     TUser ut -> mkMatch <$> symExecExpr e <*> mapM (goAlt ut) alts
 
--     -- numeric cases
--     ty -> 
--       let pats = [ (n, sl) | (PNum n, sl) <- alts ]
--           -- FIXME: inefficient for non-trivial e
--           go (patn, s) rest = S.ite <$> (S.eq <$> symExecExpr e <*> mkLit ty patn)
--                                     <*> symE s <*> rest
          
--           base = case lookup PAny alts of
--             Nothing -> panic "Pure numeric case lacks a default" []
--             Just s  -> symE s
--       in foldr go base pats
--   where
--     mkLit ty n = -- a bit hacky
--       pure (symExecOp0 (IntL n ty))
 
--     goAlt ut (p, s) = do
--       let sp = case p of
--                  PAny   -> wildcard
--                  PCon l -> S.fun (labelToField (utName ut) l) [wildcard]
--                  _      -> panic "Unknown pattern" [showPP p]
--       (,) sp <$> symE s
  
--     wildcard = S.const "_"      


symExecExpr :: SymExec Expr
symExecExpr expr = 
  case expr of
    Var n       -> symExecName n
    PureLet n e e' -> do
      (n', r) <- bindNameFreshIn n (symExecExpr e')
      mklet n' <$> symExecExpr e <*> pure r
  
    Struct ut ctors ->
      S.fun (typeNameToCtor (utName ut)) <$> mapM (symExecExpr . snd) ctors
      
    ECase c -> symExecCase symExecExpr c
    
    ELoop _lm -> panic "symExec on loops is unimplemented" []
  
    Ap0 op      -> pure (symExecOp0 op)
    Ap1 op e    -> lift . symExecOp1 op (typeOf e) =<< symExecExpr e
    Ap2 op e e' -> do
      se  <- symExecExpr e
      se' <- symExecExpr e'
      lift (symExecOp2 op (typeOf e) se se')
    Ap3 op e1 e2 e3 -> do
      se1 <- symExecExpr e1
      se2 <- symExecExpr e2
      se3 <- symExecExpr e3
      
      lift (symExecOp3 op (typeOf e1) se1 se2 se3)
    
    ApN op args -> lift .  symExecOpN op =<< mapM symExecExpr args

symExecByteSet :: SExpr -> SymExec ByteSet
symExecByteSet b bs = go bs
  where
    go bs' =
      case bs' of
        SetAny          -> pure (S.bool True)
        SetSingle  v    -> S.eq b <$> symExecExpr v
        SetRange l h    -> S.and <$> (flip S.bvULeq b <$> symExecExpr l)
                                 <*> (S.bvULeq b <$> symExecExpr h)
    
        SetComplement c -> S.not <$> go c
        SetUnion l r    -> S.or <$> go l <*> go r
        SetIntersection l r -> S.and <$> go l <*> go r

        SetLet n e bs'' -> do
          (n', r) <- bindNameFreshIn n (go bs'')
          mklet n' <$> symExecExpr e <*> pure r
          
        SetCall f es  -> S.fun (fnameToSMTName f) <$> ((++ [b]) <$> mapM symExecExpr es)
        SetCase {}    -> unimplemented
    
    unimplemented = panic "SymExec (ByteSet): Unimplemented inside" [showPP bs]

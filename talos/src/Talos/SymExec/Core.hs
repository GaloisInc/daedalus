{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Symbolically execute Core terms

module Talos.SymExec.Core where

import Control.Monad.IO.Class (MonadIO(..))

-- import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Foldable (fold, forM_, find)

import qualified Data.ByteString as BS

import SimpleSMT (SExpr, ppSExpr)
import qualified SimpleSMT as S

import Daedalus.Panic
import Daedalus.PP
import Daedalus.GUID
import Daedalus.Rec

import Daedalus.Core hiding (freshName)
import Daedalus.Core.Free
import Daedalus.Core.Type
import Daedalus.Core.TraverseUserTypes

-- import Talos.Strategy.Monad
import Talos.Analysis.Slice

import Talos.SymExec.StdLib
import Talos.SymExec.SolverT
import Talos.SymExec.SemiValue (SemiValue(..))
import qualified Talos.SymExec.SemiValue as SV

import Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Daedalus.Value.Type as V
import Data.Map (Map)
import qualified Data.Vector as Vector
import Daedalus.Core.Semantics.Expr (matches, partial, evalOp0, evalOp1, evalOp2, evalOp3, evalOpN)
import Data.Monoid
import Control.Monad.Reader

-- -----------------------------------------------------------------------------
-- Class

class SymExec a where
  -- monadio for debugging
  symExec :: (Monad m, HasGUID m, MonadIO m) => a -> SolverT m SExpr

-- Lookup previously bound names
instance SymExec Name where
  symExec = getName

--------------------------------------------------------------------------------
-- Types
--
-- Types are embedded direclty (without renaming), hence this is non-monadic
  
symExecTName :: TName -> String
symExecTName = tnameToSMTName

labelToField :: TName -> Label -> String
labelToField n l = symExecTName n ++ "-" ++ show (pp l)

typeNameToCtor :: TName -> String
typeNameToCtor n = "mk-" ++ symExecTName n

-- | Construct SMT solver datatype declaration based on DaeDaLus type
-- declarations.

tranclTypeDefs :: Module -> Set TName -> [Rec SMTTypeDef]
tranclTypeDefs md roots0 = go [] roots0 (reverse (mTypes md))
  where
    go acc _ [] = acc
    go acc roots (NonRec td : rest) 
      | tName td `Set.member` roots =
        go (NonRec (tdeclToSMTTypeDef td) : acc) (Set.union roots (freeTCons td)) rest
      | otherwise                   = go acc roots rest
      
    go acc roots (MutRec tds : rest) 
      | any (\td -> tName td `Set.member` roots) tds = 
        go (MutRec (map tdeclToSMTTypeDef tds) : acc) (Set.unions (roots : map freeTCons tds)) rest
      | otherwise                   = go acc roots rest


-- we already have recs and it is ordered, so we don't need to calculate a fixpoint
sliceToSMTTypeDefs :: Module -> Slice -> [Rec SMTTypeDef]
sliceToSMTTypeDefs md = tranclTypeDefs md . freeTCons

tdeclToSMTTypeDef :: TDecl -> SMTTypeDef      
tdeclToSMTTypeDef td@(TDecl { tTParamKNumber = _ : _ }) =
  panic "Unsupported type decl (number params)" [showPP td]

tdeclToSMTTypeDef td@(TDecl { tTParamKValue = _ : _ }) =
  panic "Unsupported type decl (type params)" [showPP td]

tdeclToSMTTypeDef TDecl { tName = name, tTParamKNumber = [], tTParamKValue = [], tDef = td } =
  SMTTypeDef { stdName = name
             , stdBody = sflds
             }
  where
    sflds = case td of
              TStruct flds -> [(typeNameToCtor name, map mkOneS flds) ]
              TUnion flds  -> map mkOneU flds
    mkOneS (l, t) = (lblToFld l, symExecTy t)
    mkOneU (l, t) = (lblToFld l, [ ("get-" ++ lblToFld l, symExecTy t) ])
    lblToFld = labelToField name
      
defineSliceTypeDefs :: (MonadIO m, HasGUID m) => Module -> Slice -> SolverT m ()
defineSliceTypeDefs md sl = mapM_ defineSMTTypeDefs (sliceToSMTTypeDefs md sl)

symExecTy :: Type -> SExpr
symExecTy = go 
  where
    go ty' =
      let unimplemented = panic "Unsupported type" [showPP ty']
      in case ty' of
        TStream         -> unimplemented
        TUInt (TSize 8) -> tByte -- prettier in the output
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
        TBuilder elTy   -> tArrayWithLength (go elTy)
        TIterator (TArray elTy) -> tArrayIter (go elTy)
        TIterator {}    -> unimplemented -- map iterators
        TUser (UserType { utName = n, utNumArgs = [], utTyArgs = [] }) -> S.const (symExecTName n)
        TUser _ut       -> panic "Saw type with arguments" [showPP ty']
        TParam _x       -> panic "Saw type variable" [showPP ty']

typeDefault :: Type -> SExpr
typeDefault = go
  where
    go ty =
      let unimplemented = panic "Unsupported type" [showPP ty]
      in case ty of
        TStream         -> unimplemented
        TUInt {}        -> symExecOp0 (IntL 0 ty)
        TSInt {}        -> symExecOp0 (IntL 0 ty)
        TInteger        -> symExecOp0 (IntL 0 ty)
        TBool           -> S.bool False
        TUnit           -> sUnit
        TArray t'       -> sEmptyL (symExecTy t') (typeDefault t')
        TMaybe t'       -> sNothing (symExecTy t')
        TMap {}         -> unimplemented
        TBuilder elTy   -> go (TArray elTy) -- probably not needed?
        TIterator (TArray _elTy) -> unimplemented
        TIterator {}    -> unimplemented -- map iterators
        TUser (UserType { utName = n, utNumArgs = [], utTyArgs = [] }) -> 
          S.const (typeNameToDefault n)
        TUser {}        -> unimplemented
        TParam {}       -> unimplemented

--------------------------------------------------------------------------------
-- Functions

-- transitive closure from the roots in the grammar functions.
-- FIXME: (perf) we recalculate freeFVars in sliceToFunDefs

calcPureDeps :: Module -> Set FName -> Set FName
calcPureDeps md roots = go roots roots
  where
    go seen new
      | Set.null new = seen
      | otherwise =
        let new' = once new `Set.difference` seen
        in go (seen `Set.union` new') new'

    once new = fold (Map.restrictKeys depM new)

    depM = Map.fromList (map mkOne (mGFuns md) ++
                         map mkOne (mFFuns md) ++
                         map mkOne (mBFuns md))
      
    mkOne :: FreeVars e => Fun e -> (FName, Set FName)
    mkOne f = (fName f, freeFVars f)

-- We don't share code with sliceToFun as they are substantially different
funToFunDef :: (Monad m, FreeVars e, TraverseUserTypes e) => (e -> SolverT m SExpr) -> [(String, SExpr)] -> Fun e ->
               SolverT m SMTFunDef
funToFunDef _ _ Fun { fDef = External } =
  panic "Saw an external function" []

funToFunDef sexec extraArgs f@(Fun { fDef = Def body }) = do
  -- FIXME: this should be local to the body, not the context, and we
  -- can't really push as it would forget the defn. when popped
  mapM_ (\n -> modifyCurrentFrame (bindName n (nameToSMTName n))) (fParams f)
  b <- sexec body
  pure SMTFunDef { sfdName = fName f
                 , sfdArgs = args
                 , sfdRet  = symExecTy (fnameType (fName f))
                 , sfdBody = b
                 , sfdPureDeps = freeFVars f
                 , sfdTyDeps   = freeTCons f
                 }
  where
    args = map (\n -> (nameToSMTName n, symExecTy (nameType n))) (fParams f) ++ extraArgs -- For bytesets
    
-- FIXME: maybe calculate some of this once in StrategyM.
-- FIXME: filter by knownFNames here instead of in SolverT 
defineSliceFunDefs :: (MonadIO m, HasGUID m) => Module -> Slice -> SolverT m ()
defineSliceFunDefs md sl = do
  fdefs <- sequence $ foldMap (mkOneF [] symExec) (mFFuns md)
  bdefs <- sequence $ foldMap (mkOneF byteArg (flip symExecByteSet byteV)) (mBFuns md)  
  let allDefs     = fdefs ++ bdefs
      rFDefs      = topoOrder (\sfd -> (sfdName sfd, sfdPureDeps sfd)) allDefs

  forM_ allDefs $ mapM_ defineSMTTypeDefs . tranclTypeDefs md . sfdTyDeps
    
  mapM_ defineSMTFunDefs rFDefs
  where
    roots = freeFVars sl -- includes grammar calls as well
    allFs = calcPureDeps md roots

    byteN        = "_$b"
    byteV        = S.const byteN
    byteArg      = [(byteN, tByte)]

    mkOneF :: (Monad m, FreeVars e, TraverseUserTypes e) => [(String, SExpr)] -> (e -> SolverT m SExpr) -> Fun e ->
              [SolverT m SMTFunDef]
    mkOneF extraArgs sexec f
      | fName f `Set.member` allFs = [funToFunDef sexec extraArgs f]
      | otherwise                  = []


    -- ppS :: PP a => Set a -> Doc
    -- ppS  = braces . commaSep . map pp . Set.toList

    -- ppM :: (a -> Doc) -> (b -> Doc) ->  Map a b -> Doc
    -- ppM kf vf m = braces (commaSep [ kf x <> " -> " <> vf y | (x, y) <- Map.toList m])
    
    -- depM = Map.fromList (map mkOne (mFFuns md) ++ map mkOne (mBFuns md))
      
    -- mkOne :: FreeVars e => Fun e -> (FName, Set FName)
    -- mkOne f = (fName f, freeFVars f)

-- -----------------------------------------------------------------------------
-- Polymorphic functions

exprToPolyFuns :: Expr -> Set PolyFun
exprToPolyFuns = go
  where
    go expr =
      let children = foldMapChildrenE go expr
      in case expr of
        Ap2 MapMember me _ke ->
          let (kt, vt) = mapTys me
          in Set.insert (PMapMember (symExecTy kt) (symExecTy vt)) children
        Ap2 MapLookup me _ke ->
          let (kt, vt) = mapTys me
          in Set.insert (PMapLookup (symExecTy kt) (symExecTy vt)) children
        Ap3 MapInsert me _ke _ve ->
          let (kt, vt) = mapTys me
          in Set.insert (PMapInsert (symExecTy kt) (symExecTy vt)) children
        _              -> children

    mapTys e = case typeOf e of
      TMap kt vt -> (kt, vt)
      _          -> panic "Expecting a map type" [showPP (typeOf e)]

byteSetToPolyFuns :: ByteSet -> Set PolyFun
byteSetToPolyFuns = ebFoldMapChildrenB exprToPolyFuns byteSetToPolyFuns

defineSlicePolyFuns :: (MonadIO m, HasGUID m) => Slice -> SolverT m ()
defineSlicePolyFuns sl = mapM_ defineSMTPolyFun polys
  where
    polys = go sl
    go sl' = case sl' of
      SDontCare _n sl''  -> go sl''
      SDo _m_x l r       -> go l <> go r
      SUnconstrained     -> mempty
      SLeaf s            -> goL s

    goL l = case l of
      SPure _fset v -> exprToPolyFuns v
      SMatch m      -> byteSetToPolyFuns m
      SAssertion (GuardAssertion e) -> exprToPolyFuns e
      SChoice cs   -> foldMap go cs
      SCall cn     -> foldMap exprToPolyFuns (callNodeActualArgs cn)
      SCase _ c@(Case e _) -> exprToPolyFuns e <> foldMap go c
      
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
      in sArrayWithLength arr (sSize (fromIntegral $ BS.length bs))
      
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
    EJust         -> sJust
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
symExecOp2 ConsBuilder elTy = \v1 v2 -> pure $ sPushBack (symExecTy elTy) v1 v2
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
    ConsBuilder -> unimplemented
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
    ConsBuilder -> unimplemented
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

-- Pure case.  c.f. symExecGCase FIXME: merge the 2 functions?

instance SymExec a => SymExec (Case a) where
  symExec (Case e alts) =
    case typeOf e of
      -- FIXME: maybe we should normalise these somehow
      TBool -> let mk = S.ite <$> symExec e in
        case alts of
          (PBool True, tc) : (PBool False, fc) : _ -> mk <*> symExec tc <*> symExec fc
          (PBool False, fc) : (PBool True, tc) : _ -> mk <*> symExec tc <*> symExec fc
          (PBool True, tc) : (PAny, fc) : _        -> mk <*> symExec tc <*> symExec fc
          (PBool False, fc) : (PAny, tc) : _       -> mk <*> symExec tc <*> symExec fc
          _ -> panic "Unknown Bool case" []
            
      TMaybe {} -> do
        let mk nce jce = do
              nc <- symExec nce
              jc <- symExec jce
              mkMatch <$> symExec e <*> pure [ (S.const "Nothing", nc)
                                             , (S.fun "Just" [S.const "_"], jc)
                                             ]
        case alts of
          (PNothing, nc) : (PJust, jc) : _ -> mk nc jc
          (PJust, jc) : (PNothing, nc) : _ -> mk nc jc
          (PNothing, nc) : (PAny, jc) : _  -> mk nc jc
          (PJust, jc) : (PAny, nc) : _     -> mk nc jc
          _ -> panic "Unknown Maybe case" []
          
      -- We translate a case into a smt case
      TUser ut -> mkMatch <$> symExec e <*> mapM (goAlt ut) alts
  
      -- numeric cases
      ty -> 
        let pats = [ (n, sl) | (PNum n, sl) <- alts ]
            -- FIXME: inefficient for non-trivial e
            go (patn, s) rest = S.ite <$> (S.eq <$> symExec e <*> (mkLit ty patn)) <*> symExec s <*> rest
            
            base = case lookup PAny alts of
              Nothing -> panic "Pure numeric case lacks a default" []
              Just s  -> symExec s
        in foldr go base pats
    where
      mkLit ty n = -- a bit hacky
        pure (symExecOp0 (IntL n ty))
  
      goAlt ut (p, s) = do
        let sp = case p of
                   PAny   -> wildcard
                   PCon l -> S.fun (labelToField (utName ut) l) [wildcard]
                   _      -> panic "Unknown pattern" [showPP p]
        (,) sp <$> symExec s
    
      wildcard = S.const "_"      
  
instance SymExec Expr where
  symExec expr = 
    case expr of
      Var n       -> symExec n
      PureLet n e e' -> 
        mklet <$> freshName n <*> symExec e <*> symExec e'
  
      Struct ut ctors ->
        S.fun (typeNameToCtor (utName ut)) <$> mapM (symExec . snd) ctors
        
      ECase c -> symExec c

      Ap0 op      -> pure (symExecOp0 op)
      Ap1 op e    -> symExecOp1 op (typeOf e) =<< symExec e
      Ap2 op e e' -> join (symExecOp2 op (typeOf e) <$> symExec e <*> symExec e')
      Ap3 op e1 e2 e3 -> join (symExecOp3 op (typeOf e1) <$> symExec e1 <*> symExec e2 <*> symExec e3)
      
      ApN (CallF fn) args -> S.fun (fnameToSMTName fn) <$> mapM symExec args
      ApN {} -> unimplemented
    where
        unimplemented = panic "SymExec (Expr): Unimplemented" [showPP expr]

symExecByteSet :: (MonadIO m, HasGUID m, Monad m) => ByteSet -> SExpr -> SolverT m SExpr
symExecByteSet bs b = go bs
  where
    go bs' =
      case bs' of
        SetAny          -> pure (S.bool True)
        SetSingle  v    -> S.eq b <$> symExec v
        SetRange l h    -> S.and <$> (flip S.bvULeq b <$> symExec l)
                                 <*> (S.bvULeq b <$> symExec h)
    
        SetComplement c -> S.not <$> go c
        SetUnion l r    -> S.or <$> go l <*> go r
        SetIntersection l r -> S.and <$> go l <*> go r

        SetLet n e bs'' -> 
          mklet <$> freshName n <*> symExec e <*> go bs''
          
        SetCall f es  -> S.fun (fnameToSMTName f) <$> ((++ [b]) <$> mapM symExec es)
        SetCase {}    -> unimplemented
    
    unimplemented = panic "SymExec (ByteSet): Unimplemented inside" [showPP bs]

-- -----------------------------------------------------------------------------
-- Value -> SExpr

valueToSExpr :: Map TName TDecl -> Type -> V.Value -> SExpr
valueToSExpr tys ty v =
  case v of
    V.VUInt n i ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    V.VSInt n i -> -- FIXME: correct?
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    V.VInteger i -> S.int i               
    V.VBool b -> S.bool b
    V.VUnionElem _ l v'
      | TUser ut <- ty
      , Just TDecl { tDef = TUnion flds } <- Map.lookup (utName ut) tys
      , Just ty' <- lookup l flds
      -> S.fun (labelToField (utName ut) l) [go ty' v']
    
    -- FIXME: assumes the order is the same
    V.VStruct _ els
      | TUser ut <- ty
      , Just TDecl { tDef = TStruct flds } <- Map.lookup (utName ut) tys
      -> S.fun (typeNameToCtor (utName ut)) (zipWith goStruct els flds)
    
    V.VArray vs | TArray elty <- ty ->
      let sVals     = map (go elty) (Vector.toList vs)
          emptyArr = sArrayL (sEmptyL (symExecTy elty) (typeDefault elty)) -- FIXME, a bit gross?
          arr      = foldr (\(i, b) arr' -> S.store arr' (sSize i) b) emptyArr (zip [0..] sVals)
      in sArrayWithLength arr (sSize (fromIntegral $ Vector.length vs))
      
    V.VMaybe mv | TMaybe ty' <- ty ->
      case mv of
        Nothing -> sNothing (symExecTy ty')
        Just v' -> sJust (go ty' v')
        
    V.VMap m | TMap kt vt <- ty ->
      -- FIXME: breaks abstraction of maps
      sFromList (tTuple (symExecTy kt) (symExecTy vt))
                [ sTuple (go kt k) (go vt v) | (k, v) <- Map.toList m ]

      
    V.VStream {}                       -> unimplemented
    V.VBuilder vs
      | TBuilder elTy <- ty -> sFromList (symExecTy elTy) (reverse (map (go elTy) vs))
    V.VIterator vs ->
      let (kt, vt) = case ty of
            TIterator (TArray elTy) -> (sizeType, elTy)
            TIterator (TMap   kt vt) -> (kt, vt)
            _ -> panic "Malformed iterator type" []
      in sFromList (tTuple (symExecTy kt) (symExecTy vt)) [ sTuple (go kt k) (go vt v) | (k, v) <- vs ]

    _ -> unexpectedTy
  where
    unexpectedTy = panic "Unexpected type" [showPP v, showPP ty]
    
    go = valueToSExpr tys
    goStruct (l, e) (l', ty') | l == l' = go ty' e
    goStruct (l, _) (l', _) = panic "Mis-matched labels" [showPP l, showPP l']
      
    unimplemented = panic "Unimplemented" [showPP v]
    
-- -----------------------------------------------------------------------------
-- Semi symbolic/concrete evaluation

type SemiSExpr = SemiValue SExpr

semiSExprToSExpr :: Map TName TDecl -> Type -> SemiSExpr -> SExpr
semiSExprToSExpr tys ty sv =
  case sv of
    VValue v -> valueToSExpr tys ty v
    VOther s -> s
    -- FIXME: copied from valueToSExpr, unify.
    VUnionElem l v'
      | TUser ut <- ty
      , Just TDecl { tDef = TUnion flds } <- Map.lookup (utName ut) tys
      , Just ty' <- lookup l flds
      -> S.fun (labelToField (utName ut) l) [go ty' v']

    -- FIXME: copied from valueToSExpr, unify.
    VStruct els
      | TUser ut <- ty
      , Just TDecl { tDef = TStruct flds } <- Map.lookup (utName ut) tys
      -> S.fun (typeNameToCtor (utName ut)) (zipWith goStruct els flds)
                  
    VSequence vs ->
      let elTy = case ty of
            TArray elTy -> elTy
            TBuilder elTy -> elTy
            _ -> panic "Expecting a sequence-like structure" []
      in sFromList (symExecTy elTy) (map (go elTy) vs)
    VMaybe mv | TMaybe ty' <- ty -> case mv of
                  Nothing -> sNothing (symExecTy ty')
                  Just v' -> sJust (go ty' v')
        
    VMap ms | TMap kt vt <- ty ->
      -- FIXME: breaks abstraction of maps
      sFromList (tTuple (symExecTy kt) (symExecTy vt))
                [ sTuple (go kt k) (go vt v) | (k, v) <- ms ]
              
    VIterator vs ->
      let (kt, vt) = case ty of
            TIterator (TArray elTy) -> (sizeType, elTy)
            TIterator (TMap   kt vt) -> (kt, vt)
            _ -> panic "Malformed iterator type" []
      in sFromList (tTuple (symExecTy kt) (symExecTy vt)) [ sTuple (go kt k) (go vt v) | (k, v) <- vs ]

    _ -> panic "Malformed value" [show sv, showPP ty]
  where
    go = semiSExprToSExpr tys 
    goStruct (l, e) (l', ty') | l == l' = go ty' e
    goStruct (l, _) (l', _) = panic "Mis-matched labels" [showPP l, showPP l']

semiExecName :: (HasGUID m, Monad m) => Name -> SemiSolverM m SemiSExpr
semiExecName = undefined

bindNameIn :: Name -> SemiSExpr -> SemiSolverM m a -> SemiSolverM m a
bindNameIn = undefined

-- Stolen from Synthesis
-- projectEnvFor :: FreeVars t => t -> I.Env -> SynthEnv -> Maybe I.Env
-- projectEnvFor tm env0 se = doMerge <$> Map.traverseMaybeWithKey go (synthValueEnv se)
--   where
--     frees = freeVars tm

--     doMerge m = env0 { I.vEnv = Map.union m (I.vEnv env0) } 
    
--     go k v | k `Set.member` frees = Just <$> projectInterpValue v
--     go _ _                        = Just Nothing

-- projectEnvForM :: FreeVars t => t -> SynthesisM I.Env
-- projectEnvForM tm = do
--   env0 <- getIEnv
--   m_e <- SynthesisM $ asks (projectEnvFor tm env0)
--   case m_e of
--     Just e  -> pure e
--     Nothing -> panic "Captured stream value" []

-- Stolen from Daedalus.Core.Semantics.Expr
matches' :: Pattern -> SemiSExpr -> Maybe Bool
matches' pat (VValue v) = Just (matches pat v)
matches' pat v = 
  case pat of
    PBool {} -> Nothing
    
    PNothing | VMaybe mb <- v -> Just (isNothing mb)
    PNothing -> Nothing
    
    PJust    | VMaybe mb <- v -> Just (isJust mb)
    PJust    -> Nothing
    
    PCon l | VUnionElem l' _ <- v -> Just (l == l')
    PCon {} -> Nothing
    
    PAny   -> Just True

-- Says whether a variable occurs in a SExpr, taking into account let binders.
freeInSExpr :: String -> SExpr -> Bool
freeInSExpr n = getAny . go
  where
    go (S.Atom a) = Any (a == n)
    go (S.List [S.Atom "let", S.List es, e]) =
      let (Any stop, occ) = goL es
      in if stop then occ else occ <> go e
      
    go (S.List es) = foldMap go es

    goL (S.List [S.Atom a, be] : es) = (Any (a == n), go be) <> goL es
    goL [] = (mempty, mempty)
    goL _ = panic "Malformed let binding" []
    
freeInSemiSExpr :: String -> SemiSExpr -> Bool
freeInSemiSExpr n = getAny . foldMap (Any . freeInSExpr n)

data SemiSolverEnv = SemiSolverEnv
  { localBoundNames :: Map Name SemiSExpr
  , typeDefs        :: Map TName TDecl
  }
  
type SemiSolverM m = ReaderT SemiSolverEnv (SolverT m)

semiExecExpr :: (HasGUID m, Monad m, MonadIO m) => Map TName TDecl -> Expr -> SolverT m SemiSExpr
semiExecExpr tys e = runReaderT (semiExecExpr' e) (SemiSolverEnv mempty tys)

semiExecExpr' :: (HasGUID m, Monad m, MonadIO m) => Expr -> SemiSolverM m SemiSExpr
semiExecExpr' expr =
  case expr of
    Var n          -> semiExecName n
    PureLet n e e' -> do
      ve  <- go e
      bindNameIn n ve (go e') -- Maybe we should generate a let?
      
    Struct ut ctors -> do
      let (ls, es) = unzip ctors
      sves <- mapM go es
      pure (VStruct (zip ls sves))

    ECase (Case e pats) -> do
      ve <- go e
      -- we need to be careful not to match 'Any' if the others can't
      -- be determined.
      let ms = zip pats <$> mapM (flip matches' ve . fst) pats
      case ms of
        -- can't determine match, just return a sexpr
        Nothing -> VOther <$> lift (symExec expr)
        Just ps | Just ((_, e'), _) <- find snd ps -> go e'
        _ -> panic "Missing case" []

    Ap0 op      -> pure (VValue $ partial (evalOp0 op))
    Ap1 op e     -> join (semiExecOp1 op (typeOf e) <$> go e)
    Ap2 op e1 e2 -> join (semiExecOp2 op (typeOf e1) <$> go e1 <*> go e2)
    Ap3 op e1 e2 e3 -> join (semiExecOp3 op (typeOf e1) <$> go e1 <*> go e2 <*> go e3)
    ApN opN vs     -> semiExecOpN opN =<< mapM go vs
  where
    unimplemented = panic "semiExecExpr': unimplemented" [showPP expr]
    go = semiExecExpr'
    
-- Might be able to just use the value instead of requiring t
semiExecOp1 :: (Monad m, HasGUID m) => Op1 -> Type -> SemiSExpr -> SemiSolverM m SemiSExpr
semiExecOp1 op ty (VValue v) = pure $ VValue (evalOp1 op ty v)
semiExecOp1 op ty (VOther s) = lift (VOther <$> symExecOp1 op ty s)
semiExecOp1 op ty sv =
  -- We only care about operations over the compound semivalues (i.e., concrete values are handled above)
  case op of
    IsEmptyStream -> unimplemented
    Head          -> unimplemented
    StreamOffset  -> unimplemented
    StreamLen     -> unimplemented
    ArrayLen | Just svs <- SV.toList sv -> pure $ VValue (V.vSize (toInteger (length svs)))
    Concat   | Just svs <- SV.toList sv
             , Just svss <- mapM SV.toList svs -> pure (SV.fromList (mconcat svss))
    FinishBuilder -> pure sv
    NewIterator
      | TArray {} <- ty,
        Just svs <- SV.toList sv -> pure $ VIterator (zip (map (VValue . V.vSize) [0..]) svs)
    NewIterator -> unimplemented -- maps
    IteratorDone | VIterator els <- sv -> pure (VValue (V.VBool (null els)))
    IteratorKey  | VIterator ((k, _) : _) <- sv  -> pure k
    IteratorVal  | VIterator ((_, el) : _) <- sv -> pure el
    IteratorNext
      | VIterator (_ : els) <- sv -> pure (VIterator els)
      | VIterator [] <- sv -> panic "empty iterator" []
    
    EJust -> pure (VMaybe (Just sv))
    FromJust
      | VMaybe (Just sv') <- sv -> pure sv'
      | VMaybe Nothing    <- sv -> panic "FromJust: Nothing" []
      
    SelStruct _ty l
      | VStruct flds <- sv
      , Just v <- lookup l flds -> pure v
      
    InUnion ut l                      -> pure (VUnionElem l sv)
    FromUnion _ty l
      | VUnionElem l' sv' <- sv, l == l' -> pure sv'
      | VUnionElem {} <- sv -> panic "Incorrect union tag" []

    -- symbolic
    _ -> do
      tys <- asks typeDefs
      VOther <$> lift (symExecOp1 op ty (semiSExprToSExpr tys ty sv))
  where
    unimplemented = panic "semiEvalOp1: Unimplemented" [showPP op]

semiExecOp2 :: (Monad m, HasGUID m) => Op2 -> Type ->
               SemiSExpr -> SemiSExpr -> SemiSolverM m SemiSExpr
semiExecOp2 op _ty (VValue v1) (VValue v2) = pure $ VValue (evalOp2 op v1 v2)
semiExecOp2 op ty  (VOther v1) (VOther v2) =
  lift (VOther <$> symExecOp2 op ty v1 v2)
semiExecOp2 op ty sv1 sv2 = 
  case op of
    IsPrefix -> unimplemented
    Drop     -> unimplemented
    Take     -> unimplemented
    -- sv1 is arr, sv2 is ix
    ArrayIndex
      | Just svs <- SV.toList sv1
      , VValue v <- sv2, Just ix <- V.valueToIntSize v
        -> pure (svs !! ix)
    ConsBuilder
      | VSequence svs <- sv2 -> pure (VSequence (svs ++ [sv1]))
      
    -- sv1 is map, sv2 is key
    MapLookup -> mapOp (VValue $ V.VMaybe Nothing) (sNothing . symExecTy)
                       (VMaybe . Just)             sJust

    MapMember -> mapOp (VValue $ V.VBool False)        (const (S.bool False))
                       (const (VValue $ V.VBool True)) (const (S.bool True))

    ArrayStream -> unimplemented

    _ -> def
  where
    def = do
      tys <- asks typeDefs
      VOther <$> lift (symExecOp2 op ty
                       (semiSExprToSExpr tys ty sv1)
                       (semiSExprToSExpr tys ty sv2))

    
    unimplemented = panic "semiEvalOp2: Unimplemented" [showPP op]


    -- sv1 is map, sv2 is key
    mapOp missing smissingf found sfound
      | VMap els <- sv1, VValue kv <- sv2
      , Just res <- mapLookupV missing found kv els
        = pure res
      -- Expand into if-then-else.
      | VMap els <- sv1, TMap kt vt <- ty = do
          tys <- asks typeDefs
          let symkv = semiSExprToSExpr tys kt sv2
              mk    = sfound . semiSExprToSExpr tys vt
          pure (VOther $ foldr (mapLookupS tys mk kt symkv sv2)
                                (smissingf vt)
                                els)
      | otherwise = def

    mapLookupV z _f  _kv  [] = Just z
    mapLookupV z f  kv ((VValue kv', el) : rest) =
      if kv == kv' then Just (f el) else mapLookupV z f kv rest
    mapLookupV _ _ _ _ = Nothing

    mapLookupS tys f kTy symkv skv (skv', sel) rest =
      case (skv, skv') of
        (VValue kv, VValue kv')
          -> if kv == kv' then f sel else rest
        _ -> S.ite (S.eq symkv (semiSExprToSExpr tys kTy skv')) (f sel) rest


semiExecOp3 :: (Monad m, HasGUID m) => Op3 -> Type ->
               SemiSExpr -> SemiSExpr -> SemiSExpr ->
               SemiSolverM m SemiSExpr
semiExecOp3 op _ty (VValue v1) (VValue v2) (VValue v3) = pure $ VValue (evalOp3 op v1 v2 v3)
semiExecOp3 op ty  (VOther v1) (VOther v2) (VOther v3) =
  lift (VOther <$> symExecOp3 op ty v1 v2 v3)
semiExecOp3 MapInsert _ty (VMap ms) k v = pure (VMap ((k, v) : ms))
semiExecOp3 op        _   _         _ _ = panic "Unimplemented" [showPP op]  

semiExecOpN :: (Monad m, HasGUID m) => OpN -> [SemiSExpr] -> 
               SemiSolverM m SemiSExpr
semiExecOpN op vs = undefined--  | Just vs' <- mapM unValue vs =
                      -- do 
                      -- pure (VValue (evalOpN op vs'))
  where
    unValue (VValue v) = Just v
    unValue _ = Nothing
                                          
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



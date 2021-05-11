{-# Language GADTs, ViewPatterns, PatternGuards, OverloadedStrings #-}

-- Symbolically execute Core terms

module Talos.SymExec.Core where

import Control.Monad.IO.Class (MonadIO(..))

-- import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Foldable (fold, forM_)

import qualified Data.ByteString as BS

import SimpleSMT (SExpr)
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

-- -----------------------------------------------------------------------------
-- Class

class SymExec a where
  symExec :: (Monad m, HasGUID m) => a -> SolverT m SExpr

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
    ByteArrayL bs ->
      let sBytes   = map sByte (BS.unpack bs)
          emptyArr = S.app (S.as (S.const "const") (S.tArray tSize tByte)) [sByte 0]
          arr      = foldr (\(i, b) arr' -> S.store arr' (sSize i) b) emptyArr (zip [0..] sBytes)
      in sArrayWithLength arr (sSize (fromIntegral $ BS.length bs))
      
    NewBuilder ty -> sEmptyL (symExecTy ty) (typeDefault ty)
    MapEmpty {}   -> unimplemented
    ENothing ty   -> sNothing (symExecTy ty)
    _ -> unimplemented
  where
    unimplemented = panic "Unimplemented" [showPP op]

symExecOp1 :: Op1 -> Type -> SExpr -> SExpr
symExecOp1 op ty =
  case op of
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
    NewIterator  | TArray {} <- ty ->  sArrayIterNew
    IteratorDone | TIterator (TArray elTy) <- ty -> sArrayIterDone (symExecTy elTy)
    IteratorKey  | TIterator (TArray {}) <- ty -> sArrayIterKey
    IteratorVal  | TIterator (TArray {}) <- ty -> sArrayIterVal
    IteratorNext | TIterator (TArray {}) <- ty -> sArrayIterNext
    EJust         -> sJust
    FromJust        -> fun "fromJust"
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

symExecOp2 :: Op2 -> Type -> SExpr -> SExpr -> SExpr

-- Generic ops
symExecOp2 ConsBuilder elTy = sPushBack (symExecTy elTy)
symExecOp2 Eq          _elTy = S.eq
symExecOp2 NotEq       _elTy = \x y -> S.distinct [x, y]

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
  where unimplemented = panic "Unimplemented" [showPP bop]

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
      Ap1 op e    -> symExecOp1 op (typeOf e) <$> symExec e
      Ap2 op e e' -> symExecOp2 op (typeOf e) <$> symExec e <*> symExec e'
      Ap3 {}      -> unimplemented
      ApN (CallF fn) args -> S.fun (fnameToSMTName fn) <$> mapM symExec args
      ApN {} -> unimplemented
    where
      unimplemented = panic "SymExec (Expr): Unimplemented" [showPP expr]

symExecByteSet :: (HasGUID m, Monad m) => ByteSet -> SExpr -> SolverT m SExpr
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

      

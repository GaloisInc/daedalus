{-# Language BangPatterns #-}
{-# Language BlockArguments #-}
{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language RankNTypes #-}
module Daedalus.Core.Semantics.Expr where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.ByteString as BS
import Data.Maybe(isJust)
import Data.Bits(shiftL,(.|.))

import Data.BitVector.Sized(BV)
import qualified Data.BitVector.Sized as BV
import Data.Parameterized.NatRepr

import Daedalus.Panic(panic)

import RTS.Input as RTS

import Daedalus.Core.Basics
import Daedalus.Core.Expr

import Daedalus.Core.Semantics.Value
import Daedalus.Core.Semantics.Env


eval :: Expr -> Env -> Value
eval expr env =
  case expr of

    Var x -> lookupVar x env

    PureLet x e1 e2 ->
      eval e2 $! defLocal x (eval e1 env) env

    Struct t fs ->
      let flist = [ seq v (f,v) | (f,e) <- fs, let v = eval e env ]
      in VStruct (SType t (map fst flist)) (Map.fromList flist)

    Ap0 op          -> evalOp0 op
    Ap1 op e        -> evalOp1 op e env
    Ap2 op e1 e2    -> evalOp2 op e1 e2 env
    Ap3 op e1 e2 e3 -> evalOp3 op e1 e2 e3 env
    ApN op es       -> evalOpN op es env

evalArgs :: [Expr] -> Env -> [Value]
evalArgs xs env =
  case xs of
    e : more ->
      let a = eval e env
          as = evalArgs more env
      in a `seq` as `seq` (a : as)
    []       -> []


--------------------------------------------------------------------------------

evalOp0 :: Op0 -> Value
evalOp0 op =
  case op of
    Unit -> VUnit

    IntL i t ->
      case t of
        TInteger        -> VInt    i
        TUInt (TSize n) -> vUInt n i
        TSInt (TSize n) -> vSInt n i
        _ -> panic "evalOp0" [ "Numeric type" ]

    BoolL b         -> VBool b

    ByteArrayL bs   -> VArray t $
                       Vector.fromList [ vByte w | w <- BS.unpack bs ]
      where t = TUInt (TSize 8)

    NewBuilder t    -> VBuilder t []

    MapEmpty t1 t2  -> VMap t1 t2 Map.empty

    ENothing t      -> VNothing t


--------------------------------------------------------------------------------

evalOp1 :: Op1 -> Expr -> Env -> Value
evalOp1 op e env =
  let v = eval e env
  in case op of
      CoerceTo t -> fst (doCoerceTo t v)
      CoerceMaybeTo t ->
        case doCoerceTo t v of
          (r,Exact) -> VJust r
          _         -> VNothing t

      IsEmptyStream ->
        VBool $ inputEmpty $ fromVInput v

      Head ->
        case inputByte (fromVInput v) of
          Just (w,_) -> vByte w
          Nothing    -> panic "evalOp1" ["Head of empty list"]

      StreamOffset ->
        VInt $ toInteger $ inputOffset $ fromVInput v

      StreamLen ->
        VInt $ toInteger $ inputLength $ fromVInput v

      OneOf bs ->
        VBool $ isJust $ BS.elemIndex (fromVByte v) bs

      Neg -> numeric1 negate BV.negate BV.negate v

      BitNot ->
        case v of
          VUInt w x -> VUInt w (BV.complement w x)
          _         -> typeError "UInt" v

      Not ->
        VBool $ not $ fromVBool v

      ArrayLen ->
        VInt $ toInteger $ Vector.length $ fromVArray v

      Concat ->
        case v of
          VArray (TArray t) a ->
            VArray t $ Vector.concat $ Vector.toList $ fromVArray <$> a
          _ -> typeError "Array of Array" v

      FinishBuilder ->
        case v of
          VBuilder t a ->
            VArray t $ Vector.fromList $ reverse a
          _ -> typeError "Builder" v

      NewIterator ->
        case v of
          VArray t a ->
            VIterator (TArray t) $ zip (map VInt [ 0 .. ]) (Vector.toList a)
          VMap t1 t2 mp ->
            VIterator (TMap t1 t2) $ Map.toList mp
          _ -> typeError "Array or Map" v

      IteratorDone ->
        case v of
          VIterator _ xs -> VBool $ null xs
          _ -> typeError "Iterator" v

      IteratorKey ->
        case v of
          VIterator _ ((k,_) : _) -> k
          _ -> panic "evalOp1" [ "Key of empty iterator" ]

      IteratorVal ->
        case v of
          VIterator _ ((_,x) : _) -> x
          _ -> panic "evalOp1" [ "Value of empty iterator" ]

      IteratorNext ->
        case v of
          VIterator t ( ( !_, _) : xs) -> VIterator t xs
          _ -> panic "evalOp1" [ "Next of empty iterator" ]

      EJust -> VJust v

      FromJust ->
        case v of
          VJust r -> r
          _ -> panic "evalOp1" [ "Not Just" ]

      SelStruct _ l ->
        case v of
          VStruct _ mp | Just r <- Map.lookup l mp -> r
          _ -> typeError ("Sturct with " ++ show l) v

      InUnion t l ->
        VUnion t l v

      FromUnion _ _ ->
        case v of
          VUnion _ _ a -> a
          _ -> typeError "union" v

data Lossy = Exact | Lossy

doCoerceTo :: Type -> Value -> (Value, Lossy)
doCoerceTo t v =
  case t of

    TInteger ->
      case v of
        VInt {}     -> (v, Exact)
        VUInt _ i   -> (VInt (BV.asUnsigned i), Exact)
        VSInt w i   -> (VInt (BV.asSigned w i), Exact)
        _           -> typeError "Numeric type" v

    TUInt (TSizeParam _) -> panic "doCoerceTo" [ "Type variable" ]
    TUInt (TSize n) ->
      case v of

        VInt i ->
          let r = vUInt n i
          in (r, check i r)

        VUInt _ ui ->
          let i = BV.asUnsigned ui
              r = vUInt n i
          in (r, check i r)

        VSInt w si ->
          let i = BV.asSigned w si
              r = vSInt n i
          in (r, check i r)

        _ -> typeError "Numeric type" v

    TSInt (TSizeParam _) -> panic "doCoerceTo" [ "Type variable" ]
    TSInt (TSize n) ->
      case v of

        VInt i ->
          let r = vSInt n i
          in (r, check i r)

        VUInt _ ui ->
          let i = BV.asUnsigned ui
              r = vUInt n i
          in (r, check i r)

        VSInt w si ->
          let i = BV.asSigned w si
              r = vSInt n i
          in (r, check i r)

        _ -> typeError "Numeric type" v

    TStream         -> (v, Exact)
    TBool           -> (v, Exact)
    TUnit           -> (v, Exact)
    TMaybe {}       -> (v, Exact)
    TArray {}       -> (v, Exact)
    TMap {}         -> (v, Exact)
    TBuilder {}     -> (v, Exact)
    TIterator {}    -> (v, Exact)
    TUser {}        -> (v, Exact)
    TParam {}       -> panic "doCoerceTo" [ "Type parameter" ]



  where
  check x y = case y of
                VUInt _ z -> if x == BV.asUnsigned z then Exact else Lossy
                VSInt w z -> if x == BV.asSigned w z then Exact else Lossy
                _ -> panic "doCoerceTo.check" [ "Unexpected" ]

--------------------------------------------------------------------------------

evalOp2 :: Op2 -> Expr -> Expr -> Env -> Value
evalOp2 op e1 e2 env =
  let v1 = eval e1 env
      v2 = eval e2 env
  in case op of
       IsPrefix ->
         VBool $ fromVByteArray v1 `BS.isPrefixOf` inputBytes (fromVInput v2)

       Drop ->
        let n   = fromVInt v1
            i   = fromVInput v2
        in case advanceBy n i of
             Just i' -> VInput i'
             Nothing -> panic "evalOp2.Drop" [ "Not enough bytes." ]

       Take ->
        let n   = fromVInt v1
            i   = fromVInput v2
        in case limitLen n i of
             Just i' -> VInput i'
             _       -> panic "evalOp2.Take" [ "Not enough bytes." ]

       Eq       -> VBool (v1 == v2)
       NotEq    -> VBool (v1 /= v2)
       Leq      -> VBool (v1 <= v2)
       Lt       -> VBool (v1 < v2)

       Add      -> numeric2 (+) BV.add BV.add v1 v2
       Sub      -> numeric2 (-) BV.sub BV.sub v1 v2
       Mul      -> numeric2 (*) BV.mul BV.mul v1 v2
       Div      -> numeric2 quot (const BV.uquot) BV.squot v1 v2
       Mod      -> numeric2 rem  (const BV.urem)  BV.srem v1 v2

       BitAnd   -> bitOp2 BV.and  v1 v2
       BitOr    -> bitOp2 BV.or   v1 v2
       BitXor   -> bitOp2 BV.xor  v1 v2
       Cat ->
         case (v1,v2) of
           (VUInt w x, VUInt w' y) -> VUInt (addNat w w') (BV.concat w w' x y)
           _ -> panic "evalOp2.Cat" [ "Bad inputs" ]

       LCat
         | VUInt w2 y <- v2
         , let fInt i = (i `shiftL` widthVal w2) .|. i

               fUInt :: NatRepr w -> BV w -> BV w
               fUInt w i =
                 case testNatCases w2 w of
                   NatCaseLT LeqProof ->
                     BV.shl w i (natValue w2) `BV.or` BV.zext w y
                   NatCaseEQ   -> i
                   NatCaseGT _ -> BV.select' 0 w y

               fSInt :: (1 <= w) => NatRepr w -> BV w -> BV w
               fSInt w i =
                 case testNatCases w2 w of
                   NatCaseLT LeqProof ->
                     BV.shl w i (natValue w2) `BV.or` BV.zext w y
                   NatCaseEQ   -> i
                   NatCaseGT _ -> BV.select' 0 w y


             -> numeric1 fInt fUInt fSInt v1

         | otherwise -> typeError "Bit vecotr" v2

       LShift ->
         case (v1,v2) of
           (VUInt w i, VInt amt)
              | amt >= 0  -> VUInt w (BV.shl w i (fromIntegral amt))
              | otherwise -> VUInt w (BV.lshr w i (fromIntegral (negate amt)))
           _ -> panic "evalOp2.LShift" ["Type error"]

       RShift ->
         case v1 of
           VUInt w i
              | amt >= 0  -> VUInt w (BV.lshr w i (fromIntegral amt))
              | otherwise -> VUInt w (BV.shl  w i (fromIntegral (negate amt)))
                where amt = fromVInt v2
           _ -> typeError "UInt" v1

       Or  -> VBool (fromVBool v1 || fromVBool v2)    -- lazy
       And -> VBool (fromVBool v1 && fromVBool v2)    -- lazy

       -- array is 1st
       ArrayIndex -> fromVArray v1 Vector.! ix
         where ix = let i = fromVInt v2
                        j = fromIntegral (fromVInt v2)
                    in if toInteger j == i
                          then j else error "Array lookup out of bounds."

       -- builder is 2nd
       ConsBuilder ->
         case v2 of
           VBuilder t xs -> v1 `seq` VBuilder t (v1 : xs)
           _ -> typeError "Builder" v2

       -- map is 1st
       MapLookup ->
         case v1 of
           VMap _kT vT mp ->
             case Map.lookup v2 mp of
               Just v  -> VJust v
               Nothing -> VNothing vT
           _ -> typeError "Map" v1

       -- map is 1st
       MapMember -> VBool $ Map.member v2 $ fromVMap v1


       ArrayStream ->
         let name  = fromVByteArray v1
             bytes = fromVByteArray v2
         in VInput (newInput name bytes)


bitOp2 :: (forall w. BV w -> BV w -> BV w) -> Value -> Value -> Value
bitOp2 f v1 v2 =
  case (v1,v2) of
    (VUInt w x, VUInt w' y)
      | Just Refl <- testEquality w w' -> VUInt w (f x y)
    _ -> panic "bitOp2" ["Not matched bit vectors."]


numeric1 ::
  (Integer -> Integer) ->
  (forall w. NatRepr w -> BV w -> BV w) ->
  (forall w. (1 <= w) => NatRepr w -> BV w -> BV w) ->
  Value -> Value
numeric1 fInt fUInt fSInt v =
  case v of
    VInt x    -> VInt (fInt x)
    VUInt w x -> VUInt w (fUInt w x)
    VSInt w x -> VSInt w (fSInt w x)
    _ -> typeError "numeric type" v



numeric2 ::
  (Integer -> Integer -> Integer) ->
  (forall w. NatRepr w -> BV w -> BV w -> BV w) ->
  (forall w. (1 <= w) => NatRepr w -> BV w -> BV w -> BV w) ->
  Value -> Value -> Value
numeric2 fInt fUInt fSInt v1 v2 =
  case (v1,v2) of
    (VInt x, VInt y)                    -> VInt (fInt x y)
    (VUInt w x, VUInt w' y)
      | Just Refl <- testEquality w w'  -> VUInt w (fUInt w x y)
    (VSInt w x, VSInt w' y)
      | Just Refl <- testEquality w w'  -> VSInt w (fSInt w x y)
    _ -> typeError "numeric type" v1

--------------------------------------------------------------------------------

evalOp3 :: Op3 -> Expr -> Expr -> Expr -> Env -> Value
evalOp3 op e1 e2 e3 env =
  let v1 = eval e1 env
      v2 = eval e2 env
      v3 = eval e3 env
  in case op of
       PureIf -> if fromVBool v1 then v2 else v2    -- lazy
       RangeUp ->
         case (v1,v2,v3) of
           (VInt start, VInt end, VInt step)
              | step > 0 -> rangeArray TInteger VInt
                              [ start, start + step .. end - 1 ]
              | otherwise -> error "Range must be positive"

           (VUInt w start, VUInt w' end, VUInt w'' step)
              | Just Refl <- testEquality w w'
              , Just Refl <- testEquality w w'' ->
                rangeArray (TUInt (TSize (intValue w))) (VUInt w) $
                vRangeUp w (const BV.ult) start step end

           (VSInt w start, VSInt w' end, VSInt w'' step)
              | Just Refl <- testEquality w w'
              , Just Refl <- testEquality w w'' ->
                rangeArray (TSInt (TSize (intValue w))) (VSInt w) $
                vRangeUp w BV.slt start step end

           _ -> panic "evalOp3.rangeUp" [ "Type error" ]

       RangeDown ->
         case (v1,v2,v3) of
           (VInt start, VInt end, VInt step)
              | step > 0 -> rangeArray TInteger VInt
                              [ start, start - step .. end + 1 ]
              | otherwise -> error "Range must be positive"

           (VUInt w start, VUInt w' end, VUInt w'' step)
              | Just Refl <- testEquality w w'
              , Just Refl <- testEquality w w'' ->
                rangeArray (TUInt (TSize (intValue w))) (VUInt w) $
                vRangeDown w (const BV.ult) start step end

           (VSInt w start, VSInt w' end, VSInt w'' step)
              | Just Refl <- testEquality w w'
              , Just Refl <- testEquality w w'' ->
                rangeArray (TSInt (TSize (intValue w))) (VSInt w) $
                vRangeDown w BV.slt start step end

           _ -> panic "evalOp3.rangeDown" [ "Type error" ]




       MapInsert ->
         case v1 of
           VMap kT kV mp -> v3 `seq` VMap kT kV (Map.insert v2 v3 mp)
           _ -> typeError "Map" v1


rangeArray :: Type -> (a -> Value) -> [a] -> Value
rangeArray ty mkV els = VArray ty $ Vector.fromList $ map mkV els


vRangeUp ::
  NatRepr w ->
  (NatRepr w -> BV w -> BV w -> Bool) ->
  BV w -> BV w -> BV w -> [BV w]
vRangeUp w isLt start step end
  | isLt w step (BV.mkBV w 1) = error "Step must be positive"
  | isLt w start end = enum w isLt BV.add step start (BV.sub w end start)
  | otherwise = []

vRangeDown ::
  NatRepr w ->
  (NatRepr w -> BV w -> BV w -> Bool) ->
  BV w -> BV w -> BV w -> [BV w]
vRangeDown w isLt start step end
  | isLt w step (BV.mkBV w 1) = error "Step must be positive"
  | isLt w end start = enum w isLt BV.sub step end   (BV.sub w start end)
  | otherwise = []


enum ::
  NatRepr w ->
  (NatRepr w -> BV w -> BV w -> Bool) ->
  (NatRepr w -> BV w -> BV w -> BV w) ->
  BV w -> BV w -> BV w -> [ BV w ]
enum w isLt upd step = go
    where
    go cur todo = cur : more
      where
      more = if isLt w step todo
               then go (upd w cur step) (BV.sub w todo step)
               else []



--------------------------------------------------------------------------------

evalOpN :: OpN -> [Expr] -> Env -> Value
evalOpN op es env =
  let vs = evalArgs es env
  in case op of
       ArrayL t -> VArray t (Vector.fromList vs)
       CallF f  -> case vs of
                     [] -> lookupConst f env
                     _  -> lookupFun f env vs



{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Daedalus.VM.Compile.Expr where

import Data.Void(Void)
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad(forM)

import Daedalus.Panic(panic)
import Daedalus.PP(pp)

import qualified Daedalus.Core as Src
import qualified Daedalus.Core.Type as Src

import Daedalus.VM
import Daedalus.VM.Compile.BlockBuilder
import Daedalus.VM.Compile.Monad
import Daedalus.VM.Compile.StrPat


compileEs :: [Src.Expr] -> ([E] -> BlockBuilder Void) -> C (BlockBuilder Void)
compileEs es0 k = go [] es0
  where
  go ls es =
    case es of

      [] ->
        pure
          do vs <- mapM getLocal ls
             k (reverse vs)

      e : more ->
        do l <- newLocal (TSem (Src.typeOf e))
           b <- go (l : ls) more
           compileE e $ Just \v ->
             do setLocal l v
                b

type CE = Maybe (E -> BlockBuilder Void) -> C (BlockBuilder Void)

continue :: Maybe (E -> BlockBuilder Void) -> E -> BlockBuilder Void
continue = fromMaybe (term . ReturnPure)

-- | Get the current source location from the debug mode.
getCurLoc :: C Text
getCurLoc =
  do dbg <- getDebugMode
     pure case dbg of
       NoDebug -> ""
       DebugStack _ ls ->
         case ls of
           a : _ -> showAnnot a
           _     -> ""
  where
  showAnnot a = case a of
    Src.SrcAnnot txt -> txt
    Src.SrcRange r   -> Text.pack (show (pp r))
    _                -> ""

-- | Guard an operation: if the boolean check is true, run the body;
-- otherwise raise an exception.  The check and body are given access
-- to the block builder so they can reference already-compiled operands.
guarded :: Text -> BlockBuilder E -> BlockBuilder Void -> C (BlockBuilder Void)
guarded msg check body =
  do loc  <- getCurLoc
     okL  <- label0 NormalBlock body
     excL <- label0 NormalBlock (term (Throw loc msg))
     pure do cond <- check
             jumpIf cond okL excL

compileE :: Src.Expr -> CE
compileE expr k =
  case expr of

    Src.Var x ->
      do b <- lookupN x
         pure (continue k =<< b)

    Src.PureLet x e1 e2 ->
      do l <- newLocal (TSem (Src.typeOf x))
         b <- gdef x l (compileE e2 k)
         compileE e1 $ Just \v ->
           do setLocal l v
              b

    Src.Struct t fs ->
      let labs = case Src.tnameFlav (Src.utName t) of
                   Src.TFlavStruct ls -> ls
                   _ -> panic "compileE" ["Not TStructFlav"]
      in
      compileEs (map snd fs) \vs ->
         do let mp = Map.fromList (zip (map fst fs) vs)
                args = [ mp Map.! l | l <- labs ]
            s <- stmt ty (\x -> CallPrim x (StructCon t) args)
            continue k s

    Src.ECase (Src.Case x as) -> compileCase (Src.typeOf expr) x as k
    Src.ELoop {} -> panic "compileE" ["Saw a ELoop"]

    Src.Ap0 op          -> compileOp0 op ty k
    Src.Ap1 op e        -> compileOp1 op ty e k
    Src.Ap2 op e1 e2    -> compileOp2 op ty e1 e2 k
    Src.Ap3 op e1 e2 e3 -> compileOp3 op ty e1 e2 e3 k
    Src.ApN op es       -> compileOpN op ty es k

  where ty = TSem (Src.typeOf expr)

compileOp0 :: Src.Op0 -> VMT -> CE
compileOp0 op ty k' =
  pure
  case op of
    Src.Unit              -> k EUnit
    Src.IntL i t
      | Src.TInteger <- t -> k =<< stmt ty (\x -> CallPrim x (Integer i) [])
      | otherwise         -> k (ENum i t)
    Src.BoolL b           -> k (EBool b)
    Src.FloatL f t        -> k (EFloat f t)
    Src.ByteArrayL bs     -> k =<< stmt ty (\x -> CallPrim x (ByteArray bs) [])
    Src.NewBuilder t      -> k =<< stmt ty (\x -> CallPrim x (NewBuilder t) [])
    Src.MapEmpty t1 t2    -> k (EMapEmpty t1 t2)
    Src.ENothing t        -> k (ENothing t)
  where
  k = continue k'

compileOp1 :: Src.Op1 -> VMT -> Src.Expr -> CE
compileOp1 op ty e k =
  case op of
    Src.Neg
      | Src.TSInt (Src.TSize n) <- Src.typeOf e ->
        guardNegSInt n ty e k
      | Src.TUInt _ <- Src.typeOf e ->
        guardNegUInt ty e k
    Src.CoerceTo tgt
      | isFloatToInt (Src.typeOf e) tgt -> guardFloatToInt tgt ty e k

    _ ->
      compileE e $ Just \v -> continue k =<< stmt ty (\x -> CallPrim x (Op1 op) [v])

-- Check: e != minBound
guardNegSInt :: Integer -> VMT -> Src.Expr -> CE
guardNegSInt n ty e k =
  do let minVal = negate (2 ^ (n - 1))
         argTy  = Src.typeOf e
         check v = stmt (TSem Src.TBool)
                     (\x -> CallPrim x (Op2 Src.NotEq) [v, ENum minVal argTy])
         body v = continue k =<<
                    stmt ty (\x -> CallPrim x (Op1 Src.Neg) [v])
     l <- newLocal (TSem argTy)
     code <- guarded "negation overflow"
               (check =<< getLocal l) (body =<< getLocal l)
     compileE e $ Just \v ->
       do setLocal l v
          code

-- Check: e == 0
guardNegUInt :: VMT -> Src.Expr -> CE
guardNegUInt ty e k =
  do let argTy = Src.typeOf e
         check v = stmt (TSem Src.TBool)
                     (\x -> CallPrim x (Op2 Src.Eq) [v, ENum 0 argTy])
         body v = continue k =<<
                    stmt ty (\x -> CallPrim x (Op1 Src.Neg) [v])
     l <- newLocal (TSem argTy)
     code <- guarded "negation of non-zero unsigned integer"
               (check =<< getLocal l) (body =<< getLocal l)
     compileE e $ Just \v ->
       do setLocal l v
          code

-- Check: not (isNaN e) && not (isInfinite e)
guardFloatToInt :: Src.Type -> VMT -> Src.Expr -> CE
guardFloatToInt tgt ty e k =
  do let argTy = Src.typeOf e
         tgtTxt = Text.pack (show (pp tgt))
         checkNot prim v =
           do r <- stmt (TSem Src.TBool)
                     (\x -> CallPrim x (Op1 prim) [v])
              stmt (TSem Src.TBool)
                (\x -> CallPrim x (Op1 Src.Not) [r])
         body v = continue k =<<
                    stmt ty (\x -> CallPrim x (Op1 (Src.CoerceTo tgt)) [v])
     l <- newLocal (TSem argTy)
     infCode <- guarded ("coercion of Infinity to " <> tgtTxt)
                  (checkNot Src.IsInfinite =<< getLocal l) (body =<< getLocal l)
     nanCode <- guarded ("coercion of NaN to " <> tgtTxt)
                  (checkNot Src.IsNaN =<< getLocal l) infCode
     compileE e $ Just \v ->
       do setLocal l v
          nanCode

isFloatToInt :: Src.Type -> Src.Type -> Bool
isFloatToInt src tgt =
  case src of
    Src.TFloat  -> isIntegerType tgt
    Src.TDouble -> isIntegerType tgt
    _           -> False

isIntegerType :: Src.Type -> Bool
isIntegerType t =
  case t of
    Src.TInteger -> True
    Src.TUInt _  -> True
    Src.TSInt _  -> True
    _            -> False

isBoundedType :: Src.Type -> Bool
isBoundedType t =
  case t of
    Src.TUInt _ -> True
    Src.TSInt _ -> True
    _           -> False

compileOp2 :: Src.Op2 -> VMT -> Src.Expr -> Src.Expr -> CE
compileOp2 op ty e1 e2 k =
  case op of
    Src.Add
      | isBoundedType (Src.typeOf e1) -> guardedArith Src.Add e1 e2 k
    Src.Sub
      | isBoundedType (Src.typeOf e1) -> guardedArith Src.Sub e1 e2 k
    Src.Mul
      | isBoundedType (Src.typeOf e1) -> guardedArith Src.Mul e1 e2 k
    Src.Div
      | Src.TSInt (Src.TSize n) <- Src.typeOf e1 -> guardDivModSInt n Src.Div ty e1 e2 k
      | isIntegerType (Src.typeOf e1) -> guardDivMod Src.Div ty e1 e2 k
    Src.Mod
      | Src.TSInt (Src.TSize n) <- Src.typeOf e1 -> guardDivModSInt n Src.Mod ty e1 e2 k
      | isIntegerType (Src.typeOf e1) -> guardDivMod Src.Mod ty e1 e2 k
    _ ->
      compileEs [e1,e2] \vs -> continue k =<<
                                      stmt ty (\x -> CallPrim x (Op2 op) vs)

guardedArith :: Src.Op2 -> Src.Expr -> Src.Expr -> CE
guardedArith op e1 e2 k =
  do let argTy = Src.typeOf e1
         msg = case op of
                 Src.Add -> "addition out of bounds"
                 Src.Sub -> "subtraction out of bounds"
                 Src.Mul -> "multiplication out of bounds"
                 _       -> panic "guardedArith" [show (pp op)]
     loc  <- getCurLoc
     okL  <- setCurTy (TSem argTy) (label1 (continue k))
     excL <- label0 NormalBlock (term (Throw loc msg))
     compileEs [e1,e2] \[v1, v2] ->
       do (overflow, result) <-
            stmt2 (TSem Src.TBool) (TSem argTy)
                  (\x y -> CallPrim2 x y (Op2 op) [v1, v2])
          jumpIf overflow excL (okL result)

-- Check: b != 0
guardDivMod :: Src.Op2 -> VMT -> Src.Expr -> Src.Expr -> CE
guardDivMod op ty e1 e2 k =
  do let doOp l1 l2 = pure
           do a <- getLocal l1
              b <- getLocal l2
              continue k =<< stmt ty (\x -> CallPrim x (Op2 op) [a, b])
     guardDivMod' (Src.typeOf e2) doOp e1 e2

-- Check: b != 0 && !(a == minBound && b == -1)
guardDivModSInt :: Integer -> Src.Op2 -> VMT -> Src.Expr -> Src.Expr -> CE
guardDivModSInt n op ty e1 e2 k =
  do let argTy  = Src.typeOf e1
         minVal = negate (2 ^ (n - 1))
         doOp l1 l2 =
           do let body =
                    do a <- getLocal l1
                       b <- getLocal l2
                       continue k =<< stmt ty (\x -> CallPrim x (Op2 op) [a, b])
                  -- Check: !(a == minBound && b == -1)
                  check =
                    do a <- getLocal l1
                       b <- getLocal l2
                       aMin <- stmt (TSem Src.TBool)
                                 (\x -> CallPrim x (Op2 Src.Eq) [a, ENum minVal argTy])
                       bNeg1 <- stmt (TSem Src.TBool)
                                  (\x -> CallPrim x (Op2 Src.Eq) [b, ENum (-1) argTy])
                       both <- stmt (TSem Src.TBool)
                                 (\x -> CallPrim x (Op2 Src.BitAnd) [aMin, bNeg1])
                       stmt (TSem Src.TBool)
                         (\x -> CallPrim x (Op1 Src.Not) [both])
              guarded "signed division overflow (minBound / -1)" check body
     guardDivMod' argTy doOp e1 e2

-- Shared: check b != 0, then run the given body.
guardDivMod' ::
  Src.Type ->
  (FV -> FV -> C (BlockBuilder Void)) ->
  Src.Expr -> Src.Expr -> C (BlockBuilder Void)
guardDivMod' argTy mkBody e1 e2 =
  do l1 <- newLocal (TSem argTy)
     l2 <- newLocal (TSem argTy)
     body <- mkBody l1 l2
     code <- guarded "division by zero"
               (do b <- getLocal l2
                   stmt (TSem Src.TBool)
                     (\x -> CallPrim x (Op2 Src.NotEq) [b, ENum 0 argTy]))
               body
     compileEs [e1,e2] \[v1,v2] ->
       do setLocal l1 v1
          setLocal l2 v2
          code

compileOp3 :: Src.Op3 -> VMT -> Src.Expr -> Src.Expr -> Src.Expr -> CE
compileOp3 op ty e1 e2 e3 k =
  case op of
    Src.RangeUp   -> guardedRange
    Src.RangeDown -> guardedRange
    _ -> compileEs [e1,e2,e3] \vs -> continue k =<<
                                       stmt ty (\x -> CallPrim x (Op3 op) vs)
  where
  guardedRange =
    do let stepTy = Src.typeOf e3
       l1 <- newLocal (TSem (Src.typeOf e1))
       l2 <- newLocal (TSem (Src.typeOf e2))
       l3 <- newLocal (TSem stepTy)
       let check = do v <- getLocal l3
                      stmt (TSem Src.TBool)
                        (\x -> CallPrim x (Op2 Src.Lt) [ENum 0 stepTy, v])
           body  = do v1 <- getLocal l1
                      v2 <- getLocal l2
                      v3 <- getLocal l3
                      continue k =<<
                        stmt ty (\x -> CallPrim x (Op3 op) [v1, v2, v3])
       code <- guarded "invalid range step (must be positive)" check body
       compileEs [e1,e2,e3] \[v1,v2,v3] ->
         do setLocal l1 v1
            setLocal l2 v2
            setLocal l3 v3
            code


compileOpN :: Src.OpN -> VMT -> [Src.Expr] -> CE
compileOpN op ty es k =
  case op of
    Src.ArrayL _ ->
      compileEs es \vs ->
        do res <- stmt ty (\x -> CallPrim x (OpN op) vs)
           continue k res

    Src.CallF f ->
      do doCall <-
           case k of
             Nothing -> pure \vs -> term (TailCall f NoCapture vs)
             Just k' ->
               do mkL <- retPure (Src.typeOf f) k'
                  pure \vs ->
                    do l <- mkL
                       term (CallPure f (jumpNoFree l) vs mempty)

         compileEs es doCall


compileCase ::
  Src.Type -> Src.Name -> [(Src.Pattern, Src.Expr)] ->
  Maybe (E -> BlockBuilder Void) ->
  C (BlockBuilder Void)
compileCase resT x as k =
  do next' <- case k of
                Nothing -> pure Nothing
                Just kont ->
                  do res  <- newLocal (TSem resT)
                     nextL <- label0 NormalBlock (kont =<< getLocal res)
                     pure $ Just \v -> do setLocal res v
                                          jump nextL

     codes <- forM as \(p,rhs) ->
                do l <- label0 NormalBlock =<< compileE rhs next'
                   pure (p, l)

     compileCaseBranches x codes


compileCaseBranches ::
  Src.Name -> [(Pattern, BlockBuilder JumpPoint)] -> C (BlockBuilder Void)
compileCaseBranches x codes =
  case Src.typeOf x of
    Src.TArray _ -> compileStrCase x codes
    _            -> do b <- lookupN x
                       pure (jumpCase (Map.fromList codes) =<< b)



compileStrCase ::
  Src.Name -> [(Pattern, BlockBuilder JumpPoint)] -> C (BlockBuilder Void)
compileStrCase x codes =
  do lenCodes <- forM (Map.toList decision) \(n,opts) ->
                    do l <- label0 NormalBlock =<< compileN 0 opts
                       pure (PNum (toInteger n), l)
     let lenMap = Map.insert PAny dflt (Map.fromList lenCodes)
     let srcLenExp = Src.arrayLen (Src.Var x)
     compileE srcLenExp $ Just \e -> jumpCase lenMap e


  where
  (strAlts,dflt) = splitUp [] codes
  decision = strDecisionTree strAlts

  compileN ::
    Integer -> StrTree (BlockBuilder JumpPoint) -> C (BlockBuilder Void)
  compileN n tree =
    case tree of
      StrDone l -> pure (jump l)
      StrCase mp ->
        do let mkPat k = PNum (toInteger k)
               mkBranch b = label0 NormalBlock =<< compileN (n+1) b
           caseMap0 <- traverse mkBranch (Map.mapKeys mkPat mp)
           let caseMap = Map.insert PAny dflt caseMap0
           let sizeT = Src.TUInt (Src.TSize 64)
           let byteExpr = Src.arrayIndex (Src.Var x) (Src.intL n sizeT)
           compileE byteExpr $ Just \e -> jumpCase caseMap e

  splitUp alts ps =
    case ps of
      (p,b) : more ->
        case p of
          PBytes bs -> splitUp ((bs,b) : alts) more
          PAny   -> (reverse alts, b)
          _      -> panic "compileStrCase" [ "Unexpected pattern", show (pp p) ]
      [] -> panic "compileStrCase" [ "Missing default in StrPat" ]



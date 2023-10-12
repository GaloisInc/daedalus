{-# Language BlockArguments #-}
module Daedalus.VM.Compile.Expr where

import Data.Void(Void)
import Data.Maybe(fromMaybe)
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
  compileE e $ Just \v -> continue k =<< stmt ty (\x -> CallPrim x (Op1 op) [v])

compileOp2 :: Src.Op2 -> VMT -> Src.Expr -> Src.Expr -> CE
compileOp2 op ty e1 e2 k =
  compileEs [e1,e2] \ vs -> continue k =<<
                                   stmt ty (\x -> CallPrim x (Op2 op) vs)


compileOp3 :: Src.Op3 -> VMT -> Src.Expr -> Src.Expr -> Src.Expr -> CE
compileOp3 op ty e1 e2 e3 k =
  compileEs [e1,e2,e3] \vs -> continue k =<<
                                      stmt ty (\x -> CallPrim x (Op3 op) vs)


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
                       term (CallPure f (jumpNoFree l) vs)

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



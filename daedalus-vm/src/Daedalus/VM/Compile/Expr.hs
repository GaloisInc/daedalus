{-# Language BlockArguments #-}
module Daedalus.VM.Compile.Expr where

import Data.Void(Void)
import Data.Maybe(fromMaybe)
import qualified Data.Map as Map
import Control.Monad(forM)

import qualified Daedalus.Core as Src
import qualified Daedalus.Core.Type as Src

import Daedalus.VM
import Daedalus.VM.BlockBuilder
import Daedalus.VM.Compile.Monad


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
      compileEs (map snd fs) \vs ->
         do s <- stmt ty (\x -> CallPrim x (StructCon t) vs)
            continue k s

    Src.ECase (Src.Case e as) ->
      do next' <- case k of
                    Nothing -> pure Nothing
                    Just kont ->
                      do res  <- newLocal (TSem (Src.typeOf expr))
                         nextL <- label0 NormalBlock (kont =<< getLocal res)
                         pure $ Just \v -> do setLocal res v
                                              jump nextL

         codes <- forM as \(p,rhs) ->
                    do l <- label0 NormalBlock =<< compileE rhs next'
                       pure (p, l)
         compileE e $ Just \v -> jumpCase v (Map.fromList codes)

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
                       term (CallPure f l vs)

         compileEs es doCall








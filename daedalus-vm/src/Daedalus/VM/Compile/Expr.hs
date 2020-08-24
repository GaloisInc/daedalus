{-# Language BlockArguments #-}
module Daedalus.VM.Compile.Expr where

import Data.Void(Void)
import Data.Maybe(fromMaybe)

import qualified Daedalus.Core as Src
import qualified Daedalus.Core.Type as Src

import Daedalus.VM
import Daedalus.VM.BlockBuilder
import Daedalus.VM.Compile.Monad

import Daedalus.PP
import Debug.Trace

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
         do s <- stmt ty (CallPrim (StructCon t) vs)
            continue k s

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
    Src.IntL i t          -> k (ENum i t)
    Src.BoolL b           -> k (EBool b)
    Src.ByteArrayL bs     -> k (EByteArray bs)
    Src.NewBuilder t      -> do v <- stmt ty (CallPrim (NewBuilder t) [])
                                k v
    Src.MapEmpty t1 t2    -> k (EMapEmpty t1 t2)
    Src.ENothing t        -> k (ENothing t)
  where
  k = continue k'

compileOp1 :: Src.Op1 -> VMT -> Src.Expr -> CE
compileOp1 op ty e k =
  compileE e $ Just \v -> continue k =<< stmt ty (CallPrim (Op1 op) [v])

compileOp2 :: Src.Op2 -> VMT -> Src.Expr -> Src.Expr -> CE
compileOp2 op ty e1 e2 k =
  case op of

    Src.And ->
      compileOp3 Src.PureIf ty e1 e2 (Src.boolL False) k

    Src.Or ->
      compileOp3 Src.PureIf ty e1 (Src.boolL True) e2 k

    _ -> compileEs [e1,e2] \ vs -> continue k =<< stmt ty (CallPrim (Op2 op) vs)


compileOp3 :: Src.Op3 -> VMT -> Src.Expr -> Src.Expr -> Src.Expr -> CE
compileOp3 op ty e1 e2 e3 k =
  case op of

    Src.PureIf ->
      do whatNext <- case k of
                       Nothing -> pure Nothing
                       Just kont ->
                         do l <- newLocal (TSem (Src.typeOf e2))
                            nextL <- label0 (kont =<< getLocal l)
                            pure $ Just \v ->
                                      do setLocal l v
                                         jump nextL

         let branch e = do code <- compileE e whatNext
                           label0 code

         thenL <- branch e2
         elseL <- branch e3

         compileE e1 $ Just \v -> jumpIf v thenL elseL

    _ -> compileEs [e1,e2,e3] \vs -> continue k =<< stmt ty (CallPrim (Op3 op) vs)


compileOpN :: Src.OpN -> VMT -> [Src.Expr] -> CE
compileOpN op ty es k =
  compileEs es \vs ->
    case op of
      Src.ArrayL _ -> continue k =<< stmt ty (CallPrim (OpN op) vs)
      Src.CallF f ->
        case k of
          Just k' -> k' =<< stmt ty (CallPrim (OpN op) vs)
          Nothing -> term (TailCall f NoCapture vs)


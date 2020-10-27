{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Daedalus.VM.Compile.Grammar where

import Data.Void(Void)
import Data.Maybe(fromMaybe)

import qualified Daedalus.Core as Src
import qualified Daedalus.Core.Type as Src
import qualified Daedalus.Core.Effect as Src

import Daedalus.VM
import Daedalus.VM.BlockBuilder
import Daedalus.VM.Compile.Monad
import Daedalus.VM.Compile.Expr



compile :: Src.Grammar -> WhatNext -> C (BlockBuilder Void)
compile expr next0 =
  let next = if Src.canFail expr then next0 else next0 { onNo = Nothing }
  in

  case expr of

    Src.Pure e ->
      compileE e $ Just $ nextYes next

    -- XXX: Don't ignore the errors
    Src.Fail _ _ _ ->
      pure
        do stmt_$ NoteFail
           nextNo next

    Src.GetStream ->
      pure
        do v <- stmt (TSem Src.TStream) GetInput
           nextYes next v

    Src.SetStream e ->
      compileE e $ Just \v ->
        do stmt_ $ SetInput v
           nextYes next EUnit

    Src.Annot a e ->
      case a of
        Src.NoFail -> compile e next { onNo = Nothing }
        -- XXX
        Src.SrcAnnot ann -> compile e next    -- XXX:

    Src.If e p q ->
      do next' <- sharedYes =<< sharedNo next

         pCode <- label0 NormalBlock =<< compile p next'
         qCode <- label0 NormalBlock =<< compile q next'

         compileE e $ Just \v ->
           jumpIf v pCode qCode


    Src.Do_ p q ->

      do next' <- sharedNo next
         qCode <- compile q next'
         setCurTy (TSem (Src.typeOf p)) $
           compile p next' { onYes = Just \_ -> qCode }

    Src.Let x e p ->

      do l     <- newLocal (TSem (Src.typeOf x))
         pCode <- gdef x l (compile p next)
         compileE e $ Just \v ->
           do setLocal l v
              pCode


    Src.Do x p q ->

      do next' <- sharedNo next

         let lty = TSem (Src.typeOf x)
         l <- newLocal lty
         qCode <- gdef x l (compile q next')

         setCurTy lty $
           compile p next' { onYes = Just \v -> do setLocal l v
                                                   qCode
                           }


    Src.OrBiased p q ->

      do next' <- sharedYes next
         qCode <- compile q next'

         l     <- newLocal (TSem Src.TStream)
         pCode <- compile p next' { onNo = Just do i <- getLocal l
                                                   stmt_ $ SetInput i
                                                   qCode
                                  }

         pure
           do i <- stmt (TSem Src.TStream) $ GetInput
              setLocal l i
              pCode


    Src.OrUnbiased p q ->
      do next' <- sharedYes next


         leftFailed <- newLocal (TSem Src.TBool)

         qCode <-
            do finished <-
                 label0 NormalBlock $ term $ Yield

               bothFailed <-
                 label0 NormalBlock $ nextNo next'

               compile q next'
                 { onNo  = Just do v <- getLocal leftFailed
                                   jumpIf v bothFailed finished
                 }

         -- used to process the RHS
         doRHS <- label1' ThreadBlock (Just (TSem Src.TBool)) \didFail ->
                  do setLocal leftFailed didFail
                     qCode

         rightId <- newLocal TThreadId
         pCode <- compile p
                  next' { onNo = Just do tid <- getLocal rightId
                                         stmt_ $ Notify tid
                                         term  $ Yield

                        }

         pure
           do clo <- doRHS
              tid <- stmt TThreadId $ \x -> Spawn x clo
              setLocal rightId tid
              pCode


    Src.Call f es ->
      do doCall <-
           case (onNo next, onYes next) of
             (Nothing,Nothing) -> pure \vs -> term $ TailCall f Capture vs

             _ ->

               do noL <- label0 (ReturnBlock 0) $ nextNo next

                  yesL <- label1' (ReturnBlock 1) Nothing \v -> nextYes next v

                  pure \vs -> do cloNo  <- noL
                                 cloYes <- yesL
                                 term $ Call f Capture cloNo cloYes vs

         compileEs es \vs -> doCall vs


--------------------------------------------------------------------------------

data WhatNext = Next { onNo  :: Maybe (BlockBuilder Void)
                     , onYes :: Maybe (E -> BlockBuilder Void)
                     }

nextNo :: WhatNext -> BlockBuilder Void
nextNo = fromMaybe (term ReturnNo) . onNo

nextYes :: WhatNext -> E -> BlockBuilder Void
nextYes = fromMaybe (term . ReturnYes) . onYes

ret :: WhatNext
ret = Next { onNo = Nothing, onYes = Nothing }

sharedNo :: WhatNext -> C WhatNext
sharedNo next =
  case onNo next of
    Nothing -> pure next
    Just c  -> do l <- label0 NormalBlock c
                  pure next { onNo = Just (jump l) }

sharedYes :: WhatNext -> C WhatNext
sharedYes next =
  case onYes next of
    Nothing -> pure next
    Just c  -> do l <- label1 NormalBlock c
                  pure next { onYes = Just \v -> jump (l v) }



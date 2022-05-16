{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Use const" #-}
module Daedalus.VM.Compile.Grammar where


import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void(Void)
import Control.Monad(forM,when)
import qualified Data.Map as Map

import Daedalus.Panic(panic)

import qualified Daedalus.Core as Src
import qualified Daedalus.Core.Type as Src
import qualified Daedalus.Core.Effect as Src

import Daedalus.VM
import Daedalus.VM.Compile.BlockBuilder
import Daedalus.VM.Compile.Monad
import Daedalus.VM.Compile.Expr
import Daedalus.PP ( PP(pp) )


ppText :: PP a => a -> Text
ppText = Text.pack . show . pp

compile :: Src.Grammar -> WhatNext -> C (BlockBuilder Void)
compile expr next0 =
  let next = if Src.canFail expr then next0 else next0 { onNo = Nothing }
  in

  case expr of

    Src.Pure e ->
      do yesK <- nextYes next
         compileE e (Just yesK)

    -- XXX: Don't ignore the errors
    Src.Fail _ _ _ ->
      do noK <- nextNo next
         pure
           do i <- getInput
              stmt_ (NoteFail i)
              noK

    Src.GetStream ->
      do yesK <- nextYes next
         pure (yesK =<< getInput)

    Src.SetStream e ->
      do yesK <- nextYes next
         compileE e $ Just \v ->
           do setInput v
              yesK EUnit

    Src.Annot a e ->
      case a of
        Src.NoFail -> compile e next { onNo = Nothing }
        Src.SrcAnnot {} -> compile e next
        Src.SrcRange {} -> compile e next

{-
        -- XXX
        Src.SrcAnnot ann ->
          do d <- getDebugging
             if d then do
               next' <- addPops next
               stuff <- compile e next'
               pure (stmt_ (PushDebug ann) >> stuff)
             else
               compile e next
        Src.SrcRange ann ->
          do d <- getDebugging
             if d then do
               next' <- addPops next
               stuff <- compile e next'
               pure (stmt_ (PushDebug (Text.pack (show (pp ann)))) >> stuff)
             else
               compile e next
-}

    Src.GCase (Src.Case x as) ->
      do next' <- sharedYes =<< sharedNo next
         codes <- forM as \(p,g) ->
                    do l <- label0 NormalBlock =<< compile g next'
                       pure (p, l)

         compileCaseBranches x codes

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
         pCode <- compile p next' { onNo = Just do setInput =<< getLocal l
                                                   qCode
                                  }

         pure
           do setLocal l =<< getInput
              pCode


    Src.OrUnbiased p q ->
      do next' <- sharedYes next

         leftFailed <- newLocal (TSem Src.TBool)
         l          <- newLocal (TSem Src.TStream)

         noK   <- nextNo next'
         qCode <-
            do finished <-
                 label0 NormalBlock $ term Yield

               bothFailed <-
                 label0 NormalBlock noK

               compile q next'
                 { onNo  = Just do v <- getLocal leftFailed
                                   jumpIf v bothFailed finished
                 }

         -- used to process the RHS
         doRHS <- spawnBlock \didFail ->
                  do setLocal leftFailed didFail
                     i <- getLocal l
                     setInput i
                     qCode

         rightId <- newLocal TThreadId
         pCode <- compile p
                  next' { onNo = Just do tid <- getLocal rightId
                                         stmt_ $ Notify tid
                                         term  $ Yield

                        }

         pure
           do i <- getInput
              setLocal l i
              clo <- doRHS
              tid <- stmt TThreadId $ \x -> Spawn x clo
              setLocal rightId tid
              pCode


    Src.Call f es ->
      do dbg <- getDebugMode
         let dbgEnter how =
                case dbg of
                  NoDebug -> pure ()
                  DebugStack fs ->
                    stmt_ (PushDebug how case Map.lookup f fs of
                                           Just rng -> ppText rng
                                           Nothing  -> ppText f)



         doCall <-
           case (onNo next, onYes next) of
             (Nothing,Nothing) -> pure \vs ->
                do dbgEnter DebugTailCall
                   i <- getInput
                   term $ TailCall f Unknown (i:vs)

             _ ->
               do noL  <- retNo  =<< nextNo next
                  yesL <- retYes =<< nextYes next
                  pure \vs -> do cloNo  <- noL
                                 cloYes <- yesL
                                 dbgEnter DebugCall
                                 i <- getInput
                                 term $ Call f Unknown cloNo cloYes (i:vs)

         compileEs es \vs -> doCall vs

    Src.Match {} -> panic "compile" [ "Match was not desugared" ]


--------------------------------------------------------------------------------

data WhatNext = Next { onNo  :: Maybe (BlockBuilder Void)
                     , onYes :: Maybe (E -> BlockBuilder Void)
                     }

nextNo :: WhatNext -> C (BlockBuilder Void)
nextNo next =
  case onNo next of
    Just noK -> pure noK
    Nothing  ->
      do dbg <- isDebugging
         pure do when dbg (stmt_ PopDebug)
                 term ReturnNo

nextYes :: WhatNext -> C (E -> BlockBuilder Void)
nextYes next =
  case onYes next of
    Just k  -> pure k
    Nothing ->
      do dbg <- isDebugging
         pure \res -> do when dbg (stmt_ PopDebug)
                         i <- getInput
                         term (ReturnYes res i)

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
    Just c  -> do l <- label1 c
                  pure next { onYes = Just \v -> jump (l v) }

{-
addPops :: WhatNext -> C WhatNext
addPops next =
  do y <- case onYes next of
            Just k  -> pure (\e -> stmt_ PopDebug >> k e)
            Nothing -> do l <- label1 $ \e -> do stmt_ PopDebug >> getInput >>= \i -> term (ReturnYes e i)
                          pure (jump . l)
     n <- case onNo next of
            Just k  -> pure (stmt_ PopDebug >> k)
            Nothing -> do l <- label0 NormalBlock
                                  do stmt_ PopDebugterm ReturnNo
                          pure (jump l)
     pure Next{ onYes = Just y, onNo = Just n}
-}

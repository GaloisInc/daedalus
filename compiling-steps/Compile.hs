{-# Language BlockArguments #-}
module Compile (compileFuns) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Void(Void)
import Data.Maybe(fromMaybe)

import qualified AST
import VM
import BlockBuilder
import CompileM
import InlineBlock



compileE :: AST.E -> C (BlockBuilder E)
compileE expr =
  case expr of
    AST.Var n  -> lookupN n
    AST.Char c -> pure $ pure $ EChar c
    AST.Int  i -> pure $ pure $ EInt i
    AST.App f es ->
      do fv  <- lookupPrim f
         ces <- mapM compileE es
         pure $ do vs <- sequence ces
                   stmt (getType fv) $ CallPrim fv vs




compile :: AST.P -> WhatNext -> C (BlockBuilder Void)
compile expr next0 =
  computeEff expr >>= \eff ->
  let next = case eff of
               AST.OnlyYes -> next0 { onNo = Nothing }
               AST.OnlyNo  -> next0 { onYes = Nothing }
               _           -> next0
  in

  case expr of

    AST.Pure e ->
      do ebuilder <- compileE e
         pure
           do ce <- ebuilder
              nextYes next ce

    AST.Fail _ ->
      pure
        do stmt_$ NoteFail
           nextNo next

    AST.Peek ->
      pure
        do v <- stmt (TSem AST.TInput) GetInput
           nextYes next v

    AST.SetInput e ->
      do ce <- compileE e
         pure
           do v <- ce
              stmt_ $ SetInput v
              nextYes next EUnit


    AST.If e p q ->
      do next' <- sharedYes =<< sharedNo next

         ce    <- compileE e
         pCode <- label0 =<< compile p next'
         qCode <- label0 =<< compile q next'

         pure
           do v <- ce
              jumpIf v pCode qCode



    AST.Do x p q ->

      do next' <- sharedNo next

         let lty = TSem (snd x)
         l <- newLocal lty
         qCode <- gdef x l (compile q next')

         setCurTy lty $
           compile p next' { onYes = Just \v -> do setLocal l v
                                                   qCode
                           }



    p AST.:<| q ->

      do next' <- sharedYes next
         qCode <- compile q next'

         l     <- newLocal (TSem AST.TInput)
         pCode <- compile p next' { onNo = Just do i <- getLocal l
                                                   stmt_ $ SetInput i
                                                   qCode
                                  }

         pure
           do i <- stmt (TSem AST.TInput) $ GetInput
              setLocal l i
              pCode



    p AST.:|| q ->
      do next' <- sharedYes next


         leftFailed <- newLocal (TSem AST.TBool)

         qCode <-
            do finished <-
                 label0
                   do stmt_$ Say "only RHS failed"
                      term $ Yield

               bothFailed <-
                 label0
                   do stmt_ $ Say "Both sides failed: using noC"
                      nextNo next'

               compile q next'
                 { onNo  = Just do v <- getLocal leftFailed
                                   jumpIf v bothFailed finished
                 }

         -- used to process the RHS
         doRHS <- label1' \didFail ->
                  do stmt_ $ Say "Start RHS"
                     setLocal leftFailed didFail
                     qCode

         rightId <- newLocal TThreadId
         pCode <- compile p
                  next' { onNo = Just do stmt_ $ Say "LHS failed"
                                         tid <- getLocal rightId
                                         stmt_ $ Notify tid
                                         term  $ Yield

                        }

         pure
           do stmt_ $ Say "Start OR"
              clo <- doRHS
              tid <- stmt TThreadId $ Spawn clo
              setLocal rightId tid
              pCode


    AST.Call f es ->
      do fv   <- lookupRule f
         args <- mapM compileE es
         let fun = fst f

         doCall <-
           case (onNo next, onYes next) of
             (Nothing,Nothing) -> pure \vs -> term $ TailCall fv vs

             _ ->

               do noL <- label0
                         do stmt_ $ Say ("Returning from " ++ fun ++ " with N")
                            nextNo next

                  yesL <- label1' \v ->
                          do stmt_ $ Say ("Returning from " ++ fun ++ " with Y")
                             nextYes next v

                  pure \vs -> do cloNo  <- noL
                                 cloYes <- yesL
                                 term $ Call fv cloNo cloYes vs


         pure
           do stmt_ $ Say ("Calling " ++ fun)
              vs <- sequence args
              doCall vs




compileFun :: AST.Fun -> FunLab -> Map AST.F FunLab -> VMFun
compileFun (AST.Fun f xs e) fl env0 =
  let args       = zipWith argN xs [ 0 .. ]
      argN x n   = BA n (TSem (snd x))
      getArgC (x,a) k =
        do v <- newLocal (getType a)
           code <- gdef x v k
           pure do setLocal v (EBlockArg a)
                   code

      (l,ls)     = runC f env0 (foldr getArgC (compile e ret) (zip xs args))
      addArgs b = b { blockArgs = args }

  in inlineBlocks
      VMFun { vmfName   = fl
            , vmfEntry  = l
            , vmfBlocks = Map.adjust addArgs l ls
            }


compileFuns :: ([AST.Fun], AST.P) -> Program
compileFuns (fs, pe) =
  Program { pEntry = l
          , pBoot = p
          , pFuns = Map.fromList [ (vmfName f, f) | f <- progs ]
          }
  where

  (l,p) = runC ("__", AST.typeOf pe) env
        $ compile pe
        $ Next { onNo = Just
                   do stmt_ $ Say "Branch failed, resuming"
                      term  $ Yield
               , onYes = Just \v ->
                   do stmt_ $ Say "Branch succeeded, resuming"
                      stmt_ $ Output v
                      term  $ Yield
               }

  progs = [ compileFun f (toFL f) env | f <- fs ]
  env   = Map.fromList [ (f, x) | x@(FL f _) <- map vmfName progs ]

  effs    = AST.funEffect fs
  toFL (AST.Fun f _ _) = FL f (effs Map.! f)


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
    Just c  -> do l <- label0 c
                  pure next { onNo = Just (jump l) }

sharedYes :: WhatNext -> C WhatNext
sharedYes next =
  case onYes next of
    Nothing -> pure next
    Just c  -> do l <- label1 c
                  pure next { onYes = Just \v -> jump (l v) }



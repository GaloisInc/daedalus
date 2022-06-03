{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Daedalus.VM.Compile.Decl where

import qualified Data.Map as Map
import Data.Void(Void)

import qualified Daedalus.Core as Src
import qualified Daedalus.Core.Type as Src

import Daedalus.VM
import Daedalus.VM.Compile.BlockBuilder
import Daedalus.VM.Compile.Monad
import Daedalus.VM.Compile.Expr
import Daedalus.VM.Compile.Grammar
import Daedalus.VM.InlineBlock
import Daedalus.VM.CaptureAnalysis
import Daedalus.VM.FindLoops
import Daedalus.VM.TailCallJump



moduleToProgram :: [Module] -> Program
moduleToProgram ms =
  tailProgram $
  captureAnalysis
  Program { pModules = map loopAnalysis ms }

compileModule :: Bool -> Src.Module -> Module
compileModule useDebug m =
  Module { mName = Src.mName m
         , mImports = Src.mImports m
         , mTypes = Src.mTypes m
         , mFuns  = map (compileFFun dm) (Src.mFFuns m)
                 ++ map (compileGFun dm) (Src.mGFuns m)
         }
  where
  dm        = if useDebug then DebugStack fi [] else NoDebug
  fi        = Map.fromList (concatMap getInfo (Src.mFFuns m) ++
                            concatMap getInfo (Src.mGFuns m))
  getInfo f = [ (Src.fName f, r) | Src.SrcRange r <- Src.fAnnot f ]


inpArg :: BA
inpArg = BA 0 (TSem Src.TStream) Borrowed

compileSomeFun ::
  Bool ->
  DebugMode ->
  (a -> C (BlockBuilder Void)) -> Src.Fun a -> VMFun
compileSomeFun isPure dm doBody fun =
  let xs         = Src.fParams fun
      name       = Src.fName fun

      inpArgs    = if isPure then [] else [ inpArg ]
      args       = zipWith argN xs [ length inpArgs .. ]
      argN x n   = BA n (TSem (Src.typeOf x)) Borrowed{-placeholder-}

      getArgC (x,a) k =
        do v <- newLocal (getType a)
           code <- gdef x v k
           pure do setLocal v (EBlockArg a)
                   code

      setInp i k = do code <- k
                      pure do setInput (EBlockArg i)
                              code

      def = case Src.fDef fun of
               Src.Def e    ->
                 VMDef
                   let body   = foldr setInp (doBody e) inpArgs
                       (l,ls) = runC name (Src.typeOf name) dm
                                         (foldr getArgC body (zip xs args))
                   in VMFBody { vmfEntry = l
                              , vmfBlocks = Map.adjust addArgs l ls
                              }
               Src.External -> VMExtern (inpArgs ++ args)

      addArgs b = b { blockArgs = inpArgs ++ args }

  in inlineBlocks
      VMFun { vmfName   = Src.fName fun
            , vmfCaptures = case Src.fDef fun of
                              Src.Def {}   -> Unknown
                              Src.External -> NoCapture
            , vmfPure   = isPure
            , vmfLoop   = False
            , vmfDef    = def
            , vmfIsEntry = Src.fIsEntry fun
            }

compileFFun :: DebugMode -> Src.Fun Src.Expr -> VMFun
compileFFun dm = compileSomeFun True dm \e -> compileE e Nothing

compileGFun :: DebugMode -> Src.Fun Src.Grammar -> VMFun
compileGFun dm = compileSomeFun False dm (\e -> compile e ret)

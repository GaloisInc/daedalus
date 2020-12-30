{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Daedalus.VM.Compile.Decl where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Void(Void)

import Daedalus.Panic(panic)
import Daedalus.PP(pp)

import qualified Daedalus.Core as Src
import qualified Daedalus.Core.Type as Src

import Daedalus.VM
import Daedalus.VM.BlockBuilder
import Daedalus.VM.Compile.Monad
import Daedalus.VM.Compile.Expr
import Daedalus.VM.Compile.Grammar
import Daedalus.VM.InlineBlock
import Daedalus.VM.CaptureAnalysis



compileProgram :: Src.FName -> [Src.Module] -> Program
compileProgram entry ms =
  moduleToProgram entry (captureAnalysis (map compileModule ms))

moduleToProgram :: Src.FName -> [Module] -> Program
moduleToProgram entry ms =
  Program
    { pModules = ms
    , pType    = Src.fnameType entry
    , pEntry   = l
    , pBoot    = b
    }
  where
  (l,b) = compileEntry (Src.Call entry [])


compileEntry :: Src.Grammar -> (Label, Map Label Block)
compileEntry pe =
  runC "__" (Src.typeOf pe) $
  compile pe $
  Next { onNo  = Just
                 do stmt_ $ Say "Branch failed, resuming"
                    term  $ Yield
       , onYes = Just \v ->
                 do stmt_ $ Say "Branch succeeded, resuming"
                    stmt_ $ Output v
                    term  $ Yield
       }


compileModule :: Src.Module -> Module
compileModule m =
  Module { mName = Src.mName m
         , mImports = Src.mImports m
         , mTypes = Src.mTypes m
         , mFuns = map compileFFun (Src.mFFuns m) ++
                   map compileGFun (Src.mGFuns m)
         }


compileSomeFun ::
  Bool -> (Maybe a -> C (BlockBuilder Void)) -> Src.Fun a -> VMFun
compileSomeFun isPure doBody fun =
  let xs         = Src.fParams fun
      name       = Src.fName fun

      args       = zipWith argN xs [ 0 .. ]
      argN x n   = BA n (TSem (Src.typeOf x)) Borrowed{-placeholder-}
      getArgC (x,a) k =
        do v <- newLocal (getType a)
           code <- gdef x v k
           pure do setLocal v (EBlockArg a)
                   code

      body = case Src.fDef fun of
               Src.Def e    -> doBody (Just e)
               Src.External -> doBody Nothing

      lab = Text.pack $ show $ pp name

      (l,ls)     = runC lab (Src.typeOf name)
                        (foldr getArgC body (zip xs args))
      addArgs b = b { blockArgs = args }

  in inlineBlocks
      VMFun { vmfName   = Src.fName fun
            , vmfCaptures = Capture -- Conservative
            , vmfPure   = isPure
            , vmfEntry  = l
            , vmfBlocks = Map.adjust addArgs l ls
            }

compileFFun :: Src.Fun Src.Expr -> VMFun
compileFFun = compileSomeFun True \mb ->
  case mb of
    Just e -> compileE e Nothing
    Nothing -> panic "compileFFun" ["XXX: External primitives"]

compileGFun :: Src.Fun Src.Grammar -> VMFun
compileGFun = compileSomeFun False \mb ->
  case mb of
    Just e -> compile e ret
    Nothing -> panic "compileGFun" ["XXX: External primitives"]


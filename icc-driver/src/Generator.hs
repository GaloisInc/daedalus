{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Main where

import qualified Data.Map as Map
import Control.Monad(when)

import Daedalus.Driver
import Daedalus.Type.AST
import Daedalus.Compile.LangHS
import Daedalus.Compile.Config


main :: IO ()
main =
  daedalus
  do ddlSetOpt optSearchPath ["specs"]
     let demo = "ICC"
     ddlLoadModule demo
     when specialize (passSpecialize roots)
     todo <- ddlBasis demo
     mapM_ (\m -> do ph <- ddlGetPhase m
                     ddlPrint (phasePass ph)
                     saveHS (Just "src") cfg m) todo

  where
  specialize = False

  roots :: [(ModuleName,Ident)]
  roots = [ ("ICC", "Main")
          , ("ICC", "ParseTag")
          ]

cfg :: CompilerCfg
cfg = CompilerCfg
  { cPrims      = Map.empty
  , cParserType = "RTS.Parser"
  , cImports    = [ Import "RTS.Parser" (QualifyAs "RTS") ]
  , cQualNames  = UseQualNames
  }


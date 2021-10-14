{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Main where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo

import qualified Data.Map as Map
import Control.Exception(catch)

import Daedalus.Driver
import Daedalus.Type.AST
import Daedalus.Compile.LangHS

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ ->
      do putStrLn "Compiling DDL"
         compileDDL
         pure emptyHookedBuildInfo
  }




compileDDL :: IO ()
compileDDL =
  daedalus
  do ddlSetOpt optSearchPath ["specs"]
     let demo = "ICC"
     ddlLoadModule demo
     todo <- ddlBasis demo
     mapM_ (\m -> saveHS (Just "src") cfg m) todo
  `catch` \d -> putStrLn =<< prettyDaedalusError d

  where
  -- This would be used if we specialized
  _roots :: [(ModuleName,Ident)]
  _roots = [ ("ICC", "Main")
          , ("ICC", "ParseTag")
          ]

cfg :: CompilerCfg
cfg = CompilerCfg
  { cPrims      = Map.empty
  , cParserType = "RTS.Parser"
  , cImports    = [ Import "RTS.Parser" (QualifyAs "RTS") ]
  , cQualNames  = UseQualNames
  }


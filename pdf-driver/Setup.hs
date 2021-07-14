{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo

import qualified Data.Map as Map
import qualified Data.Text

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
  do ddlSetOpt optSearchPath [ "spec"             -- new ddl modules here
                             , "../pdf-cos/spec"  -- other ddl modules in pdf-cos pkg
                             ]
     let mods = [ "PdfDemo", "PdfValidate", "PdfDOM"]
     mapM_ ddlLoadModule mods
     let todo = mods
                -- Varying from ../pdf-cos/Setup.hs here:
                --  we only generate hs for the listed files, not any dependencies
     ddlIO $
       mapM putStrLn $ [ "generating .hs for these .ddl modules:"]
                       ++ map (("  " ++) . Data.Text.unpack) todo
                       ++ [""]
     let cfgFor m = case m of
                      "PdfValidate" -> cfgPdfValidate
                      _             -> cfg
     mapM_ (\m -> saveHS (Just "src/spec") (cfgFor m) m) todo

  where

  -- XXX: untested and currently unused
  _roots :: [(ModuleName,Ident)]
  _roots = [ ("PdfDemo", "CatalogIsOK")
           , ("PdfDemo", "TopDeclCheck")
           , ("PdfDemo", "ResolveObjectStreamEntryCheck")
           , ("PdfDOM",  "DOMTrailer")
           ]




cfg :: CompilerCfg
cfg = CompilerCfg
  { cPrims      = Map.empty
  , cParserType = "D.Parser"
  , cImports    = [ Import "PdfMonad" (QualifyAs "D") ]
  , cQualNames = UseQualNames
  }


cfgPdfValidate :: CompilerCfg
cfgPdfValidate = CompilerCfg
  { cPrims = Map.fromList
      [ primName "PdfValidate" "IsValidated" AGrammar |->
        aps "D.primIsValidated" [ "obj", "gen", "ty" ]

      , primName "PdfValidate" "StartValidating" AGrammar |->
        aps "D.primStartValidating" [ "obj", "gen", "ty" ]
      ]
  , cParserType = "D.Parser"
  , cImports    = [ Import "Primitives.Validate" (QualifyAs "D"),
                    Import "PdfMonad" (QualifyAs "D")
                  ]
  , cQualNames = UseQualNames
  }
  where
  x |-> y = (x,y)



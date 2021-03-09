{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo

import qualified Data.Map as Map
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



{- We split up basic PDF validation from DOM related stuff into separate
packages.  This allows us to avoid recompiling things that have not changed. -}
compileDDL :: IO ()
compileDDL =
  daedalus
  do ddlSetOpt optSearchPath ["spec"]
     let mods = [ "PdfDemo", "PdfValidate", "PdfDOM", "PdfContentStream" ]
     mapM_ ddlLoadModule mods
     todo <- filter (not . (`elem` external)) <$> ddlBasisMany mods
     let cfgFor m = case m of
                      "PdfValidate" -> cfgPdfValidate
                      _             -> cfg
     mapM_ (\m -> saveHS (Just "src/spec") (cfgFor m) m) todo

  where

  -- in pdf-cos
  external = [ "PdfValue", "PdfXRef", "PdfDecl", "Stdlib", "Jpeg" ]

  -- XXX: untested and currently unused
  _roots :: [(ModuleName,Ident)]
  _roots = [ ("PdfDemo", "CatalogIsOK")
           , ("PdfDemo", "TopDeclCheck")
           , ("PdfDemo", "ResolveObjectStreamEntryCheck")
           , ("PdfXRef", "CrossRef")
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



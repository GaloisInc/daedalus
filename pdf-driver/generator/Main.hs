{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Main where

import qualified Data.Map as Map
import Control.Monad(when)

import Daedalus.Driver
import Daedalus.Type.AST
import Daedalus.Compile.LangHS


{- We split up basic PDF validation from DOM related stuff into separate
packages.  This allows us to avoid recompiling things that have not changed. -}
main :: IO ()
main =
  daedalus
  do ddlSetOpt optSearchPath ["spec"]
     let mods = [ "PdfDemo", "PdfDOM" ]
     mapM_ ddlLoadModule mods
     when specialize (passSpecialize roots)
     todo <- filter (not . (`elem` external)) <$> ddlBasisMany mods
     let cfgFor m = case m of
                      "PdfValidate" -> cfgPdfValidate
                      _             -> cfg
     mapM_ (\m -> saveHS (Just "src/spec") (cfgFor m) m) todo

  where
  specialize = False

  -- in pdf-cos
  external = [ "PdfValue", "PdfXRef", "PdfDecl" ]

  -- XXX: untested and currently unused
  roots :: [(ModuleName,Ident)]
  roots = [ ("PdfDemo", "CatalogIsOK")
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



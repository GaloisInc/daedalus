{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Main where

import qualified Data.Map as Map
import Control.Monad(when)

import Daedalus.Driver
import Daedalus.Type.AST
import Daedalus.Compile.LangHS


main :: IO ()
main =
  daedalus
  do ddlSetOpt optSearchPath ["spec"]
     let mods = [ "PdfDemo", "PdfValidate", "PdfDOM" ]
     mapM_ ddlLoadModule mods
     when specialize (passSpecialize roots)
     todo <- ddlBasisMany mods
     let cfgFor m = case m of
                      "PdfDecl"     -> cfgPdfDecl
                      "PdfValidate" -> cfgPdfValidate
                      _             -> cfg
     mapM_ (\m -> do ph <- ddlGetPhase m
                     ddlPrint (phasePass ph)
                     saveHS (Just "src/spec") (cfgFor m) m) todo

  where
  specialize = False

  roots :: [(ModuleName,Ident)]
  roots = [ ("PdfDemo", "CatalogIsOK")
          , ("PdfDemo", "TopDeclCheck")
          , ("PdfDemo", "ResolveObjectStreamEntryCheck")
          , ("PdfXRef", "CrossRef")
          , ("PdfDom", "CheckCatalog")
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


cfgPdfDecl :: CompilerCfg
cfgPdfDecl = CompilerCfg
  { cPrims = Map.fromList
      [ primName "PdfDecl" "ResolveRef" AGrammar |->
        aps "D.resolveImpl" [ "PdfDecl.pTopDecl"
                            , "PdfDecl.pResolveObjectStreamEntry"
                            , fld "obj" "r"
                            , fld "gen" "r"
                            ]

      , primName "PdfDecl" "FlateDecode" AGrammar |->
        aps "D.flateDecode"
                  [ "predictor", "colors", "bpc", "columns", "body" ]

      , primName "PdfDecl" "LZWDecode" AGrammar |->
        aps "D.lzwDecode"
                  [ "predictor", "colors", "bpc", "columns", "earlychange", "body" ]

      , primName "PdfDecl" "ASCIIHexDecode" AGrammar |->
        aps "D.asciiHexDecode" [ "body" ]

      , primName "PdfDecl" "ASCII85Decode" AGrammar |->
        aps "D.ascii85Decode" [ "body" ]
      ]
  , cParserType = "D.Parser"
  , cImports    = [ Import "Primitives.Resolve" (QualifyAs "D"),
                    Import "Primitives.Deflate" (QualifyAs "D"),
                    Import "Primitives.LZW" (QualifyAs "D"),
                    Import "Primitives.ASCIIHex" (QualifyAs "D"),
                    Import "Primitives.ASCII85" (QualifyAs "D"),
                    Import "PdfMonad" (QualifyAs "D")
                  ]
  , cQualNames = UseQualNames
  }

  where
  fld x r = "HS.getField" `Ap` TyParam (Raw (x :: String)) `Ap` r
  x |-> y = (x,y)


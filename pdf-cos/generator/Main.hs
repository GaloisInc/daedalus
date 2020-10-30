{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Main where

import qualified Data.Map as Map

import Daedalus.Driver
import Daedalus.Type.AST
import Daedalus.Compile.LangHS


{- We split up basic PDF validation from DOM related stuff into separate
packages.  This allows us to avoid recompiling things that have not changed. -}
main :: IO ()
main =
  daedalus
  do ddlSetOpt optSearchPath ["spec"]
     let mods = [ "PdfXRef" ]
     mapM_ ddlLoadModule mods
     todo <- ddlBasisMany mods
     let cfgFor m = case m of
                      "PdfDecl"     -> cfgPdfDecl
                      _             -> cfg
     mapM_ (\m -> saveHS (Just "src") (cfgFor m) m) todo

  where
  -- XXX: This shoudl list all parsers we need, and it'd be used if
  -- we specialized things
  _roots :: [(ModuleName,Ident)]
  _roots = [ ("PdfXRef", "CrossRef")
           ]



cfg :: CompilerCfg
cfg = CompilerCfg
  { cPrims      = Map.empty
  , cParserType = "D.Parser"
  , cImports    = [ Import "PdfMonad" (QualifyAs "D") ]
  , cQualNames = UseQualNames
  }


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

      , primName "PdfDecl" "Decrypt" AGrammar |-> 
        aps "D.decrypt" [ "body" ]
      ]
  , cParserType = "D.Parser"
  , cImports    = [ Import "Primitives.Resolve" (QualifyAs "D"),
                    Import "Primitives.Deflate" (QualifyAs "D"),
                    Import "Primitives.LZW" (QualifyAs "D"),
                    Import "Primitives.ASCIIHex" (QualifyAs "D"),
                    Import "Primitives.ASCII85" (QualifyAs "D"),
                    Import "Primitives.Decrypt" (QualifyAs "D"),
                    Import "PdfMonad" (QualifyAs "D")
                  ]
  , cQualNames = UseQualNames
  }

  where
  fld x r = "HS.getField" `Ap` TyParam (Raw (x :: String)) `Ap` r
  x |-> y = (x,y)


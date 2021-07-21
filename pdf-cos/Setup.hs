{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}

import System.Directory
import System.IO
import System.Posix.Temp(mkstemps)

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


{- We split up basic PDF validation from DOM related stuff into separate
packages.  This allows us to avoid recompiling things that have not changed. -}
compileDDL :: IO ()
compileDDL =
  daedalus
  do ddlSetOpt optSearchPath ["spec"]
     let mods = [ "PdfXRef"
                , "PdfCrypto"
                , "PdfValue"

                -- library-like, just in case no-one imports:
                , "Array"
                , "Map"
                , "Pair"
                , "Stdlib"
                , "Sum"

                , "CMap"
                , "FontDict"
                , "GenPdfValue"
                , "JpegBasics"
                , "Page"
                , "PageTreeNode"
                , "ResourceDict"
                , "Type1Font"

                -- added for text extraction
                , "ContentStreamLight"
                , "PdfExtractText"
                , "PdfText"
                , "Unicode"
                ]
     mapM_ ddlLoadModule mods
     todo <- ddlBasisMany mods
     ddlIO $
       mapM putStrLn $ [ "daedalus generating .hs for these .ddl modules:"
                       , "  (effciently: no updates when file contents would be equal)"
                       ]
                       ++ map (("  " ++) . Data.Text.unpack) todo
                       ++ [""]
     let cfgFor m = case m of
                      "PdfDecl"     -> cfgPdfDecl
                      _             -> cfg
         saveHS' = saveHSCustomWriteFile smartWriteFile
                   -- more efficient replacement for 'saveHS'
     mapM_ (\m -> saveHS' (Just "src") (cfgFor m) m) todo

  where
  -- XXX: This should list all parsers we need, and it'd be used if
  -- we specialized things
  _roots :: [(ModuleName,Ident)]
  _roots = [ ("PdfXRef", "CrossRef")
           ]


-- equivalent to writeFile except that file access dates untouched when file-data unchanged.
smartWriteFile :: FilePath -> String -> IO ()
smartWriteFile outFile s =
  do
  exists <- doesFileExist outFile
  if not exists then
    writeFile outFile s
  else
    do
    hOutfile <- openFile outFile ReadMode
    s' <- hGetContents hOutfile
    if s' == s then hClose hOutfile
               else do
                    (tmpFile,hTmpFile) <- mkstemps "swf" "temp"
                    hPutStr hTmpFile s
                    hClose hTmpFile
                    hClose hOutfile
                    renameFile tmpFile outFile

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

      , primName "PdfDecl" "InputAtRef" AGrammar |-> -- get a stream:
        aps "D.resolveImpl" [ "PdfDecl.pWrapGetStream"
                            , "PdfDecl.pParamWrapGetStream"
                            , fld "obj" "r"
                            , fld "gen" "r"
                            ]

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


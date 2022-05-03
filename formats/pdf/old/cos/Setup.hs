{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}

import System.Directory
import System.IO
import System.Posix.Temp(mkstemps)
import Control.Exception

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
     let mods = [ "Pair"
                , "Debug"     -- not used for "production" :-)
                , "Sum"
                , "Stdlib"
                , "Map"
                , "PdfValue"  -- nice to remove this out of lowest level prims
                , "GenPdfValue"

                , "Glyph"
                , "Unicode"
                , "GlyphList"
                , "GlyphListNewFonts"
                , "GlyphEnc"

                , "JpegBasics"
                , "PdfDecl"
                , "FontDesc"
                , "CIDFont"
                , "Array"
                , "CMap"
                , "ColourSpaceOps"
                , "MacEncoding"
                , "MacExpert"
                , "PdfCrypto"
                , "StdEncoding"
                , "WinEncoding"
                , "Maybe"
                , "Encoding"
                , "FontCommon"
                , "SymbolEncoding"
                , "ZapfDingbatsEncoding"
                , "Type1Font"
                , "TrueTypeFont"
                , "Type0Font"
                , "Rectangle"
                , "Type3Font"
                , "FontDict"
                , "GraphicsStateOps"
                , "MarkedContentOps"
                , "ResourceDict"
                , "TextEffect"
                , "TextPosOp"
                , "TextShowOp"
                , "TextStateOp"
                , "TextObj"
                , "ContentStreamLight"
                , "Page"
                , "PageTreeNode"
                , "PdfXRef"
                , "PdfExtractText"
                , "TE"
                ]

                -- This order is intentional: we order in reverse dependency order, thus no module
                -- depends on anything after it in the list.  Three impacts are:
                --  1. 'ddlLoadModel' should never load extra dependencies implicitly
                --  2. the load order is known & constant (unless the constraint is broken)
                --  3. hopefully, we increase the number of short-circuited file writes
                --
                -- We determined this "topological sort" by referring to this generated file
                --   spec/doc-pdfextracttext-import.dag
                -- and then moving "GlyphList" as high up as possible.

                -- NOTE re this 'hack'
                --  - daedalus's temp var generation appears to be done
                --    at 'ddlLoadModule' time and the "nextTmp" "persists" through the calls to
                --    to ddlLoadModule.

                -- FIXME: This appears to no longer be necessary due to improvements in daedalus:
                --  - confirm and remove.

     mapM_ ddlLoadModule mods
     todo <- ddlBasisMany mods
     ddlIO $
       mapM putStrLn $ [ "daedalus generating .hs for these .ddl modules:"
                       ]
                       ++ map (("  " ++) . Data.Text.unpack) todo
     let cfgFor m = case m of
                      "PdfDecl"     -> cfgPdfDecl
                      "TE"          -> cfgTE
                      _             -> cfgDefault
         -- more efficient replacement for 'saveHS'
         saveHS' dir cfg mod =
            saveHSCustomWriteFile (smartWriteFile log) dir cfg mod

         log fn = putStrLn $ "  " ++ fn

     ddlIO $ putStrLn "... but only creating/updating these Haskell files:"
     mapM_ (\m -> saveHS' (Just "src/spec")   (cfgFor m) m)  todo
     ddlIO $ putStrLn "... daedalus done"
  `catch` \d -> putStrLn =<< prettyDaedalusError d

  where
  -- XXX: This should list all parsers we need, and it'd be used if
  -- we specialized things
  _roots :: [(ModuleName,Ident)]
  _roots = [ ("PdfXRef", "CrossRef")
           ]

-- FIXME:
--   This 'saveHSCustomWriteFile smartWriteFile' would work even better if we
--   were able to reset daedalus's "tmp var supply" counter between calls to
--   saveHSCustomWriteFile; Because a change in just one file can cause the names
--   of all temporary variables to change in the files that are compiled after
--   that one.

-- equivalent to writeFile except that file access dates are untouched when
-- the file-data is unchanged.
smartWriteFile :: (FilePath -> IO ()) -> FilePath -> String -> IO ()
smartWriteFile logUpdate outFile s =
  do
  exists <- doesFileExist outFile
  if not exists then
    do
    writeFile outFile s
    logUpdate outFile
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
                    logUpdate outFile

-- Common
cfgDefault :: CompilerCfg
cfgDefault = CompilerCfg
  { cPrims      = Map.empty
  , cParserType = Just "D.Parser"
  , cImports    = [ Import "PdfMonad" (QualifyAs "D") ]
  , cQualNames  = UseQualNames
  }

cfgPdfDecl :: CompilerCfg
cfgPdfDecl = cfgDefault
  { cPrims = Map.fromList
      [ primName "PdfDecl" "ResolveRef" |->
        \ ~[r] ->
        aps "D.resolveImpl" [ "PdfDecl.pTopDecl"
                            , "PdfDecl.pResolveObjectStreamEntry"
                            , fld "obj" r
                            , fld "gen" r
                            ]

      , primName "PdfDecl" "FlateDecode"    |-> aps "D.flateDecode"
      , primName "PdfDecl" "LZWDecode"      |-> aps "D.lzwDecode"
      , primName "PdfDecl" "ASCIIHexDecode" |-> aps "D.asciiHexDecode"
      , primName "PdfDecl" "ASCII85Decode"  |-> aps "D.ascii85Decode"
      , primName "PdfDecl" "Decrypt"        |-> aps "D.decrypt"

      , primName "PdfDecl" "InputAtRef" |-> -- get a stream:
        \ ~[r] ->
        aps "D.resolveImpl" [ "PdfDecl.pWrapGetStream"
                            , "PdfDecl.pResolveObjectStreamPoint"
                            , fld "obj" r
                            , fld "gen" r
                            ]

      ]
  , cImports    = [ Import "Primitives.Resolve"   (QualifyAs "D"),
                    Import "Primitives.Deflate"   (QualifyAs "D"),
                    Import "Primitives.LZW"       (QualifyAs "D"),
                    Import "Primitives.ASCIIHex"  (QualifyAs "D"),
                    Import "Primitives.ASCII85"   (QualifyAs "D"),
                    Import "Primitives.Decrypt"   (QualifyAs "D")
                  ] ++ cImports cfgDefault
  }

  where
  fld x r = "HS.getField" `Ap` TyParam (Raw (x :: String)) `Ap` r
  x |-> y = (x,y)

cfgTE :: CompilerCfg
cfgTE = cfgDefault
  { cPrims = Map.singleton (primName "TE" "EmitChar") (aps "D.emitChar")
  }

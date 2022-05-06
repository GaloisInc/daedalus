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
import Daedalus.DriverHS
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

     cfg <- ddlIO (moduleConfigsFromFile mempty "CodeGenConfig.conf")

         -- more efficient replacement for 'saveHS'
     let saveHS' dir cfg mod =
            saveHSCustomWriteFile (smartWriteFile log) dir cfg mod

         log fn = putStrLn $ "  " ++ fn

     ddlIO $ putStrLn "... but only creating/updating these Haskell files:"
     mapM_ (\m -> saveHS' (Just "src/spec") cfg m)  todo
     ddlIO $ putStrLn "... daedalus done"
  `catches` [ Handler \d -> putStrLn =<< prettyDaedalusError d
            , Handler \d -> print (ppParseError d)
            ]

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


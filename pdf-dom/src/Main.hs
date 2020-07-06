{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language BlockArguments #-}
import Prelude hiding (div)

import System.Environment(getArgs)
import System.FilePath(replaceExtension,(</>),takeDirectory,takeBaseName)
import System.Directory(createDirectoryIfMissing)

import HTML(specToHTML)
import DDL(parseTypes,saveMod)
import Parse(parseFile)

main :: IO ()
main =
  do args <- getArgs
     case args of
       "--html" : fs   -> mapM_ htmlFromFile fs
       "--ddl"  : inDir : outDir : [] -> ddlFromDir inDir outDir
       _ -> putStrLn $ unlines
                         [ "USAGE:"
                         , "  --html FILES\t\tGenerate HTML."
                         , "  --ddl  INDIR OUTDIR \t\tGenerate DDL."
                         ]

htmlFromFile :: FilePath -> IO ()
htmlFromFile f =
  do putStrLn f
     let name = takeBaseName f
     spec <- parseFile name f
     let outFile = "html" </> replaceExtension f ".html"
     createDirectoryIfMissing True (takeDirectory outFile)
     writeFile outFile (specToHTML spec)

ddlFromDir :: FilePath -> FilePath -> IO ()
ddlFromDir inDir outDir =
  do fs <- parseTypes inDir ["Catalog"]
     mapM_ (saveMod outDir) fs


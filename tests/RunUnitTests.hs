{-# Language BlockArguments #-}
module Main where

import Data.List(partition,sort)
import Control.Monad(unless,when)
import Control.Exception(SomeException(..),catch)
import System.Directory
import System.FilePath
import System.Exit
import System.Process(callProcess,proc,CreateProcess(..))
import qualified System.Process.ByteString as BS
import System.IO
import SimpleGetOpt
import qualified Data.ByteString as BS

exeName :: String
exeName = "daedalus"

ddlExt :: String
ddlExt = ".ddl"

testExt :: String
testExt = ".test"

inputExt :: String
inputExt = ".input"

testBin :: FilePath
testBin = "bin/daedalus"

defaultOutDir :: FilePath
defaultOutDir = "test-output"

data Options = Options
  { inputs    :: [FilePath]
  , outDir    :: FilePath
  , showHelp  :: Bool
  } deriving Show


defaultOptions :: Options
defaultOptions = Options
  { inputs = []
  , outDir = defaultOutDir
  , showHelp = False
  }


options :: OptSpec Options
options = optSpec
  { progOptions =
      [ Option [] ["out-dir"]
        "Save test output in this directory"
        $ ReqArg "DIR" $ \s o -> Right o { outDir = s }

      , Option [] ["help"]
        "Show this help"
        $ NoArg $ \o -> Right o { showHelp = True }
      ]
  , progParams = \p s -> Right s { inputs = p : inputs s }
  , progParamDocs = [("(FILE|DIR)*", "Tests to run")]
  , progArgOrder = Permute
  , progDescription = ["Test runner"]
  }


main :: IO ()
main =
  do opts <- getOpts defaultOptions options
     when (showHelp opts) $
        do dumpUsage options
           exitFailure

     buildExe

     tests <- findManyTests
              ( case inputs opts of
                  [] -> ["tests"]
                  is -> is
              )
     results <- mapM (runTest (outDir opts)) tests
     summarize results


buildExe :: IO ()
buildExe = callProcess "cabal" [ "build", "exe:" ++ exeName ]





type TestFile = FilePath

findManyTests :: [FilePath] -> IO [TestFile]
findManyTests files = sort.concat <$> mapM findTests files

findTests :: FilePath -> IO [TestFile]
findTests file =
  do isDir <- doesDirectoryExist file
     if isDir
        then do fs <- getDirectoryContents file
                findManyTests [ file </> f | f <- fs, take 1 f /= "." ]
        else pure $! fileToTest file

fileToTest :: FilePath -> [TestFile]
fileToTest file =
  [ file | takeExtension file `elem` [ ddlExt, testExt, inputExt ] ]




data TestResult = Ok
                | Fail Err
                  deriving Show

data Err = Diff FilePath FilePath | Except SomeException
           deriving Show

summarize :: [TestResult] -> IO ()
summarize rs =
  do putStrLn $ unwords [ "Passed", show (tot - bad)
                        , "of", show tot, "tests."
                        ]
     if bad == 0 then exitSuccess else exitFailure
  where bad = length [ () | Fail {} <- rs ]
        tot = length rs



runTest :: FilePath -> TestFile -> IO TestResult
runTest odir file =
  done (
  do putStr file
     hFlush stdout
     let testDir = takeDirectory file
         dir     = odir </> testDir
     createDirectoryIfMissing True dir
     let ps = [ "exec", exeName, "--", takeFileName file ]
         cp = (proc "cabal" ps) { cwd = Just testDir }
     (_exit,out,err) <- BS.readCreateProcessWithExitCode cp BS.empty
     BS.writeFile stdFile out
     BS.writeFile errFile err
     ok1 <- check stdGoldFile stdFile out
     case ok1 of
       Ok -> check errGoldFile errFile err
       _  -> pure ok1

   `catch` \e -> pure (Fail (Except e))
  )

  where
  stdFile = odir </> file <.> "stdout"
  errFile = odir </> file <.> "stderr"
  stdGoldFile = file <.> "stdout"
  errGoldFile = file <.> "stderr"

  check g s out =
    do yes <- doesFileExist g
       if yes
          then do expect <- BS.readFile g
                  pure (if out == expect then Ok else Fail (Diff g s))
          else pure Ok

  done m =
    do x <- m
       case x of
         Ok -> putStrLn " [Ok]"
         Fail err ->
           do putStrLn " [Fail]"
              case err of
               Diff g s -> putStrLn $ unwords
                             [ "***", "meld", show g, show s ]
               Except e -> putStrLn ("*** " ++ show x)
       pure x


#!/usr/bin/env runhaskell
{-# Language BlockArguments #-}
{-# Language ImplicitParams #-}
{-# Language ConstraintKinds #-}
module Main where

import Text.Read(readMaybe)
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad(filterM,forM)
import Control.Exception(SomeException(..),catch)
import System.FilePath
import System.Process
import System.Directory
import System.Environment
import System.IO
import System.Exit(exitFailure,exitSuccess)

main :: IO ()
main =
  do args <- getArgs
     let ?verbosity = 1
     case args of
       "compile" : fs ->
          case fs of
            [file]  -> compile file
            _       -> putStrLn "Usage: compile DDL_FILE"

       "run" : fs ->
          case fs of
            [file]        -> run file Nothing
            [file,input]  -> run file (Just input)
            _             -> putStrLn "Usage: run DDL_FILE [INPUT]"

       "check" : fs ->
          case fs of
            [file]        -> validate file Nothing
            [file,input]  -> validate file (Just input)
            _             -> putStrLn "Usage: check DDL_FILE [INPUT]"

       "diff" : fs->
          case fs of
            a : b : f : rest
              | Just be1 <- readMaybe a
              , Just be2 <- readMaybe b
                -> case rest of
                     []      -> diff be1 be2 f Nothing
                     [input] -> diff be1 be2 f (Just input)
                     _ -> putStrLn
                            "Usage: diff BACKEND1 BACKEND2 DDL_FILE [INPUT]"
            _ -> putStrLn "Usage: diff BACKEND1 BACKEND2 DDL_FILE [INPUT]"

       "all" : fs ->
          case fs of
            [] -> do let ?verbosity = 1
                     doAllTests
            _  -> putStrLn "Usage: all"

       "clean" : fs ->
          case fs of
            [] -> clean
            _  -> putStrLn "Usage: clean"

       [ file ]        -> allPhases file Nothing
       [ file, input ] -> allPhases file (Just input)

       _ -> mapM_ putStrLn
              [ "Usage:"
              , "          DDL_FILE [INPUT]"
              , "  compile DDL_FILE"
              , "  run     DDL_FILE [INPUT]"
              , "  check   DDL_FILE [INPUT]"
              , "  diff    BACKEND1 BACKEND2 DDL_FILE [INPUT]"
              , "  all"
              , "  clean"
              ]

type Quiet = (?verbosity :: Int)


data TestResult = OK | OutputsDiffer [[Backend]] | Fail SomeException
  deriving Show

isOK :: TestResult -> Bool
isOK result =
  case result of
    OK -> True
    _  -> False


--------------------------------------------------------------------------------
allPhases :: Quiet => FilePath -> Maybe FilePath -> IO ()
allPhases file mbInp =
  do compile file
     run file mbInp
     validate file mbInp



--------------------------------------------------------------------------------
-- Compilation

compile :: Quiet => FilePath -> IO ()
compile file = mapM_ (`compileWith` file) allBackends

compileWith :: Quiet => Backend -> FilePath -> IO ()
compileWith be ddl =
  do putStrLn ("[COMPILE " ++ show be ++ "] " ++ ddl)
     case be of
       InterpDaedalus -> pure ()
       InterpCore     -> pure ()
       CompileHaskell -> compileHaskell ddl
       CompileCPP     -> compileCPP ddl

compileHaskell :: Quiet => FilePath -> IO ()
compileHaskell ddl =
  do let root   = buildRootDirFor CompileHaskell
         build  = buildDirFor CompileHaskell ddl
     createDirectoryIfMissing True build
     callProcess' "cp" ["template_cabal_project", root </> "cabal.project"]
     callProcess' "cabal"
        [ "run", "-v0", "exe:daedalus", "--"
        , "compile-hs", "--out-dir=" ++ build, ddl
        ]
     callProcessIn_ build "cabal" ["build"]
     callProcessIn_ build "rm" ["-f", "parser"]
     path <- callProcessIn build "cabal" [ "-v0", "exec", "which", short ddl ]
     callProcessIn_ build "ln" ["-s", head (lines path), "parser"]
     pure ()

compileCPP :: Quiet => FilePath -> IO ()
compileCPP ddl =
  do let build = buildDirFor CompileCPP ddl
     createDirectoryIfMissing True build
     callProcess' "cabal"
        [ "run", "-v0", "exe:daedalus", "--"
        , "compile-c++", "--out-dir=" ++ build, ddl
        ]
     callProcess' "make" [ "-C", build, "parser" ]


--------------------------------------------------------------------------------
-- Running

run :: FilePath -> Maybe FilePath -> IO ()
run ddl mbInput = mapM_ (\be -> runWith be ddl mbInput) allBackends

runWith :: Backend -> FilePath -> Maybe FilePath -> IO ()
runWith be ddl mbInput =
  do putStrLn $ unwords [ "[RUN " ++ show be ++ "]", ddl, fromMaybe "" mbInput ]
     let file = outputFileFor be ddl mbInput
     createDirectoryIfMissing True (takeDirectory file)
     let interp = [ "run", "-v0", "exe:daedalus", "--"
                  , "--no-warn-unbiased", "run", "--json"
                  ]
     save file =<<
        case be of

          InterpDaedalus ->
            readProcessWithExitCode "cabal" (interp ++ [ ddl ] ++ inp) ""

          InterpCore ->
            readProcessWithExitCode "cabal" (interp ++ ["--core", ddl] ++ inp) ""

          CompileHaskell ->
            readProcessWithExitCode (buildDirFor be ddl </> "parser")
                                    (maybeToList mbInput)
                                    ""

          CompileCPP ->
            readProcessWithExitCode (buildDirFor be ddl </> "parser")
                                    (maybeToList mbInput)
                                    ""
  where
  inp = case mbInput of
          Nothing    -> []
          Just input -> ["--input=" ++ input]

  save f (_,o,_) = writeFile f o

--------------------------------------------------------------------------------
-- Validation

equiv :: Eq b => [(a,b)] -> [[a]]
equiv xs0 =
  case xs0 of
    [] -> []
    (x,b) : xs ->
      case partition ((== b) . snd) xs of
        (as,bs) -> (x : map fst as) : equiv bs

load :: Backend -> FilePath -> Maybe FilePath -> IO (Backend,String)
load be ddl mbInput =
  do let file = outputFileFor be ddl mbInput
     txt <- readProcess "jq" [".",file] ""
                `catch` \SomeException{} ->
                   do putStrLn ("Failed to parse output: " ++ show file)
                      putStrLn =<< readFile file
                      pure ""
     pure (be,txt)

validate :: FilePath -> Maybe FilePath -> IO ()
validate x y = validate' x y >> pure ()

validate' :: FilePath -> Maybe FilePath -> IO TestResult
validate' ddl mbInput =
  do results <- mapM (\be -> load be ddl mbInput) allBackends
     case equiv results of
       [_] -> putStrLn "OK" >> pure OK
       rs  -> do putStrLn "DIFFERENT"
                 mapM_ (putStrLn . unwords . map show) rs
                 pure (OutputsDiffer rs)


--------------------------------------------------------------------------------
-- Diff

diff :: Backend -> Backend -> FilePath -> Maybe FilePath -> IO ()
diff be1 be2 ddl mbInput =
  do f1 <- formatted be1
     f2 <- formatted be2
     (_,out,_) <- readProcessWithExitCode "diff" [f1,f2] ""
     putStrLn out
     removeFile f1
     removeFile f2
  where
  formatted be =
    do (f,h) <- openTempFile "/tmp" "diff"
       txt <- readProcess "jq" [".", outputFileFor be ddl mbInput] ""
       hPutStr h txt
       hClose h
       pure f


--------------------------------------------------------------------------------
-- Cleaning

clean :: IO ()
clean =
  do removeDirectoryRecursive buildDir
     removeDirectoryRecursive outputDir


--------------------------------------------------------------------------------
-- Run all tests

doAllTests :: Quiet => IO ()
doAllTests =
  do rs <- doAllTestsIn testsDir
     let (good,bad) = partition isOK rs
         ok    = length good
         notOk = length bad
         total = ok + notOk
     putStrLn ("Passed " ++ show ok ++ " / " ++ show total)
     if notOk > 0 then exitFailure else exitSuccess

doAllTestsIn :: Quiet => FilePath -> IO [TestResult]
doAllTestsIn dirName =
  do files <- listDirectory dirName
     if "Main.ddl" `elem` files
        then doOneTestInDir files
        else concat <$>
             forM files \f ->
               do let file = dirName </> f
                  isDir <- doesDirectoryExist file
                  if isDir
                    then doAllTestsIn file
                    else if takeExtension f == ".ddl"
                            then doOneTest f =<< findInputsFile files f
                            else pure []

  where
  findInputsFile siblings file =
    do let root = dropExtension file
       let inputDir = dirName </> root
       hasInput <- doesDirectoryExist inputDir
       fs1 <- if hasInput
                then do fs <- listDirectory inputDir
                        pure (map (inputDir </>) fs)
                else pure []
       let fs2' = [ dirName </> f
                  | f <- siblings, takeExtension f /= ".ddl" &&
                                   dropExtensions f == root ]
       fs2 <- filterM doesFileExist fs2'
       pure (fs1 ++ fs2)

  doOneTest ddl ins =
    attempt
    do putStrLn ("--- " ++ ddl ++ " ------------------------------------------")
       let file = dirName </> ddl
       compile file
       case ins of
         [] -> attempt
               do run file Nothing
                  (:[]) <$> validate' file Nothing
         _  -> concat <$>
               forM ins \i ->
                 attempt
                 do run file (Just i)
                    (:[]) <$> validate' file (Just i)

  -- XXX
  doOneTestInDir siblings = error "Not yet implemented"


  attempt m = m `catch` \e@SomeException{} ->
                            do print e
                               pure [Fail e]


--------------------------------------------------------------------------------
-- Directory structure


testsDir :: FilePath
testsDir = "tests"

buildDir :: FilePath
buildDir = "build"

outputDir :: FilePath
outputDir = "output"

data Backend = InterpDaedalus
             | InterpCore
             | CompileHaskell
             | CompileCPP
               deriving (Eq,Ord,Enum,Bounded,Show,Read)

allBackends :: [Backend]
allBackends = [ minBound .. maxBound ]

buildRootDirFor :: Backend -> FilePath
buildRootDirFor be = buildDir </> show be

buildDirFor :: Backend -> FilePath -> FilePath
buildDirFor be ddl = buildRootDirFor be </> short ddl

outputFileFor :: Backend -> FilePath -> Maybe FilePath -> FilePath
outputFileFor be ddl mbInput =
  outputDir </> short ddl </> maybe "" short mbInput </> show be


--------------------------------------------------------------------------------
-- Utilities

callProcessIn_ :: Quiet => FilePath -> String -> [String] -> IO ()
callProcessIn_ dir f xs =
  do _ <- callProcessIn dir f xs
     pure ()

callProcessIn :: Quiet => FilePath -> String -> [String] -> IO String
callProcessIn dir f xs =
  do verbose f xs
     (_exit,stdout,err) <-
        readCreateProcessWithExitCode (proc f xs) { cwd = Just dir } ""
     quiet err
     pure stdout

callProcess' :: Quiet => FilePath -> [String] -> IO ()
callProcess' f xs =
  do verbose f xs
     (_,out,err) <- readCreateProcessWithExitCode (proc f xs) ""
     quiet out
     quiet err
     pure ()

verbose :: Quiet => String -> [String] -> IO ()
verbose f xs
  | ?verbosity > 1  = putStrLn (unwords (map esc (f : xs)))
  | otherwise       = pure ()
  where
  escChar c = isSpace c || c == '"'
  esc s     = if any escChar s then show s else s

quiet :: Quiet => String -> IO ()
quiet err
  | ?verbosity < 1 = pure ()
  | otherwise      = hPutStr stderr err >> hFlush stderr

short :: FilePath -> String
short = dropExtension . takeFileName



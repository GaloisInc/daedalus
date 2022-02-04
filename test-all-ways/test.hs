#!/usr/bin/env runhaskell
module Main where

import Text.Read(readMaybe)
import Data.Maybe
import Data.List
import System.FilePath
import System.Process
import System.Directory
import System.Environment
import System.IO

main :: IO ()
main =
  do args <- getArgs
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
            _ -> putStrLn "Usage: diff BACKEND1 BACKEND2 DDL_FILE [INPUT]"

       "clean" : fs ->
          case fs of
            [] -> clean
            _  -> putStrLn "Usage: clean"

       [ file ] -> compile file >> run file Nothing >> validate file Nothing
       [ file, input ] ->
          compile file >> run file (Just input) >> validate file (Just input)

       _ -> mapM_ putStrLn
              [ "Usage:"
              , "          DDL_FILE [INPUT]"
              , "  compile DDL_FILE"
              , "  run     DDL_FILE [INPUT]"
              , "  check   DDL_FILE [INPUT]"
              , "  diff    BACKEND1 BACKEND2 DDL_FILE [INPUT]"
              , "  clean"
              ]

--------------------------------------------------------------------------------
-- Compilation

compile :: FilePath -> IO ()
compile file = mapM_ (`compileWith` file) allBackends

compileWith :: Backend -> FilePath -> IO ()
compileWith be ddl =
  do putStrLn ("[COMPILE " ++ show be ++ "] " ++ ddl)
     case be of
       InterpDaedalus -> pure ()
       InterpCore     -> pure ()
       CompileHaskell -> compileHaskell ddl
       CompileCPP     -> compileCPP ddl

compileHaskell :: FilePath -> IO ()
compileHaskell ddl =
  do let root   = buildRootDirFor CompileHaskell
         build  = buildDirFor CompileHaskell ddl
     createDirectoryIfMissing True build
     callProcess "cp" ["template_cabal_project", root </> "cabal.project"]
     callProcess "cabal"
        [ "exec", "daedalus", "--"
        , "--compile-hs", "--out-dir=" ++ build, ddl
        ]
     callProcessIn_ build "cabal" ["build"]
     callProcessIn_ build "rm" ["-f", "parser"]
     path <- callProcessIn build "cabal" [ "exec", "which", short ddl ]
     callProcessIn_ build "ln" ["-s", head (lines path), "parser"]
     pure ()

compileCPP :: FilePath -> IO ()
compileCPP ddl =
  do let build = buildDirFor CompileCPP ddl
     createDirectoryIfMissing True build
     callProcess "cabal"
        [ "exec", "daedalus", "--"
        , "--compile-c++", "--out-dir=" ++ build, ddl
        ]
     callProcess "make" [ "-C", build, "parser" ]


--------------------------------------------------------------------------------
-- Running

run :: FilePath -> Maybe FilePath -> IO ()
run ddl mbInput = mapM_ (\be -> runWith be ddl mbInput) allBackends

runWith :: Backend -> FilePath -> Maybe FilePath -> IO ()
runWith be ddl mbInput =
  do putStrLn $ unwords [ "[RUN " ++ show be ++ "]", ddl, fromMaybe "" mbInput ]
     let file = outputFileFor be ddl mbInput
     createDirectoryIfMissing True (takeDirectory file)
     let interp = [ "exec", "daedalus", "--"
                  , "--json", "--no-warn-unbiased"
                  ]
     save file =<<
        case be of

          InterpDaedalus ->
            readProcessWithExitCode "cabal" (interp ++ [ ddl, inp ]) ""

          InterpCore ->
            readProcessWithExitCode "cabal" (interp ++ ["--core", ddl, inp ]) ""

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
          Nothing    -> "--run"
          Just input -> "--interp=" ++ input

  save f (_,o,_) = writeFile f o

--------------------------------------------------------------------------------
-- Validation

equiv :: Eq b => [(a,b)] -> [[a]]
equiv xs =
  case xs of
    [] -> []
    (x,b) : xs ->
      case partition ((== b) . snd) xs of
        (as,bs) -> (x : map fst as) : equiv bs

load :: Backend -> FilePath -> Maybe FilePath -> IO (Backend,String)
load be ddl mbInput =
  do let file = outputFileFor be ddl mbInput
     txt <- readProcess "jq" [".",file] ""
     pure (be,txt)

validate :: FilePath -> Maybe FilePath -> IO ()
validate ddl mbInput =
  do results <- mapM (\be -> load be ddl mbInput) allBackends
     case equiv results of
       [_] -> putStrLn "OK"
       rs  -> do putStrLn "DIFFERENT"
                 mapM_ (putStrLn . unwords . map show) rs


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
-- Directory structure

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

callProcessIn_ :: FilePath -> String -> [String] -> IO ()
callProcessIn_ dir f xs =
  do _ <- callProcessIn dir f xs
     pure ()

callProcessIn :: FilePath -> String -> [String] -> IO String
callProcessIn dir f xs = readCreateProcess (proc f xs) { cwd = Just dir } ""

short :: FilePath -> String
short = dropExtension . takeFileName



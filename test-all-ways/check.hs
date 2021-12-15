#! /usr/bin/env runhaskell

import Data.List(partition)
import System.FilePath((</>),dropExtension,takeFileName)
import System.Process(readProcess)
import System.Environment(getArgs)
import System.Exit(exitFailure)

equiv :: Eq b => [(a,b)] -> [[a]]
equiv xs =
  case xs of
    [] -> []
    (x,b) : xs ->
      case partition ((== b) . snd) xs of
        (as,bs) -> (x : map fst as) : equiv bs

load :: FilePath -> String -> IO (String,String)
load dir name =
  do let file = dir </> name
     txt <- readProcess "jq" [".",file] ""
     pure (name,txt)

main :: IO ()
main =
  do args <- getArgs
     case args of
       [dir] -> validate dir
       _ -> putStrLn "Usage: INPUT_DIR"

validate :: FilePath -> IO ()
validate dir =
  do results <- mapM (load dir) [ "daedalus", "core", "haskell", "c++" ]
     case equiv results of
       [_] -> putStrLn "OK"
       rs  -> do mapM (putStrLn . unwords) rs
                 exitFailure





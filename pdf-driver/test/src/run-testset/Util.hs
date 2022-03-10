module Util where

-- system:
import           System.Exit

-- shake:
import           Development.Shake.FilePath

---- utilities ---------------------------------------------------------------

cmpFileContents :: (String -> String -> Bool)
                -> FilePath -> FilePath -> IO Bool
cmpFileContents eqv fa fb =
  do
  ca <- readFile fa
  cb <- readFile fb
  return (eqv ca cb)


triviallyFormat []     = ""
triviallyFormat (c:cs) =
  if c `elem` "{,}" then
    "\n  " ++ [c] ++ triviallyFormat cs
  else
    c : triviallyFormat cs
 
timeInMs x = round(x * 1000)

sfStripExtension :: String -> FilePath -> FilePath
sfStripExtension ext fp = case stripExtension ext fp of
                            Just fp' -> fp'
                            Nothing  -> error "sfStripExtension"

quit :: String -> IO a
quit msg =
  do putStrLn msg
     exitFailure
     


module Util where

-- system:
import           Data.Char
import           System.Exit

-- shake:
import           Development.Shake.FilePath

---- utilities ---------------------------------------------------------------

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
     
cmpFileContents :: (String -> String -> a)
                -> FilePath -> FilePath -> IO a
cmpFileContents eqv fa fb =
  do
  ca <- readFile fa
  cb <- readFile fb
  return (eqv ca cb)


rmTrailingWhitespace :: [Char] -> [Char]
rmTrailingWhitespace = reverse . dropWhile isSpace . reverse
  
---- split,unsplit -----------------------------------------------------------

-- split,unsplit a generalization of words,unwords!

split        :: Eq a => a -> [a] -> [[a]]
split x = splitBy (==x)

splitBy      :: (a->Bool) -> [a] -> [[a]]
splitBy _ []  = []
splitBy p xs  = let (l,xs') = break p xs
                in l : case xs' of []       -> []
                                   (_:xs'') -> splitBy p xs''

unsplit :: a -> [[a]] -> [a]
unsplit _ [] = []
unsplit x xs = foldr1 (\x' s -> x' ++ x:s) xs

unsplit' x xs = foldr (\x' s -> x' ++ x:s) [] xs

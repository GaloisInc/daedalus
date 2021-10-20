module Main(main) where

import System.Environment(getArgs)
import Data.List.NonEmpty(toList)
import Text.PrettyPrint(vcat)

import RTS.Parser(runParser)
import RTS(Result(..))
import RTS.Input(newInputFromFile)

import ICC(pMain)
import PP(pp)

main :: IO ()
main = mapM_ doFile =<< getArgs

doFile :: FilePath -> IO ()
doFile f =
  do i <- newInputFromFile (Just f)
     case runParser pMain i of
       NoResults e -> fail (show e)
       Results rs  -> print (vcat (map pp (toList rs)))



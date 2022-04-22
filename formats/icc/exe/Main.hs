module Main(main) where

import System.Environment(getArgs)
import Data.List.NonEmpty(toList)
import Text.PrettyPrint(vcat)

import RTS.Parser(Parser,runParser)
import RTS(Result(..))
import RTS.Input(newInputFromFile)

import qualified Parser
import qualified Validator
import PP(PP,pp)

main :: IO ()
main =
  do args <- getArgs
     case args of
       "--ast" : xs -> mapM_ (doFile Parser.pMain) xs
       xs -> mapM_ (doFile Validator.pMain) xs

doFile :: PP a => Parser a -> FilePath -> IO ()
doFile p f =
  do i <- newInputFromFile (Just f)
     case runParser p i of
       NoResults e -> print (pp e)
       Results rs  -> print (vcat (map pp (toList rs)))



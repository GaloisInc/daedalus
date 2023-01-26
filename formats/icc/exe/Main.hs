module Main(main) where

import System.Environment(getArgs)
import Data.List.NonEmpty(toList)
import Text.PrettyPrint(vcat)

import RTS.Parser(ParserG,runParser)
import RTS(ResultG(..))
import RTS.ParseError
import Daedalus.RTS.Input(newInputFromFile)
import RTS.Annot(Annotation)

import qualified Parser
import qualified Validator
import PP(PP,pp)

main :: IO ()
main =
  do args <- getArgs
     case args of
       "--ast" : xs -> mapM_ (doFile Parser.pMain) xs
       xs -> mapM_ (doFile Validator.pMain) xs

doFile :: PP a => ParserG Annotation a -> FilePath -> IO ()
doFile p f =
  do i <- newInputFromFile (Just f)
     case runParser p SingleError i of
       NoResults e -> print (pp e)
       Results rs  -> print (vcat (map (pp . fst) (toList rs)))



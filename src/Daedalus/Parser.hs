module Daedalus.Parser (parseFromFile, ParseError(..), prettyParseError) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Control.Exception(throwIO)

import Daedalus.AST
import Daedalus.Parser.Grammar
import Daedalus.Parser.Monad
import Daedalus.PrettyError

parseFromFile :: FilePath -> IO ([Located ModuleName], [Rule])
parseFromFile file =
  do txt <- Text.readFile file
     case runParser moduleP (Text.pack file) txt of
        Right a  -> pure a
        Left err -> throwIO err

prettyParseError :: ParseError -> IO String
prettyParseError (ParseError { errorLoc = loc, errorMsg = msg }) =
  prettyError loc msg


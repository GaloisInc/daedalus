module Daedalus.Parser (parseFromText, parseFromFile, ParseError(..), prettyParseError) where

import Data.Text (Text)

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Control.Exception(throwIO)

import Daedalus.AST
import Daedalus.Parser.Grammar
import Daedalus.Parser.Monad
import Daedalus.PrettyError
import Data.Either (partitionEithers)
import Daedalus.Rec (Rec(NonRec))

parseFromText :: Text -> ModuleName -> Text -> Either ParseError Module
parseFromText txtName n txt = do
  (imps, ds) <- runParser moduleP txtName txt
  let (rs, bds) = partitionEithers (map declToEither ds)
      rs' = map NonRec rs
  pure (Module n imps bds rs')
  where
    declToEither :: Decl -> Either Rule BitData
    declToEither (DeclRule rl) = Left rl
    declToEither (DeclBitData bd) = Right bd
    
parseFromFile :: FilePath -> ModuleName -> IO (Either ParseError Module)
parseFromFile file n = parseFromText (Text.pack file) n <$> Text.readFile file
        
prettyParseError :: ParseError -> IO String
prettyParseError (ParseError { errorLoc = loc, errorMsg = msg }) =
  prettyError loc msg


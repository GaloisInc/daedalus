module Daedalus.Parser (parseFromText, parseFromTokens, parseFromFile
                       , ParseError(..), prettyParseError) where

import           Data.Either             (partitionEithers)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text

import           Daedalus.AST
import           Daedalus.Parser.Grammar
import           Daedalus.Parser.Lexer   (Lexeme, Token)
import           Daedalus.Parser.Monad
import           Daedalus.PrettyError
import           Daedalus.Rec            (Rec (NonRec))

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

parseFromTokens :: Text -> ModuleName -> [Lexeme Token] -> Either ParseError Module
parseFromTokens txtName n toks = do
  (imps, ds) <- runParserOnTokens moduleP txtName toks
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


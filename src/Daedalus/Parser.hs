module Daedalus.Parser
  ( parseFromText, parseFromTextAt, parseFromTokens, parseFromFile
  , ParseError(..), prettyParseError
  , unlitMarkdown
  ) where

import           Data.Either             (partitionEithers)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           System.FilePath(takeExtension)

import           Daedalus.Rec            (Rec (NonRec))
import           Daedalus.AST
import           Daedalus.Parser.Grammar
import           Daedalus.Parser.Lexer   (Lexeme, Token,SourcePos, startPos)
import           Daedalus.Parser.Monad
import           Daedalus.Parser.Unlit
import           Daedalus.PrettyError

parseFromText :: Text -> ModuleName -> Text -> Either ParseError Module
parseFromText f = parseFromTextAt (startPos f)

parseFromTextAt :: SourcePos -> ModuleName -> Text -> Either ParseError Module
parseFromTextAt loc n txt = do
  (imps, ds) <- runParserAt moduleP loc txt
  let (rs, bds) = partitionEithers (map declToEither ds)
      rs' = map NonRec rs
  pure (Module n imps bds rs')
  where
    declToEither :: Decl -> Either TRule BitData
    declToEither (DeclRule rl) = Left (DRule rl)
    declToEither (DeclType rl) = Left (DType rl)
    declToEither (DeclBitData bd) = Right bd

parseFromTokens ::
  Text -> ModuleName -> [Lexeme Token] -> Either ParseError Module
parseFromTokens txtName n toks = do
  (imps, ds) <- runParserOnTokens moduleP txtName toks
  let (rs, bds) = partitionEithers (map declToEither ds)
      rs' = map NonRec rs
  pure (Module n imps bds rs')
  where
    declToEither :: Decl -> Either TRule BitData
    declToEither (DeclRule rl) = Left (DRule rl)
    declToEither (DeclType rl) = Left (DType rl)
    declToEither (DeclBitData bd) = Right bd

parseFromFile :: FilePath -> ModuleName -> IO (Either ParseError Module)
parseFromFile file n =
  parseFromText (Text.pack file) n . unlit <$> Text.readFile file
  where unlit = if takeExtension file == ".md"
                  then unlitMarkdown
                  else id

prettyParseError :: ParseError -> IO String
prettyParseError (ParseError { errorLoc = loc, errorMsg = msg }) =
  prettyError loc msg


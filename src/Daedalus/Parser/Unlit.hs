{-# Language BlockArguments, OverloadedStrings #-}
module Daedalus.Parser.Unlit where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

unlitMarkdown :: Text -> Text
unlitMarkdown = Text.unlines . skipping . Text.lines
  where
  fence l = "```" `Text.isPrefixOf` l

  skipping xs =
    case xs of
      []     -> []
      l : ls -> "" : if fence l then code ls else skipping ls

  code xs =
    case xs of
      [] -> []
      l : ls
        | fence l   -> "" : skipping ls
        | otherwise -> l  : code ls

unlitMarkdownFile :: FilePath -> IO Text
unlitMarkdownFile file = unlitMarkdown <$> Text.readFile file

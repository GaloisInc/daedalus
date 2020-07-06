module Daedalus.PrettyError (prettyError) where

import qualified Data.Text as Text

import Daedalus.SourceRange

prettyError :: SourcePos -> String -> IO String
prettyError loc msg =
  do let file = sourceFile loc
         line = sourceLine loc
         col  = sourceColumn loc
     contents <- lines <$> readFile (Text.unpack file)
     return $
       prettySourcePosLong loc <> ": " <> msg <>
       case getFileLine (line - 1) contents of
         Nothing -> "no file: " <> show contents
         Just l ->
           let lineNum = show line
           in unlines [ ""
                      , " " <> lineNum <> " | " <> l
                      , replicate (3 + length lineNum + col) ' ' <> "^"
                      ]
  where
    getFileLine _ [] = Nothing
    getFileLine n (l:ls)
      | n <= 0 = Just l
      | otherwise = getFileLine (n - 1) ls

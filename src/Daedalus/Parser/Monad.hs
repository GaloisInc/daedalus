{-# Language BlockArguments, OverloadedStrings #-}
module Daedalus.Parser.Monad
  ( Parser
  , ParseError(..)
  , runParser
  , happyError, parseError
  , nextToken
  , currentLoc
  ) where


import Control.Monad(ap,liftM)
import Data.Text(Text)
import Control.Exception(Exception)

import Daedalus.SourceRange
import Daedalus.Parser.Lexer


-- | A computation describing a parser.
newtype Parser a = Parser (Input -> Either ParseError (a,Input))

-- | A parse error.
data ParseError = ParseError { errorLoc :: !SourcePos
                             , errorMsg :: !String
                             } deriving Show

instance Exception ParseError


-- | The input for a parser.
data Input = Input { inputTokens    :: ![Lexeme Token]
                   , inputLastToken :: !SourceRange
                     -- ^ Last consumed token, if any
                   }

-- | Apply the given parser to some input.
runParser :: Parser a -> Text {- Input name -} -> Text -> Either ParseError a
runParser (Parser m) f txt =
  case m i of
    Left err -> Left err
    Right (a,j) ->
      case inputTokens j of
        []    -> Right a
        t : _ -> Left ParseError { errorLoc = sourceFrom (lexemeRange t)
                                 , errorMsg = "Unconsumed input."
                                 }

  where
  i = Input { inputTokens    = lexer f txt
            , inputLastToken = SourceRange { sourceFrom = p, sourceTo = p }
            }
  p = SourcePos { sourceIndex  = 0
                , sourceLine   = 1
                , sourceColumn = 1
                , sourceFile   = f
                }


-- | Used by Happy when none of its rules match.
happyError :: Parser a
happyError = Parser \i ->
  Left ParseError { errorLoc = sourceFrom (inputLastToken i)
                  , errorMsg = "Parse error"
                  }

-- | Used by Happy to consume the next token, so that it can figure out
-- what rule matches.
nextToken :: (Lexeme Token -> Parser a) -> Parser a
nextToken k = Parser \i ->
  let (t,ts) = case inputTokens i of
                 t' : ts' -> (t',ts')
                 [] -> let r = inputLastToken i
                           l = Lexeme
                                 { lexemeToken = TokEOF
                                 , lexemeText  = ""
                                 , lexemeRange = r { sourceFrom = sourceTo r }
                                 }
                       in l `seq` (l, [])

      Parser m = k t
  in case lexemeToken t of
       TokError e -> Left ParseError { errorLoc = sourceFrom (lexemeRange t)
                                     , errorMsg = e
                                     }
       _ -> m i { inputTokens = ts, inputLastToken = lexemeRange t }


-- | Abort parsing with the given parse error.
parseError :: SourcePos -> String -> Parser a
parseError l msg = Parser \_ -> Left ParseError { errorLoc = l
                                                , errorMsg = msg }

-- | The current token under examination
currentLoc :: Parser SourceRange
currentLoc = Parser \i -> Right (inputLastToken i, i)


instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser \i -> Right (a,i)
  (<*>)  = ap

instance Monad Parser where
  Parser m >>= k = Parser \i -> case m i of
                                  Left err -> Left err
                                  Right (a,j) ->
                                    let Parser m1 = k a
                                    in m1 j




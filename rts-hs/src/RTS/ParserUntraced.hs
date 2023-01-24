module RTS.ParserUntraced
  ( Parser, ParseError, Result
  , runParser
  , module RTS.ParserAPI
  , module RTS.Annot
  , module RTS.InputTrace
  ) where

import RTS.Parser
import RTS.ParserAPI
import RTS.Annot
import RTS.InputTrace
import RTS.ParseError

type Parser     = ParserG     () Annotation
type ParseError = ParseErrorG () Annotation
type Result     = ResultG     () Annotation


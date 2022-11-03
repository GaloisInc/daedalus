module RTS.ParserTraced
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

type Parser     = ParserG     InputTrace Annotation
type ParseError = ParseErrorG InputTrace Annotation
type Result     = ResultG     InputTrace Annotation


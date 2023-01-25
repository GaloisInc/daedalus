module Daedalus.Core.Semantics.Parser where

import RTS.ParserAPI
import RTS.ParseError
import RTS.Parser
import RTS.Annot

type Parser     = ParserG Annotation
type ParseError = ParseErrorG Annotation
type Result     = ResultG Annotation



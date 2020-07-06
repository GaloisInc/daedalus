module Daedalus.Compile.Config where

import Data.Map(Map)

import RTS.ParserAPI
import RTS.Parser

import Daedalus.Type.AST(Name)
import Daedalus.Compile.LangHS

data CompilerCfg = CompilerCfg
  { cImports :: [ Import ]
    -- ^ Additional imports to add to each module

  , cPrims :: Map Name Term
    -- ^ Implementation for primitives

  , cParserType :: Term
    -- ^ Use this type for parsers.

  , cQualNames :: UseQual
  }

data UseQual = UseQualNames | DoNotUseQualNames


pToken :: Parser a -> Parser a
pToken = undefined


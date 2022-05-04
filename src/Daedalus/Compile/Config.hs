{-# Language OverloadedStrings, QuasiQuotes #-}
module Daedalus.Compile.Config where

import Data.Map(Map)
import qualified Data.Map as Map

import Daedalus.Type.AST(Name)
import Daedalus.Compile.LangHS


data CompilerCfg = CompilerCfg
  { cImports :: [ Import ]
    -- ^ Additional imports to add to each module

  , cPrims :: Map Name ([Term] -> Term)
    -- ^ Implementation for primitives

  , cParserType :: Maybe Term
    -- ^ Use this type for parsers, default is to use RTS.Parser

  , cQualNames :: UseQual
  }

defaultCompilerCfg :: CompilerCfg
defaultCompilerCfg = CompilerCfg
  { cImports    = []
  , cPrims      = Map.empty
  , cParserType = Nothing
  , cQualNames  = UseQualNames
  }

data UseQual = UseQualNames | DoNotUseQualNames





{-# Language GADTs #-}

module Daedalus.ParserGen.LL
  ( createDFA
  , statsDFA
  , AutDet
  , DFA(..)
  , lookupAutDet
  , Prediction
  , destrPrediction
  , predictLL
  , ChoiceTag(..)
  )
where

import Daedalus.ParserGen.LL.Closure
import Daedalus.ParserGen.LL.DFA

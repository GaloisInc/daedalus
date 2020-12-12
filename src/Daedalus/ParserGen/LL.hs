{-# Language GADTs #-}

module Daedalus.ParserGen.LL
  ( createLLA
  , statsLLA
  , LLA(..)
  , Prediction
  , SynthLLAState
  , destrPrediction
  , predictLL
  , ChoiceTag(..)
  )
where

import Daedalus.ParserGen.LL.Closure
import Daedalus.ParserGen.LL.DFA

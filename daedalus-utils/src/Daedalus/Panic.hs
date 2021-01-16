{-# Language TemplateHaskell #-}
module Daedalus.Panic (panic, panicRange) where

import Daedalus.SourceRange

import Panic hiding (panic)
import qualified Panic

data Daedalus = Daedalus

instance PanicComponent Daedalus where
  panicComponentName _     = "Daedalus"
  panicComponentIssues _   = "https://github.com/GaloisInc/daedalus/issues"
  panicComponentRevision   = $useGitRevision

panic :: HasCallStack => String -> [String] -> a
panic = Panic.panic Daedalus

panicRange :: HasRange r => r -> String -> [String] -> a
panicRange r s ss = panic s (("at " ++ prettySourceRangeLong (range r)) : ss)

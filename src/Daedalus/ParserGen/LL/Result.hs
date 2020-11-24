module Daedalus.ParserGen.LL.Result
  ( AbortOption(..)
  , Result(..)
  , abortToString
  , coerceAbort
  ) where

import Daedalus.ParserGen.Action (Action(..))


data AbortOption =
    AbortNotStatic
  | AbortAcceptingPath
  | AbortNonClassInputAction Action
  | AbortUnhandledAction
  | AbortOverflowMaxDepth
  | AbortLoopWithNonClass
  | AbortNonEmptyIntersection
  | AbortClassIsDynamic
  | AbortClassNotHandledYet String
  | AbortSymbolicExec
  | AbortIncompatibleInput

  | AbortAmbiguous
  | AbortOverflowK

instance Show(AbortOption) where
  show a =
    case a of
      AbortNotStatic -> "AbortNotStatic"
      AbortAcceptingPath -> "AbortAcceptingPath"
      AbortNonClassInputAction _ -> "AbortNonClassInputAction"
      AbortUnhandledAction -> "AbortUnhandledAction"
      AbortOverflowMaxDepth -> "AbortOverflowMaxDepth"
      AbortLoopWithNonClass -> "AbortLoopWithNonClass"
      AbortNonEmptyIntersection -> "AbortNonEmptyIntersection"
      AbortClassIsDynamic -> "AbortClassIsDynamic"
      AbortClassNotHandledYet str -> "AbortClassNotHandledYet-" ++ str
      AbortSymbolicExec -> "AbortSymbolicExec"
      AbortIncompatibleInput -> "AbortIncompatibleInput"
      AbortAmbiguous -> "AbortAmbiguous"
      AbortOverflowK -> "AbortOverflowK"

data Result a =
    Abort AbortOption
  | Result a
  deriving(Show)


abortToString :: Result a -> String
abortToString r =
  case r of
    Abort ab -> show ab
    _ -> error "No Abort result"

coerceAbort :: Result a -> Result b
coerceAbort (Abort a) = Abort a
coerceAbort (Result _) = error "Abort result expected"

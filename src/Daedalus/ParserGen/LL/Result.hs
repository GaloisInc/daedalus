module Daedalus.ParserGen.LL.Result
  ( Result(..),
    abortToString
  ) where

import Daedalus.ParserGen.Action (Action(..))

data Result a =
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

  | AbortAmbiguous
  | AbortOverflowK

  | Result a
  deriving(Show)


abortToString :: Result a -> String
abortToString r =
  case r of
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
    AbortAmbiguous -> "AbortAmbiguous"
    AbortOverflowK -> "AbortOverflowK"
    _ -> error "No Abort result"

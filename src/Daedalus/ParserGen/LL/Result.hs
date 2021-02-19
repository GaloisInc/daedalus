module Daedalus.ParserGen.LL.Result
  ( AbortOption(..)
  , Result(..)
  , abortToString
  , coerceAbort
  ) where


data AbortOption =
    -- Abort cases for `SlkCfg`
    AbortSlkCfgExecution

    -- Abort cases for `Closure`
  | AbortClosureUnhandledInputAction
  | AbortClosureUnhandledAction
  | AbortClosureOverflowMaxDepth
  | AbortClosureInfiniteloop

    -- Abort cases for `ClassInterval`
  | AbortClassIsDynamic
  | AbortClassNotHandledYet String

    -- Abort cases for DFA
  | AbortDFAIncompatibleInput
  | AbortDFAOverflowInitCfg
  | AbortDFAOverflowLookahead
  | AbortDFAOverflowNbStates

instance Show(AbortOption) where
  show a =
    case a of
      AbortSlkCfgExecution -> "AbortSlkCfgExecution"

      AbortClosureUnhandledInputAction -> "AbortClosureUnhandledInputAction"
      AbortClosureUnhandledAction -> "AbortClosureUnhandledAction"
      AbortClosureOverflowMaxDepth -> "AbortClosureOverflowMaxDepth"
      AbortClosureInfiniteloop -> "AbortClosureInfiniteloop"

      AbortClassIsDynamic -> "AbortClassIsDynamic"
      AbortClassNotHandledYet str -> "AbortClassNotHandledYet_" ++ str

      AbortDFAIncompatibleInput -> "AbortDFAIncompatibleInput"
      AbortDFAOverflowInitCfg -> "AbortDFAOverflowInitCfg"
      AbortDFAOverflowLookahead -> "AbortDFAOverflowLookahead"
      AbortDFAOverflowNbStates -> "AbortDFAOverflowNbStates"

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

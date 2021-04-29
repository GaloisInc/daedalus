module Daedalus.ParserGen.LL.Result
  ( AbortOption(..)
  , Result(..)
  , abortToString
  , coerceAbort
  ) where


data AbortOption =
    -- Abort cases for `SlkCfg`
    AbortSlkCfgExecution
  | AbortSlkCfgClassIsDynamic
  | AbortSlkCfgClassNotHandledYet String

    -- Abort cases for `Closure`
  | AbortClosureUnhandledInputAction
  | AbortClosureUnhandledAction
  | AbortClosureOverflowMaxDepth
  | AbortClosureInfiniteloop

    -- Abort cases for DFA
  | AbortDFAIncompatibleInput
  | AbortDFAOverflowCfg
  | AbortDFAOverflowLookahead
  | AbortDFAOverflowNbStates

    -- Abort cases for LLA
  | AbortLLAOverflow

instance Show(AbortOption) where
  show a =
    case a of
      AbortSlkCfgExecution -> "AbortSlkCfgExecution"
      AbortSlkCfgClassIsDynamic -> "AbortSlkCfgClassIsDynamic"
      AbortSlkCfgClassNotHandledYet str -> "AbortSlkCfgClassNotHandledYet_" ++ str

      AbortClosureUnhandledInputAction -> "AbortClosureUnhandledInputAction"
      AbortClosureUnhandledAction -> "AbortClosureUnhandledAction"
      AbortClosureOverflowMaxDepth -> "AbortClosureOverflowMaxDepth"
      AbortClosureInfiniteloop -> "AbortClosureInfiniteloop"

      AbortDFAIncompatibleInput -> "AbortDFAIncompatibleInput"
      AbortDFAOverflowCfg -> "AbortDFAOverflowCfg"
      AbortDFAOverflowLookahead -> "AbortDFAOverflowLookahead"
      AbortDFAOverflowNbStates -> "AbortDFAOverflowNbStates"

      AbortLLAOverflow -> "AbortLLAOverflow"

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

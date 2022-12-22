module Daedalus.Interp.Error where

import Control.Exception
import Daedalus.Value
import Daedalus.AST hiding (Value)
import Daedalus.PP

interpError :: InterpError -> a
interpError = throw

partial :: Partial a -> a
partial val =
  case val of
    Left err -> interpError (PartialValue err)
    Right a  -> a

partial2 :: (Value -> Value -> Partial Value) -> Value -> Value -> Value
partial2 f = \x y -> partial (f x y)

partial3 :: (Value -> Value -> Value -> Partial Value) ->
            Value -> Value -> Value -> Value
partial3 f = \x y z -> partial (f x y z)



-- | We throw these exceptions for dynaimc errors encountered furing evaluation
-- (e.g., division by 0)
data InterpError = PartialValue String
                 | PatternMatchFailure String
                 | MultipleStartRules [ ScopedIdent ]
                 | UnknownStartRule ScopedIdent
                 | InvalidStartRule ScopedIdent
                 | MissingExternal ScopedIdent
  deriving (Show)

ppInterpError :: InterpError -> Doc
ppInterpError err =
  case err of
    PartialValue msg -> text msg
    PatternMatchFailure msg -> text msg
    MultipleStartRules rs ->
      hang "Multiple start rules:" 2 (bullets (map pp rs))
    UnknownStartRule r -> "Unknown start rule" <+> backticks (pp r)
    InvalidStartRule r ->
      vcat
        [ hang
            (vcat [ backticks (pp r) <+> "is not a valid start rule."
                  , "The interpreter start rule should not have any:"
                  ])
            2
            (bullets [ "type parameters"
                     , "implicit parameters"
                     , "explicit parameters" ])
        , "You may use the `show-types` command to see the types of the parsers"
        ]
    MissingExternal x ->
      hang
        ("Tried to execute external declaration" <+> backticks (pp x))
        2
        (bullets
          [ "The interpreter cannot execute externally defined parsers"
          , "Only the the compiled backends may use external primiteves."
          ])


instance Exception InterpError where
  displayException = show . ppInterpError



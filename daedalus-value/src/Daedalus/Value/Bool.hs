module Daedalus.Value.Bool where

import Daedalus.Panic
import Daedalus.Value.Type

vNot :: Value -> Value
vNot a =
  case a of
    VBool x -> VBool (not x)
    _       -> panic "vNot" [ "Invalid `not`", show a ]


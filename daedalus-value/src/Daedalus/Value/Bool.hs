{-# Language BlockArguments #-}
module Daedalus.Value.Bool where

import Daedalus.Panic
import Daedalus.Value.Type
import Daedalus.Value.Utils

vNot :: Value -> Value
vNot =
  tracedFun \a ->
  case a of
    VBool x     -> VBool (not x)
    _           -> panic "vNot" [ "Invalid `not`", show a ]


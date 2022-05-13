{-# Language QuasiQuotes, DataKinds #-}
{-# Options_GHC -ddump-splices #-}
module Daedalus.Test where

import Daedalus.Quote

[daedalus_compiled| Main

def Main =
  block
    x = true
    y = UInt8
|]

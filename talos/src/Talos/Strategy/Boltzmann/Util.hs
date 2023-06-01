module Talos.Strategy.Boltzmann.Util (
    unimplemented,
    HasCallStack,
    module Daedalus.PP,
    ) where

import Daedalus.PP
import GHC.Stack

unimplemented :: HasCallStack => String -> a
unimplemented = error . (++ " not implemented, because I have no idea what behavior it should have. Feel free to give it the behavior you were hoping for when you called it.")

{-# Language QuasiQuotes #-}
module CPPDriver where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as B
import Text.Heredoc(there)

main_driver_content :: ByteString
main_driver_content = B.pack [there|exe/driver-template/main.cpp|]

makefile_content :: ByteString
makefile_content = B.pack [there|exe/driver-template/Makefile|]



{-# Language TemplateHaskell #-}
module CPPDriver where

import Data.ByteString(ByteString)
import Data.FileEmbed

template_files :: [(FilePath,ByteString)]
template_files = $(embedDir "exe/driver-template")



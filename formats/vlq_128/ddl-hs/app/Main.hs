{-# Language TemplateHaskell #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
--{-# Options_GHC -ddump-simpl #-}
--{-# Options_GHC -dsuppress-module-prefixes -dsuppress-coercions -dno-typeable-binds -dsuppress-uniques #-}


module Main where

import Daedalus.TH.Compile
import Daedalus.RTS

compileDDLWith defaultConfig { nicerErrors = False } (FromFile "vlq_128.ddl")

main :: IO ()
main =
  do bytes <- newInputFromFile (Just "../data.dat")
     case runDParser (pMain bytes) of
       Right a -> print a
       Left a  -> print a


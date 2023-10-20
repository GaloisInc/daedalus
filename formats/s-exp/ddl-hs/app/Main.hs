{-# Language TemplateHaskell #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}

module Main where

import Daedalus.TH.Compile
import Daedalus.RTS

compileDDLWith defaultConfig { errorLevel = 0 } (FromFile "Main.ddl")

main :: IO ()
main =
  do bytes <- newInputFromFile (Just "../data.dat")
     case runDParser' (pMain bytes) of
       Just a  -> print a
       Nothing -> putStrLn "parse error"


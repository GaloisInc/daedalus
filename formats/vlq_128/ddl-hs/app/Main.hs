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

compileDDLWith defaultConfig { errorLevel = 0 } (FromFile "vlq_128.ddl")


{-# SCC many_58 #-}
{-# SCC many_61 #-}


main :: IO ()
main =
  do bytes <- newInputFromFile (Just "../data.dat")
     case runDParser' (pMain bytes) of
       --Right a -> print a
       --Left e -> print e
       Just a -> print a
       Nothing -> putStrLn "parse error"


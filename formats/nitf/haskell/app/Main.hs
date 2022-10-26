{-# Language TemplateHaskell #-}
{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeApplications #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-local-binds #-}
module Main where

import System.Environment(getArgs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Daedalus.TH.Compile
import Daedalus.RTS.Parser(initParseErrorState,dparserToEither,ppParseError)
import Daedalus.RTS.Input(newInput)

compileDDLWith
  defaultConfig
    { specPath = ["spec"]
    }
  (FromFile "spec/nitf_main.ddl")

main :: IO ()
main =
  do args <- getArgs
     case args of
       [ file ] ->
          do bytes <- BS.readFile file
             let input  = newInput (BS8.pack file) bytes
                 result = dparserToEither (pMain input initParseErrorState)
             case result of
               Left err  -> print (ppParseError err)
               Right val -> print val
       _ -> putStrLn "Program requires exactly one file"

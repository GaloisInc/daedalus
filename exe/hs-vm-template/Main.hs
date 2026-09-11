{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language KindSignatures #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
module Main where

import qualified Data.ByteString.Char8 as BS8
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hPutStrLn,stderr)

import qualified Daedalus.RTS as RTS
import qualified Daedalus.RTS.JSON as JSON
import Daedalus.TH.Compile

$(compileDDLWith defaultConfig
    { specPath = $SEARCH_PATH, errorLevel = $ERROR_LEVEL }
    (FromFileAs $DDL_FILE $SOURCE_NAME))

main :: IO ()
main =
  do args <- getArgs
     input <- case args of
                []     -> RTS.newInputFromFile Nothing
                [file] -> RTS.newInputFromFile (Just file)
                _      -> do hPutStrLn stderr "Usage: $EXE [FILE]"
                             exitFailure
     case RTS.runParserResults (pMain input) of
       Left err ->
         do BS8.putStrLn (JSON.jsonToBytes (JSON.toJSON err))
            exitFailure
       Right results ->
         BS8.putStrLn
           (JSON.jsonToBytes (JSON.jsArray (map JSON.toJSON results)))

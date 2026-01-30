module Main where


import Control.Monad(unless)
import System.IO(hPrint,stderr)
import System.Exit(exitFailure)

import Daedalus.Driver

import Daedalus.PP(pp)
import SimpleGetOpt
import Options
import Parser
import Check


main :: IO ()
main =
  do
    opts <- getOpts defaultOptions options
    (spec,tds) <-
      case optExportFile opts of
        Nothing -> reportUsageError options ["Missing export specification."]
        Just f  ->
          do
            mb <- daedalus
                    do
                      unless (null (optSearchPathForDDL opts))
                        (ddlSetOpt optSearchPath (reverse (optSearchPathForDDL opts))) 
                      parseFromFile f moduleParser
            case mb of
              Left err -> hPrint stderr (pp err) >> exitFailure
              Right a  -> pure a
    case checkModule tds spec of
      Left err -> hPrint stderr (pp err) >> exitFailure
      Right a -> print (pp a)



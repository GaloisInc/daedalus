module Main where


import Control.Monad(unless)
import System.IO(hPrint,stderr)
import System.Exit(exitFailure)

import Daedalus.Driver

import Daedalus.PP(pp)
import SimpleGetOpt
import Options
import Parser


main :: IO ()
main =
  do
    opts <- getOpts defaultOptions options
    spec <-
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
    print (pp spec)


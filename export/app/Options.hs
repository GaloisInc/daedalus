module Options where

import SimpleGetOpt

data Options = Options {
  ddlFile    :: Maybe FilePath,
  ddlEntries :: [String],
  exportFile :: Maybe FilePath
}

defaultOptions :: Options
defaultOptions = Options {
    ddlFile    = Nothing,
    ddlEntries = [],
    exportFile = Nothing
}


options :: OptSpec Options
options = OptSpec
  { progArgOrder = Permute

  , progDescription =
    [ "The DaeDaLus export generator."
    , "Use --help to see a list of all flags."
    ]

  , progOptions =
      [ Option [] ["entry"]
        "Specify an entry point for the Daedalus spec."
        $ ReqArg "IDENT" \s o -> Right o { ddlEntries = s : ddlEntries o }

      , Option [] ["ddl"]
        "Use this file for the Daedalus spec."
        $ ReqArg "FILE" \s o ->
            case ddlFile o of
              Nothing -> Right o { ddlFile = Just s }
              Just _  -> Left "Multiple --ddl flags are not supported."
      ]

  , progParamDocs = [ ("FILE", "Export specification") ]

  , progParams = \s o ->
      case exportFile o of
        Nothing -> Right o { exportFile = Just s }
        Just _ -> Left "Multiple export specifications are not supported."
  }



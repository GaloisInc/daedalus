module Options where

import SimpleGetOpt

data Options = Options {
  optSearchPathForDDL :: [FilePath],    -- ^ reversed
  optSearchPathForDex :: [FilePath],    -- ^ reversed
  optOutputFile :: Maybe FilePath,
  optExportFile :: Maybe FilePath
}

defaultOptions :: Options
defaultOptions = Options {
    optSearchPathForDDL = [],
    optSearchPathForDex = [],
    optOutputFile = Nothing,
    optExportFile = Nothing
}


options :: OptSpec Options
options = OptSpec
  { progArgOrder = Permute

  , progDescription =
    [ "The DaeDaLus export generator."
    , "Use --help to see a list of all flags."
    ]

  , progOptions =
      [ Option [] ["ddl-path"]
        "Add the given directory to the search path for DaeDaLs modules."
        $ ReqArg "DIR" \s o -> Right o { optSearchPathForDDL = s : optSearchPathForDDL o }

      , Option [] ["dex-path"]
        "Add the given directory to the search path for Dex modules."
        $ ReqArg "DIR" \s o -> Right o { optSearchPathForDex = s : optSearchPathForDex o }

      , Option [ ]["output"]
        "Generate code in this file."
        $ ReqArg "FILE" \s o -> Right o { optOutputFile = Just s }
      ]

  , progParamDocs = [ ("FILE", "Export specification") ]

  , progParams = \s o ->
      case optExportFile o of
        Nothing -> Right o { optExportFile = Just s }
        Just _ -> Left "Multiple export specifications are not supported."
  }



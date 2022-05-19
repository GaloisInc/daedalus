{-# Language BlockArguments #-}
module CommandLine where

import Text.Read(readMaybe)
import SimpleGetOpt

import qualified Data.ByteString.Char8 as C 

data Command =
    PrettyPrint
  | PrettyPrintAll
  | Validate          -- FIXME: Misnomer: just validates trailer. 
  | ListXRefs
  | ParseType String  -- string must be name of a select few parsers (see Main.hs)
  | ListIncUpdates
  | ListCavities
  | ParseValue
  | ShowHelp

  -- | ShowEncrypt  

data Settings = Settings
  { command     :: Command
  , object      :: Integer
  , generation  :: Integer
  , password    :: C.ByteString 
  , files       :: [FilePath]
  }

defaults :: Settings
defaults =
  Settings
    { command     = Validate  -- FIXME: remove or change default
    , object      = -1    -- means show trailer
    , generation  = 0
    , password    = C.empty 
    , files       = []
    }


options :: OptSpec Settings
options = optSpec
  { progOptions =
      [ Option [] ["xrefs"]
        "List the cross-reference table."
        $ NoArg $ \s -> Right s { command = ListXRefs }

      , Option [] ["updates"]
        "List incremental updates."
        $ NoArg $ \s -> Right s { command = ListIncUpdates }

      , Option [] ["cavities"]
        "List cavities (in each incr. update)."
        $ NoArg $ \s -> Right s { command = ListCavities }

      , Option [] ["pp"]
        "Pretty print trailer or the reference --obj --gen"
        $ NoArg \s -> Right s { command = PrettyPrint }

      -- , Option [] ["enc"]
      --   "Print the Encryption table."
      --   $ NoArg \s -> Right s { command = ShowEncrypt }

      , Option [] ["list"]
        "Pretty print all objects in the PDF."
        $ NoArg \s -> Right s { command = PrettyPrintAll }

      , Option [] ["obj"]
        "Focus on this object."
      $ ReqArg "NUM" $ integerArg "obj" \o s -> Right s { object = o }

      , Option [] ["gen"]
        "Set the generation of the focused object."
      $ ReqArg "NUM" $ integerArg "gen" \g s -> Right s { generation = g }

      , Option [] ["pwd"]
        "Set the encryption password for the document."
      $ ReqArg "STRING" $ (\v opts -> Right opts { password = C.pack v} ) 

      , Option [] ["parse-value"]
        "File contains a value instead of a PDF document."
      $ NoArg $ \opts -> Right opts { command = ParseType "Value" }

      , Option [] ["parse-type"]
        "File contains an object of type PARSER instead of a PDF document."
      $ ReqArg "PARSER" $ (\v opts -> Right opts { command = ParseType v} ) 

      , Option [] ["help"]
        "Show this help."
      $ NoArg $ \s -> Right s { command = ShowHelp }
      ]

  , progParamDocs =
      [ ("FILE", "The file that need processing.") ]

  , progParams = \p s -> Right s { files = p : files s }
  }


integerArg ::
  String ->
  (Integer -> OptSetter Settings) ->
  (String  -> OptSetter Settings)
integerArg msg f = \a s ->
  case readMaybe a of
    Just i  -> f i s
    Nothing -> Left ("Invalid number in parameter " ++ show msg)




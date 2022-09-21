{-# Language BlockArguments #-}
module CommandLine ( Command(..)
                   , Options(..), Backend(..)
                   , getOptions
                   , options
                   , throwOptError
                   , OptsHS(..)
                   ) where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.List(intercalate)
import Data.String(fromString)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Exception(throwIO)
import System.Environment(getArgs)
import System.FilePath(takeExtension)
import System.Exit(exitSuccess)
import SimpleGetOpt

data Command =
    DumpRaw
  | DumpResolve
  | DumpTC
  | DumpTypes
  | DumpSpec
  | DumpCore
  | DumpVM
  | DumpRel
  | DumpGraph Bool
  | DumpGen
  | CompileHS
  | CompileCPP
  | Interp (Maybe FilePath)
  | JStoHTML
  | ShowHelp

data Backend = UseInterp | UseCore | UseVM | UsePGen Bool

data Options =
  Options { optCommand   :: Command
          , optParserDDL :: FilePath
          , optEntries   :: [String]
          , optBackend   :: Backend
          , optForceUTF8 :: Bool
          , optShowJS    :: Bool
          , optShowHTML  :: Bool
          , optInline    :: Bool
          , optInlineThis :: [String]
          , optStripFail :: Bool
          , optSpecTys   :: Bool
          , optDeterminize :: Bool
          , optCheckCore  :: Bool
          , optOutDir    :: Maybe FilePath
          , optOutDirHeaders :: Maybe FilePath
          , optHS        :: OptsHS
          , optNoWarnUnbiasedFront :: Bool
          , optNoWarnUnbiased :: Bool
          , optErrorStacks :: Bool
          , optUserState :: Maybe String
          , optExtraInclude :: [String]
          , optFileRoot :: String
          , optUserNS :: String
          , optExternMods :: Map Text String
            -- ^ maps external module to namespace qualifier in generated code

          , optModulePath :: [String]
            -- ^ Search for modules in these paths

          , optParams :: [String]
          }

defaultOptions :: Options
defaultOptions =
  Options { optCommand   = DumpTC
          , optParserDDL = ""
          , optBackend   = UseInterp
          , optEntries   = []
          , optForceUTF8 = True
          , optShowJS    = False
          , optShowHTML  = False
          , optInline    = False
          , optInlineThis = []
          , optStripFail = False
          , optSpecTys   = False
          , optCheckCore = True
          , optDeterminize = False
          , optOutDir    = Nothing
          , optOutDirHeaders    = Nothing
          , optNoWarnUnbiasedFront = False
          , optNoWarnUnbiased = False
          , optHS        = noOptsHS
          , optParams    = []
          , optErrorStacks = True
          , optUserState = Nothing
          , optExtraInclude = []
          , optFileRoot = "main_parser"
          , optUserNS = defaultUserSpace
          , optExternMods = Map.empty
          , optModulePath = []
          }

defaultUserSpace :: String
defaultUserSpace = "User"


--------------------------------------------------------------------------------
data OptsHS =
  OptsHS
    { hsoptMonad   :: Maybe String
    , hsoptImports :: [ (String,Maybe String) ]   -- module as A
    , hsoptPrims   :: [ (Text,Text,String) ]  -- (mod,prim,haskellVar)
    , hsoptFile    :: Maybe FilePath
    , hsoptCore    :: Bool
    }

noOptsHS :: OptsHS
noOptsHS =
  OptsHS
    { hsoptMonad   = Nothing
    , hsoptImports = []
    , hsoptPrims   = []
    , hsoptFile    = Nothing
    , hsoptCore    = False
    }

noOptHS :: (OptsHS -> Either String OptsHS) ->
           Options -> Either String Options
noOptHS f o =
  do o1 <- f (optHS o)
     pure o { optHS = o1 }

reqOptHS :: (String -> OptsHS -> Either String OptsHS) ->
            String -> Options -> Either String Options
reqOptHS f s o =
  do o1 <- f s (optHS o)
     pure o { optHS = o1 }
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
type CommandSpec = (Options -> Options, OptSpec Options)

commands :: [(String,CommandSpec)]
commands =
  [ ("run",           cmdRunOptions)
  , ("show-types",    cmdShowTypesOptions)
  , ("compile-hs",    cmdCompileHSOptions)
  , ("compile-c++",   cmdCompileCPPOptions)
  , ("json-to-html",  cmdJsonToHtmlOptions)
  , ("dump",          cmdDumpOptions)
  ]

options :: OptSpec Options
options = OptSpec
  { progArgOrder = RequireOrder
  , progDescription =
    [ "The DaeDaLus specification processor."
    , "To see command specific flags pass `--help` to the command."
    ]
  , progOptions =
      [ Option [] ["no-utf8"]
        "Use the locale settings instead of UTF8 for output."
       $ NoArg \o -> Right o { optForceUTF8 = False }

      , Option [] ["no-warn-unbiased"]
        "Do not warn about uses of unbiased choice."
        $ NoArg \s -> Right s { optNoWarnUnbiased = True }

      , Option [] ["path"]
        "Add this to the search path for modules."
        $ ReqArg "DIR" \s o -> Right o { optModulePath = s : optModulePath o }

      , helpOption
      ]

  , progParamDocs = [ (r, intercalate "\n" (progDescription h))
                    | (r,(_,h)) <- commands ]

  , progParams = \s o -> Right o { optParams = s : optParams o }
  }




cmdShowTypesOptions :: CommandSpec
cmdShowTypesOptions = (\o -> o { optCommand = DumpTypes }, opts)
  where
  opts = optWithDDL
          { progDescription = [ "Show the types of the definitions in a file." ]
          , progOptions = [ helpOption ]
          }

cmdRunOptions :: CommandSpec
cmdRunOptions = (\o -> o { optCommand = Interp Nothing }, opts)
  where
  opts = optWithDDL
          { progDescription = [ "Run the interpreter." ]

          , progOptions =
              [ Option ['i'] ["input"]
                "Parse this file"
                $ ReqArg "FILE" \s o -> Right o { optCommand = Interp (Just s) }

              , Option [] ["json"]
                "Show semantics values as JSON."
                $ NoArg \o -> Right o { optShowJS = True }

              , Option [] ["html"]
                "Show semantics values as HTML."
                $ NoArg \o -> Right o { optShowJS = True, optShowHTML = True }

             , Option [] ["gen"]
                "Use the parser-generator interpreting"
                $ OptArg "metrics" \s o ->
                  case s of
                    Nothing -> Right o { optBackend = UsePGen False}
                    Just "metrics" -> Right o { optBackend = UsePGen True }
                    Just _ -> Left "Invalid setting for `gen`, expected `metrics`"

              , Option [] ["core"]
                "Use the Core interpreter"
                $ NoArg \o -> Right o { optBackend = UseCore }

              , Option [] ["vm"]
                "Use the VM interpreter"
                $ NoArg \o -> Right o { optBackend = UseVM }

              ] ++ coreOptions ++
              [ helpOption
              ]
          }

cmdDumpOptions :: CommandSpec
cmdDumpOptions = (\o -> o { optCommand = DumpTC }, opts)
  where
  opts = optWithDDL
           { progDescription = [ "Dump an intermediate compiler representation." ]

           , progOptions =
               [ Option [] ["parsed"]
                 "Dump parsed AST"
                 $ simpleCommand DumpRaw

               , Option [] ["resolved"]
                 "Dump name-resolved AST"
                 $ simpleCommand DumpResolve

               , Option [] ["tc"]
                 "Dump type-checked AST"
                 $ simpleCommand DumpTC

               , Option [] ["spec"]
                 "Dump specialised type-checked AST"
                 $ simpleCommand DumpSpec

               , Option [] ["core"]
                 "Dump core AST"
                $ simpleCommand DumpCore

               , Option [] ["rel"]
                 "Dump relational core"
                $ simpleCommand DumpRel

               , Option [] ["vm"]
                 "Dump VM AST"
                $ simpleCommand DumpVM

               , Option [] ["vm-graph"]
                 "Dump VM AST"
                $ OptArg "FUN|BLOCK" \s o ->
                  Right o { optCommand = DumpGraph (s == Just "FUN") }

               , Option [] ["gen"]
                 "Dump parser-generator automaton-based parser"
                $ simpleCommand DumpGen
               ] ++
               coreOptions ++
               [ helpOption
               ]
           }

cmdJsonToHtmlOptions :: CommandSpec
cmdJsonToHtmlOptions = (\o -> o { optCommand = JStoHTML }, opts)
  where
  opts = optSpec
          { progDescription = [ "Render externally produced JSON as HTML." ]
          , progOptions =
              [ helpOption
              ]
          , progParamDocs = [ ("FILE", "The JSON file to process.") ]
          , progParams    = \s o -> Right o { optParserDDL = s }
          }

cmdCompileHSOptions :: CommandSpec
cmdCompileHSOptions = (\o -> o { optCommand = CompileHS }, opts)
  where
  opts = optWithDDL
    { progDescription = [ "Generate Haskell code." ]
    , progOptions =
      [ Option [] ["out-dir"]
        "Save output in this directory."
        $ ReqArg "DIR" \s o -> Right o { optOutDir = Just s }

      , Option [] ["config"]
        "Read compiler configuraiton from this file."
        $ ReqArg "FILE"
        $ reqOptHS \s o ->
          Right o { hsoptFile = Just s }

      , Option [] ["monad"]
        "Use this parser monad (default `Parser`)."
        $ ReqArg "[QUAL.]NAME"
        $ reqOptHS \s o -> Right o { hsoptMonad = Just s }

      , Option [] ["import"]
        "Add this import to all generated Haskell modules."
        $ ReqArg "MODULE[:QUAL]"
        $ reqOptHS \s o ->
          Right o { hsoptImports = colonSplit s : hsoptImports o }

      , Option [] ["prim"]
        "Define an external Haskell primitive."
        $ ReqArg "MODULE:PRIM_NAME:EXTERNAL_NAME"
        $ reqOptHS \s o ->
          case colonSplitText s of
            (m,Just rest) | (n,Just p) <- colonSplitText rest ->
                Right o { hsoptPrims = (m, n, p) : hsoptPrims o }
            _ -> Left
              "Invalid primitve, expected: MODULE:PRIM_NAME:EXTERNAL_NAME"

      , Option [] ["vm"]
        "Use the VM backend (experimental)."
        $ NoArg $ noOptHS \o -> pure o { hsoptCore = True }

      , helpOption
      ]
    }

cmdCompileCPPOptions :: CommandSpec
cmdCompileCPPOptions = (\o -> o { optCommand = CompileCPP }, opts)
  where
  opts = optWithDDL
    { progDescription = [ "Generate C++ code" ]
    , progOptions =
      [ Option [] ["out-dir"]
        "Save output in this directory."
        $ ReqArg "DIR" \s o -> Right o { optOutDir = Just s }

      , Option [] ["out-dir-headers"]
        "Save generated headers in this directory (defaults to `out-dir`)"
        $ ReqArg "DIR" \s o -> Right o { optOutDirHeaders = Just s }

      , Option [] ["file-root"]
        "Output file template (default: main_parser)"
        $ ReqArg "FILE" \s o -> Right o { optFileRoot = s }

      , Option [] ["user-namespace"]
        "Place type declarations in this namesapce (default: User)"
        $ ReqArg "NAMESPACE" \s o -> Right o { optUserNS = s }

      , Option [] ["no-error-stack"]
        "Do not generate a grammar stack trace on error."
        $ NoArg \o -> Right o { optErrorStacks = False }

      , Option [] ["user-state"]
        "Generate a parser using this type for custom user state"
        $ ReqArg "STATE_TYPE" \s o -> Right o { optUserState = Just s }

      , Option [] ["add-include"]
        "Add #include INCLUDE to generated files"
        $ ReqArg "INCLUDE"
          \s o -> Right o { optExtraInclude = s : optExtraInclude o }
      ] ++
      coreOptions ++
      [ helpOption
      ]
    }

coreOptions :: [OptDescr Options]
coreOptions =
  [ Option [] ["entry"]
    "Generate a library containg this parser."
    $ ReqArg "[MODULE.]NAME"
      \s o -> Right o { optEntries = s : optEntries o }

  , Option [] ["determinize"]
    "Determinize core"
    $ NoArg \o -> Right o { optDeterminize = True }

  , Option [] ["inline"]
    "Do aggressive inlining on Core"
    $ NoArg \o -> Right o { optInline = True }

  , Option [] ["inline-this"]
    "Inline this decl"
    $ ReqArg "[MODULE.]NAME" \s o ->
      Right o { optInline = True, optInlineThis = s : optInlineThis o }

  , Option [] ["strip-fail"]
    "Strip failure nodes in Core"
    $ NoArg \o -> Right o { optStripFail = True }

  , Option [] ["spec-types"]
    "Specialise types"
    $ NoArg \o -> Right o { optSpecTys = True }

  , Option [] ["no-core-check"]
    "Do not validate Core"
    $ NoArg \o -> Right o { optCheckCore = False }

  , Option [] ["extern"]
    "Do not generate definitions for the types in this module."
    $ ReqArg "MODULE[:NAMESPACE]"
      \s o -> Right o { optExternMods =
                          let (m,u) = case break (== ':') s of
                                        (as,[]) -> (as, defaultUserSpace)
                                        (as,_:bs) -> (as,bs)
                          in Map.insert (Text.pack m) u (optExternMods o)
                       }
  ]



--------------------------------------------------------------------------------

dispatchCommand :: Options -> IO Options
dispatchCommand opts =
  impliedOptions <$>
  case reverse (optParams opts) of
    p : ps | Just spec <- lookup p commands -> cmdMaybeHelp opts spec ps
    [file] | takeExtension file == ".test" -> getOptionsFromFile file
           | otherwise -> cmdMaybeHelp opts cmdDumpOptions [file]
    _ -> dumpUsage options >> exitSuccess


cmdMaybeHelp :: Options -> CommandSpec -> [String] -> IO Options
cmdMaybeHelp dflt (updDflt, spec) args =
  do opts <- case getOptsFrom (updDflt dflt) spec args of
               Left (GetOptException err) -> reportUsageError spec err
               Right a  -> pure a
     case optCommand opts of
       ShowHelp ->
         do dumpUsage spec
            exitSuccess
       _ -> pure opts

getOptions :: IO Options
getOptions = getOptions' =<< getArgs

getOptions' :: [String] -> IO Options
getOptions' args =
  do opts <- cmdMaybeHelp defaultOptions (id,options) args
     dispatchCommand opts

getOptionsFromFile :: FilePath -> IO Options
getOptionsFromFile file =
  do inp <- readFile file
     getOptions' (lines inp)

throwOptError :: [String] -> IO a
throwOptError err = throwIO (GetOptException err)

impliedOptions :: Options -> Options
impliedOptions opts0 =
  case optBackend opts of
    UseCore -> noTCUnbiased
    _ ->
      case optCommand opts of
        DumpRaw         -> opts
        DumpResolve     -> opts
        DumpTC          -> opts
        DumpTypes       -> opts
        DumpSpec        -> opts
        DumpCore        -> noTCUnbiased
        DumpRel         -> noTCUnbiased
        DumpVM          -> noTCUnbiased
        DumpGraph {}    -> opts
        DumpGen         -> opts
        CompileHS       -> opts
        CompileCPP      -> noTCUnbiased
        Interp {}       -> opts
        JStoHTML        -> opts
        ShowHelp        -> opts

  where
  opts = if optNoWarnUnbiased opts0
          then opts0 { optNoWarnUnbiasedFront = True }
          else opts0
  noTCUnbiased = opts { optNoWarnUnbiasedFront = True }



--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Utilities

simpleCommand :: Command -> ArgDescr Options
simpleCommand x = NoArg \o -> Right o { optCommand = x }

helpOption :: OptDescr Options
helpOption =
  Option ['h'] ["help"]
  "Show this help"
  $ simpleCommand ShowHelp

optWithDDL :: OptSpec Options
optWithDDL = optSpec
  { progParamDocs = [ ("FILE", "The DDL specification to process.") ]
  , progParams    = \s o -> Right o { optParserDDL = s }
  }

colonSplitText :: String -> (Text, Maybe String)
colonSplitText a = (fromString x, xs)
  where (x,xs) = colonSplit a

colonSplit :: String -> (String, Maybe String)
colonSplit a =
  case break (== ':') a of
    (xs,ys) -> (xs, case ys of
                      _ : more -> Just more
                      _        -> Nothing)





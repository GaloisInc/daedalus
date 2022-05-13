{-# Language BlockArguments #-}
module CommandLine ( Command(..)
                   , Options(..), Backend(..)
                   , getOptions
                   , options
                   , throwOptError
                   , OptsHS(..)
                   ) where

import Data.Text(Text)
import Data.String(fromString)
import Control.Monad(when)
import Control.Exception(throwIO)
import System.FilePath(takeExtension)
import System.Exit(exitSuccess)
import SimpleGetOpt

data Command =
    DumpRaw
  | DumpTC
  | DumpTypes
  | DumpSpec
  | DumpRuleRanges
  | DumpCore
  | DumpVM
  | DumpGraph Bool
  | DumpGen
  | CompileHS
  | CompileCPP
  | Interp (Maybe FilePath)
  | JStoHTML
  | ShowHelp

data Backend = UseInterp | UseCore | UsePGen Bool

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
          , optHS        :: OptsHS
          , optNoWarnUnbiasedFront :: Bool
          , optNoWarnUnbiased :: Bool
          }

data OptsHS =
  OptsHS
    { hsoptMonad   :: Maybe String
    , hsoptImports :: [ (String,Maybe String) ]   -- module as A
    , hsoptPrims   :: [ (Text,Text,String) ]  -- (mod,prim,haskellVar)
    , hsoptFile    :: Maybe FilePath
    }

noOptsHS :: OptsHS
noOptsHS =
  OptsHS
    { hsoptMonad   = Nothing
    , hsoptImports = []
    , hsoptPrims   = []
    , hsoptFile    = Nothing
    }

reqOptHS :: (String -> OptsHS -> Either String OptsHS) ->
            String -> Options -> Either String Options
reqOptHS f s o =
  do o1 <- f s (optHS o)
     pure o { optHS = o1 }


simpleCommand :: Command -> ArgDescr Options
simpleCommand x = NoArg \o -> Right o { optCommand = x }

options :: OptSpec Options
options = OptSpec
  { progDefaults = Options { optCommand   = DumpTC
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
                           , optNoWarnUnbiasedFront = False
                           , optNoWarnUnbiased = False
                           , optHS        = noOptsHS
                           }
  , progOptions =
      [ Option ['s'] ["spec"]
        "Dump specialised type-checked AST"
        $ simpleCommand DumpSpec

      , Option ['t'] ["dump-tc"]
        "Dump type-checked AST"
        $ simpleCommand DumpTC

      , Option [] ["show-types"]
        "List declarations with their types"
        $ simpleCommand DumpTypes

      , Option ['r'] ["dump-raw"]
        "Dump parsed AST"
        $ simpleCommand DumpRaw

      , Option [] ["dump-core"]
        "Dump core AST"
       $ simpleCommand DumpCore

      , Option [] ["dump-vm"]
        "Dump VM AST"
       $ simpleCommand DumpVM

      , Option [] ["dump-vm-graph"]
        "Dump VM AST"
       $ OptArg "FUN|BLOCK" \s o ->
         Right o { optCommand = DumpGraph (s == Just "FUN") }

      , Option [] ["dump-gen"]
        "Dump parser-generator automaton-based parser"
       $ simpleCommand DumpGen

      , Option ['i'] ["interp"]
        "Parse this file"
        $ ReqArg "FILE" \s o -> Right o { optCommand = Interp (Just s) }

      , Option [] ["run"]
        "Run a parser with empty input"
        $ NoArg \o -> Right o { optCommand = Interp Nothing }

      , Option [] ["json"]
        "Show semantics values as JSON."
        $ NoArg \o -> Right o { optShowJS = True }

      , Option [] ["html"]
        "Show semantics values as HTML."
        $ NoArg \o -> Right o { optShowJS = True, optShowHTML = True }

      , Option [] ["json-to-html"]
        "Render externally produced JSON as HTML."
        $ NoArg \o -> Right o { optCommand = JStoHTML }

      , Option ['g'] ["gen"]
        "Use parser-generator backend when interpreting"
        $ NoArg \o -> Right o { optBackend = UsePGen False}

      , Option [] ["core"]
        "Use the Core interpreter"
        $ NoArg \o -> Right o { optBackend = UseCore }

      , Option [] ["no-core-check"]
        "Do not validate Core"
        $ NoArg \o -> Right o { optCheckCore = False }

      , Option [] ["gen-metrics"]
        "Use parser-generator backend when interpreting and print metrics"
        $ NoArg \o -> Right o { optBackend = UsePGen True}

      , Option [] ["no-utf8"]
        "Use the locale settings instead of using UTF8 for output."
       $ NoArg \o -> Right o { optForceUTF8 = False }

      , Option [] ["compile-hs"]
        "Generate Haskell code."
        $ NoArg \o -> Right o { optCommand = CompileHS }

      , Option [] ["hs-config"]
        "Configuraiton file to use for Haskell compilation"
        $ ReqArg "FILE"
        $ reqOptHS \s o ->
          Right o { hsoptFile = Just s }

      , Option [] ["hs-monad"]
        "Use this parser monad (default `Parser`)."
        $ ReqArg "[QUAL.]NAME"
        $ reqOptHS \s o -> Right o { hsoptMonad = Just s }

      , Option [] ["hs-import"]
        "Add this import to all generated Haskell modules."
        $ ReqArg "MODULE[:QUAL]"
        $ reqOptHS \s o ->
          Right o { hsoptImports = colonSplit s : hsoptImports o }

      , Option [] ["hs-prim"]
        "Define an external Haskell primitive."
        $ ReqArg "MODULE:PRIM_NAME:EXTERNAL_NAME"
        $ reqOptHS \s o ->
          case colonSplitText s of
            (m,Just rest) | (n,Just p) <- colonSplitText rest ->
                Right o { hsoptPrims = (m, n, p) : hsoptPrims o }
            _ -> Left
              "Invalid primitve, expected: MODULE:PRIM_NAME:EXTERNAL_NAME"

      , Option [] ["compile-c++"]
        "Generate C++ code"
        $ NoArg \o -> Right o { optCommand = CompileCPP }

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

      , Option [] ["determinize"]
        "Determinize core"
        $ NoArg \o -> Right o { optDeterminize = True }

      , Option [] ["entry"]
        "Generate a library containg this parser."
        $ ReqArg "[MODULE.]NAME"
          \s o -> Right o { optEntries = s : optEntries o }

      , Option [] ["rule-ranges"]
        "Output the file ranges of all rules in the input"
        $ NoArg \o -> Right o { optCommand = DumpRuleRanges }

      , Option [] ["out-dir"]
        "Save output in this directory."
        $ ReqArg "DIR" \s o -> Right o { optOutDir = Just s }

      , Option [] ["no-warn-unbiased"]
        "Do not warn about uses of unbiased choice."
        $ NoArg \s -> Right s { optNoWarnUnbiased = True }

      , Option ['h'] ["help"]
        "Show this help"
        $ simpleCommand ShowHelp
      ]

  , progParamDocs =
      [ ("FILE", "The DDL specification to process.") ]

  , progParams = \s o -> Right o { optParserDDL = s }
  }

colonSplit :: String -> (String, Maybe String)
colonSplit a =
  case break (== ':') a of
    (xs,ys) -> (xs, case ys of
                      _ : more -> Just more
                      _        -> Nothing)

colonSplitText :: String -> (Text, Maybe String)
colonSplitText a = (fromString x, xs)
  where (x,xs) = colonSplit a



getOptions :: IO Options
getOptions =
  do opts <- getOpts options
     case optCommand opts of
       ShowHelp -> dumpUsage options >> exitSuccess
       JStoHTML -> pure (impliedOptions opts)
       _ | let file = optParserDDL opts
         , takeExtension file == ".test" -> getOptionsFromFile file
         | otherwise ->
            do when (null (optParserDDL opts)) (dumpUsage options)
               pure (impliedOptions opts)

getOptionsFromFile :: FilePath -> IO Options
getOptionsFromFile file =
  do inp <- readFile file
     case getOptsFrom options (lines inp) of
       Left err   -> throwIO err
       Right opts -> pure (impliedOptions opts)

throwOptError :: [String] -> IO a
throwOptError err = throwIO (GetOptException err)

impliedOptions :: Options -> Options
impliedOptions opts0 =
  case optBackend opts of
    UseCore -> noTCUnbiased
    _ ->
      case optCommand opts of
        DumpRaw         -> opts
        DumpTC          -> opts
        DumpTypes       -> opts
        DumpSpec        -> opts
        DumpRuleRanges  -> opts
        DumpCore        -> noTCUnbiased
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

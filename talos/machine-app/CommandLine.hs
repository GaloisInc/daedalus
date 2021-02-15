module CommandLine ( Options(..)
                   , Outfile(..)
                   , Mode(..)
                   , getOptions
                   ) where

import Options.Applicative

data Outfile = AllOutput FilePath | PatOutput FilePath

data Mode = SynthesisMode | SummaryMode

data Options =
  Options { optSolver    :: FilePath
          -- Logging
          , optLogOutput :: Maybe FilePath
          , optLogLevel  :: Maybe Int
          -- Model generation
          , optNModels   :: Int
          , optSeed      :: Maybe Int
          , optInfile    :: Maybe FilePath
          , optOutfile   :: Maybe Outfile
          , optPrettyModel :: Bool
          -- Prover options
          , optValidateModel :: Bool
          -- Debugging options
          , optMode :: Mode
          , optDDLInput  :: FilePath
          }

solverOpt = strOption
   ( long "solver"
  <> short 's'
  <> metavar "FILE"
  <> value "z3"
  <> showDefault
  <> help "Path to z3" )

logfileOpt = strOption
   ( long "logfile"
  <> short 'L'
  <> metavar "FILE"
  <> help "Write log to FILE (default stdout)" )

inFileOpt = strOption
  ( long "input"
 <> short 'i'
 <> metavar "FILE"
 <> help "Read concrete input from FILE")

loglevelOpt = option auto
   ( long "log"
  <> short 'l'
  <> metavar "INT"
  <> help "Log verbosity (default don't log)" )

validateModelFlag = switch
  ( long "validate"
  <> short 'V'
  <> help "Tell solver to validate models" )

nModelsOpt = option auto
   ( long "nmodels"
  <> short 'n'
  <> metavar "INT"
  <> value 1
  <> showDefault
  <> help "Number of models to produce." )

seedOpt = option auto
   ( long "seed"
  <> short 'S'
  <> metavar "INT"
  <> help "Initial random seed" )

optPrettyOutput = switch
  ( long "pretty"
  <> help "Pretty-print model bytes and value to stdout"
  )

allOutputOpt = strOption
   ( long "all-output"
  <> short 'o'
  <> metavar "FILE"
  <> help "A file to write all models into (default: stdout)." )

patOutputOpt = strOption
   ( long "output"
  <> short 'O'
  <> metavar "FILE-PATTERN"
  <> help "A collection of files to write models to.  The first '%' in the file name is replaced by the model number.  If there is no '%', an implicit '.%' is added to the end" )

modeOpt = flag SynthesisMode SummaryMode ( long "summary" <> help "Print out analysis results")

options :: Parser Options
options = Options <$> solverOpt
                  <*> optional logfileOpt
                  <*> optional loglevelOpt
                  <*> nModelsOpt
                  <*> optional seedOpt
                  <*> optional inFileOpt
                  <*> optional ((AllOutput <$> allOutputOpt) <|> (PatOutput <$> patOutputOpt))
                  <*> optPrettyOutput
                  <*> validateModelFlag
                  <*> modeOpt
                  <*> argument str (metavar "FILE")

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> header "talos - a daedalus-based input synthesiser." )

getOptions :: IO Options
getOptions = execParser opts

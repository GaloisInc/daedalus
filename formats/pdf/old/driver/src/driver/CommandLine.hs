
module CommandLine ( Options(..)
                   , RunMode(..)
                   , RunOps(..)
                   , getOptions
                   ) where

import Options.Applicative


data RunMode = FAW | Demo

data RunOps = Validate | ExtractText

data Options =
  Options { optPDFInput :: FilePath
          , optOutput   :: FilePath
          , optMode     :: RunMode
          , optOps      :: RunOps
          , optTextOutput :: FilePath
          , optPassword :: String
          }

outputOpt :: Parser [Char]
outputOpt = strOption
   ( long "output"
  <> short 'o'
  <> metavar "FILE"
  <> value "-"
  <> help "Write output to FILE (- for stdout)" )

modeOpt :: Parser RunMode
modeOpt = flag Demo FAW
  ( long "faw"
 <> short 'f'
 <> help "Enable debug (non-demo)  mode" )

opsOpt :: Parser RunOps
opsOpt = flag Validate ExtractText
  ( long "text"
 <> short 't'
 <> help "Extract text from PDF" )

textOutputOpt :: Parser [Char]
textOutputOpt = strOption
   ( long "text-output"
  <> short 'x'
  <> value ""
  <> metavar "FILE"
  <> value "-"
  <> help "Write extracted text to FILE (- for stdout)" )

passwordOpt :: Parser [Char]
passwordOpt = strOption
   ( long "pwd"
  <> short 'p'
  <> value ""
  <> metavar "PASSWORD")

options :: Parser Options
options = Options <$> argument str (metavar "FILE")
                  <*> outputOpt
                  <*> modeOpt
                  <*> opsOpt
                  <*> textOutputOpt
                  <*> passwordOpt

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "PDF driver"
  <> header "pdf-driver - a daedalus-based PDF parser." )

getOptions :: IO Options
getOptions = execParser opts

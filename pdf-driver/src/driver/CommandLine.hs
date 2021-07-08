
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
          , optPassword :: String 
          }

outputOpt = strOption
   ( long "output"
  <> short 'o'
  <> metavar "FILE"
  <> value "-"
  <> help "Write output to FILE (- for stdout)" )

modeOpt = flag Demo FAW
  ( long "faw"
 <> short 'f'
 <> help "Enable debug (non-demo)  mode" )

opsOpt = flag Validate ExtractText
  ( long "text"
 <> short 't'
 <> help "Extract text from PDF" )

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
                  <*> passwordOpt 
          
opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "PDF driver"
  <> header "pdf-driver - a daedalus-based PDF parser." )

getOptions :: IO Options
getOptions = execParser opts

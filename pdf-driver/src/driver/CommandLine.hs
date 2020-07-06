
module CommandLine ( Options(..)
                   , RunMode(..)
                   , getOptions
                   ) where

import Options.Applicative


data RunMode = FAW | Demo

data Options =
  Options { optPDFInput :: FilePath
          , optOutput   :: FilePath
          , optMode     :: RunMode
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

options :: Parser Options
options = Options <$> argument str (metavar "FILE")
                  <*> outputOpt
                  <*> modeOpt
          
opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "PDF driver"
  <> header "pdf-driver - a daedalus-based PDF parser." )

getOptions :: IO Options
getOptions = execParser opts

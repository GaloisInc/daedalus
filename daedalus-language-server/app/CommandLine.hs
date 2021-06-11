

module CommandLine ( ProgramOptions(..)
                   , getOptions
                   ) where

import Options.Applicative

type ProgramOptions = ()

options :: Parser ProgramOptions
options = pure ()

opts :: ParserInfo ProgramOptions
opts = info (options <**> helper)
  ( fullDesc
  <> header "daedalus-language-server - a language-server-protocol server for daedalus"
  )

getOptions :: IO ProgramOptions
getOptions = execParser opts

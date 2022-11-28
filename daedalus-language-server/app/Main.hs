{-# LANGUAGE LambdaCase            #-}

-- Based upon lsp/lsp/example/Reactor.hs from the lsp package

{- |
This is an example language server built with haskell-lsp using a 'Reactor'
design. With a 'Reactor' all requests are handled on a /single thread/.
A thread is spun up for it, which repeatedly reads from a 'TChan' of
'ReactorInput's.
The `lsp` handlers then simply pass on all the requests and
notifications onto the channel via 'ReactorInput's.
This way there is the option of executing requests on multiple threads, without
blocking server communication.

To try out this server, install it with
> cabal install lsp-demo-reactor-server -fdemo
and plug it into your client of choice.
-}
module Main (main) where

import System.Exit
import CommandLine

import Daedalus.LSP.Server (run)

main :: IO ()
main = do
  _ <- getOptions
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

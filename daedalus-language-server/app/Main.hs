{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

import qualified Data.Text as Text
import Data.Text (Text)

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                     as E
import           Control.Lens hiding (Iso)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Reader
import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import           GHC.Generics (Generic)
import           Language.LSP.Server

import qualified Language.LSP.Types.Lens       as J
import           Language.LSP.VFS
import           System.Exit
import           System.Log.Logger
import           Control.Concurrent

import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Map (Map)
import qualified Data.Map as Map


import Daedalus.Parser.Monad (ParseError(..))
import Daedalus.Parser (parseFromText)

import Daedalus.Scope (resolveModule, ScopeError(..))

import Daedalus.Type.Monad (TypeError(..), runMTypeM)
import Daedalus.Module (ModuleException(..))
import Daedalus.Driver
import Daedalus.AST (Located(..), ModuleName, Module)
import Daedalus.Type.AST (TCModule)

import Daedalus.GUID
import Daedalus.Pass
import Daedalus.PP
import Daedalus.SourceRange

import CommandLine
import Data.Maybe (fromMaybe)
import Daedalus.Type (inferRules)

import Daedalus.LSP.Monad
import Daedalus.LSP.Server (run)

-- ----------------------------------------------------------------------------------------
-- Server monad
  
-- ---------------------------------------------------------------------

main :: IO ()
main = do
  _ <- getOptions
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c



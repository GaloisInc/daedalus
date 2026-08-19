{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Exit
import Daedalus.LSP.Server (run)

main :: IO ()
main =
  run >>= \case
    0 -> exitSuccess
    c -> exitWith (ExitFailure c)

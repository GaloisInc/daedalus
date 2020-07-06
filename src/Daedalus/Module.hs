{-# Language OverloadedStrings #-}
-- This file interfaces between the file system and the module sytem
module Daedalus.Module
  ( ModuleException(..)
  , resolveModulePath
  , pathToModuleName
  ) where

import Control.Exception(Exception)
import System.FilePath.Posix
import qualified Data.Text as Text

import Daedalus.PP hiding ((<.>))
import Daedalus.AST


data ModuleException = ImportLoop ModuleName | MissingModule ModuleName
  deriving Show

instance Exception ModuleException

instance PP ModuleException where
  pp x = case x of
           ImportLoop m ->
              "Recursive module dependency. See module" <+> backticks (pp m)
           MissingModule m ->
              "Missing module" <+> backticks (pp m)

-- FIXME
resolveModulePath :: [FilePath] -> ModuleName -> IO (Maybe FilePath)
resolveModulePath [] _n = return Nothing
resolveModulePath (p : _searchPaths) n =
                          return (Just (p </> Text.unpack n <.> "ddl"))

pathToModuleName :: FilePath -> ([FilePath], ModuleName)
pathToModuleName f = ([dir], Text.pack (dropExtension rest))
  where
    (dir, rest) = splitFileName f


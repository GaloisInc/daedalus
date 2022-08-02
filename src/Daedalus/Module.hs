{-# Language OverloadedStrings #-}
-- This file interfaces between the file system and the module sytem
module Daedalus.Module
  ( ModuleException(..)
  , resolveModulePath
  , pathToModuleName
  ) where

import Control.Exception(Exception)
import System.FilePath.Posix
import System.Directory

import qualified Data.Text as Text

import Daedalus.PP hiding ((<.>))
import Daedalus.AST


data ModuleException =
    ImportLoop ModuleName
  | MissingModule [FilePath] ModuleName
  deriving Show

instance Exception ModuleException

instance PP ModuleException where
  pp x = case x of
           ImportLoop m ->
              "Recursive module dependency. See module" <+> backticks (pp m)
           MissingModule fs m ->
              vcat [ "Missing module" <+> backticks (pp m)
                   , "Searched paths:"
                   , nest 2 $ vcat [ "*" <+> text f | f <- fs ]
                   ]

resolveModulePath :: [FilePath] -> ModuleName -> IO (Maybe FilePath)
resolveModulePath [] _n = return Nothing
resolveModulePath (p : searchPaths) n =
  attempt "ddl" $ attempt "md" $ resolveModulePath searchPaths n
  where
  root = p </> Text.unpack n
  attempt ext k =
    do let p' = root </> ext
       yes <- doesFileExist p'
       if yes then pure (Just p') else k
    

pathToModuleName :: FilePath -> (FilePath, ModuleName)
pathToModuleName f = (dir, Text.pack (dropExtension rest))
  where
    (dir, rest) = splitFileName f


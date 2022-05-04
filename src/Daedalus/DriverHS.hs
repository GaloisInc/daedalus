-- | Functions for saving and generating Haskell
module Daedalus.DriverHS
  ( module Daedalus.DriverHS
  , module Daedalus.Compile.Config
  ) where

import System.FilePath((</>),addExtension)
import System.Directory(createDirectoryIfMissing,doesFileExist)
import qualified System.IO as IO

import Daedalus.PP(pp)

import Daedalus.AST(ModuleName)
import Daedalus.Compile.Config
import qualified Daedalus.Compile.LangHS as HS
import Daedalus.CompileHS(hsModule)
import Daedalus.Driver


-- | Save Haskell for the given module.
-- Assumes that the module is in one of the `astTC` phases.
-- Does not do the dependencies.
-- Does not affect the state.
saveHS ::
  Maybe FilePath {- ^ Directory to save things in. -} ->
  CompilerCfg ->
  ModuleName ->
  Daedalus ()
saveHS = saveHSCustomWriteFile writeFile


-- | Save Haskell for the given module.: parameterized over writeFile
saveHSCustomWriteFile ::
  (FilePath -> String -> IO ()) ->
  Maybe FilePath {- ^ Directory to save things in. -} ->
  CompilerCfg ->
  ModuleName ->
  Daedalus ()
saveHSCustomWriteFile writeFile' mb cfg' m =
  do ast <- ddlGetAST m astTC
     tdefs <- ddlGet declaredTypes
     cfg <- getHaskellConfigFor cfg' m

     let hs = hsModule cfg tdefs ast
     case mb of
       Nothing  -> ddlPrint (pp hs)
       Just dir ->
         ddlIO
         do createDirectoryIfMissing True dir
            let file = addExtension (dir </> HS.hsModName hs) "hs"
            writeFile' file (show (pp hs))

getHaskellConfigFor :: CompilerCfg -> ModuleName -> Daedalus CompilerCfg
getHaskellConfigFor cfg' m =
  do srcFile <- ddlGet (moduleSourcePath m)
     case srcFile of
       Nothing -> pure cfg'
       Just f ->
         ddlIO
         do let cfgFile = addExtension f ".hs"
            yes <- doesFileExist cfgFile
            if yes then processFile =<< readFile cfgFile else pure cfg'

  where
  processFile txt =
    do print txt
       pure cfg'


writeOnlyIfChanged :: FilePath -> String -> IO ()
writeOnlyIfChanged file cont =
  do have <- doesFileExist file
     if not have
       then writeFile file cont
       else do h <- IO.openFile file IO.ReadMode
               s <- IO.hGetContents h
               if s == cont
                 then IO.hClose h
                 else do IO.hClose h
                         writeFile file cont



-- | Functions for saving and generating Haskell
module Daedalus.DriverHS
  ( module Daedalus.DriverHS
  , module Daedalus.Compile.Config
  ) where

import System.FilePath((</>),addExtension)
import System.Directory(createDirectoryIfMissing,doesFileExist)
import qualified System.IO as IO
import Text.PrettyPrint(Doc)

import Daedalus.PP(pp)

import qualified RTS.ParseError as RTS
import qualified RTS.Annot as RTS
import Daedalus.AST(ModuleName)
import Daedalus.Compile.Config
import qualified Daedalus.Compile.LangHS as HS
import Daedalus.CompileHS(hsModule)
import Daedalus.Driver

ppParseError :: RTS.ParseErrorG RTS.Annotation -> Doc
ppParseError = RTS.ppParseError


-- | Save Haskell for the given module.
-- Assumes that the module is in one of the `astTC` phases.
-- Does not do the dependencies.
-- Does not affect the state.
saveHS ::
  Maybe FilePath {- ^ Directory to save things in. -} ->
  (ModuleName -> CompilerCfg) ->
  ModuleName ->
  Daedalus ()
saveHS = saveHSCustomWriteFile writeFile


-- | Save Haskell for the given module.: parameterized over writeFile
saveHSCustomWriteFile ::
  (FilePath -> String -> IO ()) ->
  Maybe FilePath {- ^ Directory to save things in. -} ->
  (ModuleName -> CompilerCfg) ->
  ModuleName ->
  Daedalus ()
saveHSCustomWriteFile writeFile' mb cfg' m =
  do ast <- ddlGetAST m astTC
     tdefs <- ddlGet declaredTypes
     let cfg = cfg' m

     let hs = hsModule cfg tdefs ast
     case mb of
       Nothing  -> ddlPrint (pp hs)
       Just dir ->
         ddlIO
         do createDirectoryIfMissing True dir
            let file = addExtension (dir </> HS.hsModName hs) "hs"
            writeFile' file (show (pp hs))

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



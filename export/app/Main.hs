module Main(main) where


import Data.Text qualified as Text
import Data.Set qualified as Set
import System.FilePath(takeBaseName)
import System.IO(hPrint,stderr)
import System.Exit(exitFailure)

import Daedalus.Driver qualified as Daedalus

import Daedalus.PP(pp)
import Daedalus.Core qualified as Core
import SimpleGetOpt
import Options
import Name
import Parser
import Renamer qualified
import Check
import AST


main :: IO ()
main =
  do
    opts <- getOpts defaultOptions options
    spec <-
      case optExportFile opts of
        Nothing -> reportUsageError options ["Missing export specification."]
        Just f  ->
          do
            mb <- parseFromFile f moduleParser
{-
              do
                -- unless (null (optSearchPathForDDL opts))
                --  (ddlSetOpt optSearchPath (reverse (optSearchPathForDDL opts))) 
  -}              
            case mb of
              Left err -> hPrint stderr (pp err) >> exitFailure
              Right a  -> pure (a (nameFromText (Text.pack (takeBaseName f))))
    ddlTys <- Daedalus.daedalus (getTypeDecls <$> loadDaedalus (moduleRoots spec))

    case Renamer.checkModule ddlTys [] spec of
      Left err -> hPrint stderr (pp err) >> exitFailure
      Right a -> print (pp a)


loadDaedalus :: [Roots] -> Daedalus.Daedalus Core.Module
loadDaedalus ents =
  do
    let toText = nameToText . locThing
    let ms = Set.toList (Set.fromList (map (toText . rootModule) ents))
    mapM_ Daedalus.ddlLoadModule ms
    let entries =
          [ (toText (rootModule e), toText x) | e <- ents, x <- rootNames e ]
    let specMod = "DaedalusMain"
    Daedalus.passSpecialize specMod entries
    Daedalus.passCore specMod
    Daedalus.ddlGetAST specMod Daedalus.astCore

-- | Get the type declarations, indexed by original module name.
getTypeDecls :: Core.Module -> [(Name,Name,Core.TDecl)]
getTypeDecls m =
  [ (nameFromText mo, nameFromText (Core.tnameText nm), d)
  | r <- Core.mTypes m,
    d <- Core.recToList r,
    let nm = Core.tName d
        mo = Core.mNameText (Core.tnameMod nm)
  ]
  

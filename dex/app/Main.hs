module Main(main) where


import Data.Text qualified as Text
import Data.Set qualified as Set
import Data.Map qualified as Map
import System.FilePath(takeBaseName)
import System.IO(hPrint,stderr)
import System.Exit(exitFailure)

import Daedalus.Driver qualified as Daedalus

import Daedalus.PP
import Daedalus.Core qualified as Core
import SimpleGetOpt
import Options
import Name
import Parser
import Check
import AST
import ExportCPP

main :: IO ()
main =
  do
    opts <- getOpts defaultOptions options
    case optExportFile opts of
      Nothing -> reportUsageError options ["Missing export specification."]
      Just f  ->
        do
          spec <- loadSpec f
          let ?ftAliases = Map.fromList [ (QName { qModule = moduleName spec, qName = locThing (ftName d) }, d) | d <- moduleForeignTypes spec ]
              ?nsUser = "User"
              ?nsInputType = "INPUT"
              ?nsExternal = mempty
              ?ddlTPMap = mempty
          print (genModule spec)
    

        
loadSpec :: FilePath -> IO (Module DDLTCon QName)
loadSpec f =
  do
    mb <- parseFromFile f moduleParser
    spec <-
      case mb of
        Left err -> hPrint stderr (pp err) >> exitFailure
        Right a  -> pure (a (nameFromText (Text.pack (takeBaseName f))))
    ddlTys <-
      Daedalus.daedalus (getTypeDecls <$> loadDaedalus (moduleRoots spec))
    let res = runValidator ddlTys (checkModule spec)
    case res of
      Left err -> hPrint stderr (pp err) >> exitFailure
      Right a  -> pure a

{-
XXX:
              do
                -- unless (null (optSearchPathForDDL opts))
                --  (ddlSetOpt optSearchPath (reverse (optSearchPathForDDL opts))) 
  -}              

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
getTypeDecls :: Core.Module -> DDLTypes
getTypeDecls mo =
  DDLTypes {
    ddlTypesByTName =
      Map.fromList [ (Core.tName d, d) | (_,_,d) <- ds ],
    ddlTypesByName =
      Map.fromListWith (++) [ (x, [(m,t)]) | (m,x,t) <- ds ],
    ddlTypesByModule =
      Map.fromListWith Map.union [ (m, Map.singleton x t) | (m,x,t) <- ds ]
  }
  where
  ds = 
    [ (nameFromText modu, nameFromText (Core.tnameText nm), d)
    | r <- Core.mTypes mo,
      d <- Core.recToList r,
      let nm = Core.tName d
          modu = Core.mNameText (Core.tnameMod nm)
    ]
  

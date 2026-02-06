module Main(main) where


import Data.Text qualified as Text
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Map qualified as Map
import System.FilePath(takeBaseName,(</>),hasExtension,replaceExtension)
import System.IO(hPutStrLn,stderr)
import System.Directory(doesFileExist)
import System.Exit(exitFailure)
import Control.Monad(foldM,unless)
import Control.Applicative((<|>))

import AlexTools

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
    mos <- loadSpec opts
    let tyAliasesFrom m =
          Map.fromList
            [ (QName {
                qModule = moduleName m,
                qName = locThing (ftName d)
              }, d)
            | d <- moduleForeignTypes m ]
    let ?ftAliases = Map.unions (map tyAliasesFrom mos)
        ?nsUser = "User"
        ?nsInputType = "INPUT"
        ?nsExternal = mempty
        ?ddlTPMap = mempty

    let saveOut ext out =
          let name = fmap (`replaceExtension` ext)
                      (optOutputFile opts <|> optExportFile opts)
          in case name of
               Nothing -> print out
               Just f -> writeFile f (show out)
        (header,impl) = genModules mos
    saveOut ".h" header
    saveOut ".cpp" impl

abortWith :: String -> IO a
abortWith x = hPutStrLn stderr x >> exitFailure


data ParsingState = ParsingState {
  parsedModules :: [Module PName PName],
  parsedFiles   :: Set FilePath
}

findModuleFile :: Options -> Maybe SourceRange -> String -> IO FilePath
findModuleFile opts mbLoc mo = search searchPaths
  where
  searchPaths =
    case optSearchPathForDex opts of
      [] -> ["."]
      p  -> reverse p

  file = mo ++ ".dex"

  search paths =
    case paths of
      [] ->
        do
          let loc =
                case mbLoc of
                  Nothing -> ""
                  Just r -> prettySourcePosLong (sourceFrom r) ++ ": "
          abortWith (loc ++ "Cannot find file for module " ++ show mo)
        
      path : more ->
        do
          let candidate = path </> file
          yes <- doesFileExist candidate
          if yes then pure candidate else search more

parseDexFile ::
  Options -> ParsingState -> FilePath -> IO ParsingState
parseDexFile opts state f
  | f `Set.member` parsedFiles state = pure state
  | otherwise =
    do
      mb <- parseFromFile f moduleParser
      case mb of
        Left err -> abortWith (show (pp err))
        Right moF  ->
          do
            let mo = moF (nameFromText (Text.pack (takeBaseName f)))
                newState =
                  state {
                    parsedFiles = Set.insert f (parsedFiles state),
                    parsedModules = mo : parsedModules state
                  }
            foldM parseModule newState (moduleUsing mo)
  where
  parseModule s m =
    do
      file <- findModuleFile opts (Just (locRange m))
                                  (Text.unpack (nameToText (locThing m)))
      parseDexFile opts s file

loadSpec :: Options -> IO [Module DDLTCon QName]
loadSpec opts =
  case optExportFile opts of
    Nothing -> reportUsageError options ["Missing export specification."]
    Just specName ->
      do
        file <-
          if hasExtension specName
            then
              do
                yes <- doesFileExist specName
                unless yes (abortWith ("File " ++ show specName ++ " does not exist."))
                pure specName
            else
              findModuleFile opts Nothing specName

        mos <- parsedModules <$>
                parseDexFile opts
                  ParsingState { parsedFiles = mempty, parsedModules = [] }
                  file

        ddlTys <-
          Daedalus.daedalus
            do
              unless (null (optSearchPathForDDL opts))
                (Daedalus.ddlSetOpt Daedalus.optSearchPath
                    (reverse (optSearchPathForDDL opts)))           
              getTypeDecls <$> loadDaedalus (concatMap moduleRoots mos)
        let res = runValidator ddlTys (mapM checkModule mos)
        case res of
          Left err -> abortWith (show (pp err))
          Right a  -> pure a


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
  

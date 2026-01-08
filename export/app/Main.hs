module Main where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Map(Map)
import Data.Map qualified as Map
import System.IO(hPrint,stderr)
import System.Exit(exitFailure)

import Daedalus.Driver

import Daedalus.PP(pp)
import Daedalus.Core
import SimpleGetOpt
import Options
import Parser
import Lexer


main :: IO ()
main =
  do
    opts <- getOpts defaultOptions options
    tys <-
      case ddlFile opts of
        Nothing -> pure mempty
        Just f  -> getTypeDecls <$> loadDaedalus f
    spec <-
      case exportFile opts of
        Nothing -> reportUsageError options ["Missing export specification."]
        Just f  ->
          do
            testFromFile f
            mb <- parseFromFile f tys moduleParser
            case mb of
              Left err -> hPrint stderr (pp err) >> exitFailure
              Right a  -> pure a
    print (pp spec)
  
loadDaedalus :: FilePath -> IO Module
loadDaedalus path =
  daedalus
  do
    mm       <- ddlPassFromFile ddlLoadModule path
    _allMods <- ddlBasis mm
    let entries = parseEntries [] mm -- XXX: entries
    let specMod = "DaedalusMain"
    passSpecialize specMod entries
    passCore specMod
    ddlGetAST specMod astCore

--  testFromFile "export/tests/simple.export"

type ModuleName = Text
type Ident = Text

parseEntries :: [String] -> ModuleName -> [(ModuleName,Ident)]
parseEntries ents mm =
  case ents of
    [] -> [(mm,"Main")]
    es -> map (parseEntry mm) es

-- | Notation for entry points.
parseEntry :: ModuleName -> String -> (ModuleName,Ident)
parseEntry mm x =
  case break (== '.') x of
    (as,_:bs) -> (Text.pack as, Text.pack bs)
    _         -> (mm, Text.pack x)

-- | Get the type declarations, indexed by original module name.
getTypeDecls :: Module -> Map ModuleName (Map Ident TDecl)
getTypeDecls m =
  Map.fromListWith Map.union [
    (mo, Map.singleton (tnameText nm) d) |
    r <- mTypes m,
    d <- recToList r,
    let nm = tName d
        mo = mNameText (tnameMod nm)
  ]
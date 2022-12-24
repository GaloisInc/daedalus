{-# Language ImplicitParams, OverloadedStrings, BlockArguments #-}
module Main where

import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import System.Directory
import System.Process
import System.Exit
import SimpleGetOpt
import Cabal.Plan

main :: IO ()
main =
  do opts <- getOpts defaultOptions options
     when (optHelp opts)
       do dumpUsage options
          exitSuccess
     plan <- findAndDecodePlanJson (ProjectRelativeToDir (optProject opts))
     let ?plan = plan
     let us  = chooseTargets (optTargets opts)
         ds  = transDeps us
         gs  = mapMaybe onlyGlobal ds
     case optDownloadPath opts of
       Nothing  -> mapM_ ppU gs
       Just dir -> mapM_ (download dir) gs

onlyGlobal :: Unit -> Maybe Unit
onlyGlobal u =
  case uType u of
    UnitTypeGlobal -> Just u
    _              -> Nothing

ppU :: Unit -> IO ()
ppU u = putStrLn (Text.unpack (dispPkgId (uPId u)))

--------------------------------------------------------------------------------
-- Options

data Options = Options
  { optTargets        :: [Target]
  , optDownloadPath   :: Maybe FilePath
  , optProject        :: FilePath
  , optHelp           :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optTargets      = []
    , optDownloadPath = Nothing
    , optProject      = "."
    , optHelp         = False
    }

options :: OptSpec Options
options =
  optSpec
    { progDescription =
      [ "Identify and download external components for a Cabal project" ]

    , progOptions =
      [ Option [] ["project"]
        "Path where to look for the pre-built Cabal project."
        $ ReqArg "DIR" \s o -> Right o { optProject = s }

      , Option [] ["download"]
        "Download the external components to the given directory"
        $ ReqArg "DIR" \s o -> Right o { optDownloadPath = Just s }

      , Option [] ["help"]
        "Show this help."
        $ NoArg \o -> Right o { optHelp = True }
      ]

      , progParamDocs =
        [ ("COMPONENTS",  "Whose dependencies to get") ]

      , progParams = \p o ->
        do t <- parseTarget p
           pure o { optTargets = t : optTargets o }

    }


--------------------------------------------------------------------------------
-- Targets
data Target = Target (Maybe PkgName) (Maybe Ver) CompName

matches :: Target -> Unit -> Bool
matches (Target nameC verC cmp) u =
  maybeCheck nameC name && maybeCheck verC ver && cmp `Map.member` uComps u
  where
  PkgId name ver = uPId u
  maybeCheck expect actual =
    case expect of
      Nothing -> True
      Just x  -> actual == x

chooseTargets :: (?plan :: PlanJson) => [Target] -> [Unit]
chooseTargets tgts =
  case tgts of
    [] -> map getUnit (Set.toList (planJsonIdRoots ?plan))
    ts -> filter yes (Map.elems (pjUnits ?plan))
       where yes u = any (`matches` u) ts

-- XXX: parse more types of targets, currently we only allow to specify
-- an executable name.
parseTarget :: String -> Either String Target
parseTarget txt
  | pref == "exe:" = pure (Target Nothing Nothing (CompNameExe (Text.pack exe)))
  | otherwise = Left "At present we only support exe:name componenets"
  where
  (pref,exe) = splitAt 4 txt

--------------------------------------------------------------------------------
download :: FilePath -> Unit -> IO ()
download dir u =
  do let pid = uPId u
     createDirectoryIfMissing True dir
     let file = Text.unpack (dispPkgId pid)
     putStrLn file
     callProcess "cabal" ["get",  "--destdir=" ++ dir, file]

--------------------------------------------------------------------------------
getUnit :: (?plan :: PlanJson) => UnitId -> Unit
getUnit uid =
  case Map.lookup uid (pjUnits ?plan) of
    Just u  -> u
    Nothing -> error ("Missing unit: " ++ show uid)


--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

directDeps :: (?plan :: PlanJson) => Unit -> [Unit]
directDeps u =
  [ getUnit uid
  | ci   <- Map.elems (uComps u)
  , uids <- [ ciLibDeps ci, ciExeDeps ci ]
  , uid  <- Set.toList uids
  ]

transDeps :: (?plan :: PlanJson) => [Unit] -> [Unit]
transDeps = go Set.empty []
  where
  go done result todo =
    case todo of
      [] -> result
      u : us
        | uid `Set.member` done -> go done result us
        | otherwise -> go (Set.insert uid done)
                          (u : result)
                          (directDeps u ++ todo)
        where uid = uId u



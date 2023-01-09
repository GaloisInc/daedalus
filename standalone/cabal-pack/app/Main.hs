{-# Language ImplicitParams, OverloadedStrings, BlockArguments #-}
module Main where

import Control.Exception
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad
import System.Directory
import System.FilePath
import System.Process.Typed
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

     -- all packages in the project
     let allPs = Set.toList
               $ Set.fromList
               $ extPacks
               $ Map.elems
               $ pjUnits plan

         us    = chooseTargets (optTargets opts)  -- interesting targets
         ds    = transDeps us                     -- taregt's dependencies
         gs    = extPacks ds                      -- packages for deps
     case optDownloadPath opts of
       Nothing  -> mapM_ ppP gs
       Just dir ->
         do putStrLn "Downloading..."
            createDirectoryIfMissing True dir
            let src = Set.fromList gs
            mapM_ (download src dir) allPs
  where
  extPacks = map uPId . mapMaybe onlyExternal


onlyExternal :: Unit -> Maybe Unit
onlyExternal u =
  case uType u of
    UnitTypeGlobal  -> Just u
    _               -> Nothing

ppP :: PkgId -> IO ()
ppP p = putStrLn (Text.unpack (dispPkgId p))

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
  | pref == "lib"  = pure (Target Nothing Nothing CompNameLib)
  | otherwise = Left "At present we only support exe:name componenets"
  where
  (pref,exe) = splitAt 4 txt

--------------------------------------------------------------------------------

{- Download the source code for a package.  Some packages are *project*
dependencies rather than dependencies of our executable.
For those ones we delete everything but the cabal file, as the source code
is not actually used, but the cabal file is needed for configuration.
-}
download :: Set PkgId -> FilePath -> PkgId -> IO ()
download srcPkg dir0 pid =
  do let file    = Text.unpack (dispPkgId pid)
         needSrc = pid `Set.member` srcPkg
         dir     = if needSrc then dir0 else dir0 </> "cfg"

     createDirectoryIfMissing True dir
     let pkgDir = dir </> file

     exists <- doesDirectoryExist pkgDir

     putStrLn $
        unwords [ if exists then "[EXISTS]" else
                  if needSrc then "[SRC   ]" else "[CFG   ]"
                , file
                ]
     unless exists $
       do let cfg = setStdout nullStream
                  $ setStderr nullStream
                  $ proc "cabal" ["get",  "--destdir=" ++ dir, file]
          runProcess_ cfg
             `catch` \e@SomeException {} -> print e
          unless needSrc
            do fs <- listDirectory pkgDir
               forM_ fs \f ->
                 if takeExtension f == ".cabal"
                   then pure ()
                   else removePathForcibly (pkgDir </> f)

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



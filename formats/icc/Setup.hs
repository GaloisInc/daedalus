{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}

import System.Directory
import System.IO
import System.Posix.Temp(mkstemps)
import Control.Exception

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo

import qualified Data.Map as Map
import qualified Data.Text
import Daedalus.Driver
import Daedalus.DriverHS
import Daedalus.Type.AST
import Daedalus.Compile.LangHS

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ ->
      do putStrLn "Compiling DDL"
         compileDDL
         pure emptyHookedBuildInfo
  }


compileDDL :: IO ()
compileDDL =
  daedalus
  do ddlSetOpt optSearchPath ["spec"]
     let mods = [ "Parser", "Validator" ]
     mapM_ ddlLoadModule mods
     todo <- ddlBasisMany mods
     ddlIO $
       mapM putStrLn $ [ "daedalus generating .hs for these .ddl modules:"
                       ]
                       ++ map (("  " ++) . Data.Text.unpack) todo

     ddlIO $ putStrLn "... but only creating/updating these Haskell files:"
     mapM_ (\m -> saveHSCustomWriteFile writeOnlyIfChanged
                        (Just "src/spec") (const mempty) m) todo
     ddlIO $ putStrLn "... daedalus done"
  `catch` \d -> putStrLn =<< prettyDaedalusError d


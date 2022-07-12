module Daedalus.TH.Compile where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Exception(try)
import Control.Monad.IO.Class(liftIO)
import Language.Haskell.TH as TH

import Daedalus.SourceRange(SourcePos(..))

import qualified Daedalus.VM as VM
import qualified Daedalus.VM.Backend.Haskell as VM
import qualified Daedalus.VM.Compile.Decl as VM (moduleToProgram)

import qualified Daedalus.Driver as DDL

data CompileConfing = CompileConfing
  { userMonad :: Maybe TH.TypeQ
  , userPrimitives :: [(Text, [TH.ExpQ] -> TH.ExpQ)]
  }

defaultConfig :: CompileConfing
defaultConfig = CompileConfing
  { userMonad = Nothing
  , userPrimitives = []
  }

compileDDL :: (SourcePos, String, Text) -> TH.DecsQ
compileDDL = compileDDLWith defaultConfig

compileDDLWith :: CompileConfing -> (SourcePos, String, Text) -> TH.DecsQ
compileDDLWith cfg (start, root, txt) =
  do mb <-
        liftIO $ try $ DDL.daedalus
           do ast <- loadDDLVM start root txt
              let getPrim (x,c) =
                    do mb <- DDL.ddlGetFNameMaybe "Main" x
                       case mb of
                         Nothing -> DDL.ddlThrow
                            (DDL.ADriverError ("Unknown primitive: " <> show x))
                         Just f  -> pure (f,c)
              primMap <- Map.fromList <$> mapM getPrim (userPrimitives cfg)
              pure (ast,primMap)

     (ast,primMap) <- case mb of
                        Left e  -> fail =<< liftIO (DDL.prettyDaedalusError e)
                        Right a -> pure a

     let c = VM.defaultConfig { VM.userMonad = userMonad cfg
                              , VM.userPrimitives = primMap
                              }


     VM.compileModule c ast

loadDDLVM :: SourcePos -> String -> Text -> DDL.Daedalus VM.Module
loadDDLVM loc root txt =
  do let mo = "Main"
     DDL.parseModuleFromText mo loc txt
     DDL.ddlLoadModule mo

     let specMod = "MainCore"
     DDL.passSpecialize specMod [(mo, Text.pack root)]
     DDL.passCore specMod
     DDL.passDeterminize specMod
     DDL.passNorm specMod
     DDL.passVM specMod
     m <- DDL.ddlGetAST specMod DDL.astVM

     pure $ head $ VM.pModules $ VM.moduleToProgram [m]



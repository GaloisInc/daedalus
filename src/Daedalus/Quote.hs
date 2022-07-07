{-# Language BlockArguments, OverloadedStrings, TemplateHaskell #-}
module Daedalus.Quote
  ( daedalus
  , ddl
  , compileDDL
  , compileDDLWith
  , CompileConfing(..)
  , defaultConfig
  , namedPrim
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.ByteString(ByteString)
import Control.Monad.IO.Class(liftIO)
import Control.Exception(try)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import AlexTools(SourceRange,SourcePos(..))

import RTS.ParserAPI(Result(..),ParseError)

import Daedalus.Value(Value)

import Daedalus.AST(ScopedIdent(..))
import Daedalus.Type.AST(TCModule)
import qualified Daedalus.VM as VM
import qualified Daedalus.VM.Backend.Haskell as VM
import qualified Daedalus.VM.Compile.Decl as VM (moduleToProgram)
import qualified Daedalus.Driver as DDL
import Daedalus.Interp(interp)

daedalus :: QuasiQuoter
daedalus = QuasiQuoter
  { quotePat  = bad "pattern"
  , quoteDec  = doDecl
  , quoteType = bad "type"
  , quoteExp  = bad "expression"
  }
  where bad = nope "daedalus"

ddl :: QuasiQuoter
ddl = QuasiQuoter
  { quotePat  = bad "pattern"
  , quoteDec  = bad "declaration"
  , quoteType = bad "type"
  , quoteExp  = \s -> do (a,b,c) <- getInput s
                         [| (a,b,c) |]
  }
  where bad = nope "ddl"

nope :: String -> String -> String -> Q a
nope th thing = const (fail ("`" ++ th ++ "` may not be used as a " ++ thing))


loadDDL :: SourcePos -> Text -> IO (TCModule SourceRange)
loadDDL loc txt =
  DDL.daedalus
  do let mo = "Main"
     DDL.parseModuleFromText mo loc txt
     DDL.ddlLoadModule mo
     DDL.ddlGetAST mo DDL.astTC

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

getInput :: String -> Q (SourcePos, String, Text)
getInput str =
  do let (root,txt) =
            case break Char.isSpace (dropWhile Char.isSpace str) of
              (r,rest) -> (r, Text.pack (drop 1 rest))

     thloc <- TH.location
     let loc = SourcePos { sourceIndex  = 0
                         , sourceLine   = fst (TH.loc_start thloc) + 1
                         , sourceColumn = snd (TH.loc_start thloc) + 1 +
                                          length root + 1
                         , sourceFile   = Text.pack (TH.loc_filename thloc)
                         }

     pure (loc, root, txt)


doDecl :: String -> Q [Dec]
doDecl str =
  do (start, root, txt) <- getInput str
     mbAST <- liftIO (try (loadDDL start txt))
     ast <- case mbAST of
              Left e -> fail =<< liftIO (DDL.prettyDaedalusError e)
              Right a -> pure a

     e <- [| \b -> case interp [] "Main" b [ast] (ModScope "Main" root) of
                     NoResults err -> Left err
                     Results r     -> Right (NE.head r)
           |]
     let nm = mkName ("p" ++ root)
     t <- [t| ByteString -> Either ParseError Value |]
     pure [ SigD nm t
          , FunD nm [Clause [] (NormalB e) []]
          ]

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

namedPrim :: TH.Name -> [TH.ExpQ] -> TH.ExpQ
namedPrim f is = TH.appsE (TH.varE f : is)


{-# Language BlockArguments, OverloadedStrings, TemplateHaskell #-}
module Daedalus.Quote (daedalus, daedalus_compiled) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import Data.ByteString(ByteString)
import Control.Monad.IO.Class(liftIO)
import Control.Exception(catch)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import AlexTools(SourceRange,SourcePos(..))

import RTS.ParserAPI(Result(..),ParseError)

import Daedalus.Value(Value)
import Daedalus.Panic(panic)

import Daedalus.AST(ScopedIdent(..))
import Daedalus.Type.AST(TCModule)
import qualified Daedalus.Core as Core
import qualified Daedalus.Core.TH as Core
import qualified Daedalus.Driver as DDL
import Daedalus.Interp(interp)

daedalus :: QuasiQuoter
daedalus = QuasiQuoter
  { quotePat  = nope "pattern"
  , quoteDec  = doDecl
  , quoteType = nope "type"
  , quoteExp  = nope "expression"
  }

daedalus_compiled :: QuasiQuoter
daedalus_compiled = QuasiQuoter
  { quotePat  = nope "pattern"
  , quoteDec  = doDecl'
  , quoteType = nope "type"
  , quoteExp  = nope "expression"
  }



nope :: String -> String -> Q a
nope thing = const (fail ("`daedalus` may not be used as a " ++ thing))


loadDDL :: SourcePos -> Text -> IO (TCModule SourceRange)
loadDDL loc txt =
  DDL.daedalus
  do let mo = "Main"
     DDL.parseModuleFromText mo loc txt
     DDL.ddlLoadModule mo
     DDL.ddlGetAST mo DDL.astTC

loadDDLCore :: SourcePos -> String -> Text -> IO Core.Module
loadDDLCore loc root txt =
  DDL.daedalus
  do let mo = "Main"
     DDL.parseModuleFromText mo loc txt
     DDL.ddlLoadModule mo

     let specMod = "MainCore"
     DDL.passSpecialize specMod [(mo, Text.pack root)]
     DDL.passCore specMod
     core <- DDL.ddlGetAST specMod DDL.astCore
     case Core.checkModule core of
       Just err -> panic "Malformed Core" [ show err ]
       Nothing  -> pure ()
     pure core

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
     ast <- liftIO (loadDDL start txt
                      `catch` \e -> fail =<< DDL.prettyDaedalusError e)

     e <- [| \b -> case interp [] "Main" b [ast] (ModScope "Main" root) of
                     NoResults err -> Left err
                     Results r     -> Right (NE.head r)
           |]
     let nm = mkName ("p" ++ root)
     t <- [t| ByteString -> Either ParseError Value |]
     pure [ SigD nm t
          , FunD nm [Clause [] (NormalB e) []]
          ]

doDecl' :: String -> Q [Dec]
doDecl' str =
  do (start, root, txt) <- getInput str
     ast <- liftIO (loadDDLCore start root txt
                      `catch` \e -> fail =<< DDL.prettyDaedalusError e)
     Core.compileModule ast





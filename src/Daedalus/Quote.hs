{-# Language BlockArguments, OverloadedStrings, TemplateHaskell #-}
module Daedalus.Quote (daedalus) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import Data.ByteString(ByteString)
import Control.Monad.IO.Class(liftIO)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import AlexTools(SourceRange)

import RTS.ParserAPI(Result)
import Daedalus.Value(Value)

import Daedalus.AST(ScopedIdent(..))
import Daedalus.Type.AST(TCModule)
import qualified Daedalus.Driver as DDL
import Daedalus.Interp(interp)

daedalus :: QuasiQuoter
daedalus = QuasiQuoter
  { quotePat  = nope "pattern"
  , quoteDec  = doDecl
  , quoteType = nope "type"
  , quoteExp  = nope "expression"
  }

nope :: String -> String -> Q a
nope thing = const (fail ("`daedalus` may not be used as a " ++ thing))


loadDDL :: Text -> IO (String,TCModule SourceRange)
loadDDL txt0 =
  DDL.daedalus
  do let mo = "Main"
     DDL.parseModuleFromText mo txt
     DDL.ddlLoadModule mo
     ast <- DDL.ddlGetAST mo DDL.astTC
     pure (root,ast)
  where
  (root,txt) =
    case Text.break Char.isSpace (Text.dropWhile Char.isSpace txt0) of
      (r,rest) -> (Text.unpack r, Text.tail rest)

doDecl :: String -> Q [Dec]
doDecl str =
  do (root,ast) <- liftIO (loadDDL (Text.pack str))
     mkD ast root

  where
  mkD ast n =
    do e <- [| \b -> interp [] "Main" b [ast] (ModScope "Main" n) |]
       let nm = mkName ("p" ++ n)
       t <- [t| ByteString -> Result Value |]
       pure [ SigD nm t
            , FunD nm [Clause [] (NormalB e) []]
            ]







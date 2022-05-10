{-# Language BlockArguments, OverloadedStrings, TemplateHaskell #-}
module Daedalus.Quote (daedalus) where

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


loadDDL :: SourcePos -> Text -> IO (String,TCModule SourceRange)
loadDDL loc txt0 =
  DDL.daedalus
  do let mo = "Main"
     DDL.parseModuleFromText mo loc' txt
     DDL.ddlLoadModule mo
     ast <- DDL.ddlGetAST mo DDL.astTC
     pure (root,ast)
  where
  (root,txt) =
    case Text.break Char.isSpace (Text.dropWhile Char.isSpace txt0) of
      (r,rest) -> (Text.unpack r, Text.tail rest)
  loc' = loc { sourceColumn = sourceColumn loc + length root + 1 }

doDecl :: String -> Q [Dec]
doDecl str =
  do thloc <- TH.location
     let start = SourcePos { sourceIndex  = 0
                           , sourceLine   = fst (TH.loc_start thloc) + 1
                           , sourceColumn = snd (TH.loc_start thloc) + 1
                           , sourceFile   = Text.pack (TH.loc_filename thloc)
                           }
     (root,ast) <- liftIO $ do loadDDL start (Text.pack str)
                                  `catch` \e -> fail =<< DDL.prettyDaedalusError e
     mkD ast root

  where
  mkD ast n =
    do e <- [| \b -> case interp [] "Main" b [ast] (ModScope "Main" n) of
                       NoResults err -> Left err
                       Results r     -> Right (NE.head r)
             |]
       let nm = mkName ("p" ++ n)
       t <- [t| ByteString -> Either ParseError Value |]
       pure [ SigD nm t
            , FunD nm [Clause [] (NormalB e) []]
            ]







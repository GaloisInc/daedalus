{-# Language BlockArguments, OverloadedStrings, TemplateHaskell #-}
module Daedalus.Quote
  ( daedalus
  , ddl
  , namedPrim
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import Data.ByteString(ByteString)
import Control.Monad.IO.Class(liftIO)
import Control.Exception(try)

import AlexTools(SourceRange,SourcePos(..))

import Daedalus.TH (Q, Dec, QuasiQuoter(..))
import qualified Daedalus.TH as TH
import Daedalus.Value(Value)

import Daedalus.AST(ScopedIdent(..))
import Daedalus.Type.AST(TCModule)
import qualified Daedalus.Driver as DDL
import Daedalus.Interp(interp,ParseError,ResultG(..))
import qualified Daedalus.TH.Compile as DDL(DDLText(..))

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
  , quoteExp  = \s -> do (a,c) <- getInput' s
                         [| DDL.Inline a c |]
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

getInput' :: String -> Q (SourcePos, Text)
getInput' str =
  do thloc <- TH.location
     let loc = SourcePos { sourceIndex  = 0
                         , sourceLine   = fst (TH.loc_start thloc)
                         , sourceColumn = snd (TH.loc_start thloc) + 1
                         , sourceFile   = Text.pack (TH.loc_filename thloc)
                         }

     pure (loc, Text.pack str)



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

     let e = [| \b -> case interp [] "Main" b [ast] (ModScope "Main" root) of
                        NoResults err -> Left err
                        Results r     -> Right (fst (NE.head r))
           |]
     let nm = TH.mkName ("p" ++ root)
     let t = [t| ByteString -> Either ParseError Value |]
     sequence [ TH.sigD nm t
              , TH.funD nm [TH.clause [] (TH.normalB e) []]
              ]

namedPrim :: TH.Name -> [TH.ExpQ] -> TH.ExpQ
namedPrim f is = TH.appsE (TH.varE f : is)


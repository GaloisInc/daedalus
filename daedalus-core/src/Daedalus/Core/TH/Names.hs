module Daedalus.Core.TH.Names
  ( dataName
  , structConName
  , unionConName
  ) where

import Data.Char(isUpper)
import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH

import Daedalus.Panic(panic)
import Daedalus.Core.Basics

typeIdent :: String -> String
typeIdent x =
  case x of
    a : _ -> if isUpper a then x else 'T' : x
    _     -> panic "tdeclIdent" ["Empty name"]

-- XXX: do we need to do something for anon types?
dataName :: TName -> TH.Name
dataName = TH.mkName . typeIdent . Text.unpack . tnameText

structConName :: TName -> TH.Name
structConName = dataName

unionConName :: TName -> Label -> TH.Name
unionConName n l = TH.mkName (base ++ "_" ++ Text.unpack l)
  where
  base = typeIdent (Text.unpack (tnameText n))



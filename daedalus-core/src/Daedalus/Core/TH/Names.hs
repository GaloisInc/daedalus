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

tdeclIdent :: String -> String
tdeclIdent x =
  case x of
    a : _ -> if isUpper a then x else 'T' : x
    _     -> panic "tdeclIdent" ["Empty name"]

dataName :: TName -> TH.Name
dataName = TH.mkName . tdeclIdent . Text.unpack . tnameText

structConName :: TName -> TH.Name
structConName = dataName

unionConName :: TName -> Label -> TH.Name
unionConName = undefined -- XXX



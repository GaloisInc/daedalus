{-# Language BlockArguments #-}
module Daedalus.Core.TH.Names
  ( dataName
  , structConName
  , unionConName
  , fnameName
  ) where

import Data.Char(isUpper)
import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH

import Daedalus.Panic(panic)
import Daedalus.GUID(guidString)
import Daedalus.Core.Basics

typeIdent :: String -> String
typeIdent x =
  case x of
    a : _ -> if isUpper a then x else 'T' : x
    _     -> panic "tdeclIdent" ["Empty name"]

fnameName :: FName -> TH.Name
fnameName f =
  TH.mkName
    if fnamePublic f then root else root ++ "_" ++ guidString (fnameId f)
  where
  str  = Text.unpack (fnameText f)
  root = case str of
           a : as -> if isUpper a then 'p' : str else str
           [] -> panic "fnameName" ["Empty function name"]

-- XXX: do we need to do something for anon types?
dataName :: TName -> TH.Name
dataName = TH.mkName . typeIdent . Text.unpack . tnameText

structConName :: TName -> TH.Name
structConName = dataName

unionConName :: TName -> Label -> TH.Name
unionConName n l = TH.mkName (base ++ "_" ++ Text.unpack l)
  where
  base = typeIdent (Text.unpack (tnameText n))



{-# Language BlockArguments #-}
module Daedalus.Core.TH.Names
  ( dataName
  , structConName
  , unionConName
  , fnameName
  , zEsc
  ) where

import Data.Char(isUpper, isAlphaNum)
import qualified Data.Text as Text

import qualified Daedalus.TH as TH
import Daedalus.Panic(panic)
import Daedalus.GUID(guidString)
import Daedalus.Core.Basics


zEsc :: String -> String
zEsc = concatMap esc
  where
  esc c
    | c == 'z'      = "zz"
    | isAlphaNum c  = [c]
    | otherwise     = "z" ++ show (fromEnum c) ++ "z"

typeIdent :: String -> String
typeIdent x =
  case x of
    a : _ -> if isUpper a then x else 'T' : x
    _     -> panic "tdeclIdent" ["Empty name"]

fnameName :: FName -> TH.Name
fnameName f =
  TH.mkName
    if fnamePublic f then root False
                     else root True ++ "_" ++ guidString (fnameId f)
  where
  str  = Text.unpack (fnameText f)
  root e = case if e then zEsc str else str of
             a : as
               | isUpper a -> 'p' : str
               | '$' == a  -> "c_" ++ as
               | otherwise -> str
             [] -> panic "fnameName" ["Empty function name"]


tnameIdentText :: TName -> String
tnameIdentText nm =
  case tnameAnon nm of
    Nothing -> root
    Just n  -> root ++ "__" ++ show n
  where
  root = typeIdent (Text.unpack (tnameText nm))

dataName :: TName -> TH.Name
dataName = TH.mkName . tnameIdentText

structConName :: TName -> TH.Name
structConName = dataName

unionConName :: TName -> Label -> TH.Name
unionConName n l = TH.mkName (tnameIdentText n ++ "_" ++ Text.unpack l)



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
import qualified Language.Haskell.TH as TH

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

-- XXX: do we need to do something for anon types?
dataName :: TName -> TH.Name
dataName = TH.mkName . typeIdent . Text.unpack . tnameText

structConName :: TName -> TH.Name
structConName = dataName

unionConName :: TName -> Label -> TH.Name
unionConName n l = TH.mkName (base ++ "_" ++ Text.unpack l)
  where
  base = typeIdent (Text.unpack (tnameText n))



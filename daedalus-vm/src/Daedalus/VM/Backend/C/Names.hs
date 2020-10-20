{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Names where

import qualified Data.Text as Text

import Daedalus.PP

import qualified Daedalus.Core as Src
import Daedalus.VM.Backend.C.Lang

-- | Name of a type.
-- XXX: module names, namespaces?
cTName :: Src.TName -> CType
cTName t = case Src.tnameAnon t of
             Nothing -> root
             Just i  -> root <.> int i
  where
  root = text (Text.unpack (Src.tnameText t))


-- | Name of a type parameter.
cTParam :: Src.TParam -> CType
cTParam (Src.TP n) = "T" <.> int n

cLabel :: Src.Label -> Doc
cLabel x = text (Text.unpack x)

cField :: Int -> Doc
cField n = "_" <.> int n


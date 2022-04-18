{-# Language OverloadedStrings #-}
module Daedalus.Core.CheckFork (checkFork) where

import Data.Text(Text)

import Daedalus.Core.Basics
import Daedalus.Core.Grammar
import Daedalus.Core.Decl

checkFork :: Module -> [Text]
checkFork = concatMap checkFun . mGFuns

checkFun :: Fun Grammar -> [Text]
checkFun fun =
  case fDef fun of
    External -> []
    Def g    -> checkGrammar ann g
      where ann = case [ a | SrcAnnot a <- fAnnot fun ] of
                    a : _ -> a
                    []    -> case fnameText (fName fun) of
                               Just t  -> t
                               Nothing -> "?"

checkGrammar :: Text -> Grammar -> [Text]
checkGrammar acc gram =
  case gram of
    OrUnbiased {} -> [acc]
    Annot (SrcAnnot ann) g -> checkGrammar ann g
    _ -> collectChildren (checkGrammar acc) gram


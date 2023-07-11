{-# Language OverloadedStrings #-}
module Daedalus.Core.CheckFork (checkFork) where

import Daedalus.SourceRange

import Daedalus.Core.Basics
import Daedalus.Core.Grammar
import Daedalus.Core.Decl

type Loc = Either FName SourceRange

checkFork :: Module -> [Loc]
checkFork = concatMap checkFun . mGFuns

checkFun :: Fun Grammar -> [Loc]
checkFun fun =
  case fDef fun of
    External _mayFail -> []
    Def g    -> checkGrammar ann g
      where ann = case [ a | SrcRange a <- fAnnot fun ] of
                    a : _ -> Right a
                    []    -> Left (fName fun)

checkGrammar :: Loc -> Grammar -> [Loc]
checkGrammar acc gram =
  case gram of
    OrUnbiased {} -> [acc]
    Annot (SrcRange ann) g -> checkGrammar (Right ann) g
    _ -> collectChildren (checkGrammar acc) gram


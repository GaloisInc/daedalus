{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Lang where

import Text.PrettyPrint as P
import Daedalus.PP

type CExpr = Doc
type CStmt = Doc
type CType = Doc
type CDecl = Doc

inst :: CExpr -> [CExpr] -> CExpr
inst f es = f P.<> "<" <.> (fsep (punctuate comma es)) <.> ">"

call :: CExpr -> [CExpr] -> CExpr
call f es = f P.<> parens (fsep (punctuate comma es))

cString :: String -> CExpr
cString = text . show

cSelect :: CExpr -> CExpr -> CExpr
cSelect x l = x <.> "." <.> l

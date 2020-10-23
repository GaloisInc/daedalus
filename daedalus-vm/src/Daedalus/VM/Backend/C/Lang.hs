{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Lang where

import Text.PrettyPrint as P
import Daedalus.PP

type CExpr = Doc
type CStmt = Doc
type CType = Doc
type CDecl = Doc

cInst :: CExpr -> [CExpr] -> CExpr
cInst f es = f P.<> "<" <.> (fsep (punctuate comma es)) <.> ">"

cCall :: CExpr -> [CExpr] -> CExpr
cCall f es = f P.<> parens (fsep (punctuate comma es))


cString :: String -> CExpr
cString = text . show

cSelect :: CExpr -> CExpr -> CExpr
cSelect x l = x <.> "." <.> l

cStmt :: Doc -> CStmt
cStmt c = c <.> semi

cRefT :: CType -> CType
cRefT t = t <.> "&"

cDefineFun :: Doc -> Doc -> [Doc] -> [Doc] -> Doc
cDefineFun ty name params stmts =
  vcat [ ty <+> cCall name params <+> "{"
       , nest 2 (vcat stmts)
       , "}"
       ]


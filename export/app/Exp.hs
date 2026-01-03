module Exp where

import Data.Text(Text)

type Name   = Text
type Label  = Text
type Con    = Text

data DDLExpr = DDLVar Name | DDLSel Name Label

data ExportExpr =
    ExportDefault DDLExpr
  | ExportWith Exporter DDLExpr

data Exporter =
    ExportUser Name
  | ExportBase BaseType
  | ExportArrToVec ExportExpr
  | ExportArrToString
  -- XXX: Others

data BaseType = BTBool | BTUInt Int | BTSInt Int | BTFloat | BTDouble




data ExportDecl = ExportDecl {
  exportDeclName         :: Name,
  -- XXX: Poly and HO params
  exportDeclVar          :: Name,
  exportDeclDef          :: ExportDeclDef
}

data ExportDeclDef =
    ExportExpr ExportCode
  | ExportCase Name [(Pat, ExportCode)]

data ExportCode = ExportCode {

}

data Pat = Con Con | ConArg Con Name -- XXX: Literal?

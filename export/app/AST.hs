module AST where

import Data.List(intersperse)

import Name(Name)
import Quote

import Daedalus.PP
import Daedalus.Core qualified as Core


newtype Module = Module {
  moduleDecls :: [Decl]
}

data Decl = Decl {
  declName      :: Name,
  declArg       :: Name,
  declArgType   :: Core.Type,
  declResType   :: Q ExportType,
  declDef       :: DeclDef
}

data ExportType = ExportType

data Exporter = ExportDefault | ExportWith Name

data ExportExpr = ExportExpr {
  exportWith :: Exporter,
  exportExpr :: DDLExpr
}

data DDLExpr = DDLVar Name | DDLSelect Name Name

data DeclDef =
    DeclDef (Q ExportExpr)
  | DeclCaseXXX

instance PP Module where
  pp m = vcat (intersperse " " (map pp (moduleDecls m)))

instance PP Decl where
  pp d = vcat [
    "def" <+> pp (declName d) <.> parens (pp (declArg d) <.> ":" <+> pp (declArgType d)) <+> "->",
    nest 2 (pp (declResType d)),
    nest 2 (pp (declDef d))
    ]

instance PP Exporter where
  pp e =
    case e of
      ExportDefault -> "USE_DEFAULT"
      ExportWith f -> pp f

instance PP DDLExpr where
  pp e =
    case e of
      DDLVar x -> pp x
      DDLSelect x l -> pp x <.> pp l

instance PP ExportExpr where
  pp (ExportExpr f x) = pp f <.> parens (pp x)

instance PP DeclDef where
  pp d =
    case d of
      DeclDef f -> "->" $$ nest 2 (pp f)
      DeclCaseXXX -> "XXX"

instance PP ExportType where
  pp ExportType = mempty
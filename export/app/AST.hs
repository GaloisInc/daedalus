module AST where

import Data.Void(Void,vacuous)

import Name(Name)
import Quote
import AlexTools(SourceRange)

import Daedalus.PP
import Daedalus.Core qualified as Core


data LName = LName {
  nameName :: Name,
  nameRange :: SourceRange
}

data Module = Module {
  moduleEntries :: [Entries], -- ^ Roots for Daedalus parsers we are exporting
  moduleForeign :: [Q Void],  -- ^ Arbitrary foreign text (e.g., #include or helper functions)
  moduleDecls   :: [Decl]
}

data Entries = Entries {
  entryModule :: LName,
  entryNames  :: [LName]
}


data Decl = Decl {
  declName      :: LName,
  declArg       :: LName,
  declArgType   :: Core.Type,
  declResType   :: Q ExportType,
  declDef       :: DeclDef
}

data ExportType = ExportType

data Exporter = ExportDefault | ExportWith LName

data ExportExpr = ExportExpr {
  exportWith :: Exporter,
  exportExpr :: DDLExpr
}

data DDLExpr = DDLVar LName | DDLSelect LName LName

data DeclDef =
    DeclDef (Q ExportExpr)
  | DeclCase LName [(Pat, Q ExportExpr)]

data Pat =
  PCon LName (Maybe LName)



--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

instance PP Module where
  pp m =
    vcat
      [ vcat [ "import" <+> pp n | n <- moduleEntries m ]
      , vcat [ pp (vacuous q :: Q Decl) | q <- moduleForeign m ]
      , vcat (map pp (moduleDecls m))
      ]

instance PP Entries where
  pp ent = pp (entryModule ent) <.> parens (commaSep (map pp (entryNames ent)))

instance PP Decl where
  pp d = vcat [
    "def" <+> pp (declName d) <.>
      parens (pp (declArg d) <.> ":" <+> pp (declArgType d)) <+> "->",
    nest 2 (pp (declResType d)),
    nest 2 (pp (declDef d))
    ]

instance PP LName where
  pp = pp . nameName

instance PP Exporter where
  pp e =
    case e of
      ExportDefault -> "USE_DEFAULT"
      ExportWith f -> pp f

instance PP DDLExpr where
  pp e =
    case e of
      DDLVar x -> pp x
      DDLSelect x l -> pp x <.> "." <.> pp l

instance PP ExportExpr where
  pp (ExportExpr f x) = pp f <+> pp x

instance PP DeclDef where
  pp d =
    case d of
      DeclDef f -> "->" $$ nest 2 (pp f)
      DeclCase x alts ->
        vcat [
          "=" <+> "case" <+> pp x <+> "of",
          nest 4 (vcat [ (pp pat <+> "->") $$ nest 2 (pp rhs) | (pat,rhs) <- alts ])
        ]

instance PP Pat where
  pp (PCon c mb) = pp c <+> maybe mempty pp mb

instance PP ExportType where
  pp ExportType = mempty
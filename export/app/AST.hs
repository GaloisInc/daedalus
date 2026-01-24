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
  moduleEntries :: [Entries],
  -- ^ Roots for Daedalus parsers we are exporting

  moduleForeign :: [Q Void],
  -- ^ Arbitrary foreign text (e.g., #include or helper functions)

  moduleForeignTypes :: [ForeignTypeDecl],
  -- ^ Foreign type declarations

  moduleDecls   :: [Decl]
  -- ^ Exporter definitions
  
}
-- | Specifies a Daedalus root
data Entries = Entries {
  entryModule :: LName,
  entryNames  :: [LName]
}

data ForeignTypeDecl = ForeignTypeDecl {
  ftName    :: LName,
  ftParams  :: [LName],
  ftDef     :: Q LName
}

data ForeignType =
    ForeignType LName [ForeignType] -- ^ Parser only produces these
  | ForeignTVar LName   -- ^ Introduced by check

data BasicExporterType = Core.Type :-> ForeignType
data ExporterType = Forall {
  etDDLTypeVars     :: [Core.TParam],
  etForeignTypeVars :: [Name],
  etExporterParams  :: [BasicExporterType],
  etType            :: BasicExporterType
}


data Decl = Decl {
  declDefault         :: Bool,      -- ^ Is this the default exporter for the type
  declName            :: LName,
  declDDLTParams      :: [(LName, Core.TParam)],
  declForeignTParams  :: [LName],
  declFunParams       :: [(LName, BasicExporterType)],
  declArg             :: LName,
  declArgType         :: Core.Type,
  declResType         :: ForeignType,
  declDef             :: DeclDef
}

data Exporter =
    ExportDefault
  | ExportTop LName
  | ExportApp Exporter Exporter

data ExportExpr = ExportExpr {
  exportWith :: Exporter,
  exportExpr :: DDLExpr
}


data DDLExpr      = DDLExpr LName Selectors
type Selectors    = [Selector]
data Selector     = SelectorType :. LName
data SelectorType =
    StructSelector    -- ^ Select from a struct.  Parser only produces these.
  | BDSelector        -- ^ Select from a BD struct (introduced by `Check`)
  | UnionSelector     -- ^ Select from a union (introduced by `Check`)

data DeclDef =
    DeclDef (Q ExportExpr)
  | DeclCase LName [(Pat, Q ExportExpr)]
  | DeclExtern

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
      , vcat (map pp (moduleForeignTypes m))
      , vcat (map pp (moduleDecls m))
      ]

instance PP Entries where
  pp ent = pp (entryModule ent) <.> parens (commaSep (map pp (entryNames ent)))

instance PP ForeignTypeDecl where
  pp fd =
    vcat
      [ "type" <+> pp (ftName fd) <.> ps <+> "->"
      , nest 2 (pp (ftDef fd))
      ]
    where
    ps = case map pp (ftParams fd) of
           [] -> mempty
           ds -> "<" <.> commaSep ds <.> ">"

instance PP ForeignType where
  pp ft = 
    case ft of
      ForeignTVar x -> "?" <.> pp x
      ForeignType f xs -> pp f <.> args
        where
        args =
          case xs of
            [] -> mempty
            _  -> "<" <.> commaSep (map pp xs) <.> ">"

instance PP Decl where
  pp d = vcat [
    dflt <+> "def" <+> pp (declName d) <.> targs <+> hsep (map ppF (declFunParams d)) <+>
      parens (pp (declArg d) <.> ":" <+> pp (declArgType d)) <.>
        ":" <+> pp (declResType d) <+> ppDeclDefStarter (declDef d),
      nest 2 (ppDeclDefBody (declDef d))
    ]
    where
    dflt = if declDefault d then "default" else mempty
    ppF (f,t) = parens (pp f <.> ":" <+> pp t)
    targs =
      case map pp (map snd (declDDLTParams d)) ++
           map pp (declForeignTParams d)
      of
        [] -> mempty
        ds -> "<" <.> commaSep ds <.> ">"

instance PP BasicExporterType where
  pp (x :-> y) = pp x <+> "=>" <+> pp y

instance PP LName where
  pp = pp . nameName

instance PP Exporter where
  pp = ppP (0 :: Int)
    where
    ppP n e =
      case e of
        ExportDefault -> "default"
        ExportTop x -> pp x
        ExportApp f x -> if n > 0 then parens doc else doc
          where doc = ppP 0 f <+> ppP 1 x
          
instance PP DDLExpr where
  pp e =
    case e of
      DDLExpr x sels -> pp x <.> hcat (map pp sels)
      
instance PP Selector where
  pp (x :. l) = pp x <.> pp l

instance PP SelectorType where
  pp x =
    case x of
      StructSelector -> "."
      BDSelector     -> ":."
      UnionSelector  -> "!."

instance PP ExportExpr where
  pp (ExportExpr f x) = pp f <+> pp x

ppDeclDefBody :: DeclDef -> Doc
ppDeclDefBody d =
  case d of
    DeclExtern -> "extern"
    DeclDef f -> pp f
    DeclCase x alts ->
      vcat [
        "case" <+> pp x <+> "of",
        nest 2 (vcat [ (pp pat <+> "->") $$ nest 2 (pp rhs) | (pat,rhs) <- alts ])
      ]

ppDeclDefStarter :: DeclDef -> Doc
ppDeclDefStarter d =
  case d of
    DeclDef {} -> "->"
    DeclCase {} -> "="
    DeclExtern {} -> "="

instance PP DeclDef where
  pp d = ppDeclDefStarter d <+> ppDeclDefBody d

instance PP Pat where
  pp (PCon c mb) = pp c <+> maybe mempty pp mb

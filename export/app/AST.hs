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

data ForeignType = ForeignType LName [ForeignType]


data Decl = Decl {
  declDefault   :: Bool,      -- ^ Is this the default exporter for the type
  declName      :: LName,
  declArg       :: LName,
  declArgType   :: Core.Type,
  declResType   :: ForeignType,
  declDef       :: DeclDef
}

data ExportType = ExportType

data Exporter =
    ExportDefault
  | ExportWith LName [Exporter]

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
      [ "type" <+> pp (ftName fd) <+> hsep (map pp (ftParams fd)) <+> "->"
      , nest 2 (pp (ftDef fd))
      ]

instance PP ForeignType where
  pp ft = ppP (0 :: Int) ft
    where
    ppP n (ForeignType f xs) =
      case xs of
        [] -> pp f
        _  -> if n > 0 then parens doc else doc
          where doc = pp f <+> fsep (map (ppP 1) xs)

instance PP Decl where
  pp d = vcat [
    "def" <+> dflt <+> pp (declName d) <.>
      parens (pp (declArg d) <.> ":" <+> pp (declArgType d)) <.>
        ":" <+> pp (declResType d) <+> ppDeclDefStarter (declDef d),
      nest 2 (ppDeclDefBody (declDef d))
    ]
    where dflt = if declDefault d then "default" else mempty

instance PP LName where
  pp = pp . nameName

instance PP Exporter where
  pp e =
    case e of
      ExportDefault -> "default"
      ExportWith f as ->
        pp f <.> 
          case map pp as of
            [] -> mempty
            xs -> "<" <.> commaSep xs <.> ">"
        
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

instance PP DeclDef where
  pp d = ppDeclDefStarter d <+> ppDeclDefBody d

instance PP Pat where
  pp (PCon c mb) = pp c <+> maybe mempty pp mb

instance PP ExportType where
  pp ExportType = mempty
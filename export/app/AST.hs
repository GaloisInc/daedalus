module AST where

import Data.Void(Void,vacuous)


import AlexTools(SourceRange, (<->))

import Daedalus.PP
import Daedalus.Core qualified as Core

import Name
import Type
import Quote


-- | A module of DSL declarations
data Module = Module {
  moduleRoots :: [Roots],
  -- ^ Roots for Daedalus parsers we are exporting

  moduleForeign :: [Q Void],
  -- ^ Arbitrary foreign text (e.g., #include or helper functions)

  moduleForeignTypes :: [ForeignTypeDecl],
  -- ^ Foreign type declarations

  moduleDecls   :: [Decl]
  -- ^ Exporter definitions
  
}


-- | Specifies a Daedalus root
data Roots = Roots {
  rootModule :: LName,      -- ^ Daedalus module
  rootNames  :: [LName]     -- ^ Name of parser in that module
}

-- | Declare a foreign type alias
data ForeignTypeDecl = ForeignTypeDecl {
  ftName    :: LName,
  ftParams  :: [LName],
  ftDef     :: Q LName
}

-- | Declare an exporter
data Decl = Decl {
  declDefault         :: Bool,
  -- ^ Is this the default exporter for the type

  declName            :: LName,
  declDDLTParams      :: [(LName, Core.TParam)],
  declForeignTParams  :: [LName],
  declFunParams       :: [(LName, BasicExporterType)],
  declArg             :: LName,
  declArgType         :: Core.Type,
  declResType         :: Type,
  declDef             :: DeclDef
}

data DeclDef =
    DeclDef ForeignCode
  | DeclCase LName [(Pat, ForeignCode)]
  | DeclLoop Loop
  | DeclExtern


data Loop = Loop {
  loopInit    :: Q LName, -- ^ Splices are type parameters
  loopFor     :: ([LName],LName,ForeignCode),
  loopReturn  :: Q Void
}

data Pat =
  PCon LName (Maybe LName)

data ForeignCode =
    Splice (Q ExportExpr)
  | Direct ExportExpr


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | An exporter expression
data Exporter =
    
    ExportTop LName [Core.Type] [Type]
    -- ^ The type instances are added by module "Check"

  | ExportApp Exporter Exporter

-- | An exported value
data ExportExpr = ExportExpr {
  exportWith :: Maybe Exporter, -- ^ '`Nothing` means `default`
  exportExpr :: DDLExpr,
  exportResult :: Maybe Type -- ^ Filled in by `Check`
}

-- | A Daedalus value
data DDLExpr      = DDLExpr LName Selectors

type Selectors    = [Selector]

data Selector     = SelectorType :. LName

data SelectorType =
    StructSelector    -- ^ Select from a struct.  Parser only produces these.
  | BDSelector        -- ^ Select from a BD struct (introduced by `Check`)
  | UnionSelector     -- ^ Select from a union (introduced by `Check`)




--------------------------------------------------------------------------------
-- Computing Source Ranges
--------------------------------------------------------------------------------

ddlExprRange :: DDLExpr -> SourceRange
ddlExprRange (DDLExpr x ls) =
  foldr (<->) (nameRange x) [ nameRange l | _ :. l <- ls ]

exporterRange :: Exporter -> SourceRange
exporterRange ex =
  case ex of
    ExportTop l _ _ -> nameRange l
    ExportApp f x -> exporterRange f <-> exporterRange x

exportExprRange :: ExportExpr -> SourceRange
exportExprRange ex =
  case exportWith ex of
    Nothing -> ddlExprRange (exportExpr ex)
    Just e  -> exporterRange e <-> ddlExprRange (exportExpr ex)







--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------


instance PP Module where
  pp m =
    vcat
      [ vcat [ "import" <+> pp n | n <- moduleRoots m ]
      , vcat [ pp (vacuous q :: Q Decl) | q <- moduleForeign m ]
      , vcat (map pp (moduleForeignTypes m))
      , vcat (map pp (moduleDecls m))
      ]

instance PP Roots where
  pp ent = pp (rootModule ent) <.> parens (commaSep (map pp (rootNames ent)))

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


instance PP Exporter where
  ppPrec n e =
    case e of
      ExportTop x [] [] -> pp x
      ExportTop x cs es ->
        wrapIf (n > 0)
          (pp x <+> hsep (map (ppPrec 1) cs) <+> hsep (map (ppPrec 1) es))
      ExportApp f x -> wrapIf (n > 0) (pp f <+> ppPrec 1 x)
          
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
  pp (ExportExpr mb x resT) =
    case mb  of
      Just f -> pp f <+> pp x <+> docRes
      Nothing -> pp x <+> docRes
    where
    docRes =
      case resT of
        Nothing -> mempty
        Just t -> ":" <+> pp t

instance PP ForeignCode where
  pp code = ppCodeStarter code $$ nest 2 (ppCodeDef code)

ppCodeStarter :: ForeignCode -> Doc
ppCodeStarter code =
  case code of
    Splice {} -> "->"
    Direct {} -> "="

ppCodeDef :: ForeignCode -> Doc
ppCodeDef code =
  case code of
    Splice q -> pp q
    Direct q -> pp q

ppDeclDefBody :: DeclDef -> Doc
ppDeclDefBody d =
  case d of
    DeclExtern -> "extern"
    DeclDef f -> ppCodeDef f
    DeclCase x alts ->
      vcat [
        "case" <+> pp x <+> "of",
        nest 2 (vcat [ pp pat <+> pp rhs | (pat,rhs) <- alts ])
      ]
    DeclLoop l -> pp l

ppDeclDefStarter :: DeclDef -> Doc
ppDeclDefStarter d =
  case d of
    DeclDef code -> ppCodeStarter code
    DeclCase {} -> "="
    DeclExtern {} -> "="
    DeclLoop {} -> "="

instance PP Loop where
  pp l =
    let (xs,x,body) = loopFor l
        clause c y = (c <+> "->") $$ nest 2 (pp y)
    in
    vcat [
      clause "init" (loopInit l),
      "for" <+> commaSep (map pp xs) <+> "in" <+> pp x <+> pp body,
      clause "return" (vacuous (loopReturn l) :: Q Decl)
    ]

instance PP DeclDef where
  pp d = ppDeclDefStarter d <+> ppDeclDefBody d

instance PP Pat where
  pp (PCon c mb) = pp c <+> maybe mempty pp mb

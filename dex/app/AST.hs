module AST where

import Data.Void(Void,vacuous)


import AlexTools(SourceRange, (<->))

import Daedalus.PP

import Name
import Type
import Quote


-- | A module of DSL declarations
data Module a b = Module {
  moduleName :: Name,
  -- ^ Name of the module.  Derived from the file.

  moduleRoots :: [Roots],
  -- ^ Roots for Daedalus parsers we are exporting

  moduleUsing :: [Loc Name],
  -- ^ Other Dex modules we depend on

  moduleForeign :: [Q Void],
  -- ^ Arbitrary foreign text (e.g., #include or helper functions)

  moduleForeignTypes :: [ForeignTypeDecl],
  -- ^ Foreign type declarations

  moduleDecls   :: [Decl a b]
  -- ^ Exporter definitions
  
}


-- | Specifies a Daedalus root
data Roots = Roots {
  rootModule :: Loc Name,      -- ^ Daedalus module
  rootNames  :: [Loc Name]     -- ^ Name of parser in that module
}

-- | Declare a foreign type alias
data ForeignTypeDecl = ForeignTypeDecl {
  ftName    :: Loc Name,
  ftParams  :: [Loc Name],
  ftDef     :: Q (Loc Name)
}

-- | Declare an exporter
data Decl a b = Decl {
  declDefault         :: Bool,
  -- ^ Is this the default exporter for the type

  declName            :: Loc b,
  declDDLTParams      :: [Loc Name],
  declForeignTParams  :: [Loc Name],
  declFunParams       :: [(Loc Name, BasicExporterType a b)],
  declArg             :: Loc Name,
  declType            :: BasicExporterType a b,
  declDef             :: DeclDef a b
}

data DeclDef a b =
    DeclDef (ForeignCode a b)
  | DeclCase (Loc Name) [(Pat a, ForeignCode a b)]
  | DeclLoop (Loop a b)
  | DeclExtern


data Loop a b = Loop {
  loopInit    :: Q (Loc Name), -- ^ Splices are type parameters
  loopFor     :: ([Loc Name],Loc Name,ForeignCode a b),
  loopReturn  :: Q Void
}

data Pat a =
  PCon (Loc Name) (Maybe (Loc Name, Maybe (Type a)))

data ForeignCode a b =
    Splice (Q (ForeignCodeSplice a b))
  | Direct (ExportExpr a b)

data ForeignCodeSplice a b =
    SpliceTParam (Loc Name)
  | SpliceCode (ExportExpr a b)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | An exporter expression
data Exporter a b =    
    ExportTop (Loc b) [Type a] [Type b] [Exporter a b] (Maybe (BasicExporterType a b))
    -- ^ The type instances are added by module "Check"
  | ExportLocal (Loc Name) (Maybe (BasicExporterType a b))
    -- ^ Use a of an exporter parameter.
    -- Introduced by the renamer.


-- | An exported value
data ExportExpr a b = ExportExpr {
  exportWith :: Maybe (Exporter a b), -- ^ '`Nothing` means `default`
  exportExpr :: DDLExpr,
  exportResult :: Maybe (Type b) -- ^ Filled in by `Check`
}

-- | A Daedalus value
data DDLExpr      = DDLExpr (Loc Name) Selectors

type Selectors    = [Selector]

data Selector     = SelectorType :. Loc Name

data SelectorType =
    StructSelector    -- ^ Select from a struct.  Parser only produces these.
  | BDSelector        -- ^ Select from a BD struct (introduced by `Check`)
  | UnionSelector     -- ^ Select from a union (introduced by `Check`)




--------------------------------------------------------------------------------
-- Computing Source Ranges
--------------------------------------------------------------------------------

class HasRange a where
  getRange :: a -> SourceRange

instance HasRange (Loc a) where
  getRange = locRange

instance HasRange DDLExpr where
  getRange (DDLExpr x ls) =
    foldr (<->) (locRange x) [ locRange l | _ :. l <- ls ]

instance HasRange (Exporter a b) where
  getRange ex =
    case ex of
      ExportTop l _ _ xs _ ->
        case xs of
          [] -> getRange l
          _  -> getRange l <-> getRange (last xs)
      ExportLocal x _ -> getRange x

instance HasRange (ExportExpr a b) where
  getRange ex =
    case exportWith ex of
      Nothing -> getRange (exportExpr ex)
      Just e  -> getRange e <-> getRange (exportExpr ex)







--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------


instance (PPTyCon a, PPTyCon b) => PP (Module a b) where
  pp m =
    vcat
      [ vcat [ "import" <+> pp n | n <- moduleRoots m ]
      , vcat [ pp (vacuous q :: Q Int) | q <- moduleForeign m ]
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


instance (PPTyCon a, PPTyCon b) => PP (Decl a b) where
  pp d = vcat [
    dflt <+> "def" <+> pp (declName d) <.> targs <+> hsep (map ppF (declFunParams d)) <+>
      parens (pp (declArg d) <.> ":" <+> pp argT) <.>
        ":" <+> pp resT <+> ppDeclDefStarter (declDef d),
      nest 2 (ppDeclDefBody (declDef d))
    ]
    where
    argT :-> resT = declType d -- XXX: PP types differently
    dflt = if declDefault d then "default" else mempty
    ppF (f,t) = parens (pp f <.> ":" <+> pp t)
    targs =
      case map pp (declDDLTParams d) ++
           map pp (declForeignTParams d)
      of
        [] -> mempty
        ds -> "<" <.> commaSep ds <.> ">"


instance (PPTyCon a, PPTyCon b) => PP (Exporter a b) where
  pp e =
    case e of
      ExportTop x cs es fs _ -> pp x <.> opt_args
        where
        opt_args =
          case map pp cs ++ map pp es ++ map pp fs of
            [] -> mempty
            ds -> "<" <.> commaSep ds <.> ">"
      ExportLocal f _ -> pp f

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

instance (PPTyCon a, PPTyCon b) => PP (ExportExpr a b) where
  pp (ExportExpr mb x resT) =
    case mb  of
      Just f -> pp f <.> parens (pp x) <.> docRes
      Nothing -> pp x <+> docRes
    where
    docRes =
      case resT of
        Nothing -> mempty
        Just t -> ":" <+> pp t

instance (PPTyCon a, PPTyCon b) => PP (ForeignCode a b) where
  pp code = ppCodeStarter code $$ nest 2 (ppCodeDef code)

instance (PPTyCon a, PPTyCon b) => PP (ForeignCodeSplice a b) where
  pp spl =
    case spl of
      SpliceCode c -> pp c
      SpliceTParam t -> pp t

ppCodeStarter :: ForeignCode a b -> Doc
ppCodeStarter code =
  case code of
    Splice {} -> "->"
    Direct {} -> "="

ppCodeDef :: (PPTyCon a, PPTyCon b) => ForeignCode a b -> Doc
ppCodeDef code =
  case code of
    Splice q -> pp q
    Direct q -> pp q

ppDeclDefBody :: (PPTyCon a, PPTyCon b) => DeclDef a b -> Doc
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

ppDeclDefStarter :: DeclDef a b -> Doc
ppDeclDefStarter d =
  case d of
    DeclDef code -> ppCodeStarter code
    DeclCase {} -> "="
    DeclExtern {} -> "="
    DeclLoop {} -> "="



instance (PPTyCon a, PPTyCon b) => PP (Loop a b) where
  pp l =
    let (xs,x,body) = loopFor l
        clause c y = (c <+> "->") $$ nest 2 (pp y)
    in
    vcat [
      clause "init" (loopInit l),
      "for" <+> commaSep (map pp xs) <+> "in" <+> pp x <+> pp body,
      clause "return" (vacuous (loopReturn l) :: Q Int)
    ]

instance (PPTyCon a, PPTyCon b) => PP (DeclDef a b) where
  pp d = ppDeclDefStarter d <+> ppDeclDefBody d

instance PP (Pat a) where
  pp (PCon c mb) = pp c <+> maybe mempty (pp . fst) mb

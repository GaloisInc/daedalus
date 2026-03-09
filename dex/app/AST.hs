
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

  moduleForeign :: [ForeignBlock],
  -- ^ Arbitrary foreign text (e.g., #include or helper functions)

  moduleForeignTypes :: [ForeignTypeDecl],
  -- ^ Foreign type declarations

  moduleDecls   :: [Decl a b]
  -- ^ Exporter definitions
}

data ForeignBlock = ForeignBlock {
  foreignCode  :: Q Void,
  foreignDef   :: Bool
  -- Indicates that the user used the `def` flag on the `extern`.
  -- For C++, we interpret this to mean that this code should only go
  -- in the generated `.cpp`, as opposed to going in the generated `.h`
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
  ftDef     :: Maybe (Q (Loc Name))
    -- ^ If nothing, derive the foreign name for the Dex name.
}

-- | Declare an exporter
data Decl a b = Decl {
  declDefault         :: Bool,
  -- ^ Is this the default exporter for the type

  declName            :: Loc b,
  declDDLTParams      :: [Loc Name],
  declForeignTParams  :: [Loc Name],
  declFunParams       :: [(Loc Name, BasicExporterType a b)],
  declArg             :: [Loc Name],
  declType            :: BasicExporterType a b,
  declDef             :: DeclDef a b
}

data DeclDef a b =
    DeclDef (ForeignCode a b)
  | DeclCase (Loc Name) [(Pat a, ForeignCode a b)]
  | DeclLoop (Loop a b)
  | DeclExtern


data Loop a b = Loop {
  loopInit    :: ForeignCode a b,
  loopFor     :: ([Loc Name],Loc Name,ForeignCode a b),
  loopReturn  :: ForeignCode a b
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
  exportWith    :: Maybe (Exporter a b),
  -- ^ '`Nothing` means `default`.  In some contexts, if this is `Nothing`,
  -- then we try to see if the `exportExpr` is actually a *type* parameter.

  exportExpr    :: [DDLExpr],
  exportResult  :: Maybe (Type b) -- ^ Filled in by `Check`
}

-- | A Daedalus value
data DDLExpr      = DDLVar (Loc Name)
                  | DDLSel DDLExpr Selector

splitDDLExpr :: DDLExpr -> (Loc Name, [Selector])
splitDDLExpr e0 = (v, reverse ss)
  where
  (v,ss) = go e0
  go e =
    case e of
      DDLVar x -> (x,[])
      DDLSel x y -> (a, y : b)
        where (a,b) = go x

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
  getRange e =
    case e of
      DDLVar x -> getRange x
      DDLSel e1 l -> getRange e1 <-> getRange l

instance HasRange Selector where
  getRange (_ :. l) = locRange l

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
      Nothing ->
        case argRng of
          Just r -> r
          Nothing -> error "[BUG] HasRange (ExportExpr a b): no exporter argument"
      Just e  ->
        case argRng of
          Nothing -> getRange e
          Just r -> getRange e <-> r
    where
    argRng =
      case exportExpr ex of
        [] -> Nothing
        xs@(x : _) -> Just (getRange x <-> getRange (last xs))



--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------


instance (PPTyCon a, PPTyCon b) => PP (Module a b) where
  pp m =
    vcat
      [ vcat [ "import" <+> pp n | n <- moduleRoots m ]
      , vcat (map pp (moduleForeign m))
      , vcat (map pp (moduleForeignTypes m))
      , vcat (map pp (moduleDecls m))
      ]

instance PP Roots where
  pp ent = pp (rootModule ent) <.> parens (commaSep (map pp (rootNames ent)))

instance PP ForeignBlock where
  pp fb =
    ("extern" <+> (if foreignDef fb then "def" else mempty) <+> "->")
    $$ nest 2 (pp (vacuous (foreignCode fb) :: Q Int))

instance PP ForeignTypeDecl where
  pp fd =
    case ftDef fd of
      Just def ->
        vcat
          [ "type" <+> pp (ftName fd) <.> ps <+> "->"
          , nest 2 (pp def) 
          ]
      Nothing ->
          "extern" <+> "type" <+> pp (ftName fd) <.> ps <+> "->"
    where
    ps = case map pp (ftParams fd) of
           [] -> mempty
           ds -> "<" <.> commaSep ds <.> ">"


instance (PPTyCon a, PPTyCon b) => PP (Decl a b) where
  pp d = vcat [
    dflt <+> "def" <+> pp (declName d) <.> targs <+> hsep (map ppF (declFunParams d)) <+>
      parens argsWithTs <.>
        ":" <+> pp resT <+> ppDeclDefStarter (declDef d),
      nest 2 (ppDeclDefBody (declDef d))
    ]
    where
    argsWithTs = commaSep [ (pp x <.> ":" <+> pp y) | (x,y) <- declArg d `zip` argT ]
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
      DDLVar x -> pp x
      DDLSel x y -> pp x <+> pp y
      
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
      Just f -> pp f <.> parens (commaSep (map pp x)) <.> docRes
      Nothing ->
        case x of
          [a] -> pp a <+> docRes
          _   -> error "[BUG] PP (ExportExpr): Default exporter but not 1 argument"
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
      clause "return" (loopReturn l)
    ]

instance (PPTyCon a, PPTyCon b) => PP (DeclDef a b) where
  pp d = ppDeclDefStarter d <+> ppDeclDefBody d

instance PP (Pat a) where
  pp (PCon c mb) = pp c <+> maybe mempty (pp . fst) mb

{-# Language OverloadedStrings #-}
module Daedalus.Core.Decl where

import Daedalus.PP
import Daedalus.Rec

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.Grammar

data Module = Module
  { mName     :: MName
  , mImports  :: [MName]
  , mTypes    :: [Rec TDecl]
  , mFFuns    :: [Fun Expr]
  , mGFuns    :: [Fun Grammar]
  }

data Fun e = Fun
  { fName    :: FName
  , fParams  :: [Name]
  , fDef     :: FunDef e
  }

data FunDef e = Def e | External

data TDecl = TDecl
  { tName          :: TName
  , tTParamKNumber :: [TParam]
  , tTParamKValue  :: [TParam]
  , tDef           :: TDef
  }

data TDef =
    TStruct [(Label,Type)]
  | TUnion  [(Label,Type)]

class GetFields t where
  getFields :: t -> [(Label,Type)]

instance GetFields TDecl where
  getFields = getFields . tDef

instance GetFields TDef where
  getFields td = case td of
                   TStruct fs -> fs
                   TUnion  fs -> fs


--------------------------------------------------------------------------------

instance PP Module where
  pp m =
    vcat' $ "module" <+> pp (mName m) <+> "where"
          : vcat [ "import" <+> pp x | x <- mImports m ]
          : map pp (mTypes m)
         ++ [ "-------------" ]
         ++ map pp (mFFuns m)
         ++ map pp (mGFuns m)


instance (DefKW e, PP e) => PP (Fun e) where
  pp f = kw <+> pp (fName f)
       <.> parens (commaSep (map ppP (fParams f)))
       <+> ":" <+> pp (fnameType (fName f)) <+> "="
        $$ nest 2 (pp (fDef f))
    where ppP x = pp x <+> ":" <+> pp (nameType x)
          kw = defKW (fDef f)

instance PP e => PP (FunDef e) where
  pp def =
    case def of
      Def e   -> pp e
      External -> "_ {- external -}"


instance PP TDecl where
  pp d =
    "type" <+> pp (tName d)
           <+> hsep (map pp (tTParamKNumber d))
           <+> hsep (map pp (tTParamKValue d))
           <+> "=" $$ nest 2 (pp (tDef d))

instance PP TDef where
  pp d =
    case d of
      TStruct fs -> ppK "struct" fs
      TUnion  fs -> ppK "union"  fs
    where
    ppK k fs  = k $$ nest 2 (vcat (map ppF fs))
    ppF (l,t) = pp l <+> ":" <+> pp t


class DefKW a where
  defKW :: FunDef a -> Doc

instance DefKW Expr where
  defKW _ = "func"

instance DefKW Grammar where
  defKW _ = "proc"

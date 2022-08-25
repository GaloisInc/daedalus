{-# Language OverloadedStrings #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric, DeriveAnyClass #-}

module Daedalus.Core.Decl where

import GHC.Generics          (Generic)
import Control.DeepSeq       (NFData)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Rec
import qualified Daedalus.BDD as BDD
import Daedalus.Panic(panic)

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet
import Daedalus.Core.Grammar

data Module = Module
  { mName     :: MName
  , mImports  :: [MName]
  , mTypes    :: [Rec TDecl]
  , mFFuns    :: [Fun Expr]
  , mBFuns    :: [Fun ByteSet]
  , mGFuns    :: [Fun Grammar]
  }
  deriving (Generic,NFData)

data Fun e = Fun
  { fName    :: FName
  , fParams  :: [Name]
  , fDef     :: FunDef e
  , fIsEntry :: !Bool
  , fAnnot   :: ![Annot]
  }
  deriving (Functor, Foldable, Traversable, Generic, NFData)

data FunDef e = Def e | External
  deriving (Functor, Foldable, Traversable, Generic, NFData)

data TDecl = TDecl
  { tName          :: TName
  , tTParamKNumber :: [TParam]
  , tTParamKValue  :: [TParam]
  , tDef           :: TDef
  }
  deriving (Generic,NFData)

tExtern :: TDecl -> Bool
tExtern = tnameExternal . tName

data TDef =
    TStruct [(Label,Type)]
  | TUnion  [(Label,Type)]
  | TBitdata BDD.Pat BitdataDef
  deriving (Generic,NFData)

class GetFields t where
  getFields :: t -> [(Label,Type)]

instance GetFields TDecl where
  getFields = getFields . tDef

instance GetFields TDef where
  getFields td = case td of
                   TStruct fs -> fs
                   TUnion  fs -> fs
                   TBitdata _ bd -> getFields bd

instance GetFields BitdataDef where
  getFields bd =
    case bd of
      BDStruct fs -> [ (l,t) | f <- fs, BDData l t <- [ bdFieldType f ] ]
      BDUnion fs  -> fs

data BitdataDef =
    BDStruct [BDField]
  | BDUnion  [(Label,Type)]
  deriving (Generic,NFData)

data BDField = BDField
  { bdOffset    :: BDD.Width
  , bdWidth     :: BDD.Width
  , bdFieldType :: BDFieldType
  } deriving (Generic,NFData)

data BDFieldType = BDWild | BDTag Integer | BDData Label Type
  deriving (Generic,NFData)




-- | Get an instantiate version of a type.
-- Assumes that the decla name and user type match.
tyDeclsInst :: TDecl -> UserType -> TDef
tyDeclsInst td orig =
  case tDef td of
    TStruct ls   -> TStruct (goLabeled ls)
    TUnion  ls   -> TUnion  (goLabeled ls)
    TBitdata b d -> TBitdata b d -- nothing to instnatiate here

  where
  nenv = Map.fromList (zip (tTParamKNumber td) (utNumArgs orig))
  tenv = Map.fromList (zip (tTParamKValue  td) (utTyArgs orig))

  goTy ty =
    case ty of
      TStream   -> ty
      TUInt tsz -> TUInt (goSize tsz)
      TSInt tsz -> TSInt (goSize tsz)
      TInteger  -> ty
      TBool     -> ty
      TFloat    -> ty
      TDouble   -> ty
      TUnit     -> ty
      TArray ty' -> TArray (goTy ty')
      TMaybe ty' -> TMaybe (goTy ty')
      TMap dTy rTy -> TMap (goTy dTy) (goTy rTy)
      TBuilder ty' -> TBuilder (goTy ty')
      TIterator ty' -> TIterator (goTy ty')
      TUser ut      -> TUser (ut { utNumArgs = map goSize (utNumArgs ut)
                                 , utTyArgs  = map goTy (utTyArgs ut)
                                 })
      TParam p
        | Just ty' <- Map.lookup p tenv -> ty'
        | otherwise -> panic "Missing type param" []

  goSize (TSizeParam p) 
    | Just n <- Map.lookup p nenv = n -- _should_ be a size? 
    | otherwise = panic "Missing type param" []
  goSize sz = sz

  goLabeled = map (\(l, t) -> (l, goTy t))




--------------------------------------------------------------------------------

instance PP Module where
  pp m =
    vcat' $ "module" <+> pp (mName m) <+> "where"
          : vcat [ "import" <+> pp x | x <- mImports m ]
          : map pp (mTypes m)
         ++ [ "-------------" ]
         ++ map pp (mFFuns m)
         ++ map pp (mBFuns m)
         ++ map pp (mGFuns m)


instance (DefKW e, PP e) => PP (Fun e) where
  pp f = isEnt $$
         annot $$
         kw <+> pp (fName f)
       <.> parens (commaSep (map ppP (fParams f)))
       <+> ":" <+> pp (fnameType (fName f)) <+> "="
        $$ nest 2 (pp (fDef f))
    where ppP x = pp x <+> ":" <+> pp (nameType x)
          kw = defKW (fDef f)
          isEnt = if fIsEntry f then "-- entry pont" else empty
          annot = case fAnnot f of
                    [] -> empty
                    as -> hsep ("--" : map pp as)

instance PP e => PP (FunDef e) where
  pp def =
    case def of
      Def e   -> pp e
      External -> "_ {- external -}"


instance PP TDecl where
  pp d =
    extern $$
    "type" <+> pp (tName d)
           <+> hsep (map pp (tTParamKNumber d))
           <+> hsep (map pp (tTParamKValue d))
           <+> "=" $$ nest 2 (pp (tDef d))
    where extern = if tExtern d then "-- extern" else mempty

instance PP TDef where
  pp d =
    case d of
      TStruct fs    -> ppK "struct" fs
      TUnion  fs    -> ppK "union"  fs
      TBitdata _ bd -> "bitdata" $$ nest 2 (pp bd)
    where
    ppK k fs  = k $$ nest 2 (vcat (map ppF fs))
    ppF (l,t) = pp l <+> ":" <+> pp t

instance PP BitdataDef where
  pp d =
    case d of
      BDStruct fs -> vcat (map pp fs)
      BDUnion  fs -> vcat [ pp l <+> ":" <+> pp t | (l,t) <- fs ]

instance PP BDField where
  pp f = case bdFieldType f of
           BDWild     -> "_"  <+> ":" <+> pp w
           BDTag n    -> pp n <+> ":" <+> pp w
           BDData l t -> pp l <+> ":" <+>  pp t
    where w = TUInt (TSize (toInteger (bdWidth f)))


class DefKW a where
  defKW :: FunDef a -> Doc

instance DefKW Expr where
  defKW _ = "func"

instance DefKW ByteSet where
  defKW _ = "byteset"

instance DefKW Grammar where
  defKW _ = "proc"


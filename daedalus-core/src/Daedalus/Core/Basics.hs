{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Daedalus.Core.Basics where

import Data.Text(Text)
import Data.Function(on)

import Daedalus.PP


-- | Module names
newtype MName = MName Text
  deriving (Eq,Ord)

-- | Type declaration names
data TName = TName
  { tnameId   :: Int
  , tnameText :: Text
  , tnameMod  :: MName
  , tnameAnon :: Maybe Int    -- ^ For types that only appear in other types
  }

-- | Names of top-level functions
data FName = FName
  { fnameId         :: Int
  , fnameText       :: Maybe Text
  , fnameType       :: Type
  , fnameMod        :: MName
  }

-- | Names of local variables
data Name = Name
  { nameId   :: Int
  , nameText :: Maybe Text      -- ^ Name in source, if any
  , nameType :: Type
  }


-- | Annotation
data Annot =
    SrcAnnot Text
  | NoFail          -- ^ The grammar is known to not fail

type Label = Text

data Type =
    TStream
  | TUInt SizeType
  | TSInt SizeType
  | TInteger
  | TBool
  | TUnit
  | TArray Type
  | TMaybe Type
  | TMap Type Type
  | TBuilder Type
  | TIterator Type
  | TUser UserType
  | TParam TParam         -- ^ Only in type declaraionts
    deriving (Eq,Ord)

data SizeType =
    TSize Integer
  | TSizeParam TParam     -- ^ Only in type declarations
    deriving (Eq,Ord)

data UserType = UserType
  { utName    :: TName
  , utNumArgs :: [SizeType]
  , utTyArgs  :: [Type]
  } deriving (Eq,Ord)


newtype TParam = TP Int
  deriving (Eq,Ord)



--------------------------------------------------------------------------------
instance Eq Name where
  (==) = (==) `on` nameId

instance Ord Name where
  compare = compare `on` nameId

instance Eq FName where
  (==) = (==) `on` \x -> (fnameId x, fnameMod x)

instance Ord FName where
  compare = compare `on` \x -> (fnameId x, fnameMod x)

instance Eq TName where
  (==) = (==) `on` tnameId

instance Ord TName where
  compare = compare `on` tnameId


--------------------------------------------------------------------------------

instance PP MName where
  pp (MName x) = pp x

instance PP TName where
  pp t = pp (tnameText t) <.> case tnameAnon t of
                                Nothing -> empty
                                Just x  -> pp x

instance PP FName where
  pp f = case fnameText f of
           Nothing -> "_F" <.> int (fnameId f)
           Just t  -> pp t


instance PP Name where
  pp n = case nameText n of
           Nothing -> "_x" <.> int (nameId n)
           Just t  -> pp t


instance PP Annot where
  pp ann =
    case ann of
      SrcAnnot t -> pp t
      NoFail     -> "NoFail"


instance PP Type where
  ppPrec n ty =
    case ty of
      TStream     -> "Input"
      TUInt w     -> wrapIf (n > 0) ("Word" <+> pp w)
      TSInt w     -> wrapIf (n > 0) ("Int" <+> pp w)
      TInteger    -> "Integer"
      TBool       -> "Bool"
      TUnit       -> "()"
      TArray t    -> brackets (pp t)
      TMap k v    -> brackets (pp k <+> "->" <+> pp v)
      TMaybe t    -> wrapIf (n > 0) ("Maybe" <+> ppPrec 1 t)
      TBuilder t  -> wrapIf (n > 0) ("Builder" <+> ppPrec 1 t)
      TIterator t -> wrapIf (n > 0) ("Iter" <+> ppPrec 1 t)
      TUser t     -> pp t
      TParam x    -> pp x

instance PP UserType where
  ppPrec n t =
    case (utNumArgs t, utTyArgs t) of
      ([],[]) -> pp (utName t)
      _       -> wrapIf (n > 0)
               $ hsep [ pp (utName t)
                      , hsep (map pp (utNumArgs t))
                      , hsep (map (ppPrec 1) (utTyArgs t))
                      ]

instance PP TParam where
  pp (TP x) = "t" <.> pp x

instance PP SizeType where
  pp t =
    case t of
      TSize n -> pp n
      TSizeParam x -> pp x
  

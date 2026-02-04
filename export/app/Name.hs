module Name (
  Loc(..),
  QName(..),
  PName(..),
  DDLTCon(..),
  Name,
  nameToText,
  nameFromText
 ) where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Bits
import AlexTools(SourceRange)
import Daedalus.PP
import Daedalus.Core qualified as Core

-- | Name with a location
data Loc a = Loc {
  locThing :: a,
  locRange :: SourceRange
} deriving Functor

-- | Fully qualified
data QName = QName { qName :: Name, qModule :: Name }
  deriving (Eq,Ord)

-- | Names used in the parser to refer to things
data PName = Unqual Name | Qual QName

-- | An identifier, with a hash for fester discrimination and ordering.
data Name = Name !Int !Text
  deriving (Eq,Ord)
-- The `Int` is a simple hash of the text so that we can quickly
-- distinguish many different names

nameToText :: Name -> Text
nameToText (Name _ txt) = txt

nameFromText :: Text -> Name
nameFromText txt = Name (Text.foldl' hash 5381 txt) txt
  where
  hash h c = (shiftL h 5 + h) `xor` fromEnum c
  -- djb2

data DDLTCon = 
    TStream
  | TUInt
  | TSInt
  | TInteger
  | TBool
  | TFloat
  | TDouble
  | TUnit
  | TArray
  | TMaybe
  | TMap
  | TBuilder
  | TIterator
  | TUser Core.TName
    deriving (Eq,Ord)

instance Show Name where
  show = show . nameToText

instance PP Name where
  pp = pp . nameToText

instance PP a => PP (Loc a) where
  pp = pp . locThing

instance PP QName where
  pp x = pp (qModule x) <.> "::" <.> pp (qName x)

instance PP PName where
  pp nm =
    case nm of
      Unqual x -> pp x
      Qual x   -> pp x

instance PP DDLTCon where
  pp tc =
    case tc of
      TStream -> "stream"
      TUInt -> "uint"
      TSInt -> "sint"
      TInteger -> "int"
      TBool -> "bool"
      TFloat -> "float"
      TDouble -> "double"
      TUnit -> "{}"
      TArray -> "[]"
      TMaybe -> "maybe"
      TMap -> "[:]"
      TBuilder -> "builder"
      TIterator -> "iterator"
      TUser t -> pp t
module Name (
  LName(..),
  Name,
  toText,
  fromText
 ) where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Bits
import AlexTools(SourceRange)
import Daedalus.PP

-- | Name with a location
data LName = LName {
  nameName :: Name,
  nameRange :: SourceRange
}


-- | An identifier, with a hash for fester discrimination and ordering.
data Name = Name !Int !Text
  deriving (Eq,Ord)
-- The `Int` is a simple hash of the text so that we can quickly
-- distinguish many different names

toText :: Name -> Text
toText (Name _ txt) = txt

fromText :: Text -> Name
fromText txt = Name (Text.foldl' hash 5381 txt) txt
  where
  hash h c = (shiftL h 5 + h) `xor` fromEnum c
  -- djb2

instance Show Name where
  show = show . toText

instance PP Name where
  pp = pp . toText

instance PP LName where
  pp = pp . nameName

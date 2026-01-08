module Name (
  Name,
  toText,
  fromText
 ) where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Bits
import Daedalus.PP

-- The `Int` is a simple hash of the text so that we can quickly
-- distinguish many different names
data Name = Name !Int !Text
  deriving (Eq,Ord)

toText :: Name -> Text
toText (Name _ txt) = txt

fromText :: Text -> Name
fromText txt = Name (Text.foldl' hash 5381 txt) txt
  where
  hash h c = (shiftL h 5 + h) `xor` fromEnum c
  -- djb2

instance PP Name where
  pp = pp . toText

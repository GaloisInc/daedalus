{-# Language OverloadedStrings #-}
module RTS.JSON
  ( JSON
  , jsonToBytes
  , jsNull
  , jsText
  , jsArray
  , jsObject
  , jsTagged
  , ToJSON(..)
  ) where

import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Builder
import Data.List(intersperse)
import Data.Coerce(coerce)

newtype JSON = JSON Builder

jsonToBytes :: JSON -> ByteString
jsonToBytes = LBS.toStrict . toLazyByteString . coerce


class ToJSON a where
  toJSON :: a -> JSON

instance ToJSON Integer where
  toJSON = JSON . integerDec

instance ToJSON Float where
  toJSON = JSON . string7 . show

instance ToJSON Double where
  toJSON = JSON . string7 . show

instance ToJSON Bool where
  toJSON b = JSON (if b then "true" else "false")

instance ToJSON () where
  toJSON _ = jsObject []

-- This is DDL specific
instance (ToJSON a) => ToJSON (Maybe a) where
  toJSON a = case a of
               Nothing -> jsNull
               Just v  -> jsTagged "$null" (toJSON v)

-- This is DDL specific
instance (ToJSON a, ToJSON b) => ToJSON (Map a b) where
  toJSON = jsTagged "$$map" . jsArray . map pair . Map.toList
    where pair (k,v) = jsArray [ toJSON k, toJSON v ]

instance (ToJSON a) => ToJSON [a] where
  toJSON = jsArray . map toJSON

jsNull :: JSON
jsNull = JSON "null"

jsArray :: [ JSON ] -> JSON
jsArray xs = JSON ("[" <> mconcat (intersperse "," (coerce xs)) <> "]")

jsObject :: [ (ByteString, JSON) ] -> JSON
jsObject xs = JSON ("{" <> mconcat (intersperse "," fs) <> "}")
  where fs = [ coerce (jsText k) <> ":" <> coerce v | (k,v) <- xs ]

-- | A shortcur for a common encoding of sum types
jsTagged :: ByteString -> JSON -> JSON
jsTagged t v = jsObject [ (t, v) ]

jsText :: ByteString -> JSON
jsText x = coerce (char7 '"' <> escaped x <> char7 '"')
  where
  escaped cs =
    case BS.break esc cs of
      (as,bs)
        | BS.null bs -> byteString as
        | BS.null as -> escFirst bs
        | otherwise  -> byteString as <> escFirst bs

  escFirst cs = doEsc (BS.head cs) <> escaped (BS.tail cs)

  esc c = c == 34 {- " -} || c == 92 {- \ -} || c < 32 || c > 126

  hex d =
    case d of
      0x0 -> "0"
      0x1 -> "1"
      0x2 -> "2"
      0x3 -> "3"
      0x4 -> "4"
      0x5 -> "5"
      0x6 -> "6"
      0x7 -> "7"
      0x8 -> "8"
      0x9 -> "9"
      0xA -> "A"
      0xB -> "B"
      0xC -> "C"
      0xD -> "D"
      0xE -> "E"
      0xF -> "F"
      _   -> error "not hex"

  doEsc c =
    case c of
      08  -> "\\b"
      09  -> "\\t"
      10  -> "\\n"
      12  -> "\\f"
      13  -> "\\r"
      34  -> "\\\""
      92  -> "\\\\"
      _   -> "\\u00" <> hex (div c 16) <> hex (mod c 16)



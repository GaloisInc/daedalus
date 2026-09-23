module Daedalus.RTS.JSON
  ( JSON
  , jsonToBytes
  , jsNull
  , jsText
  , jsString
  , jsInteger
  , jsArray
  , jsObject
  , jsTagged
  , ToJSON(..)
  ) where

import Data.ByteString(ByteString)
import Data.ByteString.Short(fromShort,ShortByteString)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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

instance ToJSON JSON where
  toJSON = id

instance ToJSON Integer where
  toJSON = jsString . show

instance ToJSON Int where
  toJSON = jsInteger . toInteger



instance ToJSON Float where
  toJSON = jsFloating

instance ToJSON Double where
  toJSON = jsFloating

instance ToJSON Bool where
  toJSON b = JSON (if b then "true" else "false")

instance ToJSON () where
  toJSON _ = jsObject []

instance ToJSON Text where
  toJSON = jsText . Text.encodeUtf8

instance ToJSON ShortByteString where
  toJSON = jsText . fromShort

-- This is DDL specific
instance (ToJSON a) => ToJSON (Maybe a) where
  toJSON a = case a of
               Nothing -> jsNull
               Just v  -> jsTagged "$$just" (toJSON v)

-- This is DDL specific
instance (ToJSON a, ToJSON b) => ToJSON (Map a b) where
  toJSON = jsTagged "$$map" . jsArray . map pair . Map.toList
    where pair (k,v) = jsArray [ toJSON k, toJSON v ]

instance (ToJSON a) => ToJSON [a] where
  toJSON = jsArray . map toJSON

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
  toJSON (a,b) = jsTuple [ toJSON a, toJSON b ]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
  toJSON (a,b,c) = jsTuple [ toJSON a, toJSON b, toJSON c ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
  toJSON (a,b,c,d) = jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) =>
         ToJSON (a,b,c,d,e) where
  toJSON (a,b,c,d,e) = jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d
                               , toJSON e ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) =>
         ToJSON (a,b,c,d,e,f) where
  toJSON (a,b,c,d,e,f) = jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d
                                 , toJSON e, toJSON f ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g) => ToJSON (a,b,c,d,e,f,g) where
  toJSON (a,b,c,d,e,f,g) = jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d
                                   , toJSON e, toJSON f, toJSON g ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h) => ToJSON (a,b,c,d,e,f,g,h) where
  toJSON (a,b,c,d,e,f,g,h) = jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d
                                     , toJSON e, toJSON f, toJSON g, toJSON h ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i) => ToJSON (a,b,c,d,e,f,g,h,i) where
  toJSON (a,b,c,d,e,f,g,h,i) =
    jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f
            , toJSON g, toJSON h, toJSON i ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j) where
  toJSON (a,b,c,d,e,f,g,h,i,j) =
    jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f
            , toJSON g, toJSON h, toJSON i, toJSON j ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k) where
  toJSON (a,b,c,d,e,f,g,h,i,j,k) =
    jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f
            , toJSON g, toJSON h, toJSON i, toJSON j, toJSON k ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k,l) where
  toJSON (a,b,c,d,e,f,g,h,i,j,k,l) =
    jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f
            , toJSON g, toJSON h, toJSON i, toJSON j, toJSON k, toJSON l ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l,
          ToJSON m) => ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  toJSON (a,b,c,d,e,f,g,h,i,j,k,l,m) =
    jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f
            , toJSON g, toJSON h, toJSON i, toJSON j, toJSON k, toJSON l
            , toJSON m ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l,
          ToJSON m, ToJSON n) => ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  toJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
    jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f
            , toJSON g, toJSON h, toJSON i, toJSON j, toJSON k, toJSON l
            , toJSON m, toJSON n ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l,
          ToJSON m, ToJSON n, ToJSON o) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  toJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
    jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f
            , toJSON g, toJSON h, toJSON i, toJSON j, toJSON k, toJSON l
            , toJSON m, toJSON n, toJSON o ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l,
          ToJSON m, ToJSON n, ToJSON o, ToJSON p) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  toJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
    jsTuple [ toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f
            , toJSON g, toJSON h, toJSON i, toJSON j, toJSON k, toJSON l
            , toJSON m, toJSON n, toJSON o, toJSON p ]

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

jsTuple :: [JSON] -> JSON
jsTuple = jsTagged "$$tuple" . jsArray

jsString :: String -> JSON
jsString = toJSON . Text.pack

jsInteger :: Integer -> JSON
jsInteger = JSON . integerDec

jsFloating :: (Show a, RealFloat a) => a -> JSON
jsFloating x
  | isInfinite x = jsTagged "$$inf" jsNull
  | isNaN x      = jsTagged "$$nan" jsNull
  | otherwise    = JSON (string7 (show x))

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

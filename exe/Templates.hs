{-# Language TemplateHaskell #-}
module Templates where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Build
import Data.FileEmbed

c_template_files :: [(FilePath,ByteString)]
c_template_files = $(embedDir "exe/c-template")

hs_template_files :: [(FilePath,ByteString)]
hs_template_files = $(embedDir "exe/hs-template")

html_files :: [(FilePath,ByteString)]
html_files = $(embedDir "exe/html-template")

substTemplate :: Map ByteString ByteString -> ByteString -> ByteString
substTemplate vars = Lazy.toStrict . Build.toLazyByteString . substBuilder vars

substBuilder :: Map ByteString ByteString -> ByteString -> Builder
substBuilder vars bs =
  let varChar = fromIntegral (fromEnum '$')
  in
  case BS.break (== varChar) bs of
    (front,back) ->
      Build.byteString front <>
      case BS.uncons back of
        Just (vc,rest) ->
          let check key val  no =
                case BS.stripPrefix key rest of
                  Just yes -> Just (val,yes)
                  Nothing  -> no
          in case Map.foldrWithKey check Nothing vars of
               Just (val,yes) -> Build.byteString val <> substBuilder vars yes
               Nothing -> Build.word8 vc <> substBuilder vars rest
        Nothing -> mempty

{-# Language TemplateHaskell #-}
{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language KindSignatures #-}
{-# Language TypeApplications #-}
module Parser where

import Data.Vector(Vector)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map(Map)
import Data.Text(Text)
import System.FilePath(takeFileName, dropExtension)
import Data.List(isPrefixOf,isSuffixOf)

import Daedalus.RTS
import qualified Daedalus.RTS.Vector as RTS
import Daedalus.TH.Compile


compileDDLWith
  defaultConfig { userEntries = ["ArrayType","DictionaryType"] }
  (FromFile "app/ArlingtonPDF.md")

parseType :: ByteString -> ByteString -> Either ParseError CompositeType
parseType name bytes
  |  "ArrayOf"    `isPrefixOf` nameRoot
  || "Array"      `isSuffixOf` nameRoot
  || "ColorSpace" `isSuffixOf` nameRoot = doParse ArrayType pArrayType
  | otherwise                           = doParse DictionaryType pDictionaryType
  where
  doParse f p = f . RTS.vecToRep <$> runDParser (p input)
  input = newInput name bytes

  nameRoot = dropExtension (takeFileName (BS8.unpack name))


newtype PDFSpec      = PDFSpec (Map Text CompositeType)

data CompositeType =
    ArrayType       (Vector (Field ArrayKey))
  | DictionaryType  (Vector (Field DictionaryKey))
  deriving Show


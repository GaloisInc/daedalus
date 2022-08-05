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
import Data.Map(Map)
import Data.Text(Text)

import Daedalus.RTS
import qualified Daedalus.RTS.Vector as RTS
import Daedalus.TH.Compile


compileDDL (FromFile "app/ArlingtonPDF.md")

parseType :: ByteString -> ByteString -> Either ParseError CompositeType
parseType name bytes =
  CompositeType . RTS.vecToRep <$> runDParser (pMain (newInput name bytes))


newtype PDFSpec       = PDFSpec (Map Text CompositeType)

newtype CompositeType = CompositeType (Vector Field)
  deriving Show



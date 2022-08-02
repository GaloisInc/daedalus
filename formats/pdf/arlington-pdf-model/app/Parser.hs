{-# Language TemplateHaskell #-}
{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language KindSignatures #-}
{-# Language TypeApplications #-}
module Parser (parseType, Field(..)) where

import Data.Vector(Vector)
import Data.ByteString(ByteString)

import Daedalus.RTS
import qualified Daedalus.RTS.Vector as RTS
import Daedalus.TH.Compile


compileDDL (FromFile "app/ArlingtonPDF.md")

parseType :: ByteString -> ByteString -> Either ParseError (Vector Field)
parseType name bytes = RTS.vecToRep <$> runDParser (pMain (newInput name bytes))




module PP where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Text.PrettyPrint

import Parser

class PP a where
  pp :: a -> Doc

instance PP Text where
  pp = text . Text.unpack

instance PP PDFSpec where
  pp (PDFSpec mp) = vcat (map ppOne (Map.toList mp))
    where ppOne (x,y) = hcat [pp x, ":"] $$ nest 2 (pp y) $$ " "

instance PP CompositeType where
  pp t =
    case t of
      ArrayType vs      -> vcat (map pp (Vector.toList vs))
      DictionaryType vs -> vcat (map pp (Vector.toList vs))

instance Show a => PP (Field a) where
  pp = text . show



module RTS.Annot where

import Text.PrettyPrint
import qualified Data.Set as Set

import RTS.JSON
import RTS.ParseError


data Annotation =
    RngAnnot SourceRange
  | TextAnnot String
    deriving Show

instance HasSourcePaths Annotation where
  getSourcePaths x =
    case x of
      RngAnnot a -> getSourcePaths a
      TextAnnot {} -> Set.empty

  mapSourcePaths f x =
    case x of
      RngAnnot r -> RngAnnot (mapSourcePaths f r)
      TextAnnot {} -> x

instance IsAnnotation Annotation where
  ppAnnot ann =
    case ann of
      RngAnnot r    -> ppSourceRange r
      TextAnnot txt -> text txt

instance ToJSON Annotation where
  toJSON ann =
    case ann of
      TextAnnot a -> jsString a
      RngAnnot a  -> toJSON a




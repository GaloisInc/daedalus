module Daedalus.Interp.DebugAnnot where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Encoding(encodeUtf8)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import RTS.HasInputs
import RTS.ParseError
import RTS.JSON
import RTS.Input

import qualified Daedalus.SourceRange as Src
import Daedalus.PP

import Daedalus.Value(Value,valueToDoc)
import Daedalus.AST(Name,nameRange)

data DebugAnnot = TextAnnot Text
                | CallAnnot CallSite
                | ScopeAnnot (Map Name Value)
                  deriving Show

data CallSite = CallSite Name SourceRange Input
  deriving (Show,Eq,Ord)


instance IsAnnotation DebugAnnot where
  ppAnnot ann =
    case ann of
      TextAnnot a     -> text (Text.unpack a)
      CallAnnot (CallSite _x r _i) -> ppSourceRange r
      ScopeAnnot p  -> "scope" $$ nest 2 (vcat ents)
        where ents = [ pp x <+> "=" <+> valueToDoc v
                     | (x,v) <- Map.toList p ]

instance HasInputs DebugAnnot where
  getInputs ann =
    case ann of
      TextAnnot {}  -> Map.empty
      CallAnnot s   -> getInputs s
      ScopeAnnot mp -> Map.unions (map getInputs (Map.elems mp))

instance HasInputs CallSite where
  getInputs (CallSite _ _ i) = Map.singleton (inputName i) (inputTopBytes i)

instance HasSourcePaths Src.SourceRange where
  getSourcePaths x = getSourcePaths (Src.sourceFrom x, Src.sourceTo x)
  mapSourcePaths f x =
    Src.SourceRange { sourceFrom = mapSourcePaths f (Src.sourceFrom x)
                    , sourceTo   = mapSourcePaths f (Src.sourceTo x)
                    }

instance HasSourcePaths Src.SourcePos where
  getSourcePaths x = Set.singleton (Text.unpack (Src.sourceFile x))
  mapSourcePaths f x =
    x { Src.sourceFile = Text.pack (f (Text.unpack (Src.sourceFile x))) }

instance HasSourcePaths Name where
  getSourcePaths = getSourcePaths . nameRange
  mapSourcePaths f x = x { nameRange = mapSourcePaths f (nameRange x) }

instance HasSourcePaths DebugAnnot where
  getSourcePaths ann =
    case ann of
      TextAnnot {} -> Set.empty
      CallAnnot s -> getSourcePaths s
      ScopeAnnot mp -> getSourcePaths (Map.keys mp)
  mapSourcePaths f ann =
    case ann of
      TextAnnot {} -> ann
      CallAnnot s -> CallAnnot (mapSourcePaths f s)
      ScopeAnnot mp -> ScopeAnnot (Map.mapKeys (mapSourcePaths f) mp)

instance HasSourcePaths CallSite where
  getSourcePaths (CallSite f r _) = getSourcePaths (f,r)
  mapSourcePaths f (CallSite x r i) =
    CallSite (mapSourcePaths f x) (mapSourcePaths f r) i

instance ToJSON DebugAnnot where
  toJSON ann =
    case ann of
      TextAnnot a   -> jsObject [ ("tag", jsString "label")
                                , ("content", jsString (Text.unpack a))
                                ]
      CallAnnot cs ->
        jsObject [ ("tag", jsString "call"), ("content", toJSON cs) ]

      ScopeAnnot xs ->
        jsObject [ ("tag", jsString "scope")
                 ,  ("content",
                        jsObject
                          [ (encodeUtf8 (Text.pack (show (pp k))), toJSON v)
                          | (k,v) <- Map.toList xs
                          ])
                 ]

instance ToJSON CallSite where
  toJSON (CallSite n r i) =
    jsObject [ ("function", toJSON (Text.pack (show (pp n))))
             , ("callsite", toJSON r)
             , ("input", toJSON (inputName i))
             , ("offset", toJSON (inputOffset i))
             ]




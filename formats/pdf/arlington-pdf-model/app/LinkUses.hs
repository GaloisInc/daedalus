module LinkUses where

import GHC.Records
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import qualified Daedalus.RTS as RTS
import qualified Daedalus.RTS.Vector as RTS
import Parser
import Graph

{-
type LinkUses = Map Text (Map PrimitiveType (Set (Text,Text)))

linkUses :: PDFSpec -> Map Text (Map PrimitiveType (Set (Text,Text)))
linkUses (PDFSpec mp) = groupUses (Map.foldrWithKey usesComposite [] mp)

usesToGraph :: Map Text (Map PrimitiveType (Set (Text,Text))) -> G
usesToGraph g = G { nodes = [], edges = getEdges g }
  where
  getEdges = Map.toList . Map.fromListWith (++) . concatMap edge . Map.toList
  edge (to, from) = [ (src,[to])
                    | srcs <- Map.elems from
                    , src  <- Set.toList (Set.map fst srcs)
                    ]

groupUses :: [Use] -> Map Text (Map PrimitiveType (Set (Text,Text)))
groupUses = Map.fromListWith (Map.unionWith Set.union) . map rekey
  where
  rekey u = ( linkName u
            , Map.singleton
                (usedAs u)
                (Set.singleton (typeName u, fieldName u))
            )

usesComposite :: Text -> CompositeType -> [Use] -> [Use]
usesComposite typeN (CompositeType fs) us = foldr (usesField typeN) us fs

data Use = Use
  { typeName  :: Text
  , fieldName :: Either ArrayKey DictionaryKey
  , linkName  :: Text
  , usedAs    :: PrimitiveType
  }

usesField :: Text -> Field -> [Use] -> [Use]
usesField typeN f info =
  case getField @"link" f of
    Nothing -> info
    Just ls -> foldr (addUses typeN (asText (getField @"key" f)))
                     info
                     (RTS.toList (getField @"type" f) `zip` RTS.toList ls)

addUses ::
  Text ->
  Text ->
  ( Versioned PrimitiveType
  , Maybe (RTS.Vector (Versioned (RTS.Vector (RTS.UInt 8))))
  ) -> [Use] -> [Use]
addUses typeN fieldN (ty,mb) info =
  case mb of
    Nothing   -> info
    Just opts -> foldr addLink info (RTS.toList opts)

  where
  addLink link xs =
    Use { typeName  = typeN
        , fieldName = fieldN
        , linkName  = asText (forgetVersioned link)
        , usedAs    = forgetVersioned ty
        } : xs

--------------------------------------------------------------------------------
asText :: RTS.Vector (RTS.UInt 8) -> Text
asText = Text.decodeUtf8 . RTS.vecToRep

--------------------------------------------------------------------------------
forgetVersioned :: Versioned a -> a
forgetVersioned x =
  case x of
    Versioned_Value a     -> a
    Versioned_Versioned y -> forgetWithVersion y

forgetWithVersion :: WithVersion a -> a
forgetWithVersion x =
  case x of
    WithVersion_Deprecated y    -> forgetTversioned y
    WithVersion_SinceVersion y  -> forgetTversioned y
    WithVersion_BeforeVersion y -> forgetTversioned y
    WithVersion_IsVersion y     -> forgetTversioned y

forgetTversioned :: Tversioned (Versioned a) -> a
forgetTversioned x = forgetVersioned (getField @"value" x)
-}


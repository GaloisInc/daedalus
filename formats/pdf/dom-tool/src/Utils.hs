module Utils where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.Rec

import Types


orderSpecFields :: Spec -> Spec
orderSpecFields s = s { sFields = orderFields (sFields s) }

orderFields :: [Field] -> [Field]
orderFields fs = typeToFront $ map annot $ forgetRecs $ topoOrder deps fs
  where
  deps f    = (fName f, toFName `Set.map` fieldDeps f)
  needsVal  = Set.unions (map (snd . deps) fs)

  annot f   = f { fValNeeded = fName f `Set.member` needsVal }

  -- Note: we can't depend on "PArrayAny"
  toFName i = case i of
                ArrayIx j -> PArrayIx j
                FieldIx s -> PFieldName s

  tn = PFieldName "Type"

  typeToFront xs
    | tn `Set.member` needsVal = xs
    | otherwise = case break ((== tn) . fName) xs of
                    (as,b:bs) -> b : as ++ bs
                    _         -> xs

  {- XXX: If we need the value of a field that has a default we
  should also add the dependencies from the default, so
  we need to do some sort of fixed point thing.

  However, for the moment it appears that we don't have any such
  dependencies in the fields reachable from `Catalog`, so we postpone
  doing it properly. -}


class ValDeps t where
  fieldDeps :: t -> Set FieldIx

instance ValDeps FieldIx where
  fieldDeps = Set.singleton

instance ValDeps a => ValDeps [a] where
  fieldDeps = Set.unions . map fieldDeps

instance (ValDeps a, ValDeps b) => ValDeps (a,b) where
  fieldDeps (x,y) = fieldDeps x <> fieldDeps y

instance ValDeps a => ValDeps (Maybe a) where
  fieldDeps = maybe Set.empty fieldDeps

instance ValDeps Field where
  fieldDeps fi = fieldDeps (fType fi)
      -- we only need the free values in the default if
      -- we need a value for the field at all

instance ValDeps Expr where
  fieldDeps expr =
    case expr of
      ELitI {}    -> Set.empty
      ELit {}     -> Set.empty
      ELitR {}    -> Set.empty
      EBool {}    -> Set.empty
      ELitStr {}  -> Set.empty
      ELitName {} -> Set.empty
      EArr es     -> fieldDeps es
      ValueOf x   -> fieldDeps x

instance ValDeps Constraint where
  fieldDeps ctr =
    case ctr of
      Equals e        -> fieldDeps e
      Interval _ _    -> Set.empty
      IsLessThan e    -> fieldDeps e
      IsGreaterThan e -> fieldDeps e
      Orc x y         -> fieldDeps (x,y)

instance ValDeps Type where
  fieldDeps ty =
    case ty of
      TOr x y     -> fieldDeps (x,y)
      TPrim _ c   -> fieldDeps c
      TStruct _ _ -> Set.empty

--------------------------------------------------------------------------------
tyDeps :: FoldStruct t => t -> Set String
tyDeps = foldStruct' add Set.empty
  where add _ t = Set.insert t

tyUses :: FoldStruct t => t -> Map String [StructType]
tyUses = fmap Set.toList . foldStruct' addUse Map.empty
  where
  addUse s n = Map.insertWith Set.union n (Set.singleton s)

foldStruct' :: FoldStruct t => (StructType -> String -> a -> a) -> a -> t -> a
foldStruct' f t a = foldStruct f a t

class FoldStruct t where
  foldStruct    :: (StructType -> String -> a -> a) -> t -> a -> a

instance (FoldStruct a, FoldStruct b) => FoldStruct (a,b) where
  foldStruct f (a,b) = foldStruct f a . foldStruct f b

instance FoldStruct a => FoldStruct [a] where
  foldStruct f xs = case xs of
                      []     -> id
                      x : ys -> foldStruct f (x,ys)

instance FoldStruct Spec where
  foldStruct f = foldStruct f . sFields

instance FoldStruct Field where
  foldStruct f = foldStruct f . fType

instance FoldStruct Type where
  foldStruct f ty =
    case ty of
      TOr x y      -> foldStruct f (x,y)
      TPrim {}     -> id
      TStruct t mb -> case mb of
                        Nothing -> id
                        Just n  -> f t n






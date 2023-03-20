{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- A Value with (possibly) some symbolic contents

module Talos.SymExec.SemiValue where

import qualified Data.Map       as Map
import qualified Data.Vector    as Vector
import           Data.Word      (Word8)

import           Daedalus.PP
import qualified Daedalus.Value as V

-- import Talos.Analysis.Demands
-- import qualified Data.Map as Map

-- | A value which is partially symbolic.
--
-- Invariants:
--   * values other than VValue must contain symbolic terms, and so
--     cannot be converted to a pure value;
--
--   * list-shaped values are concrete, concrete-spined, or symbolic.
--     This means consing onto a symbolic list yields a symbolic list.
--
-- The latter invariant is a simplification that in practice shouldn't
-- matter, as we rarely construct lists by consing onto symbolic lists
-- (i.e., most lists should be concrete if constructed by a parser).

data SemiValue a =
    VValue                 !V.Value
  | VOther                 !a
  | VUnionElem             !V.Label !(SemiValue a)
  -- We don't need to support partial updates (e.g. x { foo = bar }
  -- where x is symbolic) as Daedalus doesn't (yet) support updates.
  | VStruct                ![(V.Label,SemiValue a)]

  -- Bool is true if this is a builder, false if an array
  | VSequence              !Bool ![SemiValue a]
  | VMaybe                 !(Maybe (SemiValue a))
  
  -- We support symbolic keys, so we can't use Map here
  | VMap                   ![(SemiValue a, SemiValue a)]
  | VIterator              ![(SemiValue a,SemiValue a)]
    deriving (Show, Eq, Ord, Foldable, Traversable, Functor)

-- Collapses a semi value with Value leaves into a Value
toValue :: SemiValue V.Value -> V.Value
toValue sv =
  case sv of
    VValue v           -> v
    VOther v           -> v
    VUnionElem l sv'   -> V.VUnionElem l (toValue sv')
    VStruct flds       -> V.VStruct [ (l, toValue fld) | (l, fld) <- flds ]
    VSequence True vs  -> V.VBuilder (reverse (map toValue vs))
    VSequence False vs -> V.VArray (Vector.fromList (map toValue vs))
    VMaybe mv          -> V.VMaybe (toValue <$> mv)
    VMap vs            -> V.VMap (Map.fromList [ (toValue k, toValue v) | (k, v) <- vs ])
    VIterator vs       -> V.VIterator [ (toValue k, toValue v) | (k, v) <- vs ]

-- toValue :: (a -> Maybe V.Value) -> SemiValue a -> Maybe V.Value
-- toValue mk sv =
--   case sv of
--     VOther a       -> mk a
--     VUInt n v      -> Just $ V.VUInt n v
--     VSInt n v      -> Just $ V.VSInt n v
--     VInteger v     -> Just $ V.VInteger v
--     VBool b        -> Just $ V.VBool b
--     VUnionElem l v -> V.VUnionElem l <$> go v
--     VStruct flds   -> V.VStruct <$> mapM (\(l, v) -> (,) l <$> go v) flds
--     VArray  els    -> V.VArray <$> traverse go els
--     VMaybe  el     -> V.VMaybe <$> traverse go el
--     VMap    m      ->
--       V.VMap . Map.fromList <$> traverse (\(k, v) -> (,) <$> go k <*> go v) (Map.toList m)
--     VStream i     -> Just $ V.VStream i
--     VBuilder vs   -> V.VBuilder <$> traverse go vs
--     VIterator vs  -> V.VIterator <$> traverse (\(k, v) -> (,) <$> go k <*> go v) vs
--   where go = toValue mk

-- toValue :: SemiValue a -> Maybe V.Value
-- toValue (VValue v) = Just v
-- toValue _          = Nothing

fromValue :: V.Value -> SemiValue a
fromValue = VValue

toList :: SemiValue a -> Maybe [SemiValue a]
toList sv =
  case sv of
    VSequence _ xs         -> Just xs
    VValue (V.VArray v)    -> Just (map VValue (Vector.toList v))
    -- Builders are stored in reverse order.
    VValue (V.VBuilder vs) -> Just (map VValue (reverse vs))
    _ -> Nothing

toByte :: SemiValue a -> Maybe Word8
toByte (VValue v) = Just (V.valueToByte v)
toByte _          = Nothing


toMappings :: SemiValue a -> Maybe [(SemiValue a, SemiValue a)]
toMappings sv =
  case sv of
    VMap xs            -> Just xs
    VValue (V.VMap m)  -> Just [ (VValue k, VValue v) | (k, v) <- Map.toList m ]
    _ -> Nothing

-- FIXME: if there are no symbolic values, we should return a VValue
fromList :: Bool -> [SemiValue a] -> SemiValue a
fromList = VSequence 

-- satisfiesDemands :: Demand -> SemiValue a -> Bool
-- satisfiesDemands _ VValue {} = True
-- satisfiesDemands d v = -- v isn't VValue
--   case d of
--     -- c.f. invariant on SemiValue.  DAll matches only VValue, and
--     -- VValue matches everything.
--     DAll  -> False
--     DNone -> True
    
--     DUser m ->
--       let el l v' = satisfiesDemands (Map.findWithDefault DNone l m) v'
--       in case v of
--         VUnionElem l v' -> el l v'
--         VStruct flds    -> all (uncurry el) flds
--         _               -> Map.null m -- probably always false
    
--     -- DSequence b eld ->
--     --   case v of

--     DMap kv dv -> 

--     DMaybe d' ->
--       case v of
--         VMaybe Nothing   -> True
--         VMaybe (Just v') -> satisfiesDemands d' v'
--         _ -> False
    
          
instance PP a => PP (SemiValue a) where
  ppPrec n val =
    case val of
      VValue v -> pp v
      VOther v -> pp v
      VUnionElem lbl v -> braces (pp lbl <.> colon <+> pp v)
      VStruct xs      -> block "{" "," "}" (map ppF xs)
        where ppF (x,t) = pp x <.> colon <+> pp t

      VSequence _ vs ->  block "[" "," "]" (map pp vs)

      VMaybe v   -> case v of
                      Nothing -> "Nothing"
                      Just v' -> wrapIf (n > 0) ("Just" <+> ppPrec 1 v')
      VMap m -> block "{|" ", " "|}" [ ppPrec 1 k <+> "->" <+> ppPrec 1 v | (k,v) <- m ]

      VIterator vs -> block "[iterator|" ",        " "|]"
                             [ pp x <+> "->" <+> pp y | (x,y) <- vs ]


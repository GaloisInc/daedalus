{-# LANGUAGE KindSignatures, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


module Talos.Analysis.EntangledVars where

import Data.List (isPrefixOf)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.PP
-- import Daedalus.Panic

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.TraverseUserTypes

-- Two variables are entangled if information can flow between them
-- --- i.e., the choice of value for one variable may impact the
-- choices for the other.

data BaseEntangledVar = ResultVar | ProgramVar Name
  deriving Eq

data EntangledVar = EntangledVar { baseVar :: BaseEntangledVar, fields :: [Label] }
  deriving Eq -- We define our own Ord as we want to ensure ResultVar < everything

-- | `isPrefix x y` is true when `y` is a subfield of x, it is also
-- reflexive, transitive, partial (basically a partial <=) 
isPrefix :: EntangledVar -> EntangledVar -> Bool
isPrefix (EntangledVar bv1 fs1) (EntangledVar bv2 fs2) = bv1 == bv2 && fs1 `isPrefixOf` fs2

--------------------------------------------------------------------------------
-- Field sets, used to represent all subfields for a base entangled var.
--
-- Basically if we have x.a.b then there will be "a" -> "b" -> {}

data FieldSet = FieldSet { getFieldSet :: Map Label FieldSet }
  deriving Eq

-- Not exactly empty, just has no children
emptyFieldSet :: FieldSet
emptyFieldSet = FieldSet Map.empty

mergeFieldSet :: FieldSet -> FieldSet -> FieldSet
mergeFieldSet (FieldSet fs1) (FieldSet fs2)
  | Map.null fs1 = mempty
  | Map.null fs2 = mempty
  | otherwise    = FieldSet $ Map.unionWith mergeFieldSet fs1 fs2

explodeFieldSet :: FieldSet -> [ [Label] ]
explodeFieldSet fs = [ l : ls | (l, fs') <- Map.toList (getFieldSet fs), ls <- explodeFieldSet fs' ]

instance Semigroup FieldSet where
  (<>) = mergeFieldSet
  
instance Monoid FieldSet where
  mempty = emptyFieldSet

--------------------------------------------------------------------------------
-- Emtangled vars

newtype EntangledVars = EntangledVars { getEntangledVars :: Map BaseEntangledVar FieldSet }
  deriving (Eq)

freeEntangledVars :: FreeVars a => a -> EntangledVars
freeEntangledVars = EntangledVars . Map.fromSet (\_ -> emptyFieldSet ) . Set.map ProgramVar . freeVars

instance Semigroup EntangledVars where
  (<>) = mergeEntangledVars
  
instance Monoid EntangledVars where
  mempty = EntangledVars Map.empty

mergeEntangledVars :: EntangledVars -> EntangledVars -> EntangledVars
mergeEntangledVars evs1 evs2 =
  EntangledVars $ Map.unionWith mergeFieldSet (getEntangledVars evs1) (getEntangledVars evs2)

singletonEntangledVars :: EntangledVar -> EntangledVars
singletonEntangledVars (EntangledVar bv fs) = EntangledVars $ Map.singleton bv fs'
  where
    fs' = foldr (\a b -> FieldSet $ Map.singleton a b) emptyFieldSet fs
  
mergeEntangledVarss :: [EntangledVars] -> EntangledVars
mergeEntangledVarss = mconcat

insertEntangledVar :: EntangledVar -> EntangledVars -> EntangledVars
insertEntangledVar ev evs = singletonEntangledVars ev <> evs

entangledVars :: EntangledVars -> [EntangledVar]
entangledVars evs = [ EntangledVar bv fs
                    | (bv, fs0) <- Map.toList (getEntangledVars evs)
                    , fs <- explodeFieldSet fs0
                    ]

-- deleteEntangledVar :: EntangledVar -> EntangledVars -> EntangledVars
-- deleteEntangledVar ev evs = EntangledVars (Set.delete ev (getEntangledVars evs))

-- sizeEntangledVars :: EntangledVars -> Int
-- sizeEntangledVars = Set.size . getEntangledVars

-- substEntangledVars :: (EntangledVar -> EntangledVars) -> EntangledVars -> EntangledVars
-- substEntangledVars s (EntangledVars evs) =
--   EntangledVars $ Set.unions (map (getEntangledVars . s) (Set.toList evs))

-- intersects :: EntangledVars -> EntangledVars -> Bool
-- intersects e1 e2 = not (Set.disjoint (getEntangledVars e1) (getEntangledVars e2))

-- memberEntangledVars :: EntangledVar -> EntangledVars -> Bool
-- memberEntangledVars ev evs = ev `Set.member` getEntangledVars evs

-- programVars :: EntangledVars -> Set Name
-- programVars (EntangledVars vs) =
--   Set.mapMonotonic fromProgramVar (Set.delete ResultVar vs)
--   where
--     fromProgramVar (ProgramVar v) = v
--     fromProgramVar ResultVar      = panic "Impossible" []


--------------------------------------------------------------------------------
-- Class instances

instance Ord BaseEntangledVar where
  ev <= ev' =
    case (ev, ev') of
      (ResultVar, _) -> True
      (ProgramVar v, ProgramVar v') -> v <= v'
      _ -> False

instance Ord EntangledVar where
  (EntangledVar bv1 fs1) <= (EntangledVar bv2 fs2)
    | bv1 == bv2 = fs1 <= fs2
    | otherwise  = bv1 <= bv2

instance PP BaseEntangledVar where
  pp (ResultVar) = "$result"
  pp (ProgramVar v) = pp v

instance PP EntangledVar where
  pp (EntangledVar bv []) = pp bv
  pp (EntangledVar bv fs) = pp bv <> "." <> hcat (punctuate "." (map pp fs))

instance PP EntangledVars where
  pp evs = lbrace <> commaSep (map pp (entangledVars evs)) <> rbrace

instance TraverseUserTypes BaseEntangledVar where
  traverseUserTypes f ev =
    case ev of
      ResultVar -> pure ev
      ProgramVar v -> ProgramVar <$> traverseUserTypes f v

instance TraverseUserTypes EntangledVar where
  traverseUserTypes f (EntangledVar bv fs) = EntangledVar <$> traverseUserTypes f bv <*> pure fs

instance TraverseUserTypes EntangledVars where
  traverseUserTypes f evs =
    EntangledVars . Map.fromList <$> sequenceA [ (,) <$> traverseUserTypes f k <*> pure v | (k, v) <- Map.toList (getEntangledVars evs) ]

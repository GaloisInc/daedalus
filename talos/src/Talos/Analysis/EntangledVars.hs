{-# LANGUAGE KindSignatures, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


module Talos.Analysis.EntangledVars where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Parameterized.Some

import Daedalus.Panic
import Daedalus.PP
import Daedalus.Type.AST
import Daedalus.Type.Free

-- Two variables are entangled if information can flow between them
-- --- i.e., the choice of value for one variable may impact the
-- choices for the other.

data EntangledVar =
  ResultVar Type 
  | ProgramVar (TCName Value)
  deriving (Show, Eq) -- We define our own Ord as we want to ensure ResultVar < everything


--
-- FIXME: represent as a dep. graph?
newtype EntangledVars = EntangledVars { getEntangledVars :: Set EntangledVar }
  deriving (Eq)

tcEntangledVars :: TCFree a => a-> EntangledVars
tcEntangledVars = EntangledVars . Set.map (ProgramVar . unSome) . tcFree
  where
    unSome :: Some TCName -> TCName Value
    unSome (Some t@(TCName {tcNameCtx = AValue})) = t
    unSome t = panic "BUG: saw a non-Value variable" [ viewSome showPP t ]


instance Semigroup EntangledVars where
  (<>) = mergeEntangledVars
  
instance Monoid EntangledVars where
  mempty = EntangledVars Set.empty

mergeEntangledVars :: EntangledVars -> EntangledVars -> EntangledVars
mergeEntangledVars evs1 evs2 = EntangledVars (Set.union (getEntangledVars evs1) (getEntangledVars evs2))

singletonEntangledVars :: EntangledVar -> EntangledVars
singletonEntangledVars = EntangledVars . Set.singleton

-- More efficient than mconcat?
mergeEntangledVarss :: [EntangledVars] -> EntangledVars
mergeEntangledVarss evss = EntangledVars (Set.unions (map getEntangledVars evss))

insertEntangledVar :: EntangledVar -> EntangledVars -> EntangledVars
insertEntangledVar ev evs = EntangledVars (Set.insert ev (getEntangledVars evs))

deleteEntangledVar :: EntangledVar -> EntangledVars -> EntangledVars
deleteEntangledVar ev evs = EntangledVars (Set.delete ev (getEntangledVars evs))

sizeEntangledVars :: EntangledVars -> Int
sizeEntangledVars = Set.size . getEntangledVars

substEntangledVars :: (EntangledVar -> EntangledVars) -> EntangledVars -> EntangledVars
substEntangledVars s (EntangledVars evs) =
  EntangledVars $ Set.unions (map (getEntangledVars . s) (Set.toList evs))

intersects :: EntangledVars -> EntangledVars -> Bool
intersects e1 e2 = not (Set.disjoint (getEntangledVars e1) (getEntangledVars e2))

memberEntangledVars :: EntangledVar -> EntangledVars -> Bool
memberEntangledVars ev evs = ev `Set.member` getEntangledVars evs

--------------------------------------------------------------------------------
-- Class instances

instance TypeOf EntangledVar where
  typeOf v = case v of
               ResultVar ty  -> ty
               ProgramVar pv -> typeOf pv

instance Ord EntangledVar where
  ev <= ev' =
    case (ev, ev') of
      -- FIXME: this is a bit dangerous, 
      (ResultVar ty, ResultVar ty')
        | ty == ty' -> True
        | otherwise -> panic "Comparing incomarable result variables" [show (pp ev), show (pp ev')]
      (ResultVar {}, _) -> True
      (ProgramVar v, ProgramVar v') -> v <= v'
      _ -> False

instance PP EntangledVar where
  pp (ResultVar {}) = "$result"
  pp (ProgramVar v) = pp v

instance PP EntangledVars where
  pp evs = lbrace <> commaSep (map pp (Set.toList (getEntangledVars evs))) <> rbrace

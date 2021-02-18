{-# LANGUAGE KindSignatures, GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- This module contains the datastructure representing future path
-- constraints for a variable.  A path may be thought of as an
-- abstraction of the input DDL program.

module Talos.Analysis.Domain where

import Data.List (partition, foldl1')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

import Daedalus.PP
import Daedalus.Panic

import Talos.Analysis.EntangledVars
import Talos.Analysis.PathSet

--------------------------------------------------------------------------------
-- Domains

-- Invariants: forall (vs1, ps1) (vs2, ps2) : elements, vs1 \inter vs2 = {}
--             forall (vs, ps) : elements, vs = tcFree ps

newtype Domain a = Domain { elements :: [ (EntangledVars, FuturePathSet a) ] } 

mergeDomain :: Domain a -> Domain a -> Domain a
mergeDomain dL dR = Domain (go (elements dL) (elements dR))
  where
    go [] d2 = d2
    -- d might overlap with multiple elements from d2 (but none from d1') 
    go ((els, ps) : d1') d2 = go d1' ((newEls, newPs) : indep)
      where
        newPs = foldl mergeFuturePathSet ps depPs
        newEls = mergeEntangledVarss (els : depEls)
        (depEls, depPs) = unzip dep
        (dep, indep) = partition (\(els', _) -> els `intersects` els') d2

-- FIXME: does this satisfy the laws?  Maybe for a sufficiently
-- general notion of equality?
instance Semigroup (Domain a) where
  (<>) = mergeDomain

emptyDomain :: Domain a 
emptyDomain = Domain []

singletonDomain :: EntangledVars -> FuturePathSet a -> Domain a
singletonDomain vs fp = Domain [ (vs, fp) ]

instance Monoid (Domain a) where
  mempty = emptyDomain

dontCareD :: Int -> Domain a -> Domain a
dontCareD n d = Domain [ (evs, dontCare n fp) | (evs, fp) <- elements d ]

nullDomain :: Domain a -> Bool
nullDomain (Domain []) = True
nullDomain _           = False

mapDomain :: (FuturePathSet a -> FuturePathSet a) -> Domain a -> Domain a
mapDomain f = Domain . map (\(x, y) -> (x, f y)) . elements

squashDomain :: Domain a -> Domain a
squashDomain d@(Domain []) = d
squashDomain (Domain els) =
  singletonDomain (mergeEntangledVarss fvss)
                  (foldl1' mergeFuturePathSet fpss)
  where
    (fvss, fpss) = unzip els

domainEqv :: Domain a -> Domain a -> Bool
domainEqv dL dR = go (elements dL) (elements dR)
  where
    go [] [] = True
    go [] _  = False
    go ((els, ps) : d1') d2 =
      case partition ((==) els . fst) d2 of
        ([], _)           -> False
        ([(_, ps')], d2') -> futurePathEqv ps ps' && go d1' d2'
        _ -> panic "Malformed domain" []

-- Turns a domain into a map from a representative entangle var to the
-- entangled vars and FPS.
explodeDomain :: Domain a -> Map EntangledVar (EntangledVars, FuturePathSet a)
explodeDomain d = Map.fromList [ (fmin (getEntangledVars (fst el)), el)
                               | el <- elements d ]
  where -- FIXME
    fmin s | Set.null s = panic "empty domain?" [showPP d]
           | otherwise  = Set.findMin s
--------------------------------------------------------------------------------
-- Helpers

lookupVar :: EntangledVar -> Domain a -> Maybe (EntangledVars, FuturePathSet a)
lookupVar n ds = listToMaybe [ d | d@(ns, _) <- elements ds, n `Set.member` getEntangledVars ns ]
  
splitOnVar :: EntangledVar -> Domain a -> (Maybe (EntangledVars, FuturePathSet a), Domain a)
splitOnVar n ds =
  case nin of
    []  -> (Nothing, ds)
    [d] -> (Just d, Domain nout)
    _   -> panic "Multiple occurences of a variable in a domain" []
  where
    (nin, nout) = partition (\(ns, _) -> n `Set.member` getEntangledVars ns) (elements ds)

-- doesn't merge
primAddDomainElement :: (EntangledVars, FuturePathSet a) -> Domain a -> Domain a
primAddDomainElement d ds = Domain (d : elements ds)

--------------------------------------------------------------------------------
-- Class instances

instance PP (Domain a) where
  pp d = vcat (map pp_el (elements d))
    where
      pp_el (vs, fp) = pp vs <> " => " <> pp fp

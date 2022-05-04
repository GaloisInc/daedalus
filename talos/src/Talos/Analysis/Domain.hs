{-# LANGUAGE KindSignatures, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

-- This module contains the datastructure representing future path
-- constraints for a variable.  A path may be thought of as an
-- abstraction of the input DDL program.

module Talos.Analysis.Domain where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (partition, foldl1')
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)

import Daedalus.Core (Name, Expr, ByteSet)
import Daedalus.PP
import Daedalus.Panic

import Talos.Analysis.Slice

--------------------------------------------------------------------------------
-- Abstract Environments

class AbsEnv ae where
  type AbsPred ae
  absPre     :: AbsPred ae -> Expr    -> Domain ae
  absByteSet :: AbsPred ae -> ByteSet -> Domain ae
  absProj    :: Name -> ae -> Maybe (ae, AbsPred ae)
  absUnion   :: ae -> ae -> ae
  
  
--------------------------------------------------------------------------------
-- Domains

-- Invariants: forall (vs1, ps1) (vs2, ps2) : elements, vs1 \inter vs2 = {}
--             forall (vs, ps) : elements, vs = tcFree ps

data GuardedSlice ae = GuardedSlice
  { gsEnv   :: ae
  , gsSlice :: Slice (AbsPred ae)
  }
  deriving (Generic)

mapGuardedSlice :: (Slice (AbsPred ae) -> Slice (AbsPred ae)) -> GuardedSlice ae -> GuardedSlice ae
mapGuardedSlice f gs = gs { gsSlice = f (gsSlice gs) }

deriving instance (NFData (AbsPred ae), NFData ae) => NFData (GuardedSlice ae)

data Domain ae = Domain
  { elements      :: [ GuardedSlice ae ]
  -- ^ In-progress (open, containing free vars) elements
  , closedElements :: Map Name (Slice (AbsPred ae))
  -- ^ Finalised (closed, no free vars, internal) elements
  , resultElement :: Maybe (GuardedSlice ae)
  -- ^ The slice containing the result, if a result is required.
  }
  deriving (Generic)

deriving instance (NFData (AbsPred ae), NFData ae) => NFData (Domain ae)

-- instance Merge Domain where
--   merge dL dR = Domain (go (elements dL) (elements dR))
--     where
--       go [] d2 = d2
--       -- d might overlap with multiple elements from d2 (but none from d1') 
--       go ((els, ps) : d1') d2 = go d1' ((newEls, newPs) : indep)
--         where
--           newPs = foldl merge ps depPs
--           newEls = mergeEntangledVarss (els : depEls)
--           (depEls, depPs) = unzip dep
--           (dep, indep) = partition (\(els', _) -> els `intersects` els') d2

-- FIXME: does this satisfy the laws?  Maybe for a sufficiently
-- general notion of equality?
-- instance Semigroup Domain where
--   (<>) = merge

emptyDomain :: Domain ae
emptyDomain = Domain
  { elements = []
  , closedElements = Map.empty
  , resultElement = Nothing
  }

singletonDomain :: GuardedSlice ae -> Domain ae
singletonDomain el = Domain
  { elements = [el]
  , closedElements = Map.empty
  , resultElement = Nothing
  }

singletonResultDomain :: GuardedSlice ae -> Domain ae
singletonResultDomain el = Domain
  { elements = []
  , closedElements = Map.empty
  , resultElement = Just el
  }

-- instance Monoid Domain where
--   mempty = emptyDomain

-- dontCareD :: Int -> Domain -> Domain
-- dontCareD n d = Domain [ (evs, sDontCare n fp) | (evs, fp) <- elements d ]

nullDomain :: Domain ae -> Bool
nullDomain (Domain [] ce Nothing) = Map.null ce
nullDomain _           = False

-- mapDomain :: (EntangledVars -> Slice -> Slice) -> Domain -> Domain
-- mapDomain f = Domain . map (\(x, y) -> (x, f x y)) . elements


-- substResultInDomain :: EntangledVar -> Domain -> Domain
-- substResultInDomain x d =
--   case splitOnVarWith isResult d of
--     (Nothing, _) -> d
--     (Just (evs, sl), d') ->
--       singletonDomain (substEntangledVars (\ev -> Set.singleton $ if isResult ev then x else ev) evs, sl)
--       <> d'
--     where
--       isResult (ResultVar {}) = True
--       isResult _              = False

-- squashDomain :: Domain -> Domain
-- squashDomain d@(Domain []) = d
-- squashDomain (Domain els) =
--   singletonDomain (mergeEntangledVarss fvss)
--                   (foldl1' merge sls)
--   where
--     (fvss, sls) = unzip els

-- domainEqv :: Domain -> Domain -> Bool
-- domainEqv dL dR = go (elements dL) (elements dR)
--   where
--     go [] [] = True
--     go [] _  = False
--     go ((els, ps) : d1') d2 =
--       case partition ((==) els . fst) d2 of
--         ([], _)           -> False
--         ([(_, ps')], d2') -> eqv ps ps' && go d1' d2'
--         _ -> panic "Malformed domain" []

-- Turns a domain into a map from a representative entangle var to the
-- entangled vars and FPS.
-- explodeDomain :: Domain -> [(EntangledVars, Slice)]
-- explodeDomain d = elements d

--------------------------------------------------------------------------------
-- Helpers

-- Look up exactly the variable passed in (i.e., we don't check if it
-- is covered by another variable)
-- lookupVar :: EntangledVar -> Domain -> Maybe (EntangledVars, Slice)
-- lookupVar n ds = Map.lookup n (explodeDomain ds)

-- domainElement :: EntangledVars -> Domain -> Maybe Slice
-- domainElement evs d = lookup evs (elements d)

--  | If this returns [] then the variable isn't mapped; it can also
-- return [emptyFieldSet] which means we care about all the children
-- (or it is not a struct)

-- domainFileSets :: BaseEntangledVar -> Domain -> [FieldSet]
-- domainFileSets bv ds = mapMaybe (lookupBaseEV bv . fst) (elements ds)

-- Removes bv from the domain, returning any slices rooted at bv

partitionDomain :: AbsEnv ae => Name ->
                   Domain ae ->
                   ( [ (Bool, (GuardedSlice ae, AbsPred ae)) ], Domain ae )
partitionDomain x d = ( matching', d' )
  where
    matching' = resultMatch ++ [ (False, r) | r <- matching ]
    
    (matching, nonMatching) = partitionEithers (map part (elements d))
    part gs = case absProj x (gsEnv gs) of
      Nothing      -> Right gs
      Just (e, p)  -> Left (gs { gsEnv = e }, p)
      
    (resultMatch, m_resultNonMatch) = case part <$> resultElement d of
      Just (Left r)  -> ( [(True, r)], Nothing )
      Just (Right r) -> ( [],          Just r )
      _              -> ( [],          Nothing )
      
    d' = d { elements = nonMatching, resultElement = m_resultNonMatch }


-- Maps over non-closed slices
mapSlices :: (Slice (AbsPred ae) -> Slice (AbsPred ae)) -> Domain ae -> Domain ae
mapSlices f d = d { elements      = map (mapGuardedSlice f) (elements d)
                  , resultElement = mapGuardedSlice f <$> resultElement d
                  }
      
-- splitRemoveVar :: BaseEntangledVar -> Domain -> ([(FieldSet, Slice)], Domain)
-- splitRemoveVar bv ds = (nin, Domain nout)
--   where
--     (nin, nout) =
--       partitionEithers [ case m_fs of
--                            Just fset | nullEntangledVars evs'-> Left  (fset, sl)
--                            _                                 -> Right (evs', sl)
--                        | (evs, sl) <- elements ds
--                        , let (m_fs, evs') = deleteBaseEV bv evs
--                        ]

-- doesn't merge
-- primAddDomainElement :: (EntangledVars, Slice) -> Domain -> Domain
-- primAddDomainElement d ds = Domain (d : elements ds)

--------------------------------------------------------------------------------
-- Debugging etc.

-- domainInvariant :: Domain -> Bool
-- domainInvariant dom = all (\(evs, _sl) -> evs /= mempty) (elements dom)

--------------------------------------------------------------------------------
-- Class instances

-- instance PP Domain where
--   pp d = vcat (map pp_el (elements d))
--     where
--       pp_el (vs, fp) = pp vs <> " => " <> pp fp

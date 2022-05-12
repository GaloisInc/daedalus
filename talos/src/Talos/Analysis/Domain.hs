{-# LANGUAGE KindSignatures, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

-- This module contains the datastructure representing future path
-- constraints for a variable.  A path may be thought of as an
-- abstraction of the input DDL program.

module Talos.Analysis.Domain where

import           Control.DeepSeq       (NFData)
import           Data.Either           (partitionEithers)
import           Data.Function         (on)
import           Data.List             (foldl1', partition)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           GHC.Generics          (Generic)

import           Daedalus.Core         (Name)
import           Daedalus.Panic
import           Daedalus.PP

import           Talos.Analysis.AbsEnv
import           Talos.Analysis.Eqv
import           Talos.Analysis.Merge
import           Talos.Analysis.Slice

--------------------------------------------------------------------------------
-- Domains

data GuardedSlice ae = GuardedSlice
  { gsEnv   :: ae
  , gsPred  :: [AbsPred ae]
  -- ^ The predicate(s) used to generate the result in this slice.  If
  -- non-empty, this slice returns a value (i.e., the last grammar
  -- isn't a hole or a non-result call)
  , gsSlice :: Slice
  }
  deriving (Generic)

instance AbsEnv ae => Merge (GuardedSlice ae) where
  merge gs gs' = GuardedSlice
    { gsEnv  = gsEnv gs <> gsEnv gs'
    , gsPred = gsPred gs <> gsPred gs'
    , gsSlice = merge (gsSlice gs) (gsSlice gs')
    }

instance AbsEnv ae => Eqv (GuardedSlice ae) where
  eqv gs gs' =
    eqv (gsEnv gs) (gsEnv gs') &&
    gsPred gs == gsPred gs' && -- FIXME, we have (==) over preds
    eqv (gsSlice gs) (gsSlice gs')

mapGuardedSlice :: (Slice -> Slice) ->
                   GuardedSlice ae -> GuardedSlice ae
mapGuardedSlice f gs = gs { gsSlice = f (gsSlice gs) }

bindGuardedSlice :: AbsEnv ae => Name ->
                    GuardedSlice ae -> GuardedSlice ae -> GuardedSlice ae
bindGuardedSlice x lhs rhs = GuardedSlice
  { gsEnv = gsEnv lhs <> gsEnv rhs
  , gsPred = gsPred rhs
  , gsSlice = SDo x (gsSlice lhs) (gsSlice rhs)
  }

-- A guarded slice is closed if it is not a result slice, and it has
-- no free variables.  We use absNullEnv as a proxy for empty free vars.
closedGuardedSlice :: AbsEnv ae => GuardedSlice ae -> Bool
closedGuardedSlice gs = null (gsPred gs) && absNullEnv (gsEnv gs)

deriving instance (NFData (AbsPred ae), NFData ae) => NFData (GuardedSlice ae)

data Domain ae = Domain
  { elements      :: [ GuardedSlice ae ]
  -- ^ In-progress (open, containing free vars) elements
  , closedElements :: Map Name [Slice]
  -- ^ Finalised (closed, no free vars, internal) elements.  We may
  -- have multiple starting from a single var, e.g.
  --
  --    x = ParseTuple ...
  --    Guard (x.fst > 0)
  --    Guard (x.snd > 42)
  --    ... (x not free) ...
  --
  -- might yield [ SDo x (ParseTuple | fst) (SAssertion (x.fst > 0))
  --             , SDo x (ParseTuple | snd) (SDo _ SHole (SAssertion (x.snd > 0)))]
  -- for x
  }
  deriving (Generic)

deriving instance (NFData (AbsPred ae), NFData ae) => NFData (Domain ae)

instance AbsEnv ae => Merge (Domain ae) where
  merge dL dR = Domain
    { elements       = go (elements dL) (elements dR)
    , closedElements = Map.unionWith (<>) (closedElements dL) (closedElements dR)
    }
    where
      go [] d2 = d2
      -- d might overlap with multiple elements from d2 (but none from d1') 
      go (gs : d1') d2 = go d1' (newgs : indep)
        where
          newgs = foldl merge gs dep
          (indep, dep) = partition ((absEnvOverlaps `on` gsEnv) gs) d2

-- FIXME: does this satisfy the laws?  Maybe for a sufficiently
-- general notion of equality?
-- instance Semigroup Domain where
--   (<>) = merge

emptyDomain :: Domain ae
emptyDomain = Domain
  { elements = []
  , closedElements = Map.empty
  }

singletonDomain :: AbsEnv ae => GuardedSlice ae -> Domain ae
singletonDomain gs
  -- If the element is not a result element (i.e., has no preds
  -- associated with it) and has a null env (i.e., no free vars) it is
  -- closed, otherwise it gets put in elements.
  --
  -- A non-result element with a null environment must start with SDo
  | closedGuardedSlice gs, SDo x _ _ <- gsSlice gs =
      Domain { elements = [], closedElements = Map.singleton x [gsSlice gs] }

  | closedGuardedSlice gs = panic "Expecting a slice headed by a SDo" [showPP gs]
  | otherwise = Domain { elements = [gs], closedElements = Map.empty }

-- | Constructs a domain from the possibly-overlapping elements.
domainFromElements :: AbsEnv ae => [GuardedSlice ae] -> Domain ae
domainFromElements []  = emptyDomain
domainFromElements els = foldl1 merge (map singletonDomain els)

-- singletonResultDomain :: GuardedSlice ae -> Domain ae
-- singletonResultDomain el = Domain
--   { elements = []
--   , closedElements = Map.empty
--   , resultElement = Just el
--   }

-- instance Monoid Domain where
--   mempty = emptyDomain

-- dontCareD :: Int -> Domain -> Domain
-- dontCareD n d = Domain [ (evs, sDontCare n fp) | (evs, fp) <- elements d ]

nullDomain :: Domain ae -> Bool
nullDomain (Domain [] ce) = Map.null ce
nullDomain _              = False

closedDomain :: Domain ae -> Bool
closedDomain (Domain [] _) = True
closedDomain _             = False

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

squashDomain :: AbsEnv ae => Domain ae -> Domain ae
squashDomain d@(Domain { elements = []}) = d
squashDomain d = d { elements = [ foldl1' merge (elements d) ] }

-- domainEqv :: Domain ae -> Domain ae -> Bool
-- domainEqv dL dR = go (elements dL) (elements dR)
--   where
--     go [] [] = True
--     go [] _  = False
--     go ((els, ps) : d1') d2 =
--       case partition ((==) els . fst) d2 of
--         ([], _)           -> False
--         ([(_, ps')], d2') -> eqv ps ps' && go d1' d2'
--         _ -> panic "Malformed domain" []

-- This is maybe too strict, as we require that the order of elements is the same.
domainEqv :: AbsEnv ae => Domain ae -> Domain ae -> Bool
domainEqv dL dR =
  elements dL `eqv` elements dR &&
  eqv (closedElements dL) (closedElements dR)

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

partitionDomainForVar :: AbsEnv ae => Name ->
                         Domain ae ->
                         ( [ (GuardedSlice ae, AbsPred ae) ], Domain ae )
partitionDomainForVar x d = ( matching, d' )
  where
    (matching, nonMatching) = partitionEithers (map part (elements d))
    part gs = case absProj x (gsEnv gs) of
      Nothing      -> Right gs
      Just (e, p)  -> Left (gs { gsEnv = e }, p)

    d' = d { elements = nonMatching }

partitionDomainForResult :: AbsEnv ae =>
                            Domain ae ->
                            ( [ GuardedSlice ae ], Domain ae )
partitionDomainForResult d = ( matching, d' )
  where
    (nonMatching, matching) = partition (null . gsPred) (elements d)
    d' = d { elements = nonMatching }

-- Maps over non-closed slices
mapSlices :: (Slice -> Slice) -> Domain ae -> Domain ae
mapSlices f d = d { elements = map (mapGuardedSlice f) (elements d) }

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

instance AbsEnv ae => PP (GuardedSlice ae) where
  pp gs = vcat [ pp (gsEnv gs)
               , "For" <+> brackets (commaSep $ map pp (gsPred gs))
               , pp (gsSlice gs)  ]

instance AbsEnv ae => PP (Domain ae) where
  pp d =
    hang "Open" 2 (vcat (map pp (elements d)))
    $+$
    hang "Closed" 2 (vcat (map go (Map.toList (closedElements d))))
    where
      go (k, v) = hang (pp k) 2 (bullets (map pp v))

-- instance PP Domain where
--   pp d = vcat (map pp_el (elements d))
--     where
--       pp_el (vs, fp) = pp vs <> " => " <> pp fp

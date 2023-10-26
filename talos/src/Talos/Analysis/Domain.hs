{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This module contains the datastructure representing future path
-- constraints for a variable.  A path may be thought of as an
-- abstraction of the input DDL program.

module Talos.Analysis.Domain where

import           Control.DeepSeq                 (NFData)
import           Control.Lens                    (_1, _Just, at, preview, (%~),
                                                  (&), (<>~))
import           Control.Monad                   (guard)
import           Data.Either                     (partitionEithers)
import           Data.Foldable                   (toList)
import           Data.Functor                    (($>))
import           Data.Generics.Labels            ()
import           Data.List                       (foldl1', partition)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (isNothing)
import qualified Data.Set                        as Set
import           GHC.Generics                    (Generic)

import           Daedalus.Core                   (FName, Name)
import           Daedalus.Core.CFG               (NodeID)
import           Daedalus.Core.Free              (FreeVars (..))
import           Daedalus.Core.TraverseUserTypes (TraverseUserTypes (..))
import           Daedalus.Panic                  (panic)
import           Daedalus.PP

import           Talos.Analysis.AbsEnv
import           Talos.Analysis.Eqv
import           Talos.Analysis.Merge
import           Talos.Analysis.SLExpr           (SLExpr)
import           Talos.Analysis.Slice

--------------------------------------------------------------------------------
-- Domains

data GuardedSlice ae = GuardedSlice
  { gsEnv   :: ae
  , gsBoundedStream :: Bool
  -- ^ The slice contains size information because it may be in one
  -- (or many) constrained streams.  This counts as a meta-variable in
  -- that if this is true then the slice is not closed (it gets closed
  -- when it reaches the dominator for the SetStream node).  This
  -- propagates backwards: in a bind, this is set by the lhs.
  , gsPred  :: Maybe (AbsPred ae)
  -- ^ The predicate(s) used to generate the result in this slice.  If
  -- non-empty, this slice returns a value (i.e., the last grammar
  -- isn't a hole or a non-result call)
  , gsSlice :: Slice
  , gsDominator :: NodeID
  -- ^ The node which dominates this slice (usually a Do, or a SetStream)
  }
  deriving (Generic)

instance AbsEnv ae => Merge (GuardedSlice ae) where
  merge gs gs' = GuardedSlice
    { gsEnv  = gsEnv gs `merge` gsEnv gs'
    , gsBoundedStream = gsBoundedStream gs || gsBoundedStream gs'
    , gsPred = gsPred gs `merge` gsPred gs'
    , gsSlice = merge (gsSlice gs) (gsSlice gs')
    , gsDominator = gsDominator gs -- should equal gsDominator gs'
    }

instance AbsEnv ae => Eqv (GuardedSlice ae) where
  eqv gs gs' =
    eqv (gsEnv gs) (gsEnv gs') &&
    gsBoundedStream gs == gsBoundedStream gs' &&
    gsPred gs == gsPred gs' && -- FIXME, we have (==) over preds
    eqv (gsSlice gs) (gsSlice gs')    
    -- No need to check gsDominator as they should always be the same.

mapGuardedSlice :: (Slice -> Slice) ->
                   GuardedSlice ae -> GuardedSlice ae
mapGuardedSlice f gs = gs { gsSlice = f (gsSlice gs) }

bindGuardedSlice :: AbsEnv ae => NodeID -> Maybe Name ->
                    GuardedSlice ae -> GuardedSlice ae -> GuardedSlice ae
bindGuardedSlice dom m_x lhs rhs = GuardedSlice
  { gsEnv = gsEnv lhs `merge` gsEnv rhs
  -- We can only clear this on a transition in the BoundedStream
  -- result.
  , gsBoundedStream = gsBoundedStream lhs || gsBoundedStream rhs
  , gsPred = gsPred rhs
  , gsSlice = SDo m_x (gsSlice lhs) (gsSlice rhs)
  , gsDominator = dom
  }

-- A guarded slice is closed if it is not a result slice, and it has
-- no free variables.  We use absNullEnv as a proxy for empty free vars.
closedGuardedSlice :: AbsEnv ae => GuardedSlice ae -> Bool
closedGuardedSlice gs = isNothing (gsPred gs) && absNullEnv (gsEnv gs) && not (gsBoundedStream gs)

deriving instance (NFData (AbsPred ae), NFData ae) => NFData (GuardedSlice ae)

data Domain ae = Domain
  { elements      :: [ GuardedSlice ae ]
  -- ^ In-progress (open, containing free vars) elements
  , closedElements :: Map NodeID [Slice]
  -- ^ Finalised (closed, no free vars, internal) elements.  We may
  -- have multiple starting from a single node, e.g.
  --
  --    x = ParseTuple ...
  --    Guard (x.fst > 0)
  --    Guard (x.snd > 42)
  --    ... (x not free) ...
  --
  -- might yield [ SDo x (ParseTuple | fst) (SAssertion (x.fst > 0))
  --             , SDo x (ParseTuple | snd) (SDo _ SHole (SAssertion (x.snd > 0)))]
  }
  deriving (Generic)

deriving instance (NFData (AbsPred ae), NFData ae) => NFData (Domain ae)

instance AbsEnv ae => Merge (Domain ae) where
  merge dL dR = Domain
    { elements       = mergeOverlapping overlaps (elements dL) (elements dR)
    , closedElements = Map.unionWith (<>) (closedElements dL) (closedElements dR)
    }
    where
      -- Two slices need to be merged if the environment overlaps or
      -- if they both include stream bound information.
      overlaps gs gs' = absEnvOverlaps (gsEnv gs) (gsEnv gs')
                        || (gsBoundedStream gs && gsBoundedStream gs')

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
  | closedGuardedSlice gs = mkSingleton
  | otherwise = Domain { elements = [gs], closedElements = Map.empty }
  where
    mkSingleton =
      Domain { elements = [], closedElements = Map.singleton (gsDominator gs) [gsSlice gs] }

-- | Constructs a domain from the possibly-overlapping elements.
domainFromElements :: AbsEnv ae => [GuardedSlice ae] -> Domain ae
domainFromElements = foldl merge emptyDomain . map singletonDomain

nullDomain :: Domain ae -> Bool
nullDomain (Domain [] ce) = Map.null ce
nullDomain _              = False

closedDomain :: Domain ae -> Bool
closedDomain (Domain [] _) = True
closedDomain _             = False

squashDomain :: AbsEnv ae => Domain ae -> Domain ae
squashDomain d@(Domain { elements = []}) = d
squashDomain d = d { elements = [ foldl1' merge (elements d) ] }

-- This is maybe too strict, as we require that the order of elements is the same.
domainEqv :: AbsEnv ae => Domain ae -> Domain ae -> Bool
domainEqv dL dR =
  elements dL `eqv` elements dR &&
  eqv (closedElements dL) (closedElements dR)

--------------------------------------------------------------------------------
-- Helpers

-- | Removes `x` the given slice, returning `Right` if `x` isn't
-- constrained, `Left` otherwise.
partitionSliceForVar :: AbsEnv ae => Name -> GuardedSlice ae ->
                        Either (GuardedSlice ae, AbsPred ae) (GuardedSlice ae)
partitionSliceForVar x gs =
  case absProj x (gsEnv gs) of
    Nothing      -> Right gs
    Just (e, p)  -> Left (gs { gsEnv = e }, p)

-- | Removes `x` from the domain, returning any slices mentioning x
-- and the domain less those slices
partitionDomainForVar :: AbsEnv ae => Maybe Name ->
                         Domain ae ->
                         ( [ (GuardedSlice ae, AbsPred ae) ], Domain ae )
partitionDomainForVar Nothing d = ([], d)
partitionDomainForVar (Just x) d = ( matching, d' )
  where
    (matching, nonMatching) = partitionEithers (map (partitionSliceForVar x) (elements d))
    d' = d { elements = nonMatching }

partitionDomainForResult :: AbsEnv ae =>
                            (AbsPred ae -> Bool) ->
                            Domain ae ->
                            ( [ GuardedSlice ae ], Domain ae )
partitionDomainForResult f d = ( matching, d' )
  where
    (matching, nonMatching) = partition (maybe False f . gsPred) (elements d)
    d' = d { elements = nonMatching }

partitionDomainForBounded :: AbsEnv ae => Domain ae -> (Maybe (GuardedSlice ae), Domain ae)
partitionDomainForBounded d =
  let (bnddgs, rest) = partition gsBoundedStream (elements d)
  in case bnddgs of
       []   -> (Nothing, d)
       [gs] -> (Just gs, d { elements = rest })
       _    -> panic "Multiple bounded slices" []

collectDomainBoundedHoles :: (AbsEnv ae, Traversable t) =>
                             NodeID -> (t Slice -> Slice) -> 
                             t (Domain ae) ->
                             (Domain ae, t (Domain ae))
collectDomainBoundedHoles nid mkSlice doms
  | Just gss <- m_gss, Just shs <- sameSize (toList gss) =
      (singletonDomain (mkGS shs gss), snd <$> doms')
  | otherwise = (emptyDomain, doms)
  where
    sameSize :: [GuardedSlice ae] -> Maybe SHoleSize
    sameSize gss = do
      -- This check is overly strict as we shouldn't care about order (of the summands)
      shs : shss <- traverse (sizedHoleMaybe . gsSlice) gss
      guard (all (== shs) shss) $> shs

    sizedHoleMaybe :: Slice -> Maybe SHoleSize
    sizedHoleMaybe = preview (#_SHole . _Just . _1)

    mkGS shs gss = GuardedSlice
      { gsEnv = foldl1 merge (gsEnv <$> gss)
      -- Basically be definition, as otherwise we wouldn't have sized holes.      
      , gsBoundedStream = True
      -- Holes don't generate values.
      , gsPred  = Nothing 
      , gsSlice = SHole (Just (shs, mkSlice (gsSlice <$> gss)))
      , gsDominator = nid
      }
    
    m_gss = traverse fst doms'
    doms' = fmap partitionDomainForBounded doms

boundedTransition :: AbsEnv ae => Bool -> Bool -> Domain ae -> Domain ae
boundedTransition inBounded outBounded d
  -- In this case there should be exactly 1 guarded slice in elements
  -- which has gsBoundedStream set.  If this becomes closed we move
  -- the slice to closedElements otherwise it is kept in elements.
  | not inBounded, not outBounded, (Just gs, d') <- partitionDomainForBounded d =
      let gs'      = gs { gsBoundedStream = False }
      in if closedGuardedSlice gs'
         then d' & #closedElements . at (gsDominator gs) <>~ Just [ gsSlice gs' ]
         else d' & #elements %~ (:) gs'
  | otherwise = d

-- Called on the entry point to finalise 
finaliseStreamsForEntry :: AbsEnv ae => Domain ae -> Domain ae
finaliseStreamsForEntry d
  | (Just gs, d') <- partitionDomainForBounded d =
      let gs'      = gs { gsBoundedStream = False }
      in if closedGuardedSlice gs'
         then d' & #closedElements . at (gsDominator gs') <>~ Just [ gsSlice gs' ]
         else panic "Expected a close bounded slice" []
  | otherwise = d
    
-- Maps over non-closed slices
mapSlices :: (Slice -> Slice) -> Domain ae -> Domain ae
mapSlices f d = d { elements = map (mapGuardedSlice f) (elements d) }

domainElements :: Domain ae -> ([GuardedSlice ae], Domain ae)
domainElements d = (elements d, d { elements = [] })

--------------------------------------------------------------------------------
-- Debugging etc.

--------------------------------------------------------------------------------
-- Internal slices (used during analysis)

data CallNode =
  CallNode { callClass        :: FInstId
           , callName         :: FName
           -- ^ The called function
           , callSlices      :: Map Int [Maybe Name]
           -- ^ The slices that we use --- if the args are disjoint
           -- this will be a singleton map for this slice, but we
           -- might need to merge, hence we have multiple.  The map
           -- just allows us to merge easily.
           --           , callArgs        :: [Name]
           }
  deriving (Generic, NFData)

instance Eqv CallNode where
  eqv CallNode { callClass = cl1, callSlices = paths1 }
      CallNode { callClass = cl2, callSlices = paths2 } =
    -- trace ("Eqv " ++ showPP cn ++ " and " ++ showPP cn') $
    cl1 `eqv` cl2 && paths1 == paths2

instance Merge CallNode where
  merge cn@CallNode { callClass = cl1, callSlices = sls1}
           CallNode { callClass = cl2, callSlices = sls2 }
    | cl1 /= cl2 = panic "Saw different function classes" []
    -- FIXME: check that the sets don't overlap
    | otherwise =
      -- trace ("Merging " ++ showPP cn ++ " and " ++ showPP cn') $
      -- Note: it is OK to use as the maps are disjoint
      cn { callSlices = Map.union sls1 sls2 }

instance FreeVars CallNode where
  freeVars cn  = foldMap freeVars (Map.elems (callSlices cn))
  freeFVars cn = Set.singleton (callName cn)

instance TraverseUserTypes CallNode where
  traverseUserTypes f cn  =
    (\n' sls' -> cn { callName = n', callSlices = sls' })
       <$> traverseUserTypes f (callName cn)
       <*> traverse (traverse (traverseUserTypes f)) (callSlices cn)

instance PP CallNode where
  ppPrec n CallNode { callName = fname, callClass = cl, callSlices = sls } =
    wrapIf (n > 0) $ (pp fname <> parens (pp cl))
    <+> vcat (map (\(n', vs) -> brackets (pp n') <> parens (commaSep (map ppA vs))) (Map.toList sls))
    where
      ppA Nothing = "_"
      ppA (Just v) = pp v

type Slice = Slice' CallNode SLExpr

--------------------------------------------------------------------------------
-- Class instances

instance AbsEnv ae => PP (GuardedSlice ae) where
  pp gs = vcat [ (if gsBoundedStream gs then "(bounded)" else mempty) <> pp (gsEnv gs)
               , "For" <+> brackets (maybe " (non-result) " pp (gsPred gs))
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

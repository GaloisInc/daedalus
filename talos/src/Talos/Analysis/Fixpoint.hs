{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- General support for computing a fixpoint, usually over a set of
-- definitions of functions

module Talos.Analysis.Fixpoint where

import Daedalus.PP

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
-- These rep
import Control.Monad.State
import Data.Maybe (isNothing)
import Debug.Trace (traceM)

-- -----------------------------------------------------------------------------
-- Types

type Summaries n c s   = Map n (Map c s)

type RevDeps n c = Map (n, c) (Set (n, c))

type Worklist n c = Set (n, c)

data FixpointState n c s st = FixpointState
  { worklist  :: Worklist n c
  , revDeps   :: RevDeps n c
  , summaries :: Summaries n c s
  , summaryEqv :: s -> s -> Bool
  , currentName  :: n
  , currentClass :: c
  , clientState  :: st
  }

newtype FixpointM n c s st a = FixpointM { getFixpointM :: State (FixpointState n c s st) a }
  deriving (Functor, Applicative, Monad)

runFixpointM :: FixpointM n c s st a -> FixpointState n c s st -> FixpointState n c s st
runFixpointM m = execState (getFixpointM m)

-- -----------------------------------------------------------------------------
-- Main function

type FixpointC n c s = (Ord n, Ord c, PP n, PP c, PP s)

calcFixpoint :: FixpointC n c s => (s -> s -> Bool) ->
                (n -> c -> FixpointM n c s st s) -> Worklist n c -> st ->
                (st, Summaries n c s)
calcFixpoint eqv doIt wl0 ust0 = go st0
  where
    st0 = FixpointState { worklist = wl0, revDeps = Map.empty, summaries = Map.empty
                        , summaryEqv = eqv
                        , currentName = error "Name not set"
                        , currentClass = error "Class not set"
                        , clientState = ust0
                        }

    go s@FixpointState { worklist = wl } | Just ((fn, cl), wl') <- Set.minView wl
      = let s' = s { worklist = wl', currentName = fn, currentClass = cl }
        in go $ runFixpointM (doIt fn cl >>= addSummary fn cl {- >> traceState -} ) s'
    go st = (clientState st, summaries st)

traceState :: FixpointC n c s => FixpointM n c s st ()
traceState = do
  s <- FixpointM get
  traceM (show $ tr s)
  where
    tr st = hang "Iteration" 2 $ vcat [ hang "Worklist" 4 (ppWL (worklist st))
                                      , hang "Summaries" 4 (ppSummaries (summaries st))
                                      ]
    ppWL wl = braces (commaSep [ pp fn <+> brackets (pp cl) | (fn, cl) <- Set.toList wl])
    ppSummaries s = bullets (map goF (Map.toList s))
    goF (fn, m) =
      hang (pp fn) 2 $
        bullets [ hang (pp fid) 2 (pp d)
                | (fid, d) <- Map.toList m]

-- -----------------------------------------------------------------------------
-- Worklist and rev deps

currentDeclName :: FixpointM n c s st n
currentDeclName = FixpointM $ gets currentName

currentSummaryClass :: FixpointM n c s st c
currentSummaryClass = FixpointM $ gets currentClass

addRevDep :: FixpointC n c s => n -> c -> FixpointM n c s st ()
addRevDep nm cl = do
  here    <- currentDeclName
  here_cl <- currentSummaryClass
  FixpointM $ modify (\s -> s { revDeps = Map.insertWith Set.union (nm, cl) (Set.singleton (here, here_cl)) (revDeps s) })

getRevDeps :: FixpointC n c s => n -> c -> FixpointM n c s st (Set (n, c))
getRevDeps nm cl = do
  FixpointM $ gets (Map.findWithDefault Set.empty (nm, cl) . revDeps)

lookupSummary :: FixpointC n c s => n -> c -> FixpointM n c s st (Maybe s)
lookupSummary nm cls = do
  m_summary <- FixpointM $ gets (Map.lookup nm . summaries)
  pure $ Map.lookup cls =<< m_summary

requestSummarisation :: FixpointC n c s => n -> c -> FixpointM n c s st ()
requestSummarisation nm p =
  FixpointM $ modify (\s -> s { worklist = Set.insert (nm, p) (worklist s) })

-- FixpointM n c s st interface

-- Gets the precondition for a given decl.  This may update the worklist and revdeps
requestSummary :: FixpointC n c s => n -> c -> FixpointM n c s st (Maybe s)
requestSummary nm cl = do
  addRevDep nm cl
  m_summary <- lookupSummary nm cl
  when (isNothing m_summary) $ requestSummarisation nm cl
  pure m_summary

-- FIXME: Could make propagateAndGetNext to avoid an insertion

-- propagate notifies callers of a function that its precond has
-- changed, so they need to be resummarised.

-- Note: Everything in revdeps will have a summary
propagate :: FixpointC n c s => n -> c -> FixpointM n c s st ()
propagate nm cl = do
  rdeps  <- getRevDeps nm cl
  FixpointM $ modify (\s -> s { worklist = Set.union rdeps (worklist s) })

-- Adds a summary for a function, and propagates changes if required.
addSummary :: FixpointC n c s => n -> c -> s -> FixpointM n c s st ()
addSummary fn cl newS = do
  m_oldS <- lookupSummary fn cl

  -- insertWith calls Map.union new old which prefers new.
  let upd = Map.insertWith Map.union fn (Map.singleton cl newS)
        
  FixpointM $ modify (\s -> s { summaries = upd (summaries s) })
  
  -- propagate if the summary is different than what was there before.
  eqv  <- FixpointM (gets summaryEqv)
  case m_oldS of
    Just oldS | eqv oldS newS -> pure ()
    _                         -> propagate fn cl

-- ----------------------------------------------------------------------------------------
-- Client state

fixpointState :: (st -> (a, st)) -> FixpointM n c s st a
fixpointState f = do
  st <- FixpointM (gets clientState)
  let (r, st') = f st
  FixpointM (modify (\s -> s { clientState = st' }))
  pure r




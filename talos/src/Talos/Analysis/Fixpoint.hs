{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- General support for computing a fixpoint, usually over a set of
-- definitions of functions

module Talos.Analysis.Fixpoint where

import           Control.Monad       (when)
import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (isNothing)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Debug.Trace         (traceM)

import           Daedalus.PP
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

newtype FixpointT n c s st m a = FixpointT { getFixpointT :: StateT (FixpointState n c s st) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

runFixpointT :: Monad m => FixpointT n c s st m a -> FixpointState n c s st -> m (FixpointState n c s st)
runFixpointT m = execStateT (getFixpointT m)

-- -----------------------------------------------------------------------------
-- Main function

type FixpointC n c s m = (Ord n, Ord c, PP n, PP c, PP s, Monad m)

calcFixpoint :: FixpointC n c s m => (s -> s -> Bool) ->
                (n -> c -> FixpointT n c s st m s) -> Worklist n c -> st ->
                m (st, Summaries n c s)
calcFixpoint eqv doIt wl0 ust0 = go st0
  where
    st0 = FixpointState { worklist = wl0
                        , revDeps = Map.empty
                        , summaries = Map.empty
                        , summaryEqv = eqv
                        , currentName = error "Name not set"
                        , currentClass = error "Class not set"
                        , clientState = ust0
                        }

    go s@FixpointState { worklist = wl } | Just ((fn, cl), wl') <- Set.minView wl
      = let s' = s { worklist = wl', currentName = fn, currentClass = cl }
        in go =<< runFixpointT (doIt fn cl >>= addSummary fn cl {- >> traceState -} ) s'
    go st = pure (clientState st, summaries st)

traceState :: FixpointC n c s m => FixpointT n c s st m ()
traceState = do
  s <- FixpointT get
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

currentDeclName :: Monad m => FixpointT n c s st m n
currentDeclName = FixpointT $ gets currentName

currentSummaryClass :: Monad m => FixpointT n c s st m c
currentSummaryClass = FixpointT $ gets currentClass

addRevDep :: FixpointC n c s m => n -> c -> FixpointT n c s st m ()
addRevDep nm cl = do
  here    <- currentDeclName
  here_cl <- currentSummaryClass
  FixpointT $ modify (\s -> s { revDeps = Map.insertWith Set.union (nm, cl) (Set.singleton (here, here_cl)) (revDeps s) })

getRevDeps :: FixpointC n c s m => n -> c -> FixpointT n c s st m (Set (n, c))
getRevDeps nm cl = do
  FixpointT $ gets (Map.findWithDefault Set.empty (nm, cl) . revDeps)

lookupSummary :: FixpointC n c s m => n -> c -> FixpointT n c s st m (Maybe s)
lookupSummary nm cls = do
  m_summary <- FixpointT $ gets (Map.lookup nm . summaries)
  pure $ Map.lookup cls =<< m_summary

requestSummarisation :: FixpointC n c s m => n -> c -> FixpointT n c s st m ()
requestSummarisation nm p =
  FixpointT $ modify (\s -> s { worklist = Set.insert (nm, p) (worklist s) })

-- FixpointT n c s st m interface

-- Gets the precondition for a given decl.  This may update the worklist and revdeps
requestSummary :: FixpointC n c s m => n -> c -> FixpointT n c s st m (Maybe s)
requestSummary nm cl = do
  addRevDep nm cl
  m_summary <- lookupSummary nm cl
  when (isNothing m_summary) $ requestSummarisation nm cl
  pure m_summary

-- FIXME: Could make propagateAndGetNext to avoid an insertion

-- propagate notifies callers of a function that its precond has
-- changed, so they need to be resummarised.

-- Note: Everything in revdeps will have a summary
propagate :: FixpointC n c s m => n -> c -> FixpointT n c s st m ()
propagate nm cl = do
  rdeps  <- getRevDeps nm cl
  FixpointT $ modify (\s -> s { worklist = Set.union rdeps (worklist s) })

-- Adds a summary for a function, and propagates changes if required.
addSummary :: FixpointC n c s m => n -> c -> s -> FixpointT n c s st m ()
addSummary fn cl newS = do
  m_oldS <- lookupSummary fn cl

  -- insertWith calls Map.union new old which prefers new.
  let upd = Map.insertWith Map.union fn (Map.singleton cl newS)
        
  FixpointT $ modify (\s -> s { summaries = upd (summaries s) })
  
  -- propagate if the summary is different than what was there before.
  eqv  <- FixpointT (gets summaryEqv)
  case m_oldS of
    Just oldS | eqv oldS newS -> pure ()
    _                         -> propagate fn cl

-- ----------------------------------------------------------------------------------------
-- Client state

fixpointState :: Monad m => (st -> (a, st)) -> FixpointT n c s st m a
fixpointState f = do
  st <- FixpointT (gets clientState)
  let (r, st') = f st
  FixpointT (modify (\s -> s { clientState = st' }))
  pure r




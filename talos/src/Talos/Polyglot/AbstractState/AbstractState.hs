{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.AbstractState.AbstractState where

import           Data.Map              (Map)
import qualified Data.Map              as Map

import Daedalus.Core
import Daedalus.Core.CFG
import Daedalus.PP

import           Talos.Polyglot.AbstractState.Environment (Env)
import qualified Talos.Polyglot.AbstractState.Environment as Env
import           Talos.Polyglot.AbstractState.ReadFrontier (ReadFrontier)
import qualified Talos.Polyglot.AbstractState.ReadFrontier as RF
import qualified Talos.Polyglot.AbstractState.ThreadSet as ThreadSet

import Talos.Polyglot.Location
import Talos.Polyglot.Util

-- | The abstract state serves three purposes.
-- 
-- 1. To determine if a value previously read from the stream can cause parsing
-- to fail, implying that stream read is constrained.
-- 
-- For this, we map variables to sets of abstract values with base values
-- replaced with the node ID where they originated.  Note that the analysis is
-- not flow sensitive, so the environment may admit infeasible combinations of
-- abstract values.
-- 
-- A stream read is considered "bound" on control flow branches that depend on
-- equality constraints on the value, e.g. `x == 1` is bound, `x != 1` is not,
-- nor is `x == y` so long as both x and y are unbound.
-- 
-- 2. To determine whether a cycle exists consisting of at least one unbounded
-- stream read and no bounded reads.
-- 
-- For each stream read (identified by node ID), we track whether we are part of
-- such a cycle by tracking sets of stream reads (again by node ID) encountered
-- thereafter, where each set represents the reads encountered on a unique path.
-- When a stream read is discovered to be bound, any sets containing it are
-- removed.  If all sets associated with a stream read are removed, then the
-- read is not part of a cavity cycle.
-- 
-- 3. For each such cycle, can it be reached from the program entrypoint without
-- any bounded reads, and can it in turn reach a program exitpoint without any
-- bounded reads.
--
-- TODO(cns): Add prefix/suffix to the read frontier.

-- TODO(cns): Track bounded node IDs per path?  Then a cavity is a self loop +
-- at least one path to an exit where it's not bound.  A suffix is a cavity that
-- still has itself in its read frontier at a program exit.  And a prefix is a
-- cavity that has an empty set in the bound sets at its location.  (Note, we
-- need to be more careful to track empty sets.)

-- Check mergeUnion.  I think we want two operations, union (combining different
-- paths) and sequence (adding a new node to all existing paths).

data AbstractState = AbstractState
  { asEnv :: Env
  , asReadFrontier :: ReadFrontier

  -- | Summarizes the node to which this state is attached.
  , asReturnVal :: Env.Summary 
  } deriving Eq

instance PP AbstractState where
  pp AbstractState{..} = braces . (nest 2) $ body
    where
      body =
        (text "env" <+> colon <+> Env.ppEnv asEnv)
        $$ (text "read-frontier" <+> colon <+> RF.ppReadFrontier asReadFrontier)
        $$ (text "return-value" <+> colon <+> pp asReturnVal)

empty :: AbstractState
empty = AbstractState Map.empty Map.empty ThreadSet.emptyThread

join :: AbstractState -> AbstractState -> AbstractState
join left right =
  AbstractState{asEnv=env, asReadFrontier=rf, asReturnVal=rv}
  where
    env = Env.join (asEnv left) (asEnv right)
    rf  = RF.join (asReadFrontier left) (asReadFrontier right)
    rv  = ThreadSet.join (asReturnVal left) (asReturnVal right)

addBoundsFromPattern :: AbstractState -> Name -> Pattern -> AbstractState
addBoundsFromPattern state@AbstractState{..} var pat = 
  -- Get IDs for var, then bound each ID.
  -- TODO(cns): Extract new variables from pat and copy state from var.
  let nodeIDs = ThreadSet.flatten (Map.findWithDefault ThreadSet.empty var asEnv)
      newRF   = RF.applyBounds asReadFrontier nodeIDs in
  state{asReadFrontier=newRF}

-- | Add `nodeID` to all sets.  If `nodeID` is not already in the domain of the
-- read frontier, add it with an empty set.
extendRF :: AbstractState -> NodeID -> AbstractState
extendRF state@AbstractState{..} nodeID = state{asReadFrontier=RF.sequenceOne asReadFrontier nodeID}

extendEnv :: AbstractState -> Name -> Env.Summary -> AbstractState
extendEnv state@AbstractState{..} name summary = state{asEnv=Env.extend asEnv name summary}

-- | Extends the environment at `name` with the return value in `state`.
extendEnvWithRV :: AbstractState -> Name -> AbstractState
extendEnvWithRV state@AbstractState{..} name = state{asEnv=Map.alter f name asEnv}
  where
    f Nothing        = Just asReturnVal
    f (Just summary) = Just (ThreadSet.join summary asReturnVal)

-- | Add return value to environment, replacing existing return value, if any.
extendRV :: AbstractState -> Env.Summary -> AbstractState
extendRV state rv = state{asReturnVal=rv}

-------------------------------------------------------------------------------
-- Located abstract states

-- | We track one abstract state per call stack for each node ID.  Indexing by
-- call stack gives context sensitivity; we know where to return to after a
-- function exits.
-- 
-- To keep this finite, the analysis stops on encountering a cycle.
type AbstractStates = Map (CallStack, Loc) AbstractState

abstractStatesFromList :: [((CallStack, Loc), AbstractState)] -> AbstractStates
abstractStatesFromList = Map.fromList

joins :: AbstractStates -> AbstractStates -> AbstractStates
joins left right = mapUnion join left right

-- TODO(cns): What's the Haskelly way to do this?
ppAbstractStates :: AbstractStates -> Doc
ppAbstractStates states = vcat $ map doRow (Map.toList states)
  where
    doRow (k, v) = ppStackLoc k <+> text "->" <+> pp v
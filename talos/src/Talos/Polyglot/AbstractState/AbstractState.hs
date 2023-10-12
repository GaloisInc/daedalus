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
import           Talos.Polyglot.AbstractState.ThreadSet (ThreadSet)
import qualified Talos.Polyglot.AbstractState.ThreadSet as ThreadSet

import Talos.Polyglot.Location
import Talos.Polyglot.Util

-- | Abstract state tracks information necessary to detect cavity existence and
-- location.  A stream read location is part of a cavity if
-- 
-- 1. It is part of a control flow cycle.
-- 
-- 2. No stream read on the cycle is "bounded", i.e. tested in a way that can
--    lead the parser to fail.  Note that data read from the stream may be stored
--    and tested later, so there must exist at least one control flow path to
--    parser exit on which no stream read is bounded.
-- 
-- A cavity is a prefix cavity if it can be reached without encountering a
-- bounded read; a path to parser exit must exist on which no read prior to the
-- cavity is later bounded.
-- 
-- A cavity is a suffix cavity if execution after the cavity can reach parser
-- exit without encountering a bounded read.
-- 
-- An abstract state tracks per-flow information.  The analysis is context sensitive
-- but flow-insensitive; that is, it may contain infeasible flows.  It maintains
-- 
-- 1. An environment, to map variables to stream read locations that flow into the
--    variable's value; if the variable is part of a condition that leads to
--    failure, then it's dependent stream reads are considered bounded.
-- 
-- 2. A read frontier, to map each read location to read locations that follow it in
--    the CFG.
--
--    As an optimization, all reads on a flow are removed from the read frontier if
--    any read on that flow is bounded.
-- 
-- 3. A bounded set, which contains read locations that have been bounded
--    at a given location.
-- 
-- A stream read is part of a cycle if it is present in its own read frontier at the
-- read location.
--
-- A stream read is part of a cavity if the read locations in its read frontier are
-- not in the bounded set at at least one exit location.
--
-- A stream read is part of a prefix cavity if the bounded set is empty at the read
-- location.
--
-- A stream read is part of a suffix cavity if there are no new read locations in
-- the bounded set of at least one exit location.
data AbstractState = AbstractState
  { asEnv :: Env
  , asReadFrontier :: ReadFrontier

  -- | All stream read locations.
  , asStreamReads :: ThreadSet NodeID
  -- | Bounded stream read locations.
  , asBounded     :: ThreadSet NodeID

  -- | Summarizes the node to which this state is attached.
  , asReturnVal :: Env.Summary 
  } deriving Eq

instance PP AbstractState where
  pp AbstractState{..} = braces . (nest 2) $ body
    where
      body =
        (text "env" <+> colon <+> Env.ppEnv asEnv)
        $$ (text "read-frontier" <+> colon <+> RF.ppReadFrontier asReadFrontier)
        $$ (text "bounded" <+> colon <+> pp asBounded)
        $$ (text "return-value" <+> colon <+> pp asReturnVal)

empty :: AbstractState
empty = AbstractState 
  { asEnv=Map.empty
  , asReadFrontier=Map.empty
  , asStreamReads=ThreadSet.emptyThread
  , asBounded=ThreadSet.emptyThread
  , asReturnVal=ThreadSet.emptyThread
  }

join :: AbstractState -> AbstractState -> AbstractState
join left right =
  AbstractState{asEnv=env, asReadFrontier=rf, asStreamReads=sReads, asBounded=bounded, asReturnVal=rv}
  where
    env     = Env.join (asEnv left) (asEnv right)
    rf      = RF.join (asReadFrontier left) (asReadFrontier right)
    sReads  = ThreadSet.join (asStreamReads left) (asStreamReads right)
    bounded = ThreadSet.join (asBounded left) (asBounded right)
    rv      = ThreadSet.join (asReturnVal left) (asReturnVal right)

addBoundsFromPattern :: AbstractState -> Name -> Pattern -> AbstractState
addBoundsFromPattern state@AbstractState{..} var pat = 
  -- Get IDs for var, then bound each ID.
  -- TODO(cns): Extract new variables from pat and copy state from var.
  let nodeIDs = ThreadSet.flatten (Map.findWithDefault ThreadSet.empty var asEnv)
      newRF   = RF.applyBounds asReadFrontier nodeIDs
      newBounds = ThreadSet.sequenceMany nodeIDs asBounded 
      in
  state{asReadFrontier=newRF, asBounded=newBounds}

-- | Add `nodeID` to all sets.  If `nodeID` is not already in the domain of the
-- read frontier, add it with an empty set.
extendRF :: NodeID -> AbstractState -> AbstractState
extendRF nodeID state@AbstractState{..} = state{asReadFrontier=RF.sequenceOne asReadFrontier nodeID}

extendEnv :: Name -> Env.Summary -> AbstractState -> AbstractState
extendEnv name summary state@AbstractState{..} = state{asEnv=Env.extend asEnv name summary}

extendBounded :: NodeID -> AbstractState -> AbstractState
extendBounded nodeID state@AbstractState{..} = state{asBounded=ThreadSet.sequenceOne nodeID asBounded}

extendStreamReads :: NodeID -> AbstractState -> AbstractState
extendStreamReads nodeID state@AbstractState{..} = state{asStreamReads=ThreadSet.sequenceOne nodeID asStreamReads}

-- | Extends the environment at `name` with the return value in `state`.
extendEnvWithRV :: Name -> AbstractState -> AbstractState
extendEnvWithRV name state@AbstractState{..} = state{asEnv=Map.alter f name asEnv}
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
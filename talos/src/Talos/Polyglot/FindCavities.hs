{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.FindCavities 
  ( analyze
  ) where

import qualified Data.List             as List
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)
import           Data.Maybe            (fromJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Daedalus.Core
import           Daedalus.Core.CFG

-- ----------------------------------------------------------------------------------------
-- Cavity detection

-- | Function name and node ID, which specifices a location in the CFG.
type Loc = (FName, NodeID)

-- | The sequence of call sites leading to the current location.  Does not
-- include the current function, e.g. an empty call stack means the location is
-- in the Main function.
type CallStack = [Loc]

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
data AbstractState = AbstractState
  { asEnv :: Env
  , asReadFrontier :: ReadFrontier

  -- | Summarizes the node to which this state is attached.
  , asReturnVal :: EnvSummary 
  } deriving Eq

emptyAbstractState :: AbstractState
emptyAbstractState = AbstractState Map.empty Map.empty Set.empty

-- | Maps variables to the set of stream read nodes that flow to this variable,
-- one set per path.
-- 
-- TODO(cns): Add other data types for fine-grained tracking.
type Env = Map Name EnvSummary
type EnvSummary = Set (Set NodeID)

mergeEnv :: Env -> Env -> Env
mergeEnv = mapUnion Set.union

-- | Maps each stream read location to the set of sets of stream read locations
-- downstream from it.  Each set corresponds to an execution path.
type ReadFrontier = Map NodeID (Set (Set NodeID))

mergeReadFrontier :: ReadFrontier -> ReadFrontier -> ReadFrontier
mergeReadFrontier = mapUnion Set.union

-- | We track one abstract state per call stack for each node ID.  Indexing by
-- call stack gives context sensitivity; we know where to return to after a
-- function exits.
-- 
-- To keep this finite, the analysis stops on encountering a cycle.
type AbstractStates = Map (CallStack, Loc) AbstractState

-- | Interpret the abstract state at each location, then merge new and old
-- states at the same locations.  Keys in the result indicate states that
-- may have changed.
interpret :: AbstractStates -> CFGNode -> AbstractStates
interpret states node =
  let
    mapResults = map (\(loc, state) -> interpret_ loc state node) (Map.toList states)
    in
  foldl mergeAbstractStates Map.empty mapResults

mapUnion :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
mapUnion f = merge preserveMissing preserveMissing (zipWithMatched (\_ -> f))

mergeAbstractStates :: AbstractStates -> AbstractStates -> AbstractStates
mergeAbstractStates left right = mapUnion mergeAbstractState left right
  
mergeAbstractState :: AbstractState -> AbstractState -> AbstractState
mergeAbstractState left right =
  AbstractState{asEnv=env, asReadFrontier=rf, asReturnVal=rv}
  where
    env = mergeEnv (asEnv left) (asEnv right)
    rf  = mergeReadFrontier (asReadFrontier left) (asReadFrontier right)
    rv  = Set.union (asReturnVal left) (asReturnVal right)

-- | Interpret the abstract state at a given location, producing abstract
-- states at other locations.
interpret_ :: (CallStack, Loc) -> AbstractState -> CFGNode -> AbstractStates

-- TODO(cns): Special handling for `let v = fromTag label caseVar` to assign
-- env summary from caseVar to v.
interpret_ (stack, thisID) state (CSimple _ _ _) = error "NYI"

interpret_ _ _ CFail = Map.empty

interpret_ (stack, (fname, _)) state (COr _ leftID rightID) =
  -- Copy abstract state from thisID to leftID and rightID.
  Map.fromList [((stack, (fname, leftID)), state), ((stack, (fname, rightID)), state)]

interpret_ (stack, (fname, _)) state (CCase Case{..}) =
  let interpretCase (pat, nodeID) = 
        -- Update the environment to reflect that this pattern matched.
        let newState = addBoundsFromPattern state caseVar pat in
        Map.singleton (stack, (fname, nodeID)) newState
      mapResults = map interpretCase casePats
  in
  foldl mergeAbstractStates Map.empty mapResults

-- This case handles the loop edge(s) from the body of ManyLoop back to the loop head,
-- which are identifiable when this node's location is already on the top of the
-- stack.
-- 
-- The environment carries a summary of the "return value" of an expression.  We
-- extend the environment with this summary assigned to `m_name`.
interpret_ (loc:stack, loc_@(fname, _)) state (CLoop (Just name) (ManyLoop _ _ _ _ bodyNodeID) nextNodeID) | loc == loc_ =
  let
    -- Extend the environment with a summary of the loop body.
    state' = extendWithReturnValue state name

    -- On entering the loop, we pushed this location on the stack.  Now that we're
    -- in the loop, no need to push it again.  Just (re)evaluate the body with the
    -- extended environment.
    bodyMap = ((loc:stack, (fname, bodyNodeID)), state')
    
    -- For the loop exit, pop the loop call site from the stack, and evaluate
    -- the next statement using the extended environment.
    nextMap = ((stack, (fname, nextNodeID)), state')
    in
  Map.fromList [bodyMap, nextMap]

-- This case handles the entry edge(s) to a ManyLoop.
interpret_ (stack, loc@(fname, _)) state (CLoop _ (ManyLoop _ _ _ _ bodyNodeID) nextNodeID) =
  let
    -- The CFG has edges from the loop body back to this node, so push the body
    -- node ID to the worklist, extending the stack with this location.
    bodyMap = ((loc:stack, (fname, bodyNodeID)), state)
    
    -- For the loop exit, this case handles where the loop executes zero times,
    -- i.e. there is no result to bind from the loop body. The `loc == loc_`
    -- case of `interpret_` handles the other loop cases.
    nextMap = ((stack, (fname, nextNodeID)), state)
    in
  Map.fromList [bodyMap, nextMap]

interpret_ loc state (CLoop _ _ id) = error "NYI"

-- | Updates the read frontier to reflect that `var` is bound.  That is, removes
-- all sets containing nodeIDs associated with `var`.
recordVarBounds :: AbstractState -> Name -> AbstractState
recordVarBounds state var = error "NYI"

  -- TODO(cns): Pick up here.  Break out ReadFrontier and expose update methods
  -- that, given a NodeID, remove all sets containing that ID.

-- | Merge the given summary with the environment summary for the given variable
-- name.
extendEnv :: AbstractState -> Name -> EnvSummary -> AbstractState
extendEnv state var summary = state{asEnv=Map.alter f var (asEnv state)}
  where
    f Nothing         = Just summary
    f (Just summary') = Just (Set.union summary summary')

-- | Extends the environment at `name` with the return value in `state`.
extendWithReturnValue :: AbstractState -> Name -> AbstractState
extendWithReturnValue state@AbstractState{..} name = state{asEnv=Map.alter f name asEnv}
  where
    f Nothing        = Just asReturnVal
    f (Just summary) = Just (Set.union summary asReturnVal)

-- | Adds bounds to the read frontier for `var` from `pat`, if any.  Constraints
-- are induced from matching literal values but not constructors.
addBoundsFromPattern :: AbstractState -> Name -> Pattern -> AbstractState
addBoundsFromPattern state var (PBool _)   = recordVarBounds state var
addBoundsFromPattern state var (PNum _)    = recordVarBounds state var
addBoundsFromPattern state var (PBytes _)  = recordVarBounds state var
addBoundsFromPattern state _ _             = state

-- | Starting at "Main", applies abstract interpretation to the CFG until
-- reaching a fixpoint.  Calls `error` if "Main" doesn't exist.
analyze :: CFGModule -> AbstractStates
analyze m = analyze_ initialState
  where
    initialState = Map.singleton ([], getMainEntrypoint m) emptyAbstractState
    analyze_ states =
      let 
          nodes         = map (getNode m . snd) (Map.keys states)
          newStatesList = map (interpret states) nodes
          newStates     = foldl mergeAbstractStates Map.empty (states:newStatesList)
          in
      if states == newStates then 
        states
      else
        analyze_ newStates

-- | Gets the entrypoint for the "Main" function.  Calls `error` if "Main" does not
-- exist.
getMainEntrypoint :: CFGModule -> Loc
getMainEntrypoint CFGModule{..} = (fname, cfgfunEntry)
  where
  (fname, CFGFun{..}) = fromJust $ List.find (\(FName{..}, _) -> fnameText == "Main") (Map.toList cfgFuns)

-- | Get the CFG node for a given location.  Calls `error` if the location does not
-- exist in the CFG.
getNode :: CFGModule -> Loc -> CFGNode
getNode CFGModule{..} (fname, nodeID) =
  let CFGFun{..} = cfgFuns Map.! fname in
  cfgfunCFG Map.! nodeID
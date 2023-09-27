{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.AbstractState where

import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set

import Daedalus.Core
import Daedalus.Core.CFG
import Daedalus.PP
import Talos.Polyglot.Location
import Talos.Polyglot.ReadFrontier
import Talos.Polyglot.Util
import Talos.Polyglot.PolyglotReader

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

mergeAbstractState :: AbstractState -> AbstractState -> AbstractState
mergeAbstractState left right =
  AbstractState{asEnv=env, asReadFrontier=rf, asReturnVal=rv}
  where
    env = mergeEnv (asEnv left) (asEnv right)
    rf  = mergeReadFrontier (asReadFrontier left) (asReadFrontier right)
    rv  = Set.union (asReturnVal left) (asReturnVal right)

addBoundsFromPattern :: AbstractState -> Name -> Pattern -> AbstractState
addBoundsFromPattern state@AbstractState{..} var pat = 
  -- Get IDs for var, then bound each ID.
  let nodeIDs = flattenSets (Map.findWithDefault Set.empty var asEnv)
      newRF   = applyBounds asReadFrontier nodeIDs in
  state{asReadFrontier=newRF}

instance PP AbstractState where
  pp AbstractState{..} = braces . (nest 2) $ body
    where
      body =
        (text "env" <+> colon <+> ppEnv asEnv)
        $$ (text "read-frontier" <+> colon <+> ppReadFrontier asReadFrontier)
        $$ (text "return-value" <+> colon <+> ppEnvSummary asReturnVal)

-------------------------------------------------------------------------------
-- Environment

-- | Maps variables to the set of stream read nodes that flow to this variable,
-- one set per path.
-- 
-- TODO(cns): Add other data types for fine-grained tracking.
type Env = Map Name EnvSummary
type EnvSummary = Set (Set NodeID)

ppEnv :: Env -> Doc
ppEnv env = vcat rows
  where
    rows = map doRow (Map.toList env)
    doRow (name, summary) = pp name <+> text "->" <+> ppEnvSummary summary

ppEnvSummary :: EnvSummary -> Doc
ppEnvSummary = ppSets

emptyEnv :: Env
emptyEnv = Map.empty

emptyEnvSummary :: EnvSummary
emptyEnvSummary = Set.empty

mergeEnvSummary :: EnvSummary -> EnvSummary -> EnvSummary
mergeEnvSummary = Set.union

mergeEnv :: Env -> Env -> Env
mergeEnv = mapUnion Set.union

-- | Merge the given summary with the environment summary for the given variable
-- name.
extendEnv :: AbstractState -> Name -> EnvSummary -> AbstractState
extendEnv state var summary = state{asEnv=Map.alter f var (asEnv state)}
  where
    f Nothing         = Just summary
    f (Just summary') = Just (Set.union summary summary')

-- | Add return value to environment, replacing existing return value, if any.
extendWithRV :: AbstractState -> EnvSummary -> AbstractState
extendWithRV state rv = state{asReturnVal=rv}

-- | Extends the environment at `name` with the return value in `state`.
extendVarWithRV :: AbstractState -> Name -> AbstractState
extendVarWithRV state@AbstractState{..} name = state{asEnv=Map.alter f name asEnv}
  where
    f Nothing        = Just asReturnVal
    f (Just summary) = Just (Set.union summary asReturnVal)

-- | Add `id` to all sets.  If `id` is not already in the domain of the read
-- frontier, add it with an empty set.
extendRF :: AbstractState -> NodeID -> AbstractState
extendRF state@AbstractState{..} nodeID = state{asReadFrontier=extendReadFrontier asReadFrontier nodeID}

-- | Find the stream reads that are used in this expression.  TODO: env should
-- be in a reader; we're not adding to it, just using it to look up open var
-- names.
summarizeExpr :: Env -> Expr -> PolyglotReader EnvSummary

-- This is the important case.  The rest just implement the traversal :(
summarizeExpr env (Var name) = return $ Map.findWithDefault emptyEnvSummary name env

summarizeExpr env (PureLet _ left right) = do
  leftSummary <- summarizeExpr env left
  rightSummary <- summarizeExpr env right
  return $ mergeEnvSummary leftSummary rightSummary

summarizeExpr env (Struct _ fields) = do
  summaries <- sequence $ map ((summarizeExpr env) . snd) fields
  return $ foldl mergeEnvSummary emptyEnvSummary summaries

summarizeExpr env (ECase Case{..}) = do
  let varSummary = Map.findWithDefault emptyEnvSummary caseVar env
  summaries <- sequence $ map ((summarizeExpr env) . snd) casePats
  return $ foldl mergeEnvSummary emptyEnvSummary (varSummary:summaries)

summarizeExpr env (ELoop loop) = summarizeExpr env (morphismBody loop)

summarizeExpr env (Ap0 _) = return emptyEnvSummary

summarizeExpr env (Ap1 _ e1) = summarizeExpr env e1

summarizeExpr env (Ap2 _ e1 e2) = do
  summaries <- sequence $ map (summarizeExpr env) [e1, e2]
  return $ foldl mergeEnvSummary emptyEnvSummary summaries

summarizeExpr env (Ap3 _ e1 e2 e3) = do
  summaries <- sequence $ map (summarizeExpr env) [e1, e2, e3]
  return $ foldl mergeEnvSummary emptyEnvSummary summaries

summarizeExpr env (ApN (CallF name) exprs) = do
  ffun <- getFFun name
  case fDef ffun of
    External -> return emptyEnvSummary
    Def body -> do
      fSum <- summarizeExpr env body
      summaries <- sequence $ map (summarizeExpr env) exprs
      return $ foldl mergeEnvSummary emptyEnvSummary (fSum:summaries)

summarizeExpr env (ApN op exprs) = do
  summaries <- sequence $ map (summarizeExpr env) exprs
  return $ foldl mergeEnvSummary emptyEnvSummary summaries

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

mergeAbstractStates :: AbstractStates -> AbstractStates -> AbstractStates
mergeAbstractStates left right = mapUnion mergeAbstractState left right

-- TODO(cns): What's the Haskelly way to do this?
ppAbstractStates :: AbstractStates -> Doc
ppAbstractStates states = vcat $ map doRow (Map.toList states)
  where
    doRow (k, v) = ppStackLoc k <+> text "->" <+> pp v
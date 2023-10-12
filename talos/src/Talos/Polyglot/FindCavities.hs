{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.FindCavities 
  ( findCavities
  ) where

import qualified Data.List             as List
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Daedalus.Core
import           Daedalus.Core.CFG
import           Daedalus.PP
import           Talos.Polyglot.AbstractState.AbstractState (AbstractState (AbstractState), AbstractStates)
import qualified Talos.Polyglot.AbstractState.AbstractState as AS
import qualified Talos.Polyglot.AbstractState.Environment as Env
import qualified Talos.Polyglot.AbstractState.ReadFrontier as RF
import qualified Talos.Polyglot.AbstractState.ThreadSet as ThreadSet
import           Talos.Polyglot.Location
import           Talos.Polyglot.PolyglotReader
import           Talos.Polyglot.Util

-- | Interpret the abstract state at a given location, producing abstract
-- states at other locations.
interpret :: (CallStack, Loc) -> AbstractState -> PolyglotReader AbstractStates
interpret stackLoc@(_, l) st = do
  node <- getNode l 
  res  <- interpret_ stackLoc st node
  return res `debug` (show $ text "INTERPRET" <+> ppStackLoc stackLoc <+> pp node <+> text "~~>" <+> AS.ppAbstractStates res)
  where
    interpret_ :: (CallStack, Loc) -> AbstractState -> CFGNode -> PolyglotReader AbstractStates
    -- Pure expressions cannot read from the stream or induce failure.  Because this
    -- analysis currently does not support GetStream or SetStream operations, pure
    -- expressions cannot interact with *any* stream.  Therefore, they cannot induce
    -- new bounds or new read locations; we simply collect any NodeIDs for any variables
    -- present and assign them to m_name and the return value.
    interpret_ (stack, (thisName, _)) state (CSimple m_name (CPure expr) nextNodeID) = do
      summary <- Env.summarizeExpr (AS.asEnv state) expr
      let newState  = AS.extendRV state summary
          newState' = case m_name of
            Just name -> AS.extendEnv name summary newState
            Nothing -> newState
      return $ Map.singleton (stack, (thisName, nextNodeID)) newState'

    interpret_ _ _ (CSimple _ CGetStream _)     = error "GetStream not supported"
    interpret_ _ _ (CSimple _ (CSetStream _) _) = error "SetStream not supported"

    interpret_ (stack, (thisName, thisNodeID)) state (CSimple m_name (CMatch _ match) nextNodeID) = do
      isBounded <- RF.inducesBounds match
      let summary      = ThreadSet.singleton thisNodeID
          addName      = \s -> case m_name of
            Nothing   -> s
            Just name -> AS.extendEnv name summary s
          handleBounds = \s -> if isBounded then AS.extendBounded thisNodeID s else AS.extendRF thisNodeID s
          addRV        = \s -> AS.extendRV s summary
          addRead      = \s -> AS.extendStreamReads thisNodeID s
      return $ Map.singleton (stack, (thisName, nextNodeID)) $ (addName . handleBounds . addRV . addRead) state

    -- Handle returning from a call to the call site.
    interpret_ (Returning:stack, (thisFName, _)) state (CSimple m_name (CCall _ _) nextNodeID) = do
      -- Set up the downstream edge by extending the environment with `m_name`
      -- bound to the return value.  Also pass the return value through.
      let downstreamState = case m_name of
            Just name -> AS.extendEnvWithRV name state
            Nothing   -> state
          downstreamEdge = ((stack, (thisFName, nextNodeID)), downstreamState)
      return $ Map.fromList [downstreamEdge]

    -- Handle calling a subparser.
    interpret_ (stack, thisLoc) state (CSimple _ (CCall fname exprs) _) = do
      -- Look up callee and param names
      calleeNodeID <- getEntrypoint fname
      paramNames <- getGFunParameterNames fname

      -- Set up call edge by extending the environment to map the formal
      -- parameter names to summaries of the actual arguments.
      summaries <- sequence $ map (Env.summarizeExpr $ AS.asEnv state) exprs
      let callState    = foldl (\acc (n, s) -> AS.extendEnv n s acc) state (zip paramNames summaries)
          -- This is our loop widening: flatten all cycles into one cycle.
          newCallStack = safePush (Loc thisLoc) stack
          callEdge     = ((newCallStack, (fname, calleeNodeID)), callState)

      return $ Map.fromList [callEdge]

    interpret_ _ _ CFail = return Map.empty

    interpret_ (stack, (fname, _)) state (COr _ leftID rightID) =
      -- Copy abstract state from thisID to leftID and rightID.
      return $ AS.abstractStatesFromList 
        [ ((stack, (fname, leftID)), state)
        , ((stack, (fname, rightID)), state)
        ]

    interpret_ (stack, (fname, _)) state (CCase Case{..}) =
      let interpretCase (pat, nodeID) = 
            -- Update the environment to reflect that this pattern matched.
            let newState = AS.addBoundsFromPattern state caseVar pat in
            Map.singleton (stack, (fname, nodeID)) newState
          mapResults = map interpretCase casePats
      in
      return $ foldl AS.joins Map.empty mapResults

    interpret_ loc state (CLoop m_name (ManyLoop _ _ lower _ bodyNodeID) nextNodeID) =
      return $ interpretLoop loc state m_name bodyNodeID nextNodeID (case lower of Ap0 (IntL n _) -> n == 0; _ -> True)

    interpret_ loc state (CLoop m_name (RepeatLoop _ _ _ bodyNodeID) nextNodeID) =
      return $ interpretLoop loc state m_name bodyNodeID nextNodeID True

    interpret_ loc state (CLoop m_name (MorphismLoop (FoldMorphism _ _ _ bodyNodeID)) nextNodeID) =
      return $ interpretLoop loc state m_name bodyNodeID nextNodeID True

    interpret_ loc state (CLoop m_name (MorphismLoop (MapMorphism _ bodyNodeID)) nextNodeID) =
      return $ interpretLoop loc state m_name bodyNodeID nextNodeID True

-- | Provides a common interpretation for various Daedalus loop structures.
interpretLoop :: (CallStack, Loc) -> AbstractState -> (Maybe Name) -> NodeID -> NodeID -> Bool -> AbstractStates

interpretLoop (stack, loc@(fname, _)) state m_name bodyNodeID nextNodeID canHaveZeroIterations =
  case stack of
    -- This case handles the loop edge(s) from the body of ManyLoop back to the loop head,
    -- which are identifiable when this node's location is already on the top of the
    -- stack.
    -- 
    -- The environment carries a summary of the "return value" of an expression.  We
    -- extend the environment with this summary assigned to `m_name`.
    stackHead:stackTail | stackHead == (Loc loc) ->
      let
        -- Extend the environment with a summary of the loop body.
        state' = case m_name of
          Just name -> AS.extendEnvWithRV name state
          Nothing   -> state

        -- On entering the loop, we pushed this location on the stack.  Now that we're
        -- in the loop, no need to push it again.  Just (re)evaluate the body with the
        -- extended environment.
        bodyMap = ((stack, (fname, bodyNodeID)), state')
        
        -- For the loop exit, pop the loop call site from the stack, and evaluate
        -- the next statement using the extended environment.  The return value
        -- from the loop body is passed through.
        nextMap = ((stackTail, (fname, nextNodeID)), state')
        in
      AS.abstractStatesFromList [bodyMap, nextMap]

    -- This case handles the entry edge(s) to a ManyLoop.
    _ ->
      let
        -- The CFG has edges from the loop body back to this node, so push the body
        -- node ID to the worklist, extending the stack with this location.
        bodyMap = ((safePush (Loc loc) stack, (fname, bodyNodeID)), state)
        
        -- For the loop exit, this case handles where the loop executes zero
        -- times, i.e. there is no result to bind from the loop body
        -- (technically the value is `[]` which contains no node IDs). The `loc
        -- == loc_` case of `interpret_` handles the other loop cases.
        state' = AS.extendRV state ThreadSet.emptyThread

        -- Extend the environment with an empty summary of the loop body.
        state'' = case m_name of
          Just name -> AS.extendEnv name ThreadSet.emptyThread state'
          Nothing   -> state'

        nextMap = ((stack, (fname, nextNodeID)), state'')
        in
      if canHaveZeroIterations then
        AS.abstractStatesFromList [bodyMap, nextMap]
      else
        AS.abstractStatesFromList [bodyMap]

data CavityPosition = CavityPosition
  { isPrefix :: Bool
  , isSuffix :: Bool
  } deriving (Eq, Ord)

instance PP CavityPosition where
  pp CavityPosition{..} = case (isPrefix, isSuffix) of
    (False, False) -> text "embedded"
    _ -> parens $ hsep $ punctuate "," ixes
    where
      ixes = filter ((/=) "") [if isPrefix then "prefix" else "", if isSuffix then "suffix" else ""]

data Cavity = Cavity
  { cavityLocation :: Loc
  , cavityPosition :: CavityPosition
  } deriving (Eq, Ord)

instance PP Cavity where
  pp Cavity{..} = ppLoc cavityLocation <+> pp cavityPosition

-- | Starting at "Main", applies abstract interpretation to the CFG until
-- reaching a fixpoint.  Calls `error` if "Main" doesn't exist.
findCavities :: Module -> CFGModule -> Set Cavity
findCavities m cfgm = 
  let states  = analyze_ initialState initialWorklist in
    findCavities_ states `debug` (show $ AS.ppAbstractStates states)
  where
    entrypoint = ([], getMainEntrypoint cfgm)
    initialWorklist = [entrypoint]
    initialState = Map.singleton entrypoint AS.empty
    cfState = PolyglotReaderState{cfModule=m, cfCFGModule=cfgm}

    globalCFGMap = Map.foldl (\acc f -> Map.union acc (cfgfunCFG f)) Map.empty (cfgFuns cfgm)
    isExitNode (_, nodeID) = not $ Map.member nodeID globalCFGMap


    -- Do the fixpoint.

    -- TODO(cns): Pick up here.  Add a worklist for efficiency and so it's more
    -- obvious what's happening at each step.

    analyze_ :: AbstractStates -> [(CallStack, Loc)] -> AbstractStates
    analyze_ states worklist =
      let 
          toProcess     = map (\loc -> interpret loc $ states Map.! loc) worklist
            `debug` (show $ text "WORKLIST" <+> (hsep $ map ppStackLoc worklist) $$ text "STATES" $$ AS.ppAbstractStates states)
          nextStatesList = runPolyglotReader (sequence toProcess) cfState 
          nextStates     = foldl AS.joins Map.empty nextStatesList
          nextStates'    = replaceExitNodes nextStates  
          newStates      = AS.joins states nextStates'

          -- Morally speaking, check if states == (states U nextStates).  If
          -- not, get the states that changed.
          changedStates  = Map.filterWithKey (\k state ->
              -- Keep if state is new or changed
              (not $ Map.member k states) || state /= states Map.! k
            ) newStates
          newWorklist    = Map.keys $ Map.filterWithKey (\(_, loc) _ -> not . isExitNode $ loc) changedStates
          in
      if null newWorklist then 
        states
      else
        analyze_ newStates newWorklist

    -- Handle transition from function exit nodes back to callsites.
    replaceExitNodes :: AbstractStates -> AbstractStates
    replaceExitNodes states = 
      let exitLocs = Set.fromList $ map (\CFGFun{..} -> (cfgfunName, cfgfunExit)) (Map.elems $ cfgFuns cfgm)
          (exitStates, otherStates) = Map.partitionWithKey (\(_, loc) _ -> Set.member loc exitLocs) states
          -- For each exit node, replace it with the call site popped from the stack and push
          -- a Returning node on the stack in its place.
          -- If the stack is empty, leave it.  This is a program exit.
          returnedStates = Map.mapKeys
            ( \(stack, loc) -> case stack of
              (Loc callsite):locs -> (Returning:locs, callsite)
              Returning:_ -> error "Encountered a 'Returning' call site on the stack at an exit node"
              _ -> (stack, loc) -- At exit with no call stack: Leave for now, we'll handle these later.
            ) exitStates
      in
      AS.joins otherStates returnedStates

    -- Find NodeIDs that that contain themselves in their read frontiers.
    findCavities_ :: AbstractStates -> Set Cavity
    findCavities_ states = 
      let candidateStates    = Map.filterWithKey isCandidateLoc states
          exitStates         = Map.filterWithKey (\(_, loc) _ -> isExitNode loc) states
          cavities           = Map.filterWithKey (isUnboundedAtExit exitStates) candidateStates
          positionedCavities = Map.foldlWithKey (\acc (_, loc) state ->
              Set.insert Cavity
                { cavityLocation=loc
                , cavityPosition=CavityPosition
                  { isPrefix=isPrefixCavity exitStates state
                  , isSuffix=isSuffixCavity exitStates state
                  }
                } acc
            ) Set.empty cavities
          in
      positionedCavities
      where
        -- Candidates are locations that appear in their own read frontiers.
        isCandidateLoc (_, (_, nodeID)) AbstractState{..} = RF.contains asReadFrontier nodeID nodeID

        isUnboundedAtExit :: AbstractStates -> (CallStack, Loc) -> AbstractState -> Bool
        isUnboundedAtExit exitStates (_, (_, thisNodeID)) AS.AbstractState{asReadFrontier=readFrontier} =
          Map.foldl (\acc exitState ->
            let thisRF   = readFrontier Map.! thisNodeID
                rfWithID = ThreadSet.filter (Set.member thisNodeID) thisRF
                bounded  = AS.asBounded exitState
                in
            acc || ThreadSet.existsDisjoint rfWithID bounded
          ) False exitStates

        isPrefixCavity :: AbstractStates -> AbstractState -> Bool
        isPrefixCavity exitStates AS.AbstractState{asStreamReads=readBeforeCavity} = 
          -- A prefix cavity has no reads prior to the cavity that are bounded.
          Map.foldl (\acc exitState ->
              acc || ThreadSet.existsDisjoint readBeforeCavity (AS.asBounded exitState)
            ) False exitStates

        isSuffixCavity :: AbstractStates -> AbstractState -> Bool
        isSuffixCavity exitStates AS.AbstractState{asBounded=boundedAtCavity} =
          Map.foldl (\acc exitState -> acc || boundedAtCavity == AS.asBounded exitState) False exitStates

-- | Gets the entrypoint for the "Main" function.  Calls `error` if "Main" does not
-- exist.
getMainEntrypoint :: CFGModule -> Loc
getMainEntrypoint CFGModule{..} = (fname, cfgfunEntry)
  where
  (fname, CFGFun{..}) = fromJust $ List.find (\(FName{..}, _) -> fnameText == "Main") (Map.toList cfgFuns)

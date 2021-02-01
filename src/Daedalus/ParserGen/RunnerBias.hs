module Daedalus.ParserGen.RunnerBias
  ( Result(..)
  , runnerBias
  , runnerLL
  , extractValues
  , extractMetrics
  )


where

-- import Debug.Trace

import qualified Data.ByteString as BS
import qualified Daedalus.Interp as Interp

import qualified Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), BranchAction(..), SemanticElm(..), ControlAction(..), applyAction)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..))
import Daedalus.ParserGen.Cfg (initCfg, Cfg(..), isAcceptingCfg)

import Daedalus.ParserGen.LL as LL


-- CommitStack is a stack dedicated to handle the commit operations in
-- bias choice and early commit instructions.
-- This stack somewhat mirrors the `Resumption` stack but only have stack
-- elements in the case of `SeqChoice`.

data CommitFlag =
    CFalse
  | CTrue
  | CEarly
  deriving(Eq,Show)

type CommitStack = [CommitFlag]

emptyCommitStack :: CommitStack
emptyCommitStack = []

addCommitStack :: CommitStack -> CommitStack
{-# INLINE addCommitStack #-}
addCommitStack hst = CFalse : hst

popCommitStack :: CommitStack -> CommitStack
popCommitStack hst = tail hst

hasCommitted :: CommitStack -> Bool
{-# INLINE hasCommitted #-}
hasCommitted hst = let c = (head hst) in c == CTrue || c == CEarly

updateCommitStack :: CommitStack -> CommitStack
{-# INLINE updateCommitStack #-}
updateCommitStack [] = error "broken invariant"
updateCommitStack (CFalse : r) = CTrue : r
updateCommitStack (CTrue  : r) = CTrue : updateCommitStack r
updateCommitStack (CEarly : r) = CTrue : r

earlyUpdateCommitStack :: CommitStack -> CommitStack
earlyUpdateCommitStack [] = error "broken invariant"
earlyUpdateCommitStack (CFalse : r) = CEarly : r
earlyUpdateCommitStack (CTrue  : r) = CTrue  : earlyUpdateCommitStack r
earlyUpdateCommitStack (CEarly : r) = CEarly : earlyUpdateCommitStack r



-- The `BacktrackStack` data-structure is the stack acculumating the
-- different Transitions and Choices encountered by the backtracking
-- DFSsearch for parse solutions
type DepthComputation = Int

type BacktrackStackInfo = DepthComputation

data BacktrackStack =
    BEmpty
  | BLevel !BacktrackStackInfo !BacktrackStack !Cfg !Choice
  deriving(Show)


addLevel ::  BacktrackStackInfo -> BacktrackStack -> Cfg -> Choice -> BacktrackStack
{-# INLINE addLevel #-}
addLevel info tpath cfg ch =
  BLevel info tpath cfg ch


-- The Resumption data-type is the combination of the BacktrackStack
-- and CommitStack that are somewhat synchronized.

type Resumption = (CommitStack, BacktrackStack, DepthComputation, Maybe ResumptionTip)
type ResumptionTip = (Cfg, (Action, State))

emptyResumption :: Resumption
emptyResumption = ([], BEmpty, 0, Nothing)

addResumption :: Resumption -> Cfg -> Choice -> Resumption
{-# INLINE addResumption #-}
addResumption resumption cfg ch =
  {-# SCC breakpointAddResumption #-}
  let (comm, st, depth, _tip) = resumption
      depth1 = depth + 1
  in
  case ch of
    UniChoice p -> (comm, st, depth1, Just (cfg, p))
    SeqChoice (p : _ps) _ -> (addCommitStack comm, addLevel depth st cfg ch, depth1, Just (cfg, p))
    ParChoice (p : _ps) ->   (comm, addLevel depth st cfg ch, depth1, Just (cfg, p))
    _ -> error "broken invariant: addResumption with empty choice"

getActionCfgAtLevel :: Resumption -> Maybe (Cfg, (Action, State))
getActionCfgAtLevel resumption =
  case resumption of
    (_,_,_,x) -> x

updateCommitResumption :: Resumption -> Resumption
{-# INLINE updateCommitResumption #-}
updateCommitResumption resumption =
  let (comm, st, d, tip) = resumption in
    (updateCommitStack comm, st, d, tip)

earlyUpdateCommitResumption :: Resumption -> Resumption
earlyUpdateCommitResumption resumption =
  let (comm, st, d, tip) = resumption in
    (earlyUpdateCommitStack comm, st, d, tip)

cutResumption :: Resumption -> Resumption
cutResumption (_,_,d,tip) =
  (emptyCommitStack, BEmpty, d, tip)


nextResumption :: Resumption -> Maybe Resumption
{-# INLINE nextResumption #-}
nextResumption (comm, p, _d, _tip) =
  {-# SCC breakpointNextResumption #-}
  getNext (comm, p)
  where
    getNext (commitStk, tpath) =
      case tpath of
        BLevel _ _path _ (UniChoice _) ->
          error "Broken invariant, the level cannot be UniChoice"

        BLevel _ path _ (SeqChoice [ _ ] _) ->
          getNext (popCommitStack commitStk, path)
        BLevel d path cfg (SeqChoice ((_act, _n2): actions) st) ->
          if hasCommitted commitStk -- A commit happened
          then getNext (popCommitStack commitStk, path)
          else Just (addResumption (popCommitStack commitStk, path, d,  Nothing) cfg (SeqChoice actions st))

        BLevel _ path _ (ParChoice [ _ ]) ->
          getNext (commitStk, path)
        BLevel d path cfg (ParChoice ((_act, _n2): actions)) ->
          Just (addResumption (commitStk, path, d, Nothing) cfg (ParChoice actions))

        BEmpty -> Nothing

        BLevel _ _ _ (SeqChoice [] _) -> error "Broken invariant, the current choice cannot be empty"
        BLevel _ _ _ (ParChoice []) -> error "Broken invariant, the current choice cannot be empty"


data Metrics =
  Metrics
  { metricsBacktrack :: !Int
  , metricsLL :: !Int
  }

incrMetricsBacktrack :: Metrics -> Metrics
incrMetricsBacktrack met = met { metricsBacktrack = metricsBacktrack met + 1 }

incrMetricsLL :: Metrics -> Metrics
incrMetricsLL met = met { metricsLL = metricsLL met + 1 }



data Result = Result
  { results :: ![Cfg]
  , parseError :: !(Maybe (Int, Cfg))
  , metrics :: !Metrics
  }

emptyResult :: Result
emptyResult = Result
  { results = []
  , parseError = Nothing
  , metrics = Metrics 0 0
  }

addResult :: Cfg -> Result -> Result
addResult cfg res = res { results = cfg : (results res) }

updateError :: Resumption -> Cfg -> Result -> Result
updateError (_, _, d, _) cfg res =
  case parseError res of
    Nothing -> res { parseError = Just (d, cfg) }
    Just (j, _c) ->
      if d >= j
      then res { parseError = Just (d, cfg) }
      else res

tickBacktrack :: Bool
tickBacktrack = True

tickLL :: Bool
tickLL = False

incrResultMetrics :: Bool -> Result -> Bool -> Result
incrResultMetrics b r flagMetrics =
  if flagMetrics
  then
    if (b == tickBacktrack)
    then r { metrics = incrMetricsBacktrack (metrics r) }
    else r { metrics = incrMetricsLL (metrics r) }
  else r

-- This function runs an automaton from a string and returns accepting
-- configurations using a backtracking algorithm.  Its design is
-- similar to the reactive engine by G. Huet, or its extension to
-- Eilenberg's X-machines
runnerBias :: Aut a => PAST.GblFuns -> BS.ByteString -> a -> Result
runnerBias gbl s aut =
  let react :: Cfg -> Resumption -> Result -> Result
      react cfg resumption result =
        case cfg of
          Cfg _inp _ctrl _out q ->
            -- trace (show cfg) $
            let localTransitions = nextTransition aut q
            in case localTransitions of
                 Nothing ->
                   if isAcceptingCfg cfg aut
                   then
                     let newResult = addResult cfg result
                     in backtrack resumption newResult
                   else
                     let newResumption = addResumption resumption cfg (UniChoice (CAct Pop, q)) in
                       choose newResumption result
                 Just ch ->
                   let newResumption = addResumption resumption cfg ch in
                     choose newResumption result

      choose :: Resumption -> Result -> Result
      choose resumption result =
        case getActionCfgAtLevel resumption of
          Nothing -> backtrack resumption result
          Just (cfg@(Cfg inp ctrl out _q1), (act, q2)) ->
            -- trace (show act) $
            case act of
              BAct (CutBiasAlt _st) ->
                let updResumption = updateCommitResumption resumption
                    newCfg = Cfg inp ctrl out q2
                in
                   react newCfg updResumption result
              BAct (CutLocal) ->
                let updResumption = earlyUpdateCommitResumption resumption
                    newCfg = Cfg inp ctrl out q2
                in
                   react newCfg updResumption result
              BAct (CutGlobal) ->
                let updResumption = cutResumption resumption
                    newCfg = Cfg inp ctrl out q2
                in
                   react newCfg updResumption result
              BAct (FailAction Nothing) ->
                let updResult = updateError resumption cfg result in
                backtrack resumption updResult
              BAct (FailAction _) ->
                error "FailAction not handled"
              _ ->
                case applyAction gbl (inp, ctrl, out) q2 act of
                  Nothing -> {-# SCC backtrackFailApplyAction #-}
                    let updResult = updateError resumption cfg result in
                    backtrack resumption updResult
                  Just (inp2, ctr2, out2, q2') ->
                    let newCfg = Cfg inp2 ctr2 out2 q2'
                    in react newCfg resumption result

      backtrack :: Resumption -> Result -> Result
      backtrack resumption result =
        -- trace "BACKTRACK" $
        case nextResumption resumption of
          Nothing -> result
          Just nextRes -> choose nextRes result

  in react (initCfg s aut) emptyResumption emptyResult

-- This runner is using both the NFA and the DFA to parse.
runnerLL :: Aut a => PAST.GblFuns -> BS.ByteString -> a -> LLA -> Bool -> Result
runnerLL gbl s aut laut flagMetrics =
  let react :: Cfg -> Maybe LL.SynthLLAState -> Resumption -> Result -> Result
      react cfg@(Cfg inp _ctrl _out q) mq resumption result =
        let pq = case mq of
                   Nothing -> Left q
                   Just qSynth -> Right qSynth in
            -- trace (show cfg) $
        let mpdx = LL.predictLL pq laut inp in
          case mpdx of
            Just (pdxs, finalState) ->
              -- trace (show pdxs) $
              -- trace (case cfg of Cfg inp _ _ _ -> show inp) $
              applyPredictions pdxs finalState cfg resumption result
            Nothing ->
              let localTransitions = nextTransition aut q
              in case localTransitions of
                   Nothing -> {-# SCC backtrackSetStep #-}
                     if isAcceptingCfg cfg aut
                     then
                       let newResult = addResult cfg result
                       in backtrack resumption newResult
                     else
                       let newResumption = addResumption resumption cfg (UniChoice (CAct Pop, q)) in
                         choose newResumption result
                   Just ch ->
                     let newResumption = addResumption resumption cfg ch in
                       choose newResumption result

      applyPredictions :: LL.Prediction -> Maybe LL.SynthLLAState -> Cfg -> Resumption -> Result -> Result
      applyPredictions prdx finalState cfg@(Cfg inp ctrl out q) resumption rslt =
        let result = incrResultMetrics tickLL rslt flagMetrics in
        -- trace (show q) $
        case LL.destrPrediction prdx of
          Nothing -> react cfg finalState resumption result
          Just (alt, alts) ->
            let tr = nextTransition aut q
                (act, q2) = case (tr, alt) of
                              (Nothing, (CPop, _)) -> (CAct Pop, q)
                              (Just (UniChoice (a, q1)), (CUni, _)) -> (a, q1)
                              (Just (SeqChoice lst _), (CSeq, i)) -> lst !! i
                              (Just (ParChoice lst), (CPar, i)) -> lst !! i
                              _ -> error "impossible combination"
                newResumption = addResumption resumption cfg (UniChoice (act, q2))
            in
               -- trace (show act) $
               case act of
                 BAct bact ->
                   case bact of
                     (CutBiasAlt _st) ->
                       let updResumption = updateCommitResumption newResumption
                           newCfg = Cfg inp ctrl out q2
                       in applyPredictions alts finalState newCfg updResumption result
                     _ -> undefined
                 _ ->
                     case applyAction gbl (inp, ctrl, out) q2 act of
                       Nothing -> {-# SCC backtrackFailApplyAction #-}
                         let updResult = updateError resumption cfg result
                         in backtrack newResumption updResult
                       Just (inp2, ctr2, out2, q2') ->
                         let newCfg = Cfg inp2 ctr2 out2 q2'
                         in applyPredictions alts finalState newCfg newResumption result

      choose :: Resumption -> Result -> Result
      choose resumption rslt =
        let result = incrResultMetrics tickBacktrack rslt flagMetrics in
        case getActionCfgAtLevel resumption of
          Nothing -> backtrack resumption result
          Just (cfg@(Cfg inp ctrl out _n1), (act, q2)) ->
            -- trace (show act) $
            case act of
              BAct (CutBiasAlt _st) ->
                let newResumption = updateCommitResumption resumption
                    newCfg = Cfg inp ctrl out q2
                in
                   react newCfg Nothing newResumption result
              BAct (CutLocal) ->
                let newResumption = earlyUpdateCommitResumption resumption
                    newCfg = Cfg inp ctrl out q2
                in
                   react newCfg Nothing newResumption result
              BAct (CutGlobal) ->
                let newResumption = cutResumption resumption
                    newCfg = Cfg inp ctrl out q2
                in
                   react newCfg Nothing newResumption result
              BAct (FailAction Nothing) ->
                let updResult = updateError resumption cfg result in
                backtrack resumption updResult
              BAct (FailAction _) ->
                error "FailAction not handled"
              _ ->
                case applyAction gbl (inp, ctrl, out) q2 act of
                  Nothing -> {-# SCC backtrackFailApplyAction #-}
                    let updResult = updateError resumption cfg result in
                    backtrack resumption updResult
                  Just (inp2, ctr2, out2, q2') ->
                    let newCfg = Cfg inp2 ctr2 out2 q2'
                    in react newCfg Nothing resumption result

      backtrack :: Resumption -> Result -> Result
      backtrack resumption result =
        -- trace "BACKTRACK" $
        case nextResumption resumption of
          Nothing -> result
          Just nextRes -> choose nextRes result

  in react (initCfg s aut) Nothing emptyResumption emptyResult


extractValues :: Result -> [ Interp.Value ]
extractValues res =
  [ v | Cfg _ _ ((SEVal v) : _) _ <- reverse (results res) ]

extractMetrics :: Result -> (Int, Int)
extractMetrics res =
  let met = metrics res
  in (metricsBacktrack met, metricsLL met)

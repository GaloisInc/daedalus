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
import qualified Data.Sequence as Seq

import RTS.Input(Input(..))
import qualified Daedalus.Value as Interp

import qualified Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action as Action
import Daedalus.ParserGen.Aut
import Daedalus.ParserGen.Cfg

import Daedalus.ParserGen.LL as LL



-- CommitStack is a stack dedicated to handle the commit operations in
-- bias choice and early commit instructions.
-- This stack somewhat mirrors the `Resumption` stack but only have stack
-- elements in the case of `SeqChoice`.

data CommitFlag =
    CFalse State
  | CTrue State
  | CEarly State
  deriving(Eq,Show)

type CommitStackIdx = Int
type CommitStack = (Seq.Seq (CommitStackIdx, CommitFlag), CommitStackIdx)

emptyCommitStack :: CommitStack
emptyCommitStack = (Seq.empty, -1)

addCommitStack :: State -> CommitStack -> CommitStack
{-# INLINE addCommitStack #-}
addCommitStack g cstk =
  let (comm, pos) = cstk in
  let res = (comm Seq.|> (pos, CFalse g), length comm) in
  -- trace (show res) $
  res

popCommitStack :: CommitStack -> CommitStack -> CommitStack
popCommitStack cstk savedCstk=
  let
    (comm1, pos1) = cstk
    (_comm2, pos2) = savedCstk
  in
    case Seq.lookup (Seq.length comm1 - 1) comm1 of
      Nothing -> error "broken invariant"
      Just (p, c) ->
        case c of
          CTrue _ -> (Seq.deleteAt (Seq.length comm1 - 1) comm1, pos1)
          CFalse _ ->
            if p /= pos2
            then error "broken invariant"
            else
            (Seq.deleteAt (Seq.length comm1 - 1) comm1, p) -- or p, maybe assert it
          CEarly _ -> (Seq.deleteAt (Seq.length comm1 - 1) comm1, pos1)

adjustCommitStack :: CommitStack -> CommitStack -> CommitStack
adjustCommitStack cstk savedCstk =
  let
    (comm1, _pos1) = cstk
    (_comm2, pos2) = savedCstk
  in
    (comm1, pos2)


hasCommitted :: CommitStack -> Bool
{-# INLINE hasCommitted #-}
hasCommitted hst =
  let (comm, _) = hst in
  let c = (Seq.lookup ((Seq.length comm) - 1) comm) in
  case c of
    Nothing -> error "broken invariant"
    Just (_, CTrue _) -> True
    Just (_, CEarly _) -> True
    Just (_, CFalse _) -> False

updateCommitStack :: State -> CommitStack -> CommitStack
{-# INLINE updateCommitStack #-}
updateCommitStack g cstk =
  let (comm, pos) = cstk in
  case (Seq.lookup pos comm) of
    Nothing -> error ("broken invariant: " ++ show pos ++
                      " Len: " ++ show (Seq.length comm))
    Just (p, CFalse g1) ->
      if g /= g1
      then error ("wrong commit: " ++ show g ++ " " ++ show g1 ++
                  "\n" ++ "POS: " ++ show pos ++ "\n" ++ show comm)
      else
        (Seq.update pos (p, CTrue g1) comm, p)

    Just (p, CTrue  _) ->  (comm, p)
    Just (p, CEarly g1) ->
      if g /= g1
      then error ("wrong commit" ++ show g ++ " " ++ show g1)
      else
        (Seq.update pos (p, CTrue g1) comm, p)

earlyUpdateCommitStack :: CommitStack -> CommitStack
earlyUpdateCommitStack cstk =
  let (comm, pos) = cstk in
  case (Seq.lookup pos comm) of
    Nothing -> error "broken invariant"
    Just (p, CFalse st) ->
      (Seq.update pos (p, CEarly st) comm, pos)
    Just (_p, CTrue  _) -> (comm, pos)
    Just (p, CEarly st) ->
      (Seq.update pos (p, CEarly st) comm, pos)



-- The `BacktrackStack` data-structure is the stack acculumating the
-- different Transitions and Choices encountered by the backtracking
-- DFSsearch for parse solutions
type DepthComputation = Int

type BacktrackStackInfo = DepthComputation

data BacktrackStack =
    BEmpty
  | BLevel !BacktrackStackInfo !BacktrackStack !Cfg !CommitStack !Choice
  deriving(Show)


addLevel ::  BacktrackStackInfo -> BacktrackStack -> Cfg -> CommitStack -> Choice -> BacktrackStack
{-# INLINE addLevel #-}
addLevel info tpath cfg cstk ch =
  BLevel info tpath cfg cstk ch


-- The Resumption data-type is the combination of the BacktrackStack
-- and CommitStack that are somewhat synchronized.

type Resumption = (CommitStack, BacktrackStack, DepthComputation, Maybe ResumptionTip)
type ResumptionTip = (Cfg, (Action, State))

emptyResumption :: Resumption
emptyResumption = (emptyCommitStack, BEmpty, 0, Nothing)

_getCommitStack :: Resumption -> CommitStack
_getCommitStack (comm, _, _, _) = comm

addResumption :: Resumption -> Cfg -> Choice -> Resumption
{-# INLINE addResumption #-}
addResumption resumption cfg ch =
  {-# SCC breakpointAddResumption #-}
  let (comm, st, depth, _tip) = resumption
      depth1 = depth + 1
  in
  case ch of
    UniChoice p -> (comm, st, depth1, Just (cfg, p))
    SeqChoice (p : _ps) g ->
      (addCommitStack g comm, addLevel depth st cfg comm ch, depth1, Just (cfg, p))
    ParChoice (p : _ps) ->
      (comm, addLevel depth st cfg comm ch, depth1, Just (cfg, p))
    _ -> error "broken invariant: addResumption with empty choice"

getActionCfgAtLevel :: Resumption -> Maybe (Cfg, (Action, State))
getActionCfgAtLevel resumption =
  case resumption of
    (_,_,_,x) -> x

updateCommitResumption :: State -> Resumption -> Resumption
{-# INLINE updateCommitResumption #-}
updateCommitResumption g resumption =
  let (comm, st, d, tip) = resumption in
  let newComm = updateCommitStack g comm in
  -- trace (show newComm) $
  (newComm , st, d, tip)

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
      -- trace (show commitStk) $
      case tpath of
        BLevel _ _path _ _ (UniChoice _) ->
          error "Broken invariant, the level cannot be UniChoice"

        BLevel _ path _ savedCommitStack (SeqChoice [ _ ] _) ->
          getNext (popCommitStack commitStk savedCommitStack, path)
        BLevel d path cfg savedCommitStack (SeqChoice ((_act, _n2): actions) st) ->
          if hasCommitted commitStk -- A commit happened
          then getNext (popCommitStack commitStk savedCommitStack, path)
          else Just (addResumption (popCommitStack commitStk savedCommitStack, path, d, Nothing) cfg (SeqChoice actions st))

        BLevel _ path _ _savedCommitStack (ParChoice [ _ ]) ->
          getNext (commitStk, path)
        BLevel d path cfg savedCommitStack (ParChoice ((_act, _n2): actions)) ->
          Just (addResumption (adjustCommitStack commitStk savedCommitStack, path, d, Nothing) cfg (ParChoice actions))

        BEmpty -> Nothing

        BLevel _ _ _ _ (SeqChoice [] _) -> error "Broken invariant, the current choice cannot be empty"
        BLevel _ _ _ _ (ParChoice []) -> error "Broken invariant, the current choice cannot be empty"


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
updateError (_, _, _, _) cfg@(Cfg inp _ _ _) res =
  let d = inputOffset inp in
  case parseError res of
    Nothing -> res { parseError = Just (d, cfg) }
    Just (j, _c) ->
      -- trace (showCfg cfg) $
      if d >= j
      then -- trace (_show cfg) $
           res { parseError = Just (d, cfg) }
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
runnerBias :: Aut a => BS.ByteString -> a -> Result
runnerBias s aut =
  let
      gbl :: PAST.GblFuns
      gbl = gblFunsAut aut

      react :: Cfg -> Resumption -> Result -> Result
      react cfg resumption result =
        -- trace "REACT" $
        case cfg of
          Cfg _inp _ctrl _out q ->
            -- trace (show cfg) $
            -- trace ("576: " ++ show (stateToString 576 aut)) $
            -- trace ("516: " ++ show (stateToString 516 aut)) $
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
        -- trace "CHOOSE" $
        case getActionCfgAtLevel resumption of
          Nothing -> backtrack resumption result
          Just (cfg@(Cfg inp ctrl out _q1), (act, q2)) ->
            -- trace (show $ stateToString q2 aut) $
            case act of
              BAct (CutBiasAlt g) ->
                let updResumption = updateCommitResumption g resumption
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
              BAct (FailAction _) ->
                let updResult = updateError resumption cfg result in
                backtrack resumption updResult
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
runnerLL :: Aut a => BS.ByteString -> a -> LLA -> Bool -> Result
runnerLL s aut laut flagMetrics =
  let
      gbl :: PAST.GblFuns
      gbl = gblFunsAut aut

      react :: Cfg -> Maybe LL.SynthLLAState -> Resumption -> Result -> Result
      react cfg@(Cfg inp _ctrl _out q) mq resumption result =
        -- trace "REACT" $
        -- trace (show (_getCommitStack resumption)) $
        -- trace (showCfg cfg) $
        let pq = case mq of
                   Nothing -> Left q
                   Just qSynth -> Right qSynth in
        let
          mpdx = LL.predictLL pq laut inp
        in
        case mpdx of
          Just (pdxs, finalState) ->
            -- trace (show pdxs) $
            -- trace (case cfg of Cfg inp _ _ _ -> show inp) $
            -- trace "BEFORE" $
            -- trace (showCfg cfg) $
            applyPredictions pdxs finalState cfg resumption result
          Nothing ->
            let localTransitions = nextTransition aut q in
            case localTransitions of
              Nothing -> {-# SCC backtrackSetStep #-}
                if isAcceptingCfg cfg aut
                then
                  let newResult = addResult cfg result
                  in backtrack resumption newResult
                else
                  let newResumption = addResumption resumption cfg (UniChoice (CAct Pop, q))
                  in choose newResumption result
              Just ch ->
                let newResumption = addResumption resumption cfg ch
                in choose newResumption result

      applyPredictions :: LL.Prediction -> Maybe LL.SynthLLAState -> Cfg -> Resumption -> Result -> Result
      applyPredictions prdx finalState cfg@(Cfg inp ctrl out q) resumption rslt =
        -- trace "applyPredictions" $
        let result = incrResultMetrics tickLL rslt flagMetrics in
        case LL.destrPrediction prdx of
          Nothing ->
            -- trace "AFTER" $ trace (showCfg cfg) $
            react cfg finalState resumption result
          Just (alt, alts) ->
            let tr = nextTransition aut q
                (branching, (act, q2), g1) = case (tr, alt) of
                              (Nothing, (CPop, _)) -> (CPop, (CAct Pop, q), -1)
                              (Just (UniChoice (a, q1)), (CUni, _)) -> (CUni, (a, q1), -1)
                              (Just (SeqChoice lst g), (CSeq, i)) -> (CSeq, (lst !! i), g)
                              (Just (ParChoice lst), (CPar, i)) -> (CPar, lst !! i, -1)
                              _ -> error "impossible combination"
                newResumption =
                  let
                    predChoice =
                      case branching of
                        CPop -> UniChoice (act, q2)
                        CUni -> UniChoice (act, q2)
                        CSeq -> SeqChoice [(act, q2)] g1
                        CPar -> ParChoice [(act, q2)]
                  in
                    addResumption resumption cfg predChoice
            in
               -- trace (show act) $
               case act of
                 BAct bact ->
                   case bact of
                     (CutBiasAlt g) ->
                       -- trace ("**********CUT_PRED:" ++ show (stateToString g aut)) $
                       let updResumption = updateCommitResumption g newResumption
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
        -- trace "CHOOSE" $
        -- trace (show (_getCommitStack resumption)) $
        let result = incrResultMetrics tickBacktrack rslt flagMetrics in
        case getActionCfgAtLevel resumption of
          Nothing -> backtrack resumption result
          Just (cfg@(Cfg inp ctrl out _n1), (act, q2)) ->
            --trace (show act) $
            case act of
              BAct (CutBiasAlt g) ->
                -- trace ("**********CUT:" ++ show (stateToString g aut)) $
                let newResumption = updateCommitResumption g resumption
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
              BAct (FailAction _) ->
                let updResult = updateError resumption cfg result in
                backtrack resumption updResult
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

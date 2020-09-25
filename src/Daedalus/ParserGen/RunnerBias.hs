module Daedalus.ParserGen.RunnerBias
  ( Result(..)
  , runnerBias
  , runnerLL
  , extractValues
  )


where

--import Debug.Trace

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Daedalus.Interp as Interp

import qualified Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), BranchAction(..), SemanticElm(..), applyAction)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..))
import Daedalus.ParserGen.Cfg (initCfg, Cfg(..), isAcceptingCfg)

import Daedalus.ParserGen.Det as Det
import Daedalus.ParserGen.DetUtils as DetU



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

type BacktrackStackInfo = Int

data BacktrackStack =
    BEmpty
  | BLevel !BacktrackStackInfo !BacktrackStack !Cfg !Choice
  deriving(Show)


getBacktrackStackInfo :: BacktrackStack -> BacktrackStackInfo
{-# INLINE getBacktrackStackInfo #-}
getBacktrackStackInfo BEmpty           = 0
getBacktrackStackInfo (BLevel n _ _ _) = n

addLevel :: BacktrackStack -> Cfg -> Choice -> BacktrackStack
{-# INLINE addLevel #-}
addLevel tpath cfg ch =
  BLevel (getBacktrackStackInfo tpath + 1) tpath cfg ch


-- The Resumption data-type is the combination of the BacktrackStack
-- and CommitStack that are somewhat synchronized.
type Resumption = (CommitStack, BacktrackStack)


emptyResumption :: Resumption
emptyResumption = ([], BEmpty)

addResumption :: Resumption -> Cfg -> Choice -> Resumption
{-# INLINE addResumption #-}
addResumption resumption cfg ch =
  let (comm, st) = resumption in
  case ch of
    UniChoice _ ->   (comm, addLevel st cfg ch)
    SeqChoice _ _ -> (addCommitStack comm, addLevel st cfg ch)
    ParChoice _ ->   (comm, addLevel st cfg ch)

getActionCfgAtLevel :: Resumption -> Maybe (Cfg, (Action, State))
{-# INLINE getActionCfgAtLevel #-}
getActionCfgAtLevel resumption =
  case resumption of
    (_, BLevel _ _ cfg ch) ->
      case ch of
        UniChoice p -> Just (cfg, p)
        ParChoice (p : _) -> Just (cfg, p)
        SeqChoice (p : _) _ -> Just (cfg, p)
        _ -> error "No next action"
    (_, BEmpty) -> Nothing

updateCommitResumption :: Resumption -> Resumption
{-# INLINE updateCommitResumption #-}
updateCommitResumption resumption =
  let (comm, st) = resumption in
    (updateCommitStack comm, st)

earlyUpdateCommitResumption :: Resumption -> Resumption
earlyUpdateCommitResumption resumption =
  let (comm, st) = resumption in
    (earlyUpdateCommitStack comm, st)

cutResumption :: Resumption -> Resumption
cutResumption _resumption =
  (emptyCommitStack, BEmpty)


nextResumption :: Resumption -> Maybe Resumption
{-# INLINE nextResumption #-}
nextResumption (commitStk, tpath) =
  case tpath of
    BLevel _ path _ (UniChoice _) -> nextResumption (commitStk, path)

    BLevel _ path _ (SeqChoice [ _ ] _) -> nextResumption (popCommitStack commitStk, path)
    BLevel _ path cfg (SeqChoice ((_act, _n2): actions) st) ->
            -- trace "BACKT" $
      if hasCommitted commitStk -- A commit happened
      then nextResumption (popCommitStack commitStk, path)
      else Just (addResumption (popCommitStack commitStk, path) cfg (SeqChoice actions st))

    BLevel _ path _ (ParChoice [ _ ]) ->
      nextResumption (commitStk, path)
    BLevel _ path cfg (ParChoice ((_act, _n2): actions)) ->
      Just (addResumption (commitStk, path) cfg (ParChoice actions))

    BEmpty -> Nothing

    BLevel _ _ _ (SeqChoice [] _) -> error "Broken invariant, the current choice cannot be empty"
    BLevel _ _ _ (ParChoice []) -> error "Broken invariant, the current choice cannot be empty"



data Result = Result
  { results :: [Cfg]
  , parseError :: Maybe (Int, Cfg)
  }

emptyResult :: Result
emptyResult = Result { results = [], parseError = Nothing }

addResult :: Cfg -> Result -> Result
addResult cfg res = res { results = cfg : (results res) }

updateError :: Resumption -> Cfg -> Result -> Result
updateError resumption cfg res =
  let i = getBacktrackStackInfo (snd resumption) in
  case parseError res of
    Nothing -> res { parseError = Just (i, cfg) }
    Just (j, _c) ->
      -- trace "CHANGE" $
      if i >= j
      then res { parseError = Just (i, cfg) }
      else res


-- The function that runs an automaton from a string and returns
-- accepting configurations.
runnerBias :: Aut a => PAST.GblFuns -> BS.ByteString -> a -> Result
runnerBias gbl s aut =
  let go :: Cfg -> Resumption -> Result -> Result
      go cfg resumption result =
        case cfg of
          Cfg _inp _ctrl _out q ->
            -- trace (show cfg) $
            let localTransitions = nextTransition aut q
            in setStep cfg localTransitions resumption result

      setStep :: Cfg -> Maybe Choice -> Resumption -> Result -> Result
      setStep cfg choices resumption result =
        case choices of
          Nothing -> {-# SCC backtrackSetStep #-} backtrack resumption result
          Just ch ->
            let newResumption = addResumption resumption cfg ch in
              step newResumption result

      step :: Resumption -> Result -> Result
      step resumption result =
        case getActionCfgAtLevel resumption of
          Nothing -> backtrack resumption result
          Just (cfg@(Cfg inp ctrl out _n1), (act, n2)) ->
            case act of
              BAct (CutBiasAlt _st) ->
                let updResumption = updateCommitResumption resumption
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in
                   go newCfg updResumption updResult
              BAct (CutLocal) ->
                let updResumption = earlyUpdateCommitResumption resumption
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in
                   go newCfg updResumption updResult
              BAct (CutGlobal) ->
                let updResumption = cutResumption resumption
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in
                   go newCfg updResumption updResult
              BAct (FailAction Nothing) ->
                let updResult = updateError resumption cfg result in
                backtrack resumption updResult
              BAct (FailAction _) ->
                error "FailAction not handled"
              _ ->
                case applyAction gbl (inp, ctrl, out) act of
                  Nothing -> {-# SCC backtrackFailApplyAction #-}
                    let updResult = updateError resumption cfg result in
                    backtrack resumption updResult
                  Just (inp2, ctr2, out2) ->
                    let newCfg = Cfg inp2 ctr2 out2 n2
                        updResult = if isAcceptingCfg newCfg aut
                                    then addResult newCfg result
                                    else result
                    in go newCfg resumption updResult

      backtrack :: Resumption -> Result -> Result
      backtrack resumption result =
        -- trace "BACKTRACK" $
        case nextResumption resumption of
          Nothing -> result
          Just nextRes -> step nextRes result

  in go (initCfg s aut) emptyResumption emptyResult

-- This runner is using both the NFA and the DFA to parse.
runnerLL :: Aut a => PAST.GblFuns -> BS.ByteString -> a -> AutDet -> Result
runnerLL gbl s aut autDet =
  let go :: Cfg -> Resumption -> Result -> Result
      go cfg resumption result =
        case cfg of
          Cfg inp _ctrl _out q ->
            -- trace (show cfg) $
            let detTrans = Det.lookupAutDet q autDet in
            case detTrans of
              Nothing -> callNFA ()
              Just (_tr, False) -> callNFA ()
              Just (tr, True) ->
                -- trace "YES LL lookup" $
                let a = Det.predictLL tr inp in
                case a of
                  Nothing ->
                    callNFA ()
                    -- NOTE: here we call `callNFA()` instead of
                    -- `backtrack idx resumption result` in order to
                    -- maintain the behavior of reaching the
                    -- parseError the furthest. If we called backtrack
                    -- we would not update the parseError information
                  Just pdxs ->
                    -- trace (show tr) $
                    -- trace (show pdxs) $
                    -- trace (case cfg of Cfg inp _ _ _ -> show inp) $
                    applyPredictions pdxs cfg resumption result

            where
              callNFA () =
                let localTransitions = nextTransition aut q
                in setStep cfg localTransitions resumption result

      applyPredictions :: Det.Prediction -> Cfg -> Resumption -> Result -> Result
      applyPredictions prdx cfg@(Cfg inp ctrl out n) resumption result =
        case Det.destrPrediction prdx of
          Nothing -> go cfg resumption result
          Just (alt, alts) ->
            let tr = fromJust $ nextTransition aut n
                (act, n2) = case (tr, alt) of
                              (UniChoice (a, n1), (CUni, _)) -> (a, n1)
                              (SeqChoice lst _, (CSeq, i)) -> lst !! i
                              (ParChoice lst, (CPar, i)) -> lst !! i
                              _ -> error "impossible combination"
                newResumption = addResumption resumption cfg (UniChoice (act, n2))
            in
               case act of
                 BAct bact ->
                   case bact of
                     (CutBiasAlt _st) ->
                       let updResumption = updateCommitResumption newResumption
                           newCfg = Cfg inp ctrl out n2
                           updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                       in applyPredictions alts newCfg updResumption updResult
                     _ -> undefined
                 _ ->
                     case applyAction gbl (inp, ctrl, out) act of
                       Nothing -> {-# SCC backtrackFailApplyAction #-}
                         let updResult = updateError resumption cfg result
                         in backtrack newResumption updResult
                       Just (inp2, ctr2, out2) ->
                         let newCfg = Cfg inp2 ctr2 out2 n2
                             updResult =
                               if isAcceptingCfg newCfg aut
                               then addResult newCfg result
                               else result
                         in applyPredictions alts newCfg newResumption updResult

      setStep :: Cfg -> Maybe Choice -> Resumption -> Result -> Result
      setStep cfg choices resumption result =
        case choices of
          Nothing -> {-# SCC backtrackSetStep #-} backtrack resumption result
          Just ch ->
            let newResumption = addResumption resumption cfg ch in
              step newResumption result

      step :: Resumption -> Result -> Result
      step resumption result =
        case getActionCfgAtLevel resumption of
          Nothing -> backtrack resumption result
          Just (cfg@(Cfg inp ctrl out _n1), (act, n2)) ->
            case act of
              BAct (CutBiasAlt _st) ->
                let newResumption = updateCommitResumption resumption
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in
                   go newCfg newResumption updResult
              BAct (CutLocal) ->
                let newResumption = earlyUpdateCommitResumption resumption
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in
                   go newCfg newResumption updResult
              BAct (CutGlobal) ->
                let newResumption = cutResumption resumption
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in
                   go newCfg newResumption updResult
              BAct (FailAction Nothing) ->
                let updResult = updateError resumption cfg result in
                backtrack resumption updResult
              BAct (FailAction _) ->
                error "FailAction not handled"
              _ ->
                case applyAction gbl (inp, ctrl, out) act of
                  Nothing -> {-# SCC backtrackFailApplyAction #-}
                    let updResult = updateError resumption cfg result in
                    backtrack resumption updResult
                  Just (inp2, ctr2, out2) ->
                    let newCfg = Cfg inp2 ctr2 out2 n2
                        updResult = if isAcceptingCfg newCfg aut
                                    then addResult newCfg result
                                    else result
                    in go newCfg resumption updResult

      backtrack :: Resumption -> Result -> Result
      backtrack resumption result =
        -- trace "BACKTRACK" $
        case nextResumption resumption of
          Nothing -> result
          Just nextRes -> step nextRes result

  in go (initCfg s aut) emptyResumption emptyResult


extractValues :: Result -> [ Interp.Value ]
extractValues res =
  [ v | Cfg _ _ ((SEVal v) : _) _ <- reverse (results res) ]

module Daedalus.ParserGen.RunnerBias where

-- import Debug.Trace

import qualified Data.ByteString as BS

import qualified Daedalus.Interp as Interp

import qualified Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), BranchAction(..), SemanticElm(..), applyAction)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..))
import Daedalus.ParserGen.Cfg (initCfg, Cfg(..), isAcceptingCfg)


data CommitType =
    CTrue
  | CFalse
  | CEarly
  deriving(Eq,Show)

type CommitHist = [CommitType]

emptyCommit :: CommitHist
emptyCommit = []

addCommit :: CommitHist -> CommitHist
addCommit hst = CFalse : hst

popCommit :: CommitHist -> CommitHist
popCommit hst = tail hst

hasCommitted :: CommitHist -> Bool
hasCommitted hst = let c = (head hst) in c == CTrue || c == CEarly

updateCommitList :: CommitHist -> CommitHist
updateCommitList [] = error "broken invariant"
updateCommitList (CFalse : r) = CTrue : r
updateCommitList (CTrue  : r) = CTrue : updateCommitList r
updateCommitList (CEarly : r) = CTrue : r

earlyUpdateCommitList :: CommitHist -> CommitHist
earlyUpdateCommitList [] = error "broken invariant"
earlyUpdateCommitList (CFalse : r) = CEarly : r
earlyUpdateCommitList (CTrue  : r) = CTrue  : earlyUpdateCommitList r
earlyUpdateCommitList (CEarly : r) = CEarly : earlyUpdateCommitList r

type TailInfo = Int

data TailPath =
    EmptyPath
  | Level TailInfo Choice [Choice] Path
    -- the first `Choice` is the current,
    -- the other `[Choice]` is alternatives
  deriving(Show)

type Path = (Cfg, TailPath)

getTailInfo :: Path -> TailInfo
getTailInfo (_, EmptyPath)     = 0
getTailInfo (_, Level n _ _ _) = n

addLevel :: Choice -> [Choice] -> Path -> TailPath
addLevel ch moreCh path =
  Level (getTailInfo path + 1) ch moreCh path


data Result = Result
  { results :: [Cfg]
  , parseError :: Maybe (Int, Cfg)
  }

emptyResult :: Result
emptyResult = Result { results = [], parseError = Nothing }

addResult :: Cfg -> Result -> Result
addResult cfg res = res { results = cfg : (results res) }

updateError :: Int -> Cfg -> Result -> Result
updateError i cfg res =
  case parseError res of
    Nothing -> res { parseError = Just (i, cfg) }
    Just (j, _c) ->
      if i >= j
      then res { parseError = Just (i, cfg) }
      else res


-- The function that runs an automaton from a string and returns
-- accepting configurations.
runnerBias :: Aut a => PAST.GblFuns -> BS.ByteString -> a -> Result
runnerBias gbl s aut =
  let go :: (Cfg, CommitHist, TailPath) -> Result -> Result
      go (cfg, idx, resumption) result =
        case cfg of
          Cfg _inp _ctrl _out q ->
            -- trace (show cfg) $
            let localTransitions = maybe [] (\ x -> [x]) (nextStep aut q)
            in setStep idx localTransitions (cfg, resumption) result

      setStep :: CommitHist -> [Choice] -> Path -> Result -> Result
      setStep idx choices (cfg, resumption) result =
        case choices of
          [] -> {-# SCC backtrackSetStep #-} backtrack idx resumption result
          ch : moreCh ->
            case ch of
              UniChoice (act, n2) ->
                step act n2 idx (addLevel ch moreCh (cfg, resumption)) result
              SeqChoice [] _ -> setStep idx moreCh (cfg, resumption) result
              SeqChoice ((act, n2): _) _ -> -- trace "RESET" $
                step act n2 (addCommit idx) (addLevel ch moreCh (cfg, resumption)) result
              ParChoice [] -> setStep idx moreCh (cfg, resumption) result
              ParChoice ((act, n2): _) ->
                step act n2 idx (addLevel ch moreCh (cfg, resumption)) result

      step :: Action -> State -> CommitHist -> TailPath -> Result -> Result
      step act n2 idx resumption result =
        -- trace (show (getTailInfo resumption)) $
        -- trace  (show act) $
        case resumption of
          EmptyPath -> error "Impossible"
          Level n _choice _choices (Cfg inp ctrl out n1, _res) ->
            case act of
              BAct (CutBiasAlt _st) ->
                let newCommitInfo = updateCommitList idx
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in -- trace ("IDX = " ++ show idx) $
                   go (newCfg, newCommitInfo, resumption) updResult
              BAct (CutLocal) ->
                let newCommitInfo = earlyUpdateCommitList idx
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in -- trace ("IDX = " ++ show idx) $
                   go (newCfg, newCommitInfo, resumption) updResult
              BAct (CutGlobal) ->
                let newCommitInfo = emptyCommit
                    newCfg = Cfg inp ctrl out n2
                    updResult = if isAcceptingCfg newCfg aut then addResult newCfg result else result
                in -- trace ("IDX = " ++ show idx) $
                   go (newCfg, newCommitInfo, EmptyPath) updResult
              BAct (FailAction Nothing) ->
                let updResult = updateError n (Cfg inp ctrl out n1) result in
                backtrack idx resumption updResult
              BAct (FailAction _) ->
                error "FailAction not handled"
              _ ->
                case applyAction gbl (inp, ctrl, out) act of
                  Nothing -> {-# SCC backtrackFailApplyAction #-}
                    let updResult = updateError n (Cfg inp ctrl out n1) result in
                    backtrack idx resumption updResult
                  Just (inp2, ctr2, out2) ->
                    let newCfg = Cfg inp2 ctr2 out2 n2
                        updResult = if isAcceptingCfg newCfg aut
                                     then -- trace ("RES:\n" ++ show (lengthPath resumption)) $
                                          addResult newCfg result
                                     else result
                    in go (newCfg, idx, resumption) updResult

      backtrack :: CommitHist -> TailPath -> Result -> Result
      backtrack idx resumption result =
        -- trace "BACKTRACK" $
        case resumption of
          EmptyPath -> result

          Level _ (UniChoice _) choices path ->
            setStep idx choices path result

          Level _ (SeqChoice [] _) _ _ -> error "Broken invariant, the current choice cannot be empty"
          Level _ (SeqChoice [ _ ] _) choices path ->
            setStep (popCommit idx) choices path result
          Level _ (SeqChoice ((_act, _n2): actions) st) choices path ->
            -- trace "BACKT" $
            if hasCommitted idx -- A commit happened
            then setStep (popCommit idx) choices path result
            else setStep (popCommit idx) ((SeqChoice actions st) : choices) path result

          Level _ (ParChoice []) _ _ -> error "Broken invariant, the current choice cannot be empty"
          Level _ (ParChoice [ _ ]) choices path ->
            setStep idx choices path result
          Level _ (ParChoice ((_act, _n2): actions)) choices path ->
            setStep idx ((ParChoice actions) : choices) path result

  in go (initCfg s aut, emptyCommit, EmptyPath) emptyResult


extractValues :: Result -> [ Interp.Value ]
extractValues res =
  [ v | Cfg _ _ ((SEVal v) : _) _ <- reverse (results res) ]

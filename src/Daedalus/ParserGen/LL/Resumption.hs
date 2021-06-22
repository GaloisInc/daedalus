{-# Language GADTs #-}

module Daedalus.ParserGen.LL.Resumption
  ( Path
  , Branch
  , closureLoop2
  ) where

-- This file is a quick experiment attempt at having a closure that
-- returns an abstract resumptions so that it can handle bias choice
-- precisely.


-- import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Daedalus.ParserGen.Action (
  State,
  Action(..),
  ControlAction(..),
  isClassActOrEnd,
  isUnhandledInputAction,
  isUnhandledAction)
import Daedalus.ParserGen.Aut as Aut
-- import Daedalus.ParserGen.LL.ClassInterval
import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.ParamLL (cst_CLOSURE_MAX_DEPTH)
import qualified Daedalus.ParserGen.LL.SlkCfg as Slk



data Path =
    PathCancelled
  | PathSingle
    { _lastCfg :: Slk.SlkCfg
    }
  | PathBranch
    { closureCfg :: Slk.SlkCfg
    , branch :: Branch
    }
  | PathPreMove
    { closureCfg :: Slk.SlkCfg
    , moveCfg :: ((Action, Slk.InputHeadCondition), State)
    }
  | PathPostMove
    { closureCfg :: Slk.SlkCfg
    , moveCfg :: ((Action, Slk.InputHeadCondition), State)
    , lastCfg :: Slk.SlkCfg
    }
  | PathAccepting
    { closureCfg :: Slk.SlkCfg
    }

data Branch =
    BiasAlt [ Path ]
  | UnbiasAlt [ Path ]
  | PopAlt [ Path ]
  | NoAlt Path


data BranchType =
    BiasB
  | UnbiasB
  | PopB
  | NoAltB


-- data ZPath =
--   ZPathBranch
--   { startCfg :: Slk.SlkCfg
--   , lastCfg :: Slk.SlkCfg
--   , typeBranch :: BranchType
--   , leftBranch :: [ Path ]
--   , focusBranch :: ZPath
--   , rightBranch :: [ Path ]
--   }


-- composeBranch :: Path -> Branch -> Path
-- composeBranch path branch =
--   case path of
--     PathSingle { startCfg = st, lastCfg = last } ->
--       error ""


-- addStepBranch :: Slk.SlkCfg -> Path -> Path
-- addStepBranch cfg path =
--   case path of
--     PathCancelled -> PathCancelled
--     PathSingle {} -> path { startCfg = cfg }
--     PathBranch {} -> path { startCfg = cfg }
--     PathPreMove {} -> path { startCfg = cfg }
--     PathPostMove {} -> error "should not be called"
--     PathAccepting {} -> path { startCfg = cfg }

data ChoiceTag =
    CUni (Action, State)
  | CPar [(Action, State)]
  | CSeq [(Action, State)]
  | CPop (Action, State)


type ChoicePos = (ChoiceTag, Int)

initChoicePos :: ChoiceTag -> ChoicePos
initChoicePos tag = (tag, 0)

nextChoicePos :: ChoicePos -> ChoicePos
nextChoicePos pos = (fst pos, snd pos + 1)



type ChoiceSeq = Seq.Seq ChoicePos

emptyChoiceSeq :: ChoiceSeq
emptyChoiceSeq = Seq.Empty

addChoiceSeq :: ChoicePos -> ChoiceSeq -> ChoiceSeq
addChoiceSeq pos seqpos = seqpos Seq.|> pos



-- This functions pursues all the possible paths upto a input read.  A
-- subsequent function would resolve the choice based on the possible
-- inputs and discard branches.
closureLoop2 ::
  Aut.Aut a =>
  a ->
  Set.Set Slk.SlkCfg -> Int -> Slk.SlkCfg ->
  Slk.HTable -> (Result Path, Slk.HTable)
closureLoop2 aut busy depth cfg tab =
  if Set.member cfg busy
  then (Abort AbortClosureInfiniteloop, tab)
  else
    let
      q = Slk.cfgState cfg
      ch = Aut.nextTransition aut q
    in
    case ch of
      Nothing ->
        if Aut.isAcceptingState aut q
        then (Result (PathAccepting { closureCfg = cfg }), tab)
        else
          closureOnChoice (CPop (CAct Pop, q)) tab
      Just ch1 ->
        let
          ch =
            case ch1 of
              Aut.UniChoice (act, q2) -> (CUni (act, q2))
              Aut.SeqChoice lst _     -> (CSeq lst)
              Aut.ParChoice lst       -> (CPar lst)
        in closureOnChoice ch tab

  where
    newBusy = Set.insert cfg busy

    closureStep ::
      (Action, State) ->
      Slk.HTable -> (Result [Path], Slk.HTable)
    closureStep (act, q2) stepTab
      | isClassActOrEnd act =
          let
            mIhc = Slk.convertActionToInputHeadCondition (Aut.gblFunsAut aut) act cfg
          in
          case mIhc of
            Result ihc ->
              (Result [PathPreMove { closureCfg = cfg, moveCfg = ((act, ihc), q2) }], stepTab)
            Abort AbortSlkCfgClassIsDynamic -> (coerceAbort mIhc, stepTab)
            Abort (AbortSlkCfgClassNotHandledYet _) -> (coerceAbort mIhc, stepTab)
            _ -> error "Broken invariant: this case should not happen here"
      | isUnhandledInputAction act =
          (Abort AbortClosureUnhandledInputAction, stepTab)
      | isUnhandledAction act =
          -- trace (show act) $
          -- trace (Aut.stateToString q2 aut) $
          (Abort AbortClosureUnhandledAction, stepTab)
      | depth > cst_CLOSURE_MAX_DEPTH =
          (Abort AbortClosureOverflowMaxDepth, stepTab)
      | otherwise =
          case Slk.simulateActionSlkCfg aut act q2 cfg stepTab of
            Abort AbortSlkCfgExecution -> (Abort AbortSlkCfgExecution, stepTab)
            Result Nothing -> (Result [PathCancelled], stepTab)
            Result (Just (lstCfg, tab1)) ->
              let depth1 = depth + 1 in
              let
                (r, tabAll) =
                  foldr
                  (\ p (acc, locTab) ->
                      let (r1, newTab) = closureLoop2 aut newBusy depth1 p locTab in
                      case r1 of
                        Result p -> ((Result p) : acc, newTab)
                        _ -> (r1 : acc, newTab)
                  )
                  ([], tab1)
                  lstCfg
              in
              (combineResults r, tabAll)
            _ -> error "impossible"


    closureOnChoice ::
      ChoiceTag ->
      Slk.HTable -> (Result Path, Slk.HTable)
    closureOnChoice ch tabIt =
      case ch of
        CUni (act, q2) ->
          let (r, tab1) = closureStep (act, q2) tabIt in
          case r of
            Result [] -> (Result PathCancelled, tab1)
            Result [ p ] -> (Result p, tab1)
            Result _ ->  error "broken invariant"
        CPar lst ->
          let (r, tab1) = iterateChoice lst tabIt in
          case r of
            Result lpath ->
              (Result $ PathBranch { closureCfg = cfg, branch = (UnbiasAlt lpath)}, tab1)
            _ -> (coerceAbort r, tab1)
        CSeq lst ->
          let (r, tab1) = iterateChoice lst tabIt in
          case r of
            Result lpath ->
              (Result $ PathBranch { closureCfg = cfg, branch = (BiasAlt lpath)}, tab1)
            _ -> (coerceAbort r, tab1)
        CPop (act, q2) ->
          let (r, tab1) = closureStep (act, q2) tabIt in
          case r of
            Result [] -> (Result PathCancelled, tab1)
            Result [p] -> (Result p, tab1)
            Result lst ->
              (Result $ PathBranch {closureCfg = cfg, branch = (PopAlt lst)}, tab1)



    iterateChoice ::
      [(Action, State)] ->
      Slk.HTable -> (Result [ Path ], Slk.HTable)
    iterateChoice lst tabIt =
      let
        (lstRes, tabAll) =
          foldl
          (\ (acc, locTab) (act, q2) ->
             let (r, tab1) = closureStep (act, q2) locTab in
             case r of
               Result [] -> error "broken invariant"
               Result [ p ] -> (Result p : acc, tab1)
               Result _ -> error "broken invariant"
          )
          ([], tabIt)
          lst
      in
      (combineResults (reverse lstRes), tabAll)


    combineResults :: [Result a] -> Result [a]
    combineResults lst =
      case lst of
        [] -> Result []
        r1 : rest ->
          case r1 of
            Abort AbortSlkCfgExecution -> coerceAbort r1
            Abort AbortSlkCfgClassIsDynamic -> coerceAbort r1
            Abort (AbortSlkCfgClassNotHandledYet _) -> coerceAbort r1
            Abort AbortClosureOverflowMaxDepth -> coerceAbort r1
            Abort AbortClosureInfiniteloop -> coerceAbort r1
            Abort AbortClosureUnhandledInputAction -> coerceAbort r1
            Abort AbortClosureUnhandledAction -> coerceAbort r1
            Result res1 ->
              let r2 = combineResults rest in
              case r2 of
                Abort AbortSlkCfgExecution -> coerceAbort r2
                Abort AbortSlkCfgClassIsDynamic -> coerceAbort r2
                Abort (AbortSlkCfgClassNotHandledYet _) -> coerceAbort r2
                Abort AbortClosureOverflowMaxDepth -> coerceAbort r2
                Abort AbortClosureInfiniteloop -> coerceAbort r2
                Abort AbortClosureUnhandledInputAction -> coerceAbort r2
                Abort AbortClosureUnhandledAction -> coerceAbort r2
                Result resForRest -> Result (res1 : resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"



closureLL2 ::
  Aut.Aut a =>
  a ->
  Slk.SlkCfg ->
  Slk.HTable -> (Result Path, Slk.HTable)
closureLL2 aut cfg tab =
  closureLoop2 aut Set.empty 0 cfg tab

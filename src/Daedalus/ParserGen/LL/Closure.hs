module Daedalus.ParserGen.LL.Closure
  ( ChoiceTag(..)
  , ChoicePos
  , ChoiceSeq
  , addChoiceSeq
  , ClosureMove(..)
  , simulateMoveClosure
  , getAltSeq
  , ClosureMoveSet
  , closureEpsUntilPush
  , closureLL
  ) where


-- import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Set as Set


import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..), isClassActOrEnd, isUnhandledInputAction, isUnhandledAction, isPushAction)
import qualified Daedalus.ParserGen.Aut as Aut

import Daedalus.ParserGen.LL.Result
import qualified Daedalus.ParserGen.LL.SlkCfg as Slk



cst_MAX_DEPTH_REC :: Int
cst_MAX_DEPTH_REC = 200


data ChoiceTag = CUni | CPar | CSeq | CPop
  deriving(Eq, Show, Ord)

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



data ClosureMove =
    ClosureMove
    { altSeq :: ChoiceSeq
    , closureCfg :: Slk.SlkCfg
    , moveCfg :: (ChoicePos, Action, State)
    }
  | ClosurePath
    { altSeq :: ChoiceSeq
    , closureCfg :: Slk.SlkCfg
    , infoMove :: (ChoicePos, Action, State)
    , lastCfg :: Slk.SlkCfg
    }
  | ClosureAccepting
    { altSeq :: ChoiceSeq
    , closureCfg :: Slk.SlkCfg
    }
  deriving Show

instance Ord (ClosureMove) where
  compare c1@(ClosureMove {}) c2@(ClosureMove {}) =
    case compare (closureCfg c1) (closureCfg c2) of
      LT -> LT
      GT -> GT
      EQ ->
        case compare (altSeq c1) (altSeq c2) of
          LT -> LT
          GT -> GT
          EQ ->
            let
              (ch1, _, q1) = moveCfg c1
              (ch2, _, q2) = moveCfg c2
            in
              case compare ch1 ch2 of
                LT -> LT
                GT -> GT
                EQ ->
                  compare q1 q2
  compare (ClosureMove {}) _ = LT
  compare _ (ClosureMove {}) = GT

  compare c1@(ClosureAccepting {}) c2@(ClosureAccepting {}) =
    case compare (closureCfg c1) (closureCfg c2) of
      LT -> LT
      GT -> GT
      EQ ->
        compare (altSeq c1) (altSeq c2)
  compare (ClosureAccepting {}) _ = LT
  compare _ (ClosureAccepting {}) = GT

  compare c1@(ClosurePath {}) c2@(ClosurePath {}) =
    case compare (closureCfg c1) (closureCfg c2) of
      LT -> LT
      GT -> GT
      EQ ->
        case compare (altSeq c1) (altSeq c2) of
          LT -> LT
          GT -> GT
          EQ ->
            let
              (ch1, _, q1) = infoMove c1
              (ch2, _, q2) = infoMove c2
            in
              case compare ch1 ch2 of
                LT -> LT
                GT -> GT
                EQ ->
                  case compare q1 q2 of
                    LT -> LT
                    GT -> GT
                    EQ -> compare (lastCfg c1) (lastCfg c2)

instance Eq (ClosureMove) where
  (==) c1 c2 = compare c1 c2 == EQ


getAltSeq :: ClosureMove -> ChoiceSeq
getAltSeq c =
  case c of
    ClosureMove {moveCfg = (p,_,_)} -> addChoiceSeq p (altSeq c)
    ClosurePath {} -> altSeq c
    ClosureAccepting {} -> altSeq c


simulateMoveClosure ::
  Slk.InputHeadCondition -> ClosureMove ->
  Slk.HTable -> Maybe (ClosureMove, Slk.HTable)
simulateMoveClosure ih m tab =
  let
    alt = altSeq $ m
    closCfg = closureCfg $ m
    mv@(pos,act,q) = moveCfg $ m
  in
    let mCfg = Slk.simulateMove ih closCfg act q tab in
      case mCfg of
        Nothing -> Nothing
        Just (newCfg, tab1) ->
          Just $
          ( ClosurePath
            { altSeq = addChoiceSeq pos alt
            , closureCfg = closCfg
            , infoMove = mv
            , lastCfg = newCfg
            }
          , tab1
          )


type ClosureMoveSet = [ClosureMove]


closureEpsUntilPush ::
  Aut.Aut a =>
  a -> Set.Set Slk.SlkCfg -> ClosureMove ->
  Slk.HTable -> (Result (Maybe ClosureMove), Slk.HTable)
closureEpsUntilPush aut busy cm tab =
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
        then (Result (Just cm), tab)
        else closureStep (initChoicePos CPop) (CAct Pop, q)
      Just ch1 ->
        case ch1 of
          Aut.UniChoice (act, q2) -> closureStep (initChoicePos CUni)  (act, q2)
          Aut.SeqChoice _ _     -> (Result $ Just cm, tab)
          Aut.ParChoice _       -> (Result $ Just cm, tab)

  where
    cfg = lastCfg cm
    alts = altSeq cm
    newBusy = Set.insert cfg busy

    closureStep ::
      ChoicePos -> (Action, State) ->
      (Result (Maybe ClosureMove), Slk.HTable)
    closureStep pos (act, q2)
      | isClassActOrEnd act =
          (Result (Just cm), tab)
      | isUnhandledInputAction act =
          -- trace (show act) $
          (Abort AbortClosureUnhandledInputAction, tab)
      | isUnhandledAction act =
          -- trace (show act) $
          (Abort AbortClosureUnhandledAction, tab)
      | isPushAction act =
          -- trace (show act) $
          (Result (Just cm), tab)
      | Seq.length alts > cst_MAX_DEPTH_REC = (Abort AbortClosureOverflowMaxDepth, tab)
      | otherwise =
          case Slk.simulateActionSlkCfg aut act q2 cfg tab of
            Abort AbortSlkCfgExecution -> (Abort AbortSlkCfgExecution, tab)
            Result Nothing -> (Result (Just cm), tab)
            Result (Just (lstCfg, tab1)) ->
              case lstCfg of
                [] -> (Result Nothing, tab1)
                [ newCfg ] ->
                  let cm1 =
                        ClosurePath
                        { altSeq = addChoiceSeq pos alts
                        , closureCfg = closureCfg cm
                        , infoMove = infoMove cm
                        , lastCfg = newCfg
                        }
                  in closureEpsUntilPush aut newBusy cm1 tab1
                _ -> (Result (Just cm), tab1)
            _ -> error "impossible"



closureLoop ::
  Aut.Aut a =>
  a -> Set.Set Slk.SlkCfg -> (ChoiceSeq, Slk.SlkCfg) ->
  Slk.HTable -> (Result ClosureMoveSet, Slk.HTable)
closureLoop aut busy (alts, cfg) tab =
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
          then (Result [ ClosureAccepting alts cfg ], tab)
          else iterateChoice (initChoicePos CPop) [(CAct Pop, q)] tab
        Just ch1 ->
          let (tag, lstCh) =
                case ch1 of
                  Aut.UniChoice (act, q2) -> (CUni, [(act, q2)])
                  Aut.SeqChoice lst _     -> (CSeq, lst)
                  Aut.ParChoice lst       -> (CPar, lst)
          in iterateChoice (initChoicePos tag) lstCh tab

  where
    newBusy = Set.insert cfg busy

    closureStep ::
      ChoicePos -> (Action, State) ->
      Slk.HTable -> (Result ClosureMoveSet, Slk.HTable)
    closureStep pos (act, q2) stepTab
      | isClassActOrEnd act =
          (Result [ ClosureMove alts cfg (pos, act, q2) ], stepTab)
      | isUnhandledInputAction act =
          -- trace (show act) $
          (Abort AbortClosureUnhandledInputAction, stepTab)
      | isUnhandledAction act =
          -- trace (show act) $
          (Abort AbortClosureUnhandledAction, stepTab)
      | Seq.length alts > cst_MAX_DEPTH_REC =
          (Abort AbortClosureOverflowMaxDepth, stepTab)
      | otherwise =
          case Slk.simulateActionSlkCfg aut act q2 cfg stepTab of
            Abort AbortSlkCfgExecution -> (Abort AbortSlkCfgExecution, stepTab)
            Result Nothing -> (Result [], stepTab)
            Result (Just (lstCfg, tab1)) ->
              let newAlts = addChoiceSeq pos alts in
              let
                (r, tabAll) =
                  foldr
                  (\ p (acc, locTab) ->
                      let (r1, newTab) = closureLoop aut newBusy (newAlts, p) locTab
                      in (r1 : acc, newTab)
                  )
                  ([], tab1)
                  lstCfg
              in
              let r2 = combineResults r in
              (r2, tabAll)
            _ -> error "impossible"


    iterateChoice ::
      ChoicePos -> [(Action, State)] ->
      Slk.HTable -> (Result ClosureMoveSet, Slk.HTable)
    iterateChoice pos ch tabIt =
      let
        (_ , lstRes, tabAll) =
          foldl
          (\ (pos1, acc, locTab) (act, q2) ->
             let (r, tab1) = closureStep pos1 (act, q2) locTab in
             (nextChoicePos pos1, r : acc, tab1)
          )
          (pos, [], tabIt)
          ch
      in
      (combineResults (reverse lstRes), tabAll)

    combineResults lst =
      case lst of
        [] -> Result []
        r1 : rest ->
          case r1 of
            Abort AbortSlkCfgExecution -> r1
            Abort AbortClosureOverflowMaxDepth -> r1
            Abort AbortClosureInfiniteloop -> r1
            Abort AbortClosureUnhandledInputAction -> r1
            Abort AbortClosureUnhandledAction -> r1
            Result res1 ->
              let r2 = combineResults rest in
              case r2 of
                Abort AbortSlkCfgExecution -> r2
                Abort AbortClosureOverflowMaxDepth -> r2
                Abort AbortClosureInfiniteloop -> r2
                Abort AbortClosureUnhandledInputAction -> r2
                Abort AbortClosureUnhandledAction -> r2
                Result resForRest -> Result (res1 ++ resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"



closureLL ::
  Aut.Aut a =>
  a -> Slk.SlkCfg ->
  Slk.HTable -> (Result ClosureMoveSet, Slk.HTable)
closureLL aut cfg tab = closureLoop aut Set.empty (emptyChoiceSeq, cfg) tab

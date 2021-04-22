module Daedalus.ParserGen.LL.Closure
  ( ChoiceTag(..)
  , ChoicePos
  , ChoiceSeq
  , addChoiceSeq
  , ClosureMove(..)
  , simulateMoveClosure
  , getAltSeq
  , emptyChoiceSeq
  , ClosureMoveSet
  , DataDepInstr(..)
  , closureEpsUntilDataDependent
  , closureEpsUntilPush
  , closureLL
  ) where


-- import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Set as Set


import Daedalus.ParserGen.Action (
  State,
  Action(..),
  ControlAction(..),
  isClassActOrEnd,
  isUnhandledInputAction,
  isUnhandledAction,
  isPushAction)
import qualified Daedalus.ParserGen.Aut as Aut

import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.ParamLL (cst_CLOSURE_MAX_DEPTH)
import qualified Daedalus.ParserGen.LL.SlkCfg as Slk




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
    , moveCfg :: (ChoicePos, (Action, Slk.InputHeadCondition), State)
    }
  | ClosurePath
    { altSeq :: ChoiceSeq
    , closureCfg :: Slk.SlkCfg
    , infoMove :: (ChoicePos, (Action, Slk.InputHeadCondition), State)
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
  Slk.InputHeadCondition ->
  ClosureMove ->
  Slk.HTable -> Maybe (ClosureMove, Slk.HTable)
simulateMoveClosure ih m tab =
  -- here the `InputHeadCondition` is provided as an extra parameter,
  -- instead of using the one in the move because this function is
  -- called from the DFA and the `InputHeadCondition` can be a subset
  -- of the one associated with the `Action` in the move.
  let
    alt = altSeq $ m
    closCfg = closureCfg $ m
    mv@(pos, (act, _), q) = moveCfg $ m
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


data DataDepInstr =
    DDManyBetween ChoiceSeq Slk.SlkCfg Slk.SlkCfg
  | DDStreamSet Slk.SlkCfg
  deriving (Show)

-- This a non-deterministic closure path.
closureEpsUntilDataDependent ::
  Aut.Aut a =>
  a -> Set.Set Slk.SlkCfg -> (ChoiceSeq, Slk.SlkCfg) ->
  Slk.HTable -> (Result (Maybe DataDepInstr), Slk.HTable)
closureEpsUntilDataDependent aut busy (alts, cfg) tab =
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
        then
             (Result Nothing, tab)
        else closureStep (initChoicePos CPop) (CAct Pop, q)
      Just ch1 ->
        case ch1 of
          Aut.UniChoice (act, q2) ->
            closureStep (initChoicePos CUni) (act, q2)
          Aut.SeqChoice
            [ (act1@(CAct (BoundIsMore)), i)
            , (act2@(CAct (BoundCheckSuccess)), j)
            ] _ ->
            if Slk.isManyExactDependent cfg
            then
              case
                ( Slk.simulateActionSlkCfg aut act1 i cfg tab
                , Slk.simulateActionSlkCfg aut act2 j cfg tab
                ) of
                (   Result (Just ([ cfg1 ], _tab1))
                  , Result (Just ([ cfg2 ], tab2))
                  ) ->
                  (Result $ Just (DDManyBetween alts cfg1 cfg2), tab2)
                _ -> error "should not happen"

            else
              (Result Nothing, tab)
          Aut.SeqChoice _ _ ->
            (Result Nothing, tab)
          Aut.ParChoice _ ->
            (Result Nothing, tab)

  where
    newBusy = Set.insert cfg busy

    closureStep ::
      ChoicePos -> (Action, State) ->
      (Result (Maybe DataDepInstr), Slk.HTable)
    closureStep pos (act, q2)
      | isClassActOrEnd act =
          (Result Nothing, tab)
      | isUnhandledInputAction act =
          (Abort AbortClosureUnhandledInputAction, tab)
      | isUnhandledAction act =
          (Abort AbortClosureUnhandledAction, tab)
      | isPushAction act =
          (Result Nothing, tab)
      | Seq.length alts > cst_CLOSURE_MAX_DEPTH =
          (Abort AbortClosureOverflowMaxDepth, tab)
      | Slk.isStreamSetDynamic act cfg =
          let (newCfg, newTab) = Slk.simulateDynamicStreamSet act q2 cfg tab in
          (Result $ Just $ DDStreamSet newCfg, newTab)
      | otherwise =
          case Slk.simulateActionSlkCfg aut act q2 cfg tab of
            Abort AbortSlkCfgExecution -> (Abort AbortSlkCfgExecution, tab)
            Result Nothing -> (Result Nothing, tab)
            Result (Just (lstCfg, tab1)) ->
              case lstCfg of
                [] -> (Result Nothing, tab1)
                [ newCfg ] ->
                  let
                    newAltSeq = addChoiceSeq pos alts
                  in closureEpsUntilDataDependent aut newBusy (newAltSeq, newCfg) tab1
                _ -> (Result Nothing, tab1)
            _ -> error "impossible"


closureEpsUntilPush ::
  Aut.Aut a =>
  a ->
  Set.Set Slk.SlkCfg ->
  ClosureMove ->
  Slk.HTable -> (Result (Maybe ClosureMove), Slk.HTable)
closureEpsUntilPush aut busy cm tab =
  --if True
  --then (Result (Just cm), tab)
  --else
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
          Aut.UniChoice (act, q2) -> closureStep (initChoicePos CUni) (act, q2)
          Aut.SeqChoice _ _     ->
            -- (Abort AbortClosureUnhandledAction, tab)
            (Result $ Just cm, tab)
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
          (Abort AbortClosureUnhandledInputAction, tab)
      | isUnhandledAction act =
          (Abort AbortClosureUnhandledAction, tab)
      | isPushAction act =
          (Result (Just cm), tab)
      | Seq.length alts > cst_CLOSURE_MAX_DEPTH =
          (Abort AbortClosureOverflowMaxDepth, tab)
      | Slk.isStreamSetDynamic act cfg =
        (Result (Just cm), tab)
      | otherwise =
          case Slk.simulateActionSlkCfg aut act q2 cfg tab of
            Abort AbortSlkCfgExecution -> (Abort AbortSlkCfgExecution, tab)
            Result Nothing -> (Result (Just cm), tab)
            Result (Just (lstCfg, tab1)) ->
              case lstCfg of
                [] -> (Result Nothing, tab1)
                [ newCfg ] ->
                  let
                    cm1 =
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
          let
            mIhc = Slk.convertActionToInputHeadCondition (Aut.gblFunsAut aut) act cfg
          in
          case mIhc of
            Result ihc ->
              (Result [ ClosureMove alts cfg (pos, (act, ihc), q2) ], stepTab)
            Abort AbortSlkCfgClassIsDynamic -> (coerceAbort mIhc, stepTab)
            Abort (AbortSlkCfgClassNotHandledYet _) -> (coerceAbort mIhc, stepTab)
            _ -> error "Broken invariant: this case should not happen here"
      | isUnhandledInputAction act =
          (Abort AbortClosureUnhandledInputAction, stepTab)
      | isUnhandledAction act =
          (Abort AbortClosureUnhandledAction, stepTab)
      | Seq.length alts > cst_CLOSURE_MAX_DEPTH =
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
            Abort AbortSlkCfgClassIsDynamic -> r1
            Abort (AbortSlkCfgClassNotHandledYet _) -> r1
            Abort AbortClosureOverflowMaxDepth -> r1
            Abort AbortClosureInfiniteloop -> r1
            Abort AbortClosureUnhandledInputAction -> r1
            Abort AbortClosureUnhandledAction -> r1
            Result res1 ->
              let r2 = combineResults rest in
              case r2 of
                Abort AbortSlkCfgExecution -> r2
                Abort AbortSlkCfgClassIsDynamic -> r2
                Abort (AbortSlkCfgClassNotHandledYet _) -> r2
                Abort AbortClosureOverflowMaxDepth -> r2
                Abort AbortClosureInfiniteloop -> r2
                Abort AbortClosureUnhandledInputAction -> r2
                Abort AbortClosureUnhandledAction -> r2
                Result resForRest -> Result (res1 ++ resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"



closureLL ::
  Aut.Aut a =>
  a ->
  Slk.SlkCfg ->
  Slk.HTable -> (Result ClosureMoveSet, Slk.HTable)
closureLL aut cfg tab = closureLoop aut Set.empty (emptyChoiceSeq, cfg) tab

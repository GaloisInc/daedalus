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


import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..), isClassActOrEnd, isNonClassInputAct, isUnhandledAction, isPushAction)
import qualified Daedalus.ParserGen.Aut as Aut

import Daedalus.ParserGen.LL.Result
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



simulateMoveClosure :: Slk.InputHeadCondition -> ClosureMove -> Maybe ClosureMove
simulateMoveClosure ih m =
  let
    alt = altSeq $ m
    closCfg = closureCfg $ m
    mv@(pos,act,q) = moveCfg $ m
  in
    let mCfg = Slk.simulateMove ih closCfg act q in
      case mCfg of
        Nothing -> Nothing
        Just newCfg ->
          Just $
          ClosurePath
          { altSeq = addChoiceSeq pos alt
          , closureCfg = closCfg
          , infoMove = mv
          , lastCfg = newCfg
          }


type ClosureMoveSet = [ClosureMove]


maxDepthRec :: Int
maxDepthRec = 800


closureEpsUntilPush :: Aut.Aut a => a -> Set.Set Slk.SlkCfg -> ClosureMove -> Result (Maybe ClosureMove)
closureEpsUntilPush aut busy cm =
  if Set.member cfg busy
  then Abort AbortLoopWithNonClass
  else
    let
      q = Slk.cfgState cfg
      ch = Aut.nextTransition aut q
    in
    case ch of
      Nothing ->
        if Aut.isAcceptingState aut q
        then Result $ Just cm
        else closureStep (initChoicePos CPop) (CAct Pop, q)
      Just ch1 ->
        case ch1 of
          Aut.UniChoice (act, q2) -> closureStep (initChoicePos CUni)  (act, q2)
          Aut.SeqChoice _ _     -> Result $ Just cm
          Aut.ParChoice _       -> Result $ Just cm

  where
    cfg = lastCfg cm
    alts = altSeq cm
    newBusy = Set.insert cfg busy

    closureStep :: ChoicePos -> (Action, State) -> Result (Maybe ClosureMove)
    closureStep pos (act, q2)
      | isClassActOrEnd act =
          Result $ Just cm
      | isNonClassInputAct act =
          -- trace (show act) $
          Abort $ AbortNonClassInputAction act
      | isUnhandledAction act =
          -- trace (show act) $
          Abort AbortUnhandledAction
      | isPushAction act =
          -- trace (show act) $
          Result $ Just cm
      | Seq.length alts > maxDepthRec = Abort AbortOverflowMaxDepth
      | otherwise =
          case Slk.simulateActionSlkCfg aut act q2 cfg of
            Abort AbortSymbolicExec -> Abort AbortSymbolicExec
            Result Nothing -> Result $ Just cm
            Result (Just lstCfg) ->
              case lstCfg of
                [] -> Result Nothing
                [ newCfg ] ->
                  let cm1 =
                        ClosurePath
                        { altSeq = addChoiceSeq pos alts
                        , closureCfg = closureCfg cm
                        , infoMove = infoMove cm
                        , lastCfg = newCfg
                        }
                  in closureEpsUntilPush aut newBusy cm1
                _ -> Result $ Just cm
            _ -> error "impossible"



closureLoop :: Aut.Aut a => a -> Set.Set Slk.SlkCfg -> (ChoiceSeq, Slk.SlkCfg) -> Result ClosureMoveSet
closureLoop aut busy (alts, cfg) =
  if Set.member cfg busy
  then Abort AbortLoopWithNonClass
  else
    let
      q = Slk.cfgState cfg
      ch = Aut.nextTransition aut q
    in
      case ch of
        Nothing ->
          if Aut.isAcceptingState aut q
          then Result [ ClosureAccepting alts cfg ]
          else iterateChoice (initChoicePos CPop) [(CAct Pop, q)]
        Just ch1 ->
          let (tag, lstCh) =
                case ch1 of
                  Aut.UniChoice (act, q2) -> (CUni, [(act, q2)])
                  Aut.SeqChoice lst _     -> (CSeq, lst)
                  Aut.ParChoice lst       -> (CPar, lst)
          in iterateChoice (initChoicePos tag) lstCh

  where
    newBusy = Set.insert cfg busy

    closureStep :: ChoicePos -> (Action, State) -> Result ClosureMoveSet
    closureStep pos (act, q2)
      | isClassActOrEnd act =
          Result [ ClosureMove alts cfg (pos, act, q2) ]
      | isNonClassInputAct act =
          -- trace (show act) $
          Abort $ AbortNonClassInputAction act
      | isUnhandledAction act =
          -- trace (show act) $
          Abort AbortUnhandledAction
      | Seq.length alts > maxDepthRec = Abort AbortOverflowMaxDepth
      | otherwise =
          case Slk.simulateActionSlkCfg aut act q2 cfg of
            Abort AbortSymbolicExec -> Abort AbortSymbolicExec
            Result Nothing -> Result []
            Result (Just lstCfg) ->
              let newAlts = addChoiceSeq pos alts in
              combineResults (map (\p -> closureLoop aut newBusy (newAlts, p)) lstCfg)
            _ -> error "impossible"


    iterateChoice :: ChoicePos -> [(Action, State)] -> Result ClosureMoveSet
    iterateChoice pos ch =
      let (_ , lstRes) = foldl (\ (pos1, acc) (act, q2) -> (nextChoicePos pos1, closureStep pos1 (act, q2) : acc)) (pos,[]) ch
      in
      combineResults (reverse lstRes)

    combineResults lst =
      case lst of
        [] -> Result []
        r1 : rest ->
          case r1 of
            Abort AbortOverflowMaxDepth -> r1
            Abort AbortLoopWithNonClass -> r1
            Abort (AbortNonClassInputAction _) -> r1
            Abort AbortUnhandledAction -> r1
            Abort AbortSymbolicExec -> r1
            Result res1 ->
              let r2 = combineResults rest in
              case r2 of
                Abort AbortOverflowMaxDepth -> r2
                Abort AbortLoopWithNonClass -> r2
                Abort (AbortNonClassInputAction _) -> r2
                Abort AbortUnhandledAction -> r2
                Abort AbortSymbolicExec -> r2
                Result resForRest -> Result (res1 ++ resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"



closureLL :: Aut.Aut a => a -> Slk.SlkCfg -> Result ClosureMoveSet
closureLL aut cfg = closureLoop aut Set.empty (emptyChoiceSeq, cfg)

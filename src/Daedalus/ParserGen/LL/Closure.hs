module Daedalus.ParserGen.LL.Closure
  ( ChoiceTag(..)
  , ChoicePos
  , ChoiceSeq
  , addChoiceSeq
  , ClosureMove(..)
  , ClosureMoveSet
  , closureLL
  ) where


-- import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Set as Set


import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..), isClassActOrEnd, isNonClassInputAct, isUnhandledAction)
import qualified Daedalus.ParserGen.Aut as Aut

import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.CfgDet


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



data ClosureMove = ClosureMove
  { altSeq :: ChoiceSeq
  , closureCfg :: CfgDet
  , moveCfg :: (ChoicePos, Action, State)
  }
  deriving Show

instance Ord (ClosureMove) where
  compare c1 c2 =
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

instance Eq (ClosureMove) where
  (==) c1 c2 = compare c1 c2 == EQ


type ClosureMoveSet = [ClosureMove]


maxDepthRec :: Int
maxDepthRec = 800


closureLoop :: Aut.Aut a => a -> Set.Set CfgDet -> (ChoiceSeq, CfgDet) -> Result ClosureMoveSet
closureLoop aut busy (alts, cfg) =
  if Set.member cfg busy
  then Abort AbortLoopWithNonClass
  else
    let
      q = cfgState cfg
      ch = Aut.nextTransition aut q
    in
      case ch of
        Nothing ->
          if Aut.isAcceptingState aut q
          then Abort AbortAcceptingPath
          else iterateThrough (initChoicePos CPop) [(CAct Pop, q)]
        Just ch1 ->
          let (tag, lstCh) =
                case ch1 of
                  Aut.UniChoice (act, q2) -> (CUni, [(act, q2)])
                  Aut.SeqChoice lst _     -> (CSeq, lst)
                  Aut.ParChoice lst       -> (CPar, lst)
          in iterateThrough (initChoicePos tag) lstCh

  where
    newBusy = Set.insert cfg busy

    closureStep :: ChoicePos -> (Action,State) -> Result ClosureMoveSet
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
          case simulateActionCfgDet aut act q2 cfg of
            Abort AbortSymbolicExec -> Abort AbortSymbolicExec
            Result Nothing -> Result []
            Result (Just lstCfg) -> combineResults (map (\p -> closureLoop aut newBusy (addChoiceSeq pos alts, p)) lstCfg)
            _ -> error "impossible"


    iterateThrough :: ChoicePos -> [(Action,State)] -> Result ClosureMoveSet
    iterateThrough pos ch =
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
            Abort AbortAcceptingPath -> r1
            Abort AbortSymbolicExec -> r1
            Result res1 ->
              let r2 = combineResults rest in
              case r2 of
                Abort AbortOverflowMaxDepth -> r2
                Abort AbortLoopWithNonClass -> r2
                Abort (AbortNonClassInputAction _) -> r2
                Abort AbortUnhandledAction -> r2
                Abort AbortAcceptingPath -> r2
                Abort AbortSymbolicExec -> r2
                Result resForRest -> Result (res1 ++ resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"



closureLL :: Aut.Aut a => a -> CfgDet -> Result ClosureMoveSet
closureLL aut cfg = closureLoop aut Set.empty (emptyChoiceSeq, cfg)

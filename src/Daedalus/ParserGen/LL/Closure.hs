module Daedalus.ParserGen.LL.Closure
  ( ClosureMove,
    ClosureMoveSet,
    closureLL
  ) where


-- import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Set as Set


import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..), isClassActOrEnd, isNonClassInputAct, isUnhandledAction)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..))

import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.CfgDet



type ClosureMove = (CfgDet, (ChoicePos, Action, State))

type ClosureMoveSet = [ClosureMove]


maxDepthRec :: Int
maxDepthRec = 800


closureLL :: Aut a => a -> Set.Set State -> CfgDet -> Result ClosureMoveSet
closureLL aut busy cfg =
  let
    q = cfgState cfg
    ch = nextTransition aut q
  in
    case ch of
      Nothing ->
        if isAcceptingState aut q
        then Abort AbortAcceptingPath
        else iterateThrough (initChoicePos CPop) [(CAct Pop, q)]
      --error "should not happen"
      Just ch1 ->
        let (tag, lstCh) =
              case ch1 of
                UniChoice (act, q2) -> (CUni, [(act, q2)])
                SeqChoice lst _     -> (CSeq, lst)
                ParChoice lst       -> (CPar, lst)
        in iterateThrough (initChoicePos tag) lstCh

  where
    newBusy = Set.insert (cfgState cfg) busy

    closureStep :: ChoicePos -> (Action,State) -> Result ClosureMoveSet
    closureStep pos (act, q2)
      | isClassActOrEnd act                = Result [(cfg, (pos, act, q2))]
      | isNonClassInputAct act             = -- trace (show act) $
                                             Abort $ AbortNonClassInputAction act
      | isUnhandledAction act              = Abort AbortUnhandledAction
      | Seq.length (cfgAlts cfg) > maxDepthRec = Abort AbortOverflowMaxDepth
      | Set.member q2 busy                 = -- trace (show q2 ++ " " ++ show cfg) $
                                             Abort AbortLoopWithNonClass
      | otherwise =
          -- trace ("q2: " ++ show (cfgState cfg)) $
          case simulateActionCfgDet aut pos act q2 cfg of
            Abort AbortSymbolicExec -> Abort AbortSymbolicExec
            Result Nothing -> Result []
            Result (Just lstCfg) -> combineResults (map (\p -> closureLL aut newBusy p) lstCfg)
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

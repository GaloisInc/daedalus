module Daedalus.ParserGen.LL.Closure
  ( ClosureMove,
    ClosureMoveSet,
    closureLL
  ) where


-- import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Set as Set


import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..), isClassActOrEnd, isNonClassInputAct)
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
        then AbortAcceptingPath
        else iterateThrough (initChoicePos CPop) [(CAct Pop, q)]
      --error "should not happen"
      Just ch1 ->
        let (tag, lstCh) =
              case ch1 of
                UniChoice (act, q1) -> (CUni, [(act,q1)])
                SeqChoice lst _     -> (CSeq, lst)
                ParChoice lst       -> (CPar, lst)
        in iterateThrough (initChoicePos tag) lstCh

  where
    newBusy = Set.insert (cfgState cfg) busy

    closureStep :: ChoicePos -> (Action,State) -> Result ClosureMoveSet
    closureStep pos (act, q1)
      | isClassActOrEnd act                = Result [(cfg, (pos, act, q1))]
      | isNonClassInputAct act             = -- trace (show act) $
                                             AbortNonClassInputAction act
      | Seq.length (cfgAlts cfg) > maxDepthRec = AbortOverflowMaxDepth
      | Set.member q1 busy                 = -- trace (show q1 ++ " " ++ show cfg) $
                                             AbortLoopWithNonClass
      | otherwise =
          case simulateActionCfgDet aut pos act q1 cfg of
            Nothing -> Result []
            Just lstCfg -> combineResults (map (\p -> closureLL aut newBusy p) lstCfg)


    iterateThrough :: ChoicePos -> [(Action,State)] -> Result ClosureMoveSet
    iterateThrough pos ch =
      let (_ , lstRes) = foldl (\ (pos1, acc) (act, q1) -> (nextChoicePos pos1, closureStep pos1 (act, q1) : acc)) (pos,[]) ch
      in
      combineResults (reverse lstRes)

    combineResults lst =
      case lst of
        [] -> Result []
        r1 : rest ->
          case r1 of
            AbortOverflowMaxDepth -> r1
            AbortLoopWithNonClass -> r1
            AbortNonClassInputAction _ -> r1
            AbortAcceptingPath -> r1
            Result res1 ->
              let r2 = combineResults rest in
              case r2 of
                AbortOverflowMaxDepth -> r2
                AbortLoopWithNonClass -> r2
                AbortNonClassInputAction _ -> r2
                AbortAcceptingPath -> r2
                Result resForRest -> Result (res1 ++ resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"

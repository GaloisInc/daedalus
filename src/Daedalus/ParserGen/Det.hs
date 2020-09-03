module Daedalus.ParserGen.Det where

import Debug.Trace

import qualified Data.Set as Set
-- import Data.Maybe (isNothing, fromJust)
import qualified Data.Map.Strict as Map

import qualified Daedalus.Interp as Interp

import Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), InputAction(..), isClassActOrEnd, isInputAction, isNonClassInputAct, getClassActOrEnd, evalNoFunCall, isSimpleVExpr)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..))

import Daedalus.ParserGen.DetUtils

import Daedalus.ParserGen.ClassInterval (IntervalEndpoint(..), ClassInterval(..))


data Result a =
    AbortNotStatic
  | AbortAcceptingPath
  | AbortNonClassInputAction Action
  | AbortOverflowMaxDepth
  | AbortLoopWithNonClass
  | AbortNonEmptyIntersection
  | AbortClassIsDynamic
  | AbortClassNotHandledYet String
  | AbortAmbiguous
  | AbortTodo
  | Result a
  deriving(Show)


maxDepthRec :: Int
maxDepthRec = 200



closureLLOne :: Aut a => a -> ClosurePath -> Result ClosureMoveSet
closureLLOne aut da =
  let
    q = getLastState da
    ch = nextTransition aut q
  in
  case ch of
    Nothing ->
      if isAcceptingState aut q
      then AbortAcceptingPath
      else error "should not happen"
    Just ch1 ->
      let (tag, lst) = case ch1 of
                            UniChoice (act, q1) -> (CUni, [(act,q1)])
                            SeqChoice l _       -> (CSeq, l)
                            ParChoice l         -> (CPar, l)
          resIterate = iterateThrough (initChoicePos tag) lst
      in resIterate

  where
    closureStep :: ChoicePos -> (Action,State) -> Result ClosureMoveSet
    closureStep pos (act, q1) =
      if isClassActOrEnd act
      then Result $ [Move (da, (act, q1))]
      else
        if isNonClassInputAct act
        then trace ("NonClassInput ACT: " ++ show act) $
             -- trace (maybe "" (\ e -> if isSimpleVExpr e then "Static" else show e) (getMatchBytes act)) $
             AbortNonClassInputAction act
        else
          if lengthClosurePath da > maxDepthRec
          then AbortOverflowMaxDepth
          else
            if stateInClosurePath q1 da
            then trace ("LoopWithNonClass ACT: " ++ show da) $
                 AbortLoopWithNonClass
            else
              case addClosurePath pos act q1 da of
                Nothing -> Result [NoMove]
                Just p -> closureLLOne aut p

    iterateThrough :: ChoicePos -> [(Action,State)] -> Result ClosureMoveSet
    iterateThrough pos ch =
      case ch of
        [] -> Result $ []
        (act, q1) : rest ->
          let cs = closureStep pos (act, q1) in
          case cs of
            AbortOverflowMaxDepth -> cs
            AbortLoopWithNonClass -> cs
            AbortNonClassInputAction _ -> cs
            AbortAcceptingPath -> cs
            Result res1 ->
              let ri = iterateThrough (nextChoicePos pos) rest in
              case ri of
                AbortOverflowMaxDepth -> ri
                AbortLoopWithNonClass -> ri
                AbortNonClassInputAction _ -> ri
                AbortAcceptingPath -> ri
                Result resForRest -> Result (res1 ++ resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"



classToInterval :: PAST.NCExpr -> Result ClassInterval
classToInterval e =
  case e of
    PAST.NSetAny -> Result $ ClassBtw MinusInfinity PlusInfinity
    PAST.NSetSingle e1 ->
      if (not $ isSimpleVExpr e1)
      then AbortClassIsDynamic
      else
        let v = evalNoFunCall e1 [] [] in
        case v of
          Interp.VUInt 8 x -> Result $ ClassBtw (CValue x) (CValue x)
          _                -> AbortClassNotHandledYet "SetSingle"
    PAST.NSetRange e1 e2 ->
      if isSimpleVExpr e1 && isSimpleVExpr e2
      then
        let v1 = evalNoFunCall e1 [] []
            v2 = evalNoFunCall e2 [] []
        in case (v1, v2) of
             (Interp.VUInt 8 x, Interp.VUInt 8 y) ->
               Result $ ClassBtw (CValue x) (CValue y)
             _ -> AbortClassNotHandledYet "SetRange"
      else AbortClassIsDynamic
    _ -> AbortClassNotHandledYet "other class case"



-- this function takes a tree representing a set of choices and
-- convert it to a Input factored deterministic transition.
determinizeClosureMoveSet :: ClosureMoveSet -> Result DetChoice
determinizeClosureMoveSet tc =
  let tc1 = filterNoMove tc in
  determinizeTree tc1

  where
    convertToInputHeadCondition ::
      ClosureMoveSetPoly (Either PAST.NCExpr InputAction) ->
      ClosureMoveSetPoly InputHeadCondition ->
      Result (ClosureMoveSetPoly InputHeadCondition)
    convertToInputHeadCondition lstAct acc =
      case lstAct of
        [] -> Result $ reverse acc
        (da, (e,q)) : es ->
          case e of
            Left c ->
              case classToInterval c of
                AbortClassIsDynamic -> AbortClassIsDynamic
                AbortClassNotHandledYet msg -> AbortClassNotHandledYet msg
                Result r -> convertToInputHeadCondition es ((da, ((HeadInput r),q)) : acc)
                _ -> error "Impossible abort"
            Right IEnd -> convertToInputHeadCondition es ((da, (EndInput,q)) : acc)
            _ -> error "Impossible abort"

    determinizeTree :: ClosureMoveSetPoly Action -> Result (DetChoice)
    determinizeTree lst =
      let lstAct = map (\ (da, (act,q)) -> (da, (getClassActOrEnd act,q))) lst
          maybeLstItv = convertToInputHeadCondition lstAct []
      in
        case maybeLstItv of
          AbortClassIsDynamic -> AbortClassIsDynamic
          AbortClassNotHandledYet a -> AbortClassNotHandledYet a
          Result lstItv ->
            Result (determinizeHelper lstItv emptyDetChoice)
          _ -> error "impossible abort"

    determinizeHelper ::
      ClosureMoveSetPoly InputHeadCondition ->
      DetChoice ->
      DetChoice
    determinizeHelper lstItv res = -- undefined
      case lstItv of
        [] -> res
        (da, (x,q)) : xs ->
          let newRes = insertDetChoice (da, (x,q)) res
          in determinizeHelper xs newRes


deterministicStateTrans :: Aut a => a -> State -> Result (DetChoice)
deterministicStateTrans aut q =
  case closureLLOne aut (initClosurePath q) of
    AbortOverflowMaxDepth -> AbortOverflowMaxDepth
    AbortLoopWithNonClass -> AbortLoopWithNonClass
    AbortAcceptingPath -> AbortAcceptingPath
    AbortNonClassInputAction x -> AbortNonClassInputAction x
    Result r -> determinizeClosureMoveSet r
    _ -> error "impossible"

createDFA :: Aut a => a -> Map.Map State (Result DetChoice)
createDFA aut =
  let transitions = allTransitions aut
      collectedStates = collectStatesArrivedByMove transitions
      statesDet = map (\ q -> (q, deterministicStateTrans aut q)) (Set.toList collectedStates)
  in
    Map.fromAscList statesDet
  where
    collectStatesArrivedByMove t =
      foldr (\ (_,ch) b -> Set.union b (choiceToArrivedByMove ch)) (Set.singleton (initialState aut)) t

    choiceToArrivedByMove ch =
      case ch of
        UniChoice (act, q) -> collectMove (act, q)
        ParChoice lst -> foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst
        SeqChoice lst _ -> foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst

    collectMove (act, q) =
      if isInputAction act then Set.singleton q else Set.empty

statsDFA :: Map.Map State (Result DetChoice) -> String
statsDFA dfa =
  let t = Map.toList dfa
  in do getReport t initReport (0 :: Int)
  where
    getReport lst report total =
      case lst of
        [] -> (foldr (\ a b -> show a ++ "\n" ++ b) "" (Map.assocs report)) ++ "\nTotal: " ++ show total
        (_, x) : xs ->
          getReport xs (incrReport report x) (total+1)

    abortNotStatic = "AbortNotStatic"
    abortAcceptingPath = "AbortAcceptingPath"
    abortNonClassInputAction = "AbortNonClassInputAction"
    abortOverflowMaxDepth = "AbortOverflowMaxDepth"
    abortLoopWithNonClass = "AbortLoopWithNonClass"
    abortNonEmptyIntersection = "AbortNonEmptyIntersection"
    abortClassIsDynamic = "AbortClassIsDynamic"
    abortClassNotHandledYet = "AbortClassNotHandledYet"
    abortAmbiguous = "AbortAmbiguous"
    abortTodo = "AbortTodo"
    detResult str = "Result" ++ str

    initReport :: Map.Map String Int
    initReport = Map.fromAscList []

    mapResultToKey :: Result DetChoice -> String
    mapResultToKey r =
      case r of
        AbortNotStatic -> abortNotStatic
        AbortAcceptingPath -> abortAcceptingPath
        AbortNonClassInputAction _ -> abortNonClassInputAction
        AbortOverflowMaxDepth -> abortOverflowMaxDepth
        AbortLoopWithNonClass -> abortLoopWithNonClass
        AbortNonEmptyIntersection -> abortNonEmptyIntersection
        AbortClassIsDynamic -> abortClassIsDynamic
        AbortClassNotHandledYet _msg -> abortClassNotHandledYet
        AbortAmbiguous -> abortAmbiguous
        AbortTodo -> abortTodo
        Result (clssLstTr, endTr) ->
          if ((maybe False (\ tr -> length tr > 1) endTr) ||
              foldr (\ (_,tr) b -> if length tr > 1 then True else b) False clssLstTr)
          then
            trace (show ((map (\ (clssAct, ft) -> (clssAct, map (\ (da,_) -> lengthClosurePath da) ft)) clssLstTr), maybe 0 (\ x -> length x) endTr)) $
            detResult "-Ambiguous"
          else
            if foldr (\ (_,tr) b ->  if (foldr (\ (da,_) b1 -> if hasBranchAction da then True else b1) False tr) then True else b) False clssLstTr
            then
              trace (show ((map (\ (clssAct, ft) -> (clssAct, map (\ (da,_) -> lengthClosurePath da) ft)) clssLstTr), maybe 0 (\ x -> length x) endTr)) $
              detResult "-Branch"
            else detResult ""

    incrReport :: Map.Map String Int -> Result (DetChoice) -> Map.Map String Int
    incrReport report r =
      let key = mapResultToKey r
      in
        Map.insertWith (\ a b -> a+b) key 1 report

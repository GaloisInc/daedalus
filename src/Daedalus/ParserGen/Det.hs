module Daedalus.ParserGen.Det where

import Debug.Trace

import qualified Data.Set as Set
-- import Data.Maybe (isNothing, fromJust)
import qualified Data.Map.Strict as Map

import qualified Daedalus.Interp as Interp

import Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), InputAction(..), isClassActOrEnd, isInputAction, isNonClassInputAct, getClassActOrEnd, evalNoFunCall, isSimpleVExpr)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..))

import Daedalus.ParserGen.DetUtils (TreeChoice(..), SymbolicStack(..), DelayedAction, addDelayedAction, stateInDelayedAction, lengthDelayedAction, execDelayedActionOnStack, FlattenTreeChoice, flattenTreeChoice)

import Daedalus.ParserGen.ClassInterval (IntervalEndpoint(..), ClassInterval(..), combineInterval)


data DetResult a =
    AbortNotStatic
  | AbortAcceptingPath
  | AbortNonClassInputAction Action
  | AbortOverflowMaxDepth
  | AbortLoopWithNonClass
  | AbortNonEmptyIntersection
  | AbortClassIsDynamic
  | AbortClassNotHandled String
  | AbortAmbiguous
  | AbortTodo
  | DetResult a
  deriving(Show)


maxDepthRec :: Int
maxDepthRec = 200

closureLLOne :: Aut a => a -> State -> DelayedAction -> DetResult TreeChoice
closureLLOne aut q da =
  let ch = nextTransition aut q
  in
  case execDelayedActionOnStack SWildcard da of
    Nothing -> DetResult NoMove
    Just _  ->
      case ch of
       Nothing ->
         if isAcceptingState aut q
         then AbortAcceptingPath
         else error "should not happen"
       Just ch1 ->
         let (chType, lst) =
               case ch1 of
                 UniChoice (act, q1) -> ("Uni", [(act,q1)])
                 SeqChoice l _       -> ("Seq", l)
                 ParChoice l         -> ("Par", l)
             resIterate = iterateThrough lst []
         in
           case resIterate of
             AbortOverflowMaxDepth -> AbortOverflowMaxDepth
             AbortLoopWithNonClass -> AbortLoopWithNonClass
             AbortNonClassInputAction x -> AbortNonClassInputAction x
             AbortAcceptingPath -> AbortAcceptingPath
             DetResult res ->
               case chType of
                 "Uni" -> DetResult (head res)
                 "Seq" -> DetResult (Seq res)
                 "Par" -> DetResult (Par res)
                 _     -> error "impossible pattern"
             _ -> error "Unexpected exception"
  where
    closureStep :: (Action,State) -> DetResult TreeChoice
    closureStep (act, q1) =
      if isClassActOrEnd act
      then DetResult $ Move (da, act, q1)
      else
        if isNonClassInputAct act
        then -- trace ("ACTION is: " ++ show act) $
             -- trace (maybe "" (\ e -> if isSimpleVExpr e then "Static" else show e) (getMatchBytes act)) $
             AbortNonClassInputAction act
        else
          if lengthDelayedAction da > maxDepthRec
          then AbortOverflowMaxDepth
          else
            if stateInDelayedAction q1 da
            then -- trace (show $ lengthDelayedAction da) $
                 AbortLoopWithNonClass
            else
              let bClos = closureLLOne aut q1 (addDelayedAction act q1 da) in
              bClos


    iterateThrough :: [(Action,State)] -> [TreeChoice] -> DetResult [TreeChoice]
    iterateThrough ch res =
      case ch of
        [] -> DetResult $ reverse res
        (act, q1) : rest ->
          let cs = closureStep (act, q1) in
          case cs of
            AbortOverflowMaxDepth -> AbortOverflowMaxDepth
            AbortLoopWithNonClass -> AbortLoopWithNonClass
            AbortNonClassInputAction x -> AbortNonClassInputAction x
            AbortAcceptingPath -> AbortAcceptingPath
            DetResult elm -> iterateThrough rest (elm:res)
            _ -> error "abort not handled here"



classToInterval :: PAST.NCExpr -> DetResult ClassInterval
classToInterval e =
  case e of
    PAST.NSetAny -> DetResult $ ClassBtw MinusInfinity PlusInfinity
    PAST.NSetSingle e1 ->
      if (not $ isSimpleVExpr e1)
      then AbortClassIsDynamic
      else
        let v = evalNoFunCall e1 [] [] in
        case v of
          Interp.VUInt 8 x -> DetResult $ ClassBtw (CValue x) (CValue x)
          _                -> AbortClassNotHandled "SetSingle"
    PAST.NSetRange e1 e2 ->
      if isSimpleVExpr e1 && isSimpleVExpr e2
      then
        let v1 = evalNoFunCall e1 [] []
            v2 = evalNoFunCall e2 [] []
        in case (v1, v2) of
             (Interp.VUInt 8 x, Interp.VUInt 8 y) ->
               DetResult $ ClassBtw (CValue x) (CValue y)
             _ -> AbortClassNotHandled "SetRange"
      else AbortClassIsDynamic
    _ -> AbortClassNotHandled "other class case"



data InputHeadCondition =
    HeadInput ClassInterval
  | EndInput


analyzeTreeChoice :: TreeChoice -> DetResult FlattenTreeChoice
analyzeTreeChoice tc =
  let tc1 = flattenTreeChoice tc in
  case isDeterministic tc1 of
    AbortClassIsDynamic -> AbortClassIsDynamic
    AbortClassNotHandled msg -> AbortClassNotHandled msg
    AbortNonEmptyIntersection -> AbortNonEmptyIntersection
    DetResult () -> DetResult tc1
    _ -> error "impossible abort"

  where
    convertToInputHeadCondition :: [Either PAST.NCExpr InputAction] -> [InputHeadCondition] -> DetResult [InputHeadCondition]
    convertToInputHeadCondition lstAct acc =
      case lstAct of
        [] -> DetResult $ reverse acc
        e : es ->
          case e of
            Left c ->
              case classToInterval c of
                AbortClassIsDynamic -> AbortClassIsDynamic
                AbortClassNotHandled msg -> AbortClassNotHandled msg
                DetResult r -> convertToInputHeadCondition es (HeadInput r : acc)
                _ -> error "Impossible abort"
            Right IEnd -> convertToInputHeadCondition es (EndInput : acc)
            _ -> error "Impossible abort"

    isDeterministic :: FlattenTreeChoice -> DetResult ()
    isDeterministic lst =
      let lstAct = map (\ (_,_,act) -> getClassActOrEnd act) lst
          maybeLstItv = convertToInputHeadCondition lstAct []
      in
        case maybeLstItv of
          AbortClassIsDynamic -> AbortClassIsDynamic
          AbortClassNotHandled a -> AbortClassNotHandled a
          DetResult lstItv ->
            if allIntersectionEmpty lstItv
            then DetResult ()
            else AbortNonEmptyIntersection
          _ -> error "impossible abort"

      where
        allIntersectionEmpty :: [InputHeadCondition] -> Bool
        allIntersectionEmpty lstItv =
          case lstItv of
            [] -> True
            x : xs ->
              if forallTest xs
              then allIntersectionEmpty xs
              else False
              where
                forallTest :: [InputHeadCondition] -> Bool
                forallTest rest =
                  case rest of
                     [] -> True
                     y : ys ->
                       case (x,y) of
                         (EndInput, EndInput) -> False
                         (EndInput, _) -> True
                         (_, EndInput) -> True
                         (HeadInput xc, HeadInput yc) ->
                           -- case isNonEmptyClassIntervalIntersection xc yc of
                           --   Nothing -> False
                           --   Just True -> False
                           --   Just False -> forallTest ys
                           let comb =  combineInterval (xc,(1 :: Int)) (yc,1) (+)
                               check = foldr (\ (_,n) b -> if n == 1 then b else False) True comb
                           in case check of
                             True -> forallTest ys
                             False -> False

deterministicStateTrans :: Aut a => a -> State -> DetResult FlattenTreeChoice
deterministicStateTrans aut q =
  case closureLLOne aut q [] of
    AbortOverflowMaxDepth -> AbortOverflowMaxDepth
    AbortLoopWithNonClass -> AbortLoopWithNonClass
    AbortAcceptingPath -> AbortAcceptingPath
    AbortNonClassInputAction x -> AbortNonClassInputAction x
    DetResult r -> analyzeTreeChoice r
    _ -> error "impossible"

createDFA :: Aut a => a -> Map.Map State (DetResult FlattenTreeChoice)
createDFA aut =
  let transitions = allTransitions aut
      collectedStates = collectStatesArrivedByMove transitions
      statesDet = map (\ q -> (q, deterministicStateTrans aut q)) (Set.toList collectedStates)
  in
    Map.fromAscList statesDet
  where
    collectStatesArrivedByMove t =
      foldr (\ (_,ch) b -> Set.union b (choiceToArrivedByMove ch)) (Set.singleton (initials aut)) t

    choiceToArrivedByMove ch =
      case ch of
        UniChoice (act, q) -> collectMove (act, q)
        ParChoice lst -> foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst
        SeqChoice lst _ -> foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst

    collectMove (act, q) =
      if isInputAction act then Set.singleton q else Set.empty

statsDFA :: Map.Map State (DetResult FlattenTreeChoice) -> String
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
    abortClassNotHandled = "AbortClassNotHandled"
    abortAmbiguous = "AbortAmbiguous"
    abortTodo = "AbortTodo"
    detResult = "DetResult"
    detResultMultiple = "DetResultMultiple"


    initReport :: Map.Map String Int
    initReport = Map.fromAscList []
      -- Map.fromAscList
      -- [ (abortNotStatic, 0)
      -- , (abortAcceptingPath, 0)
      -- , (abortNonClassInputAction, 0)
      -- , (abortOverflowMaxDepth, 0)
      -- , (abortNonEmptyIntersection, 0)
      -- , (abortClassIsDynamic, 0)
      -- , (abortClassNotHandled, 0)
      -- , (abortAmbiguous, 0)
      -- , (abortTodo, 0)
      -- , (detResult, 0)
      -- , (detResultMultiple, 0)
      -- ]

    mapResultToKey r =
      case r of
        AbortNotStatic -> abortNotStatic
        AbortAcceptingPath -> abortAcceptingPath
        AbortNonClassInputAction _ -> abortNonClassInputAction
        AbortOverflowMaxDepth -> abortOverflowMaxDepth
        AbortLoopWithNonClass -> abortLoopWithNonClass
        AbortNonEmptyIntersection -> abortNonEmptyIntersection
        AbortClassIsDynamic -> abortClassIsDynamic
        AbortClassNotHandled _msg -> abortClassNotHandled
        AbortAmbiguous -> abortAmbiguous
        AbortTodo -> abortTodo
        DetResult lst ->
          if length lst > 1
          then -- trace (show (map (\ (_,_,x) -> x) lst)) $
               detResultMultiple
          else detResult

    incrReport :: Map.Map String Int -> DetResult FlattenTreeChoice -> Map.Map String Int
    incrReport report r =
      let key = mapResultToKey r
      in
        Map.insertWith (\ a b -> a+b) key 1 report

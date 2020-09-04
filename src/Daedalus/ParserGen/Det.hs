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

  | AbortOverflowK

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
                Result r -> convertToInputHeadCondition es ((da, (HeadInput r, q)) : acc)
                _ -> error "Impossible abort"
            Right IEnd -> convertToInputHeadCondition es ((da, (EndInput, q)) : acc)
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
            Result (determinizeTransList lstItv)
          _ -> error "impossible abort"

    determinizeTransList :: ClosureMoveSetPoly InputHeadCondition -> DetChoice
    determinizeTransList lstItv =
      foldl (\ a b -> insertDetChoice b a) emptyDetChoice lstItv


deterministicStep :: Aut a => a -> CfgDet -> Result (DetChoice)
deterministicStep aut cfg =
  case closureLLOne aut (initClosurePath cfg) of
    AbortOverflowMaxDepth -> AbortOverflowMaxDepth
    AbortLoopWithNonClass -> AbortLoopWithNonClass
    AbortAcceptingPath -> AbortAcceptingPath
    AbortNonClassInputAction x -> AbortNonClassInputAction x
    Result r -> determinizeClosureMoveSet r
    _ -> error "impossible"




data DFATransition =
    LResolve (Result ())
  | LChoice [ (InputHeadCondition, TraceSet, Result DFATransition) ]

instance Show(DFATransition) where
 show t =
   showD (0::Int) t
   where
     showD d (LResolve r) = space d 0 ++ "Resolve (" ++ show r ++ ")\n"
     showD d (LChoice lst) = space d 0 ++ "LChoice\n" ++ (concat (map (showT (d+2)) lst))

     showT d (i, tr, r) = space d 0 ++ show i ++ "\n" ++ space d 0 ++ show (length tr) ++ "\n" ++ under
       where
         under =
           case r of
             Result a -> showD d a
             _ -> space d 0 ++ "Abort\n"


     space d cnt = if cnt < d then " " ++ space d (cnt+1) else ""

depthDFATransition :: DFATransition -> Int
depthDFATransition t =
  case t of
    LResolve _ -> 1
    LChoice lst -> foldr (\ (_,_,r) b -> case r of
                                       Result r1 -> 1 + max (depthDFATransition r1) b
                                       _ -> b) 0 lst



maxDepthDet :: Int
maxDepthDet = 4

detChoiceToList :: DetChoice -> [(InputHeadCondition, TraceSet)]
detChoiceToList (c,e) =
  let tr = map (\ (i,t) -> (HeadInput i,t)) c in
  case e of
    Nothing -> tr
    Just t -> tr ++ [(EndInput, t)]

deterministicK :: Aut a => a -> Int -> CfgDet -> Result DFATransition
deterministicK aut depth cfg =
  let det1 = deterministicStep aut cfg
  in case det1 of
       AbortOverflowMaxDepth -> AbortOverflowMaxDepth
       AbortLoopWithNonClass -> AbortLoopWithNonClass
       AbortAcceptingPath -> AbortAcceptingPath
       AbortNonClassInputAction x -> AbortNonClassInputAction x
       AbortClassIsDynamic -> AbortClassIsDynamic
       AbortClassNotHandledYet a -> AbortClassNotHandledYet a
       Result r ->
         let tr = detChoiceToList r
         in Result $ LChoice (iterateDeterminize depth tr)
       _ -> error "cannot be this Abort"

  where
    iterateDeterminize :: Int -> [(InputHeadCondition, TraceSet)] -> [(InputHeadCondition, TraceSet, Result DFATransition)]
    iterateDeterminize d lst =
      case lst of
        [] -> []
        (i, s) : rest ->
          case i of
            EndInput ->
              if length s > 1
              then (i, s, Result $ LResolve AbortAmbiguous) : iterateDeterminize d rest
              else (i, s, Result $ LResolve (Result())) : iterateDeterminize d rest
            HeadInput _itv ->
              let t = detSubset d s
              in (i, s, t) : iterateDeterminize d rest

    detSubset :: Int -> TraceSet -> Result DFATransition
    detSubset d s =
      case length s of
        0 -> error "empty set"
        1 -> Result (LResolve (Result ()))
        _ -> if d > maxDepthDet
             then AbortOverflowK
             else detSubsetHelper d s emptyDetChoice

    detSubsetHelper :: Int -> TraceSet -> DetChoice -> Result DFATransition
    detSubsetHelper d s acc =
      case s of
        [] -> Result (LChoice (iterateDeterminize (d+1) (detChoiceToList acc)))
        (p, _s) : rest ->
          let p_cfg = getLastCfgDet p in
          let r = deterministicStep aut p_cfg in
          case r of
            AbortOverflowMaxDepth -> AbortOverflowMaxDepth
            AbortLoopWithNonClass -> AbortLoopWithNonClass
            AbortAcceptingPath -> AbortAcceptingPath
            AbortNonClassInputAction x -> AbortNonClassInputAction x
            Result r1 ->
              let newAcc = unionDetChoice r1 acc
              in detSubsetHelper d rest newAcc
            _ -> error "cannot be this abort"


createDFA :: Aut a => a -> Map.Map State (Result DFATransition)
createDFA aut =
  let transitions = allTransitions aut
      collectedStates = collectStatesArrivedByMove transitions
      -- statesDet = map (\ q -> (q, deterministicStep aut (initCfgDet q))) (Set.toList collectedStates)
      statesDet = map (\ q -> (q, deterministicK aut 0 (initCfgDet q))) (Set.toList collectedStates)
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


statsDFA :: Map.Map State (Result DFATransition) -> String
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
    abortOverflowK = "AbortOverflowK"
    detResult str = "Result" ++ str

    initReport :: Map.Map String Int
    initReport = Map.fromAscList []

    mapResultToKey :: Result DFATransition -> String
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

        AbortOverflowK -> abortOverflowK

        -- Result (clssLstTr, endTr) ->
        --   if ((maybe False (\ tr -> length tr > 1) endTr) ||
        --       foldr (\ (_,tr) b -> if length tr > 1 then True else b) False clssLstTr)
        --   then
        --     trace (show ((map (\ (clssAct, ft) -> (clssAct, map (\ (da,_) -> lengthClosurePath da) ft)) clssLstTr), maybe 0 (\ x -> length x) endTr)) $
        --     detResult "-Ambiguous"
        --   else
        --     if foldr (\ (_,tr) b ->  if (foldr (\ (da,_) b1 -> if hasBranchAction da then True else b1) False tr) then True else b) False clssLstTr
        --     then
        --       trace (show ((map (\ (clssAct, ft) -> (clssAct, map (\ (da,_) -> lengthClosurePath da) ft)) clssLstTr), maybe 0 (\ x -> length x) endTr)) $
        --       detResult "-Branch"
        --     else detResult ""
        Result t ->
          if depthDFATransition t > 4
          then trace (show t) $
               detResult ""
          else detResult ""

    incrReport :: Map.Map String Int -> Result (DFATransition) -> Map.Map String Int
    incrReport report r =
      let key = mapResultToKey r
      in
        Map.insertWith (\ a b -> a+b) key 1 report

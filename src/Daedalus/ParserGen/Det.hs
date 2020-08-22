module Daedalus.ParserGen.Det where

import Debug.Trace

import qualified Data.Set as Set
-- import Data.Maybe (isNothing, fromJust)
import qualified Data.Map.Strict as Map

import Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), InputAction(..), ControlAction(..), isClassActOrEnd, isInputAction, isNonClassInputAct, getClassActOrEnd, evalNoFunCall, isSimpleVExpr)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..))

import qualified Daedalus.Interp as Interp

data SymbolicStack =
    SWildcard
  | SEmpty
  | SCons State SymbolicStack
  deriving (Eq, Ord)

-- data CfgDet = CfgDet
--   { state  :: State
--   , ruleNb :: Int
--   , stack  :: StackDet
--   }
--   deriving (Eq, Ord)

-- closure :: Aut -> DFAState -> CfgDet -> DFAState
-- closure _aut busy cfg =
--   if Set.member cfg busy
--   then Set.empty
--   else
--     let _newBusy = Set.insert cfg busy in
--     let _ret = Set.singleton cfg in
--     case stack cfg of
--       _ -> error "WIP"

-- type DFAState = Set.Set CfgDet

data TreeChoice =
    Move (DelayedAction, Action, State)
  | NoMove
  | Seq [ TreeChoice ]
  | Par [ TreeChoice ]


type DelayedAction = [(Action, State)]

addDelayedAction :: Action -> State -> DelayedAction -> DelayedAction
addDelayedAction a q da = (a,q) : da

stateInDelayedAction :: State -> DelayedAction -> Bool
stateInDelayedAction q da =
  case da of
    [] -> False
    (_, q1) : das -> if q == q1 then True else stateInDelayedAction q das

lengthDelayedAction :: DelayedAction -> Int
lengthDelayedAction da = length da

destrDelayedAction :: DelayedAction -> Maybe ((Action, State), DelayedAction)
destrDelayedAction da =
  let rda = reverse da in
    case rda of
      [] -> Nothing
      x : rest -> Just (x, rest)

execDelayedActionOnStack :: SymbolicStack -> DelayedAction -> Maybe SymbolicStack
execDelayedActionOnStack stk da =
  case destrDelayedAction da of
    Nothing -> Just stk
    Just ((act, _q), rest) ->
      case execAction stk act of
        Nothing -> Nothing
        Just stk1 -> execDelayedActionOnStack stk1 rest
  where
    execAction :: SymbolicStack -> Action -> Maybe SymbolicStack
    execAction st act =
      case act of
        CAct c ->
          case c of
            Push _ _ q -> Just $ SCons q stk
            Pop q ->
              case st of
                SWildcard -> Just SWildcard
                SEmpty -> Nothing
                SCons q1 rest -> if q == q1 then Just rest else Nothing
            _ -> Just stk
        _ -> Just stk

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


type ChoicePath = [Int]

type FlattenTreeChoice = [ (ChoicePath, DelayedAction, Action) ]

flattenTreeChoice :: TreeChoice -> FlattenTreeChoice
flattenTreeChoice tc =
  case tc of
    Move (da,act,_) -> [([],da,act)]
    NoMove  -> []
    Par lst -> flattenListTreeChoice lst
    Seq lst -> flattenListTreeChoice lst

  where
    flattenListTreeChoice :: [ TreeChoice ] -> FlattenTreeChoice
    flattenListTreeChoice lst =
      let lst1 = map flattenTreeChoice lst
          lst2 = zipWithInt lst1 0
          lst3 = map (\ (n, backlist) -> map (\ (lsti, da, a) -> (n:lsti, da, a)) backlist) lst2
      in foldr (\ a b -> a ++ b) [] lst3

    zipWithInt :: [a] -> Int -> [(Int,a)]
    zipWithInt l i =
      case l of
        [] -> []
        x : xs -> (i, x) : zipWithInt xs (i + 1)


data IntervalEndpoint =
    PlusInfinity
  | MinusInfinity
  | CValue Integer
  deriving(Eq)

instance Ord(IntervalEndpoint) where
  (<=) MinusInfinity _ = True
  (<=) (CValue _) MinusInfinity = False
  (<=) (CValue x) (CValue y) = x <= y
  (<=) (CValue _) PlusInfinity = True
  (<=) PlusInfinity PlusInfinity = True
  (<=) PlusInfinity _ = False

incrItv :: IntervalEndpoint -> IntervalEndpoint
incrItv i =
  case i of
    PlusInfinity -> error "cannot increment plus infinity"
    MinusInfinity -> error "cannot increment minis infinity"
    CValue n -> CValue (n+1)

decrItv :: IntervalEndpoint -> IntervalEndpoint
decrItv i =
  case i of
    PlusInfinity -> error "cannot decrement plus infinity"
    MinusInfinity -> error "cannot decrement minis infinity"
    CValue n -> CValue (n-1)


data ClassInterval =
    ClassBtw IntervalEndpoint IntervalEndpoint


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

-- isNonEmptyClassIntervalIntersection :: ClassInterval -> ClassInterval -> Maybe Bool
-- isNonEmptyClassIntervalIntersection ci1 ci2 =
--   case (ci1, ci2) of
--     (ClassBtw i1 j1, ClassBtw i2 j2) ->
--       if i1 <= i2 && i2 <= j1 || i1 <= j2 && j2 <= j1
--       then Just True
--       else if i2 <= i1 && i1 <= j2 || i2 <= j1 && j1 <= j2
--            then Just True
--            else Just False

combineInterval :: (ClassInterval, a) -> (ClassInterval, a) -> (a -> a -> a) -> [(ClassInterval, a)]
combineInterval (itv1, a1) (itv2, a2) add =
  case (itv1, itv2) of
    (ClassBtw i1 j1, ClassBtw i2 j2) ->
      case (compare i2 i1, compare i2 j1) of
        (LT, LT) -> -- i2 < i1 <= j1
          case (compare j2 i1, compare j2 j1) of
            (LT, LT) -> [(itv2, a2), (itv1, a1)]
            (LT, _ ) -> error "impossible"
            (EQ, LT) -> [ (ClassBtw i2 (decrItv j2), a2),(ClassBtw j2 j2, add a1 a2),(ClassBtw (incrItv j2) j1, a1) ]
            (EQ, EQ) -> [ (ClassBtw i2 (decrItv j2), a2),(ClassBtw j2 j2, add a1 a2)]
            (EQ, GT) -> error "impossible"
            (GT, LT) -> [ (ClassBtw i2 (decrItv i1), a2), (ClassBtw i1 j2, add a1 a2), (ClassBtw (incrItv j2) j1, a1) ]
            (GT, EQ) -> [ (ClassBtw i2 (decrItv i1), a2), (ClassBtw i1 j2, add a1 a2)]
            (GT, GT) -> [ (ClassBtw i2 (decrItv i1), a2), (ClassBtw i1 j1, add a1 a2), (ClassBtw (incrItv j1) j2, a2) ]
        (LT, EQ) -> error "impossible"
        (LT, GT) -> error "impossible"
        (EQ, EQ) -> -- i1 == j1 == i2
          case (compare j2 i1, compare j2 j1) of
            (LT, _) -> error "impossible"
            (EQ, EQ) -> -- i2 == j2 == *
              [(ClassBtw i1 i1, add a1 a2)]
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> error "impossible"
            (GT, GT) -> [(ClassBtw i1 i1, add a1 a2), (ClassBtw (incrItv i1) j2, a2)]
        (EQ, LT) -> -- i1 == i2 and i1 < j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _) -> error "impossible"
            (EQ, LT) -> [(ClassBtw i1 i1, add a1 a2), (ClassBtw (incrItv i1) j1, a1)]
            (EQ, _) -> error "impossible"
            (GT, LT) -> [(ClassBtw i1 j2, add a1 a2), (ClassBtw (incrItv j2) j1, a1)]
            (GT, EQ) -> [(ClassBtw i1 j1, add a1 a2)]
            (GT, GT) -> [(ClassBtw i1 j1, add a1 a2), (ClassBtw (incrItv j1) j2, a2)]
        (EQ, GT) -> error "impossible"
        (GT, LT) -> -- i1 < i2 < j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 j2, add a1 a2), (ClassBtw (incrItv j2) j1, a1)]
            (GT, EQ) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 j2, add a1 a2)]
            (GT, GT) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 j1, add a1 a2), (ClassBtw (incrItv j1) j2, a2)]
        (GT, EQ) -> -- i1 < i2 == j1
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 i2, add a1 a2)]
            (GT, GT) -> [(ClassBtw i1 (decrItv i2), a1), (ClassBtw i2 i2, add a1 a2), (ClassBtw (incrItv i2) j2, a2)]
        (GT, GT) -> -- i1 <= j1 < i2
          case (compare j2 i1, compare j2 j1) of
            (LT, _ ) -> error "impossible"
            (EQ, _ ) -> error "impossible"
            (GT, LT) -> error "impossible"
            (GT, EQ) -> error "impossible"
            (GT, GT) -> [(itv1, a1), (itv2, a2)]



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

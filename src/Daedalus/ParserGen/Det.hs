module Daedalus.ParserGen.Det where

import qualified Data.Set as Set
-- import Data.Maybe (isNothing, fromJust)
import qualified Data.Map.Strict as Map

import Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action, isClassAct, isNonClassInputAct, getClassAct, evalNoFunCall)
import Daedalus.ParserGen.Aut (Aut, lookupAut, Choice(..), toListTr, transition, acceptings)

import qualified Daedalus.Interp as Interp

-- data StackDet =
--     SLLWildcard
--   | SEmpty
--   | SCons State StackDet
--   deriving (Eq, Ord)

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
  | Seq [ TreeChoice ]
  | Par [ TreeChoice ]


type DelayedAction = [Action]

addDelayedAction :: Action -> DelayedAction -> DelayedAction
addDelayedAction a da = a : da

lengthDelayedAction :: DelayedAction -> Int
lengthDelayedAction da = length da


data DetResult a =
    AbortNotStatic
  | AbortAcceptingPath
  | AbortNonClassInputAction Action
  | AbortOverflowMaxDepth
  | AbortNonDeterministicChoice
  | AbortClassNotHandled String
  | AbortAmbiguousMatch
  | AbortTodo
  | DetResult a
  deriving(Show)


maxDepthRec :: Int
maxDepthRec = 100

closureLLOne :: Aut -> State -> DelayedAction -> DetResult TreeChoice
closureLLOne aut q da =
  let ch = lookupAut q aut
  in case ch of
       Nothing ->
         if q == acceptings aut
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
      if isClassAct act
      then DetResult $ Move (da, act, q1)
      else
        if isNonClassInputAct act
        then AbortNonClassInputAction act
        else
          if lengthDelayedAction da > maxDepthRec
          then AbortOverflowMaxDepth
          else
            let bClos = closureLLOne aut q1 (addDelayedAction act da) in
              bClos


    iterateThrough :: [(Action,State)] -> [TreeChoice] -> DetResult [TreeChoice]
    iterateThrough ch res =
      case ch of
        [] -> DetResult $ reverse res
        (act, q1) : rest ->
          let cs = closureStep (act, q1) in
          case cs of
            AbortOverflowMaxDepth -> AbortOverflowMaxDepth
            AbortNonClassInputAction x -> AbortNonClassInputAction x
            AbortAcceptingPath -> AbortAcceptingPath
            DetResult elm -> iterateThrough rest (elm:res)
            _ -> error "abort not handled here"

          -- if isClassAct act
          -- then iterateThrough rest (Move (da, act, q1) : res)
          -- else
          --   if lengthDelayedAction da > maxDepthRec
          --   then AbortOverflowMaxDepth
          --   else
          --     let bClos = closureLLOne aut q1 (addDelayedAction act da) in
          --     case bClos of
          --       AbortOverflowMaxDepth  -> AbortOverflowMaxDepth
          --       DetResult elm -> iterateThrough rest (elm:res)
          --       _ -> error "Unexpected exception"


backwardTreeChoice :: TreeChoice -> [ ([Int], DelayedAction, Action) ]
backwardTreeChoice tc =
  case tc of
    Move (da,act,_) -> [([],da,act)]
    Par lst -> backwardList lst
    Seq lst -> backwardList lst

  where
    backwardList :: [ TreeChoice ] -> [ ([Int], DelayedAction, Action) ]
    backwardList lst =
      let lst1 = map backwardTreeChoice lst
          lst2 = zipWithInt lst1 0
          lst3 = map (\ (n, backlist) -> map (\ (lsti, da, a) -> (n:lsti, da, a)) backlist) lst2
      in foldr (\ a b -> a ++ b) [] lst3

    zipWithInt :: [a] -> Int -> [(Int,a)]
    zipWithInt l i =
      case l of
        [] -> []
        x : xs -> (i, x) : zipWithInt xs (i + 1)

data ClassValue =
    PlusInfinity
  | MinusInfinity
  | CValue Integer
  deriving(Eq)

instance Ord(ClassValue) where
  (<=) MinusInfinity _ = True
  (<=) (CValue _) MinusInfinity = False
  (<=) (CValue x) (CValue y) = x <= y
  (<=) (CValue _) PlusInfinity = True
  (<=) PlusInfinity PlusInfinity = True
  (<=) PlusInfinity _ = False

data ClassInterval =
  ClassBtw ClassValue ClassValue

isNonEmptyClassIntervalIntersection :: ClassInterval -> ClassInterval -> Maybe Bool
isNonEmptyClassIntervalIntersection ci1 ci2 =
  case (ci1, ci2) of
    (ClassBtw i1 j1, ClassBtw i2 j2) ->
      if i1 <= i2 && i2 <= j1 || i1 <= j2 && j2 <= j1
      then Just True
      else if i2 <= i1 && i1 <= j2 || i2 <= j1 && j1 <= j2
           then Just True
           else Just False


analyzeTreeChoice :: TreeChoice -> DetResult [([Int], DelayedAction, Action)]
analyzeTreeChoice tc =
  let tc1 = backwardTreeChoice tc in
  case isDeterministic tc1 of
    AbortClassNotHandled msg -> AbortClassNotHandled msg
    AbortNonDeterministicChoice -> AbortNonDeterministicChoice
    DetResult () -> DetResult tc1
    _ -> error "impossible abort"

  where
    isDeterministic :: [([Int], DelayedAction, Action)] -> DetResult ()
    isDeterministic lst =
      let lstAct = map (\ (_,_,act) -> getClassAct act) lst
          maybeLstItv = classToIntervalOnList lstAct []
      in
        case maybeLstItv of
          AbortClassNotHandled a -> AbortClassNotHandled a
          DetResult lstItv ->
            if allIntersectionEmpty lstItv
            then DetResult ()
            else AbortNonDeterministicChoice
          _ -> error "impossible abort"

      where

        allIntersectionEmpty :: [ClassInterval] -> Bool
        allIntersectionEmpty lstItv =
          case lstItv of
            [] -> True
            x : xs ->
              if forallTest xs
              then allIntersectionEmpty xs
              else False
              where
                forallTest :: [ClassInterval] -> Bool
                forallTest rest =
                  case rest of
                     [] -> True
                     y : ys ->
                       case isNonEmptyClassIntervalIntersection x y of
                         Nothing -> False
                         Just True -> False
                         Just False -> forallTest ys

        classToIntervalOnList :: [PAST.NCExpr] -> [ClassInterval] -> DetResult [ClassInterval]
        classToIntervalOnList lstAct acc =
          case lstAct of
            [] -> DetResult $ reverse acc
            e : es ->
              case classToInterval e of
                AbortClassNotHandled msg -> AbortClassNotHandled msg
                DetResult r -> classToIntervalOnList es (r : acc)
                _ -> error "Impossible abort"

        classToInterval :: PAST.NCExpr -> DetResult ClassInterval
        classToInterval e =
          case e of
            PAST.NSetAny -> DetResult $ ClassBtw MinusInfinity PlusInfinity
            PAST.NSetSingle e1 ->
              let v = evalNoFunCall e1 [] [] in
              case v of
                Interp.VUInt 8 x -> DetResult $ ClassBtw (CValue x) (CValue x)
                _                -> AbortClassNotHandled "SetSingle"
            PAST.NSetRange e1 e2 ->
              let v1 = evalNoFunCall e1 [] []
                  v2 = evalNoFunCall e2 [] []
              in case (v1, v2) of
                   (Interp.VUInt 8 x, Interp.VUInt 8 y) ->
                     DetResult $ ClassBtw (CValue x) (CValue y)
                   _ -> AbortClassNotHandled "SetRange"
            _ -> AbortClassNotHandled "other class case"

deterministicStateTrans:: Aut -> State -> DetResult [([Int], DelayedAction, Action)]
deterministicStateTrans aut q =
  case closureLLOne aut q [] of
    AbortOverflowMaxDepth -> AbortOverflowMaxDepth
    AbortAcceptingPath -> AbortAcceptingPath
    AbortNonClassInputAction x -> AbortNonClassInputAction x
    DetResult r -> analyzeTreeChoice r
    _ -> error "impossible"

createDFA :: Aut -> Map.Map State (DetResult [([Int], DelayedAction, Action)])
createDFA aut =
  let transitions = toListTr (transition aut)
      collectedStates = collectStatesArrivedByMove transitions
      statesDet = map (\ q -> (q, deterministicStateTrans aut q)) (Set.toList collectedStates)
  in
    Map.fromAscList statesDet
  where
    collectStatesArrivedByMove t =
      foldr (\ (_,ch) b -> Set.union b (choiceToArrivedByMove ch)) Set.empty t

    choiceToArrivedByMove ch =
      case ch of
        UniChoice (act, q) -> collectMove (act, q)
        ParChoice lst -> foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst
        SeqChoice lst _ -> foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst

    collectMove (act, q) =
      if isClassAct act then Set.singleton q else Set.empty

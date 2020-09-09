module Daedalus.ParserGen.DetUtils
  ( ChoiceTag(..),
    ChoicePos,
    initChoicePos,
    nextChoicePos,
    CfgDet,
    initCfgDet,
    ClosurePath,
    initClosurePath,
    addClosurePath,
    addClassIntervalClosurePath,
    stateInClosurePath,
    lengthClosurePath,
    getLastState,
    getLastCfgDet,
    hasBranchAction,
    ClosureMove(..),
    ClosureMoveSet,
    ClosureMoveSetPoly,
    filterNoMove,
    InputHeadCondition(..),
    TraceSet,
    DetChoice,
    emptyDetChoice,
    insertDetChoice,
    unionDetChoice
  ) where


-- import Debug.Trace

--import qualified Data.Set as Set

import Daedalus.ParserGen.Action (State, Action(..), InputAction(..), ControlAction(..), isBranchAction)
-- import Daedalus.ParserGen.Aut (Aut, lookupAut, Choice(..), toListTr, transition, acceptings, initials)

import Daedalus.ParserGen.ClassInterval (ClassInterval, insertItvInOrderedList)

data SymbolicStack =
    SWildcard
  | SEmpty
  | SCons State SymbolicStack
  deriving (Eq, Ord)



data ChoiceTag = CUni | CPar | CSeq
  deriving(Eq, Show, Ord)

type ChoicePos = (ChoiceTag, Int)

initChoicePos :: ChoiceTag -> ChoicePos
initChoicePos tag = (tag, 0)

nextChoicePos :: ChoicePos -> ChoicePos
nextChoicePos pos = (fst pos, snd pos +1)


data CfgDet = CfgDet
  { cfgState  :: State
  , cfgRuleNb :: Maybe ChoicePos
  , cfgStack  :: SymbolicStack
  }
  -- deriving (Eq, Ord)

initCfgDet :: State -> CfgDet
initCfgDet q =
  CfgDet { cfgState = q, cfgRuleNb = Nothing, cfgStack = SWildcard }

type SymbolicData = SymbolicStack

data ClosurePath =
    CP_Empty CfgDet
  | CP_Cons ClosurePath Action CfgDet


instance Show(ClosurePath) where
  show p =
    show (collectActions p [])
    where
      collectActions pth acc =
        case pth of
          CP_Empty _ -> acc
          CP_Cons up act _ -> collectActions up (act:acc)

initClosurePath :: CfgDet -> ClosurePath
initClosurePath cfg =
  CP_Empty cfg


symbExecAction :: SymbolicStack -> Action -> Maybe SymbolicStack
symbExecAction stk act =
  case act of
    CAct c ->
      case c of
        Push _ _ q -> Just $ SCons q stk
        Pop q ->
          case stk of
            SWildcard -> Just SWildcard
            SEmpty -> Nothing
            SCons q1 rest -> if q == q1 then Just rest else Nothing
        _ -> Just stk
    _ -> Just stk


getLastCfgDet :: ClosurePath -> CfgDet
getLastCfgDet p =
  case p of
    CP_Empty x -> x
    CP_Cons _ _ x -> x

getLastSymbData :: ClosurePath -> SymbolicData
getLastSymbData p = cfgStack (getLastCfgDet p)

getLastState :: ClosurePath -> State
getLastState p = cfgState (getLastCfgDet p)


addClosurePath :: ChoicePos -> Action -> State -> ClosurePath -> Maybe ClosurePath
addClosurePath pos a q p =
  let symbData = getLastSymbData p in
  case symbExecAction symbData a of
    Nothing -> Nothing
    Just sd ->
      let cfgDet = CfgDet { cfgState = q, cfgRuleNb = Just pos, cfgStack = sd }
      in Just $ CP_Cons p a cfgDet

addClassIntervalClosurePath :: ClosurePath -> ClassInterval -> State -> ClosurePath
addClassIntervalClosurePath p itv q =
  let newStack = cfgStack (getLastCfgDet p) in -- TODO : there should be some symbolic execution here
  let cfg = CfgDet { cfgState = q, cfgRuleNb = Nothing, cfgStack = newStack } in
  CP_Cons p (IAct (ClssItv itv)) cfg


stateInClosurePath :: State -> ClosurePath -> Bool
stateInClosurePath q p =
  case p of
    CP_Empty cfg -> if q == cfgState cfg then True else False
    CP_Cons up _ cfg -> if q == cfgState cfg then True else stateInClosurePath q up

lengthClosurePath :: ClosurePath -> Int
lengthClosurePath p =
  helper p 0
  where
    helper pth acc =
      case pth of
        CP_Empty _ -> acc
        CP_Cons up _ _ -> helper up (acc+1)

hasBranchAction :: ClosurePath -> Bool
hasBranchAction p =
  case p of
    CP_Empty _ -> False
    CP_Cons up act _cfg ->
      if isBranchAction act
      then True
      else hasBranchAction up


-- The conjonction of a closure path and a move (pair action, destination state)

type ClosureMovePoly a = (ClosurePath, (a, State))

data ClosureMove a =
    Move (ClosureMovePoly a)
  | NoMove


type ClosureMoveSet = [ClosureMove Action]
type ClosureMoveSetPoly a = [ClosureMovePoly a]


filterNoMove :: ClosureMoveSet -> ClosureMoveSetPoly Action
filterNoMove tc =
  foldr (\ c r ->
         case c of
           Move (da,(act,q)) -> (da,(act,q)) : r
           NoMove  -> r
      ) [] tc


data InputHeadCondition =
    HeadInput ClassInterval
  | EndInput
  deriving (Show)

type TraceSet = [ (ClosurePath, State) ]

unionTraceSet :: TraceSet -> TraceSet -> TraceSet
unionTraceSet s1 s2 = s1 ++ s2

singletonTraceSet :: (ClosurePath, State) -> TraceSet
singletonTraceSet x = [x]


-- fst element is a list of class action transition, the snd possible element is for EndInput test
type DetChoice = ([ (ClassInterval, TraceSet) ], Maybe TraceSet)

emptyDetChoice :: DetChoice
emptyDetChoice = ([], Nothing)

insertDetChoice :: (ClosurePath, (InputHeadCondition, State)) -> DetChoice -> DetChoice
insertDetChoice (da, (ih,q)) d =
  let (classChoice, endChoice) = d
      tr = singletonTraceSet (da, q)
  in
  case ih of
    EndInput ->
      case endChoice of
        Nothing -> (classChoice, Just tr)
        Just tr1 -> (classChoice, Just (unionTraceSet tr tr1))
    HeadInput x ->
      (insertItvInOrderedList (x, tr) classChoice unionTraceSet, endChoice)




unionDetChoice :: DetChoice -> DetChoice -> DetChoice
unionDetChoice (cl1, e1) (cl2, e2) =
  let e3 =
        case (e1,e2) of
          (Nothing, Nothing) -> Nothing
          (Nothing, Just _tr2) -> e2
          (Just _tr1, Nothing) -> e1
          (Just tr1, Just tr2) -> Just (unionTraceSet tr1 tr2)
  in
  let cl3 =
        foldr ((\ (itv, s) acc -> insertItvInOrderedList (itv, s) acc unionTraceSet) ) cl2 cl1
  in (cl3, e3)

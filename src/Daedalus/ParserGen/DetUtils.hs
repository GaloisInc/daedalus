module Daedalus.ParserGen.DetUtils
  ( SymbolicStack,
    ChoiceTag(..),
    ChoicePos,
    extractChoicePos,
    initChoicePos,
    nextChoicePos,
    CfgDet,
    initCfgDet,
    ClosurePath,
    initClosurePath,
    addClosurePath,
    addInputHeadConditionClosurePath,
    stateInClosurePath,
    lengthClosurePath,
    getLastState,
    getLastCfgDet,
    ClosureMove,
    ClosureMoveSet,
    InputHeadCondition(..),
    matchInputHeadCondition,
    PathSet,
    DetChoice,
    emptyDetChoice,
    insertDetChoice,
    unionDetChoice
  ) where


-- import Debug.Trace
import Data.Sequence as Seq

--import qualified Data.Set as Set
import qualified RTS.Input as Input

import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..))
-- import Daedalus.ParserGen.Aut (Aut, lookupAut, Choice(..), toListTr, transition, acceptings, initials)

import Daedalus.ParserGen.ClassInterval (ClassInterval, insertItvInOrderedList, matchClassInterval)

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


type SymbolicData = SymbolicStack

data CfgDet = CfgDet
  { cfgState  :: State
  , cfgRuleNb :: Seq.Seq ChoicePos
  , cfgStack  :: SymbolicStack
  }
  deriving (Eq)

simplifyChoicePos :: Seq.Seq ChoicePos -> [ChoicePos]
simplifyChoicePos s =
  case s of
    Empty -> []
    x :<| xs -> case fst x of
                 CUni -> (CUni, snd x) : simplifyChoicePos xs
                 CPar -> (CPar, snd x) : simplifyChoicePos xs
                 CSeq -> (CSeq, snd x) : simplifyChoicePos xs

initCfgDet :: State -> CfgDet
initCfgDet q =
  CfgDet
  { cfgState = q
  , cfgRuleNb = Seq.empty
  , cfgStack = SWildcard
  }


data ClosurePath =
    CP_Empty CfgDet
  | CP_Cons ClosurePath Action CfgDet


instance Show ClosurePath where
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

getRuleNb :: ClosurePath -> Seq.Seq ChoicePos
getRuleNb p = cfgRuleNb (getLastCfgDet p)

extractChoicePos :: ClosurePath -> [ChoicePos]
extractChoicePos p =
  let e = getLastCfgDet p in
    simplifyChoicePos (cfgRuleNb e)

addClosurePath :: ChoicePos -> Action -> State -> ClosurePath -> Maybe ClosurePath
addClosurePath pos act q p =
  let symbData = getLastSymbData p in
  case symbExecAction symbData act of
    Nothing -> Nothing
    Just sd ->
      let cfgDet = CfgDet { cfgState = q, cfgRuleNb = getRuleNb p |> pos, cfgStack = sd }
      in Just $ CP_Cons p act cfgDet

addInputHeadConditionClosurePath :: InputHeadCondition -> ChoicePos -> Action -> State -> ClosurePath -> ClosurePath
addInputHeadConditionClosurePath _ih pos act q p =
  let newStack = cfgStack (getLastCfgDet p) in -- TODO : there should be some symbolic execution here
  let cfg = CfgDet { cfgState = q, cfgRuleNb = getRuleNb p |> pos, cfgStack = newStack } in
    CP_Cons p act cfg


stateInClosurePath :: State -> ClosurePath -> Bool
stateInClosurePath q p =
  case p of
    CP_Empty cfg -> q == cfgState cfg
    CP_Cons up _ cfg -> if q == cfgState cfg then True else stateInClosurePath q up

lengthClosurePath :: ClosurePath -> Int
lengthClosurePath p =
  helper p 0
  where
    helper pth acc =
      case pth of
        CP_Empty _ -> acc
        CP_Cons up _ _ -> helper up (acc+1)


-- The conjonction of a closure path and a move (pair action, destination state)

type ClosureMove = (ClosurePath, (ChoicePos, Action, State))

type ClosureMoveSet = [ClosureMove]


data InputHeadCondition =
    HeadInput ClassInterval
  | EndInput
  deriving (Show)

matchInputHeadCondition :: InputHeadCondition -> Input.Input -> Maybe Input.Input
matchInputHeadCondition c i =
  case c of
    HeadInput a ->
      case Input.inputByte i of
        Nothing -> Nothing
        Just (x, xs) -> if matchClassInterval a x then Just xs else Nothing
    EndInput ->
      if Input.inputEmpty i then Just i else Nothing


type PathK = (ClosurePath, (ChoicePos, Action, State))
type PathSet = [ PathK ]

unionPathSet :: PathSet -> PathSet -> PathSet
unionPathSet s1 s2 = s1 ++ s2

singletonPathSet :: PathK -> PathSet
singletonPathSet x = [x]


-- fst element is a list of class action transition, the snd possible element is for EndInput test
type DetChoice = ([ (ClassInterval, PathSet) ], Maybe PathSet)

emptyDetChoice :: DetChoice
emptyDetChoice = ([], Nothing)

insertDetChoice :: InputHeadCondition -> (ClosurePath, (ChoicePos, Action, State)) -> DetChoice -> DetChoice
insertDetChoice ih (da, (pos, act, q)) d =
  let (classChoice, endChoice) = d
      tr = singletonPathSet (da, (pos, act, q))
  in
  case ih of
    EndInput ->
      case endChoice of
        Nothing -> (classChoice, Just tr)
        Just tr1 -> (classChoice, Just (unionPathSet tr tr1))
    HeadInput x ->
      (insertItvInOrderedList (x, tr) classChoice unionPathSet, endChoice)


unionDetChoice :: DetChoice -> DetChoice -> DetChoice
unionDetChoice (cl1, e1) (cl2, e2) =
  let e3 =
        case (e1,e2) of
          (Nothing, Nothing) -> Nothing
          (Nothing, Just _tr2) -> e2
          (Just _tr1, Nothing) -> e1
          (Just tr1, Just tr2) -> Just (unionPathSet tr1 tr2)
  in
  let cl3 =
        foldr (\ (itv, s) acc -> insertItvInOrderedList (itv, s) acc unionPathSet) cl2 cl1
  in (cl3, e3)

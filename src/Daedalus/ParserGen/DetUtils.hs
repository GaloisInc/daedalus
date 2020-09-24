module Daedalus.ParserGen.DetUtils
  ( SymbolicStack,
    ChoiceTag(..),
    ChoicePos,
    initChoicePos,
    nextChoicePos,
    CfgDet(..),
    initCfgDet,
    simulateActionCfgDet,
    setupCfgDetFromPrev,
    ClosureMove,
    ClosureMoveSet,
    InputHeadCondition(..),
    matchInputHeadCondition,
    SourceCfg,
    PathK,
    DFAState,
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

import Daedalus.ParserGen.ClassInterval (ClassInterval, insertItvInOrderedList, matchClassInterval)

data SymbolicStack =
    SWildcard
  | SEmpty
  | SCons State SymbolicStack
  deriving (Eq, Ord, Show)



data ChoiceTag = CUni | CPar | CSeq
  deriving(Eq, Show, Ord)

type ChoicePos = (ChoiceTag, Int)

initChoicePos :: ChoiceTag -> ChoicePos
initChoicePos tag = (tag, 0)

nextChoicePos :: ChoicePos -> ChoicePos
nextChoicePos pos = (fst pos, snd pos +1)


data CfgDet = CfgDet
  { cfgState  :: State
  , cfgRuleNb :: Seq.Seq ChoicePos
  , cfgStack  :: SymbolicStack
  }
  deriving (Eq, Show)


initCfgDet :: State -> CfgDet
initCfgDet q =
  CfgDet
  { cfgState = q
  , cfgRuleNb = Seq.empty
  , cfgStack = SWildcard
  }


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


simulateActionCfgDet :: ChoicePos -> Action -> State -> CfgDet -> Maybe CfgDet
simulateActionCfgDet pos act q cfg =
  let st = cfgStack cfg in
  case symbExecAction st act of
    Nothing -> Nothing
    Just sd ->
      Just $
        CfgDet
          { cfgState = q
          , cfgRuleNb = cfgRuleNb cfg |> pos
          , cfgStack = sd
          }


setupCfgDetFromPrev :: State -> CfgDet -> CfgDet
setupCfgDetFromPrev q cfg =
  CfgDet
    { cfgState = q
    , cfgRuleNb = Empty
    , cfgStack = cfgStack cfg
    }



-- The conjonction of a closure path and a move (pair action, destination state)

type ClosureMove = (CfgDet, (ChoicePos, Action, State))

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


type SourceCfg = CfgDet

type PathK = (SourceCfg, CfgDet, (ChoicePos, Action, State))
type DFAState = [ PathK ]

unionDFAState :: DFAState -> DFAState -> DFAState
unionDFAState s1 s2 = s1 ++ s2

singletonDFAState :: PathK -> DFAState
singletonDFAState x = [x]


-- fst element is a list of class action transition, the snd possible element is for EndInput test
type DetChoice = ([ (ClassInterval, DFAState) ], Maybe DFAState)

emptyDetChoice :: DetChoice
emptyDetChoice = ([], Nothing)

insertDetChoice :: SourceCfg -> InputHeadCondition -> (CfgDet, (ChoicePos, Action, State)) -> DetChoice -> DetChoice
insertDetChoice src ih (cfg, (pos, act, q)) d =
  let (classChoice, endChoice) = d
      tr = singletonDFAState (src, cfg, (pos, act, q))
  in
  case ih of
    EndInput ->
      case endChoice of
        Nothing -> (classChoice, Just tr)
        Just tr1 -> (classChoice, Just (unionDFAState tr tr1))
    HeadInput x ->
      (insertItvInOrderedList (x, tr) classChoice unionDFAState, endChoice)


unionDetChoice :: DetChoice -> DetChoice -> DetChoice
unionDetChoice (cl1, e1) (cl2, e2) =
  let e3 =
        case (e1,e2) of
          (Nothing, Nothing) -> Nothing
          (Nothing, Just _tr2) -> e2
          (Just _tr1, Nothing) -> e1
          (Just tr1, Just tr2) -> Just (unionDFAState tr1 tr2)
  in
  let cl3 =
        foldr (\ (itv, s) acc -> insertItvInOrderedList (itv, s) acc unionDFAState) cl2 cl1
  in (cl3, e3)

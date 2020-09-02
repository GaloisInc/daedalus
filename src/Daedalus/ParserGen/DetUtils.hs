module Daedalus.ParserGen.DetUtils
  ( TreeChoice(..),
    ClosurePath,
    initClosurePath,
    addClosurePath,
    stateInClosurePath,
    lengthClosurePath,
    getLastState,
    hasBranchAction,
    FlattenTreeChoice,
    flattenTreeChoice,
    InputHeadCondition(..),
    DetChoice,
    emptyDetChoice,
    insertDetChoice
  ) where


-- import Debug.Trace

--import qualified Data.Set as Set

import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..), isBranchAction)
-- import Daedalus.ParserGen.Aut (Aut, lookupAut, Choice(..), toListTr, transition, acceptings, initials)

import Daedalus.ParserGen.ClassInterval (ClassInterval, insertItvInOrderedList)

data SymbolicStack =
    SWildcard
  | SEmpty
  | SCons State SymbolicStack
  deriving (Eq, Ord)

data CfgDet = CfgDet
  { cfgState  :: State
  , cfgRuleNb :: Maybe Int
  , cfgStack  :: SymbolicStack
  }
  -- deriving (Eq, Ord)


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

initClosurePath :: State -> ClosurePath
initClosurePath q =
  CP_Empty (CfgDet { cfgState = q, cfgRuleNb = Nothing, cfgStack = SWildcard})


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

getLastSymbData :: ClosurePath -> SymbolicData
getLastSymbData p =
  case p of
    CP_Empty x -> cfgStack x
    CP_Cons _ _ x -> cfgStack x

getLastState :: ClosurePath -> State
getLastState p =
  case p of
    CP_Empty x -> cfgState x
    CP_Cons _ _ x -> cfgState x

addClosurePath :: Action -> State -> ClosurePath -> Maybe ClosurePath
addClosurePath a q p =
  let symbData = getLastSymbData p in
  case symbExecAction symbData a of
    Nothing -> Nothing
    Just sd ->
      let cfgDet = CfgDet { cfgState = q, cfgRuleNb = Nothing, cfgStack = sd }
      in Just $ CP_Cons p a cfgDet

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



data TreeChoice =
    Move (ClosurePath, (Action, State))
  | NoMove
  | Seq [ TreeChoice ]
  | Par [ TreeChoice ]


type ChoicePath = [Int]

type FlattenTreeChoice a = [ (ChoicePath, ClosurePath, (a, State)) ]

flattenTreeChoice :: TreeChoice -> FlattenTreeChoice Action
flattenTreeChoice tc =
  case tc of
    Move (da,(act,q)) -> [([],da,(act,q))]
    NoMove  -> []
    Par lst -> flattenListTreeChoice lst
    Seq lst -> flattenListTreeChoice lst

  where
    flattenListTreeChoice :: [ TreeChoice ] -> FlattenTreeChoice Action
    flattenListTreeChoice lst =
      let lst1 = map flattenTreeChoice lst
          lst2 = zipWithInt lst1 0
          lst3 = map (\ (n, backlist) -> map (\ (lsti, da, (a,q)) -> (n:lsti, da, (a,q))) backlist) lst2
      in foldr (\ a b -> a ++ b) [] lst3

    zipWithInt :: [a] -> Int -> [(Int,a)]
    zipWithInt l i =
      case l of
        [] -> []
        x : xs -> (i, x) : zipWithInt xs (i + 1)


data InputHeadCondition =
    HeadInput ClassInterval
  | EndInput

type FullTrace = (ChoicePath, ClosurePath, SymbolicStack, State)

-- fst element is a list of class action transition, the snd possible element is for EndInput test
type DetChoice = ([ (ClassInterval, [FullTrace]) ], Maybe [FullTrace])

emptyDetChoice :: DetChoice
emptyDetChoice = ([], Nothing)

insertDetChoice :: (ChoicePath, ClosurePath, (InputHeadCondition, State)) -> DetChoice -> DetChoice
insertDetChoice (cp, da, (ih,q)) d =
  let (classChoice, endChoice) = d in
  case ih of
    EndInput ->
      case endChoice of
        Nothing -> (classChoice, Just [(cp, da, SWildcard, q)])
        Just setTrace -> (classChoice, Just ((cp, da, SWildcard, q) : setTrace))
    HeadInput x ->
      let tr = [(cp, da, SWildcard, q)] in
      (insertItvInOrderedList (x, tr) classChoice (++), endChoice)

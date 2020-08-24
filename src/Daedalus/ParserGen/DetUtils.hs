module Daedalus.ParserGen.DetUtils where


-- import Debug.Trace

import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..))
-- import Daedalus.ParserGen.Aut (Aut, lookupAut, Choice(..), toListTr, transition, acceptings, initials)

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

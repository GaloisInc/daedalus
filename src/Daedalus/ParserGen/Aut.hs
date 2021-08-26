module Daedalus.ParserGen.Aut
  ( Aut(..)
  , Choice(..)
  , noChoice
  , stateToString
  , lookupPopTrans
  , MapAut(..)
  , ArrayAut(..)
  , mkAut
  , mkAutWithPop
  , mkTr
  , mkTr1
  , dsAut
  , unionTr
  , emptyTr
  , unionPopTrans
  , emptyPopTrans
  , addPopTrans
  , addGblFunsAut
  , mkArrayAut
  , getMaxState
  , convertToArrayAut
  )

where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Array.IArray
import qualified Data.Text as T

import Daedalus.Type.AST

import qualified Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action

data Choice =
    UniChoice (Action, State)
  | SeqChoice [(Action, State)] State --   NOTE: The last state in this constructor is to facilitate checking the invariant of well-bracketedness
  | ParChoice [(Action, State)]
  deriving(Show)

class Aut a where
  initialState :: a -> State
  nextTransition :: a -> State -> Maybe Choice
  allTransitions :: a -> [(State, Choice)]
  isAcceptingState :: a -> State -> Bool
  destructureAut :: a -> (State, [(State, Action, State)], State)
  popTransAut :: a -> PopTrans
  stateMappingAut :: a -> State -> Maybe (SourceRange, PAST.Contx)
  gblFunsAut :: a -> PAST.GblFuns

type Transition = Map.Map State Choice

type Acceptings = State

type PopTrans = Map.Map State [State]

data MapAut = MapAut
  { initials    :: State
  , transition  :: Transition
  , acceptings  :: Acceptings
  , popTrans :: PopTrans
  , stateMapping :: Map.Map State (SourceRange, PAST.Contx)
  , gblFuns :: Maybe (PAST.GblFuns)
  }
  deriving Show

instance Aut MapAut where
  initialState dta = initials dta
  nextTransition dta s = lookupAut s dta
  allTransitions dta = Map.toList $ transition dta
  isAcceptingState dta s = isAccepting s dta
  destructureAut dta = toListAut dta
  popTransAut dta = popTrans dta
  stateMappingAut dta q = Map.lookup q (stateMapping dta)
  gblFunsAut dta =
    case gblFuns dta of
      Nothing -> error "gblFuns are not set"
      Just gbl -> gbl

mkAut :: State -> Transition -> Acceptings -> MapAut
mkAut initial trans accepts =
  MapAut { initials = initial
         , transition = trans
         , acceptings = accepts
         , popTrans = emptyPopTrans
         , stateMapping = Map.empty
         , gblFuns = Nothing
         }

mkAutWithPop :: State -> Transition -> Acceptings -> PopTrans -> MapAut
mkAutWithPop initial trans accepts pops =
  MapAut { initials = initial
         , transition = trans
         , acceptings = accepts
         , popTrans = pops
         , stateMapping = Map.empty
         , gblFuns = Nothing
         }

dsAut :: MapAut -> (State, Transition, Acceptings, PopTrans)
dsAut aut = (initials aut, transition aut, acceptings aut, popTrans aut)


emptyTr :: Transition
emptyTr = Map.empty


combineCh :: Choice -> Choice -> Choice
combineCh c1 c2 =
  case (c1,c2) of
    (UniChoice p1, UniChoice p2) -> ParChoice [ p1 , p2 ]
    (UniChoice p1, ParChoice l) -> ParChoice (p1:l)
    (ParChoice l, UniChoice p2) -> ParChoice (l ++ [p2])
    (ParChoice l1, ParChoice l2) -> ParChoice (l1 ++ l2)
    _ -> error "combine not handled"

mkTr1 :: (State, Choice) -> Transition
mkTr1 tr = Map.fromList [tr]

mkTr :: [ (State, Choice) ] -> Transition
mkTr lst =
  foldr (\ (s, ch) acc -> Map.insertWith combineCh s ch acc) emptyTr lst

unionTr :: Transition -> Transition -> Transition
unionTr t1 t2 =
  Map.unionWith combineCh t1 t2

emptyPopTrans :: PopTrans
emptyPopTrans = Map.empty

addPopTrans :: State -> State -> PopTrans -> PopTrans
addPopTrans f ret p = Map.insertWith (++) f [ret] p

unionPopTrans :: PopTrans -> PopTrans -> PopTrans
unionPopTrans p1 p2 = Map.unionWith (++) p1 p2

lookupPopTrans :: State -> PopTrans -> Maybe [State]
lookupPopTrans q p = Map.lookup q p

lookupAut :: State -> MapAut -> Maybe Choice
lookupAut q aut = Map.lookup q (transition aut)


toListTr :: Transition -> [(State, Choice)]
toListTr tr = Map.toList tr

toListTr2 :: Transition -> [(State, Action, State)]
toListTr2 tr =
  let tr1 = toListTr tr
  in foldr (\ (n1, ch) acc -> develop n1 ch ++ acc) [] tr1
  where develop n1 (UniChoice (act, n2)) = [(n1,act,n2)]
        develop n1 (ParChoice lst) = map (\ (act,n2) -> (n1,act,n2)) lst
        develop n1 (SeqChoice lst _) = map (\ (act,n2) -> (n1,act,n2)) lst  -- error "dont unfold sequential choice"

toListAut :: MapAut -> (State, [(State,Action,State)], State)
toListAut aut =
  let (i,t,f,_) = dsAut aut
  in (i, toListTr2 t, f)

stateToString :: Aut a => State -> a -> String
stateToString q aut =
  let annToString (srcRg, x) = T.unpack (PAST.name2Text x) ++ " " ++ PAST.showSourceRange srcRg
  in
    maybe "__START__" (\ p -> annToString p) $ stateMappingAut aut q


noChoice :: Choice
noChoice = ParChoice []


isAccepting :: State -> MapAut -> Bool
isAccepting q aut =
  q == acceptings aut

addGblFunsAut :: PAST.GblFuns -> MapAut -> MapAut
addGblFunsAut gbl aut =
  aut {gblFuns = Just gbl}

type ArrayA = Array Int (Maybe Choice)

-- For the time being we will reuse the existing implementation somewhat
-- This shouldn't affect runtime performance
data ArrayAut = ArrayAut {
  transitionArray :: {-# UNPACK #-} !(ArrayA),
  mapAut :: {-# UNPACK #-} !MapAut
}

generate :: Int -> Transition -> ArrayA
generate size tr =
  array (0, size) lst2
  where
    lst2 = [ case Map.lookup i tr of
               Nothing -> (i, Nothing)
               Just e -> (i, Just e)
           | i <- [0..size] ]


mkArrayAut :: State -> Transition -> Acceptings -> ArrayAut
mkArrayAut s t a =
  convertToArrayAut $ mkAut s t a

getMaxState :: Aut a => a -> State
getMaxState aut =
  maxState
  where
    maxState =
      let transitionStates = concatMap states $ allTransitions aut in
      let allStates = initialState aut : transitionStates in
      List.maximum allStates
    states (state, choice) = state : choiceStates choice
    choiceStates (UniChoice (_, endState)) = [endState]
    choiceStates (SeqChoice lst lastState) = lastState : map snd lst
    choiceStates (ParChoice lst) = map snd lst

convertToArrayAut :: MapAut -> ArrayAut
convertToArrayAut aut =
  let
    maxState = getMaxState aut
    arr = generate (maxState + 1) (transition aut)
  in
    ArrayAut { transitionArray = arr, mapAut = aut }


instance Aut ArrayAut where
  initialState dta = initials $ mapAut dta
  nextTransition dta s = (transitionArray dta) ! s
  allTransitions dta = Map.toList $ transition $ mapAut dta
  isAcceptingState dta s = isAccepting s $ mapAut dta
  destructureAut dta = toListAut $ mapAut dta
  popTransAut dta = popTrans $ mapAut dta
  stateMappingAut dta q = stateMappingAut (mapAut dta) q
  gblFunsAut dta = gblFunsAut $ mapAut dta
  {-# SPECIALIZE INLINE nextTransition :: ArrayAut -> State -> Maybe Choice  #-}
  {-# INLINE isAcceptingState #-}
  {-# INLINE popTransAut #-}

module Daedalus.ParserGen.Aut where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import Daedalus.ParserGen.Action


data Choice =
    UniChoice (Action, State)
  | SeqChoice [(Action, State)] State --   NOTE: The last state in this constructor is to facilitate checking the invariant of well-bracketedness
  | ParChoice [(Action, State)]
  deriving(Show)

type Transition = Map.Map State Choice

type Acceptings = State

data Aut = Aut
  { initials    :: State
  , transition  :: Transition
  , acceptings  :: Acceptings
  , acceptingsEps :: Maybe (Map.Map State Bool)
  , transitionEps :: Maybe (Map.Map State [ State ])
  , transitionWithoutEps :: Maybe Transition
  }
  deriving Show

noChoice :: Choice
noChoice = ParChoice []

mkAut :: State -> Transition -> Acceptings -> Aut
mkAut initial trans accepts =
  Aut { initials = initial
      , transition = trans
      , acceptings = accepts
      , acceptingsEps = Nothing
      , transitionEps = Nothing
      , transitionWithoutEps = Nothing
      }

dsAut :: Aut -> (State, Transition, Acceptings)
dsAut aut = (initials aut, transition aut, acceptings aut)


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

mkTr3 :: [ (State, Action, State) ] -> Transition
mkTr3 tr =
  foldr (\ (n1,act,n2) acc -> unionTr (mkTr1 (n1, UniChoice (act, n2))) acc) emptyTr tr

unionTr :: Transition -> Transition -> Transition
unionTr t1 t2 =
  Map.unionWith combineCh t1 t2

lookupAut :: State -> Aut -> Maybe Choice
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

toListAut :: Aut -> (State, [(State,Action,State)], State)
toListAut aut =
  let (i,t,f) = dsAut aut
  in (i, toListTr2 t, f)


-- Is the action effectful on the devices of a machine.
isEffectful :: Action -> Bool
isEffectful act =
  case act of
    EpsA -> False
    _    -> True

-- The transitive closure of path having no effect on the devices of a
-- machine.
noEffect_trans_clos :: State -> Aut -> [State]
noEffect_trans_clos q aut =
  {-# SCC "TRANS_CLOS" #-}
  let (_i,t,_f) = toListAut aut
      noEffectfulTrans = filter (\ (_, act, _) -> not (isEffectful act)) t
      oneStep set =
        let
          set1 = Set.foldr (\ path b -> Set.union b (oneStepOnPath path)) Set.empty set
          set2 = Set.filter hasNoRepeat set1
        in set2
      oneStepOnPath qs =
        case qs of
          [] -> error "impossible"
          qf : _r ->
            foldr (\ (n1, _act, n2) b ->
                     if n1 == qf
                     then Set.insert (n2 : qs) b
                     else b)
            Set.empty noEffectfulTrans
      hasNoRepeat qs = not (hasDuplicate qs)
      hasDuplicate lst = case lst of
                           [] -> False
                           x : xs -> if elem x xs then True else hasDuplicate xs
      fxtpt set =
        let newpaths = oneStep set
            set1 = Set.union set newpaths
        in if set == set1
           then set
           else fxtpt set1
      -- end of computing the fixpoint, now need to compute the path with transitions
  in
    map head (Set.toList (fxtpt (Set.fromList [ [q] ])))

filterEpsTr :: Choice -> Choice
filterEpsTr ch =
  case ch of
    UniChoice (EpsA, _) -> ParChoice []
    UniChoice _ -> ch
    ParChoice chs -> ParChoice $ filter (\ (act, _) -> isEffectful act) chs
    SeqChoice _ _ -> error "Dont apply in SeqChoice"

hasOnlyEpsTr :: Choice -> Bool
hasOnlyEpsTr ch =
  case ch of
    UniChoice (EpsA, _) -> True
    UniChoice _ -> False
    ParChoice chs -> foldr (\ (act, _n2) b -> if isEffectful act then False else b) True chs
    SeqChoice _ _ -> error "Dont apply in SeqChoice"

-- Compute the epsilon reflexive transitive closure and stores it in the Aut
precomputeEps :: Aut -> Aut
precomputeEps aut =
  let
    tr = transition aut
    f = acceptings aut
    completeTr = if Map.member f tr then tr else Map.insert f noChoice tr
    transEps = Map.mapWithKey (\ k _ -> noEffect_trans_clos k aut) completeTr
    acceptEps = Map.mapWithKey (\ k _ -> elem f (fromJust $ Map.lookup k transEps)) completeTr
    transEpsFiltered = Map.map (\ qs -> filter (\ q -> not $ hasOnlyEpsTr (fromJust (Map.lookup q completeTr))) qs) transEps
    transWithoutEps = Map.map (\ b -> filterEpsTr b) completeTr

  in
    Aut { initials = initials aut
        , transition = completeTr
        , acceptings = f
        , acceptingsEps = Just acceptEps
        , transitionEps = Just transEpsFiltered
        , transitionWithoutEps = Just transWithoutEps
        }

getEpsTransStates :: State -> Aut -> [ State ]
getEpsTransStates q aut =
  maybe (error "should precompute") (\ m -> fromJust (Map.lookup q m)) (transitionEps aut)

lookupTransitionWithoutEps :: State -> Aut -> Maybe Choice
lookupTransitionWithoutEps q aut =
  Map.lookup q (fromJust $ transitionWithoutEps aut)


isAccepting :: State -> Aut -> Bool
isAccepting q aut =
  q == acceptings aut


isAcceptingEps :: State -> Aut -> Bool
isAcceptingEps q aut =
  maybe (error "should precompute") (\ m -> fromJust (Map.lookup q m)) (acceptingsEps aut)

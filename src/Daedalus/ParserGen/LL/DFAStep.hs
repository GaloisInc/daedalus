{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DFAStep
  ( SourceCfg,
    DFAEntry(..),
    getInfoEntry,
    DFARegistry,
    IteratorDFARegistry,
    initIteratorDFARegistry,
    nextIteratorDFARegistry,
    isEmptyIteratorDFARegistry,
    partitionDFARegistry,
    DetChoice(..),
    DFAState(..),
    showDFAState,
    mkDFAState,
    mkDFAStateFromSlkCfg,
    isDFAStateInit,
    isDFAStateInitForDep,
    getCfgFromDFAState,
    nullDFAState,
    convertDFARegistryToDFAState,
    IteratorDFAState,
    initIteratorDFAState,
    nextIteratorDFAState,
    isEmptyIteratorDFAState,
    measureDFAState,
    closureDataDependentOnDFAState,
    determinizeDFAState,
  ) where


-- import Debug.Trace

import qualified Data.Set as Set

import Daedalus.ParserGen.Action (State)
import Daedalus.ParserGen.Aut (Aut(..), stateToString)
import Daedalus.ParserGen.LL.ClassInterval
import Daedalus.ParserGen.LL.Result
import qualified Daedalus.ParserGen.LL.SlkCfg as Slk
import qualified Daedalus.ParserGen.LL.Closure as Closure




-- `DFAEntry` is a type capturing a path of execution in the NFA,
-- starting from a source to its destination.

type SourceCfg = Slk.SlkCfg

data DFAEntry = DFAEntry
  { srcEntry :: SourceCfg
  , dstEntry :: Closure.ClosureMove
  }
  deriving Show


compareSrc :: DFAEntry -> DFAEntry -> Ordering
compareSrc p1 p2 =
  Slk.compareSlkCfg (srcEntry p1) (srcEntry p2)

compareDst :: DFAEntry -> DFAEntry -> Ordering
compareDst p1 p2 =
  compare (dstEntry p1) (dstEntry p2)

compareDFAEntry :: DFAEntry -> DFAEntry -> Ordering
compareDFAEntry p1 p2 =
  case compareDst p1 p2 of
    LT -> LT
    GT -> GT
    EQ ->
      compareSrc p1 p2


instance Eq DFAEntry where
  (==) e1 e2 = compareDFAEntry e1 e2 == EQ


instance Ord DFAEntry where
  compare p1 p2 = compareDFAEntry p1 p2



getInfoEntry :: Aut a => a -> DFAEntry -> [ String ]
getInfoEntry aut (DFAEntry {srcEntry = src, dstEntry = dst}) =
  case dst of
    Closure.ClosureMove { Closure.moveCfg = (_,_,q), Closure.closureCfg = cfg } ->
      Slk.showSlkControlData aut (Slk.cfgCtrl cfg) ++ ["MATCH: " ++ stateToString q aut]
    Closure.ClosurePath { Closure.infoMove = (_,_,q), Closure.closureCfg = cfg } ->
      Slk.showSlkControlData aut (Slk.cfgCtrl cfg) ++ ["MATCH: " ++ stateToString q aut]
    Closure.ClosureAccepting {} -> ["MATCH: " ++ stateToString (Slk.cfgState src) aut]

-- A generic iterator datatype.
-- Note the type of data is the same of the generic type

data Iterator a =
  Iterator
  { iii :: a
  , curr :: Int
  , size :: Int
  }


-- DFARegistry is a type encapsulating a set of DFAEntry. In practice
-- it is the result of a factorization of a class operation or any
-- operation that interrupts a lockstep move.

type DFARegistry = Set.Set DFAEntry

type IteratorDFARegistry = Iterator DFARegistry


initIteratorDFARegistry :: DFARegistry -> IteratorDFARegistry
initIteratorDFARegistry r =
  Iterator
  { iii = r
  , curr = 0
  , size = Set.size r
  }

nextIteratorDFARegistry ::
  IteratorDFARegistry -> Maybe (DFAEntry, IteratorDFARegistry)
nextIteratorDFARegistry iter@(Iterator { iii = r, curr = c, size = s}) =
  if c >= s
  then Nothing
  else Just (Set.elemAt c r, iter { curr = c + 1})

isEmptyIteratorDFARegistry :: IteratorDFARegistry -> Bool
isEmptyIteratorDFARegistry (Iterator { iii = _r, curr = c, size = s}) =
  if c >= s
  then True
  else False


revAppend :: [a] -> [a] -> [a]
revAppend lst1 lst2 =
  case lst1 of
    [] -> lst2
    x:xs -> revAppend xs (x:lst2)

partitionDFARegistry ::
  DFARegistry ->
  (DFAEntry -> DFAEntry -> Bool) ->
  [ [DFAEntry] ]
partitionDFARegistry s test =
  let llst = helper (initIteratorDFARegistry s) [] in
    map (\ (_, lst) -> lst) llst
  where
    helper ::
      IteratorDFARegistry -> [ (DFAEntry, [DFAEntry]) ] ->
      [ (DFAEntry, [DFAEntry]) ]
    helper iter part =
      case nextIteratorDFARegistry iter of
        Nothing -> part
        Just (x, xs) ->
          helper xs (insertInPartition x part [])

    insertInPartition ::
      DFAEntry -> [ (DFAEntry, [DFAEntry]) ] -> [ (DFAEntry, [DFAEntry]) ] ->
      [ (DFAEntry, [DFAEntry]) ]
    insertInPartition x part acc =
      case part of
        [] -> reverse ((x,[x]) : acc)
        p@(canonical, lst) : rest ->
          if test canonical x
          then revAppend acc ((canonical, x : lst) : rest)
          else insertInPartition x rest (p:acc)



emptyDFARegistry :: DFARegistry
emptyDFARegistry = Set.empty

addDFARegistry :: DFAEntry -> DFARegistry -> DFARegistry
addDFARegistry e s =
  Set.insert e s

unionDFARegistry :: DFARegistry -> DFARegistry -> DFARegistry
unionDFARegistry s1 s2 = Set.union s1 s2

singletonDFARegistry :: DFAEntry -> DFARegistry
singletonDFARegistry x = Set.singleton x



data DetChoice =
  DetChoice
  { acceptingDetChoice :: Maybe DFARegistry
  , endDetChoice       :: Maybe DFARegistry
  , classDetChoice     :: [ (ClassInterval, DFARegistry) ]
  }

emptyDetChoice :: DetChoice
emptyDetChoice =
  DetChoice
  { acceptingDetChoice = Nothing
  , endDetChoice = Nothing
  , classDetChoice = []
  }


insertDetChoiceAccepting ::
  SourceCfg -> Closure.ClosureMove -> DetChoice -> DetChoice
insertDetChoiceAccepting src cm detChoice =
  let q = singletonDFARegistry (DFAEntry src cm) in
  let acceptingDetChoice' =
        case acceptingDetChoice detChoice of
          Nothing -> Just q
          Just qs -> Just (unionDFARegistry q qs)
  in detChoice { acceptingDetChoice = acceptingDetChoice' }



insertDetChoiceInputHeadCondition ::
  SourceCfg ->
  Slk.InputHeadCondition ->
  Closure.ClosureMove ->
  DetChoice ->
  DetChoice
insertDetChoiceInputHeadCondition src ih cm detChoice =
  let q = singletonDFARegistry (DFAEntry src cm) in
  case ih of
    Slk.EndInput ->
      let endDetChoice' =
            case endDetChoice detChoice of
              Nothing -> Just q
              Just qs -> Just (unionDFARegistry q qs)
      in detChoice { endDetChoice = endDetChoice' }
    Slk.HeadInput x ->
      let classDetChoice' =
            let c = classDetChoice detChoice
            in insertByteConditionInOrderedList (x, q) c unionDFARegistry
      in detChoice { classDetChoice = classDetChoice' }


unionDetChoice :: DetChoice -> DetChoice -> DetChoice
unionDetChoice
  (DetChoice
   { acceptingDetChoice = acc1
   , endDetChoice = e1
   , classDetChoice = cl1})
  (DetChoice
   { acceptingDetChoice = acc2
   , endDetChoice = e2
   , classDetChoice = cl2})
  =
  let acc3 =
        case (acc1, acc2) of
          (Nothing, _) -> acc2
          (_, Nothing) -> acc1
          (Just a1, Just a2) -> Just (unionDFARegistry a1 a2)

      e3 =
        case (e1, e2) of
          (Nothing, _) -> e2
          (_, Nothing) -> e1
          (Just tr1, Just tr2) -> Just (unionDFARegistry tr1 tr2)
  in
  let cl3 = unionClassIntervalList cl1 cl2 unionDFARegistry in
  DetChoice
  { acceptingDetChoice = acc3
  , endDetChoice = e3
  , classDetChoice = cl3
  }


-- this function takes a tree representing a set of choices and
-- convert it to a Input factored deterministic transition.
determinizeMove :: SourceCfg -> Closure.ClosureMoveSet -> Result DetChoice
determinizeMove src tc =
  determinizeWithAccu tc Nothing emptyDetChoice

  where
    determinizeWithAccu ::
      Closure.ClosureMoveSet ->
      Maybe Slk.SlkInput ->
      DetChoice ->
      Result DetChoice
    determinizeWithAccu lst minp acc =
      case lst of
        [] -> Result acc
        t : ms ->
          let
            cfg = Closure.closureCfg t
            inp = Slk.cfgInput cfg
            newAcc =
              case t of
                Closure.ClosureMove {} ->
                  let (_pos, (_act, ihc), _q) = Closure.moveCfg t in
                  insertDetChoiceInputHeadCondition src ihc t acc
                Closure.ClosureAccepting {} ->
                  insertDetChoiceAccepting src t acc
                _ -> error "impossible case"
          in
          if testCompatibleInput inp minp
          then
            determinizeWithAccu ms (Just $ inp) newAcc
          else
            Abort AbortDFAIncompatibleInput

    testCompatibleInput :: Slk.SlkInput -> Maybe Slk.SlkInput -> Bool
    testCompatibleInput inp minp =
      case minp of
        Nothing -> True
        Just inp2 -> Slk.compatibleInput inp inp2



deterministicSlkCfg ::
  Aut a =>
  a -> Slk.SlkCfg ->
  Slk.HTable -> (Result DetChoice, Slk.HTable)
deterministicSlkCfg aut cfg tab =
  let (res, tab1) = Closure.closureLL aut cfg tab in
  case res of
    Abort AbortSlkCfgExecution -> (coerceAbort res, tab1)
    Abort AbortSlkCfgClassIsDynamic -> (coerceAbort res, tab1)
    Abort (AbortSlkCfgClassNotHandledYet _) -> (coerceAbort res, tab1)
    Abort AbortClosureOverflowMaxDepth -> (coerceAbort res, tab1)
    Abort AbortClosureInfiniteloop -> (coerceAbort res, tab1)
    Abort AbortClosureUnhandledInputAction -> (coerceAbort res, tab1)
    Abort AbortClosureUnhandledAction -> (coerceAbort res, tab1)
    Result r -> (determinizeMove cfg r, tab1)
    _ -> error "Impossible abort"


newtype DFAState =
  DFAState
  { dfaState :: Set.Set Slk.SlkCfg
  }

instance Show(DFAState) where
  show q =
    Set.fold (\ cfg s -> s ++ Slk.showSlkCfg cfg) "" (dfaState q)

showDFAState :: Aut a => a -> DFAState -> String
showDFAState aut q =
  Set.fold (\ cfg s -> s ++ Slk.showSlkCfgWithAut aut cfg) "" (dfaState q)

mkDFAState :: State -> Slk.HTable -> (DFAState, Slk.HTable)
mkDFAState q tab =
  let (initCfg, tab1) = Slk.initSlkCfg q tab
  in (DFAState (Set.singleton initCfg), tab1)

mkDFAStateFromSlkCfg :: Slk.SlkCfg -> DFAState
mkDFAStateFromSlkCfg cfg =
  DFAState { dfaState = Set.singleton cfg }

isDFAStateInit :: DFAState -> Maybe State
isDFAStateInit q =
  if (Set.size (dfaState q) == 1)
  then
    let cfg = Set.elemAt 0 (dfaState q)
    in
    case cfg of
      Slk.SlkCfg{ Slk.cfgState = qNFA } ->
        if (Slk.isInitSlkCfg cfg)
        then Just qNFA
        else Nothing
  else Nothing

isDFAStateInitForDep :: DFAState -> Maybe State
isDFAStateInitForDep q =
  if (Set.size (dfaState q) == 1)
  then
    let cfg = Set.elemAt 0 (dfaState q)
    in
    case cfg of
      Slk.SlkCfg{ Slk.cfgState = qNFA } ->
        Just qNFA
  else Nothing

getCfgFromDFAState :: DFAState -> Maybe Slk.SlkCfg
getCfgFromDFAState q =
  if (Set.size (dfaState q) == 1)
  then
    let cfg = Set.elemAt 0 (dfaState q)
    in
    Just cfg
  else Nothing

equivDFAState :: DFAState -> DFAState -> Bool
equivDFAState q1 q2 = dfaState q1 == dfaState q2

instance Eq DFAState where
  (==) q1 q2 = equivDFAState q1 q2

instance Ord DFAState where
  compare q1 q2 =
    compare (dfaState q1) (dfaState q2)

emptyDFAState :: DFAState
emptyDFAState = DFAState Set.empty

nullDFAState :: DFAState -> Bool
nullDFAState q =
  Set.null (dfaState q)

addDFAState :: Slk.SlkCfg -> DFAState -> DFAState
addDFAState cfg q =
  DFAState $ Set.insert cfg (dfaState q)


type IteratorDFAState = Iterator DFAState

initIteratorDFAState :: DFAState -> IteratorDFAState
initIteratorDFAState r@(DFAState s) =
  Iterator
  { iii = r
  , curr = 0
  , size = Set.size s
  }

nextIteratorDFAState :: IteratorDFAState -> Maybe (Slk.SlkCfg, IteratorDFAState)
nextIteratorDFAState iter@(Iterator { iii = DFAState r, curr = c, size = s}) =
  if c >= s
  then Nothing
  else Just (Set.elemAt c r, iter { curr = c + 1})


isEmptyIteratorDFAState :: IteratorDFAState -> Bool
isEmptyIteratorDFAState (Iterator { iii = _r, curr = c, size = s}) =
  if c >= s
  then True
  else False

-- Approximate measure of the `DFAState`. Remark it does not take into
-- account the size of the state. Maybe should be fixed
measureDFAState :: DFAState -> Int
measureDFAState s =
  helper (initIteratorDFAState s) 0
  where
    helper qq r =
      case nextIteratorDFAState qq of
        Nothing -> r
        Just (cfg, qs) ->
          let measCfg = Slk.measureSlkCfg cfg
          in helper qs (max measCfg r)

convertDFARegistryToDFAState :: DFARegistry -> DFAState
convertDFARegistryToDFAState r =
  helper (initIteratorDFARegistry r)
  where
    helper s =
      case nextIteratorDFARegistry s of
        Nothing -> emptyDFAState
        Just (entry, es) ->
          addDFAState (Closure.lastCfg $ dstEntry entry) (helper es)


closureDataDependentOnDFAState ::
  Aut a =>
  a ->
  DFAState ->
  Slk.HTable -> (Result (Maybe Closure.DataDepInstr), Slk.HTable)
closureDataDependentOnDFAState aut qDFA tab =
  case isDFAStateInitForDep qDFA of
    Nothing -> (Result Nothing, tab)
    Just _ ->
      let cfg = Set.elemAt 0 (dfaState qDFA) in
      Closure.closureEpsUntilDataDependent aut Set.empty (Closure.emptyChoiceSeq, cfg) tab


slkExecMoveAndEpsRegistry ::
  Aut a =>
  a ->
  Slk.InputHeadCondition ->
  DFARegistry ->
  Slk.HTable -> (Result DFARegistry, Slk.HTable)
slkExecMoveAndEpsRegistry aut ih r tab =
  helper (initIteratorDFARegistry r) tab
  where
    helper s stepTab =
      case nextIteratorDFARegistry s of
        Nothing -> (Result emptyDFARegistry, stepTab)
        Just (entry, es) ->
          let
            src = srcEntry entry
            cm = dstEntry entry
          in
          let mCm = Closure.simulateMoveClosure ih cm stepTab in
          case mCm of
            Nothing -> helper es stepTab
            Just (cmMove, tab1) ->
              let
                (mCmEps, tab2) =
                  Closure.closureEpsUntilPush aut Set.empty cmMove tab1
              in
              case mCmEps of
                Result Nothing -> helper es tab2
                Result (Just cmEps) ->
                  let
                    newEntry =
                      DFAEntry
                      { srcEntry = src
                      , dstEntry = cmEps
                      }
                    (mr, tab3) = helper es tab2
                  in
                  case mr of
                    Result h -> (Result (addDFARegistry newEntry h), tab3)
                    a -> (coerceAbort a, tab3)
                Abort _ -> (coerceAbort mCmEps, tab2)



determinizeDFAState ::
  Aut a =>
  a -> DFAState ->
  Slk.HTable -> (Result DetChoice, Slk.HTable)
determinizeDFAState aut s tab =
  let
    (detChoice1, tab1) =
      determinizeAcc (initIteratorDFAState s) emptyDetChoice tab
  in
  case detChoice1 of
    Result r1 -> slkExecClassAndEps r1 tab1
    Abort _ -> (coerceAbort detChoice1, tab1)

  where
    determinizeAcc ::
      IteratorDFAState -> DetChoice ->
      Slk.HTable -> (Result DetChoice, Slk.HTable)
    determinizeAcc states acc localTab =
      case nextIteratorDFAState states of
        Nothing -> (Result acc, localTab)
        Just (cfg, rest) ->
          let (r, tab1) = deterministicSlkCfg aut cfg localTab in
          case r of
            Abort AbortSlkCfgExecution -> (coerceAbort r, tab1)
            Abort AbortSlkCfgClassIsDynamic -> (coerceAbort r, tab1)
            Abort (AbortSlkCfgClassNotHandledYet _) -> (coerceAbort r, tab1)
            Abort AbortClosureOverflowMaxDepth -> (coerceAbort r, tab1)
            Abort AbortClosureInfiniteloop -> (coerceAbort r, tab1)
            Abort AbortClosureUnhandledInputAction -> (coerceAbort r, tab1)
            Abort AbortClosureUnhandledAction -> (coerceAbort r, tab1)
            Abort AbortDFAIncompatibleInput -> (coerceAbort r, tab1)
            Result r1 ->
              let newAcc = unionDetChoice r1 acc
              in determinizeAcc rest newAcc tab1
            _ -> error "cannot be this abort"


    slkExecClassAndEps ::
      DetChoice ->
      Slk.HTable -> (Result DetChoice, Slk.HTable)
    slkExecClassAndEps d localTab =
      let
        cdc = classDetChoice d
        edc = endDetChoice d
      in
      let
        computeOnClass lst acc loclocTab =
          case lst of
            [] -> (Result (reverse acc), loclocTab)
            (cl, r) : rest ->
              let
                ih = (Slk.HeadInput (ByteCondition [cl]))
                (sr, tab1) =
                  slkExecMoveAndEpsRegistry aut ih r loclocTab
              in
              case sr of
                Result r1 -> computeOnClass rest ((cl, r1) : acc) tab1
                Abort _ -> (coerceAbort sr, tab1)
      in
      let (mCd, tab1) = computeOnClass cdc [] localTab in
        case mCd of
          Result cd ->
            case edc of
              Nothing ->
                (Result (d { classDetChoice = cd, endDetChoice = Nothing}), tab1)
              Just r ->
                let
                  ih = Slk.EndInput
                  (mR1, tab2) =
                    slkExecMoveAndEpsRegistry aut ih r tab1
                in
                case mR1 of
                  Result r1 ->
                    (Result (d { classDetChoice = cd, endDetChoice = Just r1 }), tab2)
                  a -> (coerceAbort a, tab2)
          a -> (coerceAbort a, tab1)

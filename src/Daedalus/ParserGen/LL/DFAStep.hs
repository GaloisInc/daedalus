{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DFAStep
  ( SourceCfg,
    DFAEntry(..),
    DFARegistry,
    IteratorDFARegistry,
    initIteratorDFARegistry,
    nextIteratorDFARegistry,
    isEmptyIteratorDFARegistry,
    partitionDFARegistry,
    DetChoice(..),
    DFAState(..),
    mkDFAState,
    isDFAStateInit,
    nullDFAState,
    convertDFARegistryToDFAState,
    IteratorDFAState,
    initIteratorDFAState,
    nextIteratorDFAState,
    isEmptyIteratorDFAState,
    measureDFAState,
    determinizeDFAState,
  ) where


-- import Debug.Trace

import qualified Data.Set as Set

import Daedalus.ParserGen.Action (State, InputAction(..), getClassActOrEnd)
import Daedalus.ParserGen.Aut (Aut(..))
import Daedalus.ParserGen.LL.ClassInterval
import Daedalus.ParserGen.LL.Result
import qualified Daedalus.ParserGen.LL.SlkCfg as SCfg
import qualified Daedalus.ParserGen.LL.Closure as Closure




-- `DFAEntry` is a type capturing a path of execution in the NFA,
-- starting from a source to its destination.

type SourceCfg = SCfg.SlkCfg

data DFAEntry = DFAEntry
  { srcEntry :: SourceCfg
  , dstEntry :: Closure.ClosureMove
  }
  deriving Show


compareSrc :: DFAEntry -> DFAEntry -> Ordering
compareSrc p1 p2 =
  SCfg.compareSlkCfg (srcEntry p1) (srcEntry p2)

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

nextIteratorDFARegistry :: IteratorDFARegistry -> Maybe (DFAEntry, IteratorDFARegistry)
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

partitionDFARegistry :: DFARegistry -> (DFAEntry -> DFAEntry -> Bool) -> [ [DFAEntry] ]
partitionDFARegistry s test =
  let llst = helper (initIteratorDFARegistry s) [] in
    map (\ (_, lst) -> lst) llst
  where
    helper :: IteratorDFARegistry -> [ (DFAEntry, [DFAEntry]) ] -> [ (DFAEntry, [DFAEntry]) ]
    helper iter part =
      case nextIteratorDFARegistry iter of
        Nothing -> part
        Just (x, xs) ->
          helper xs (insertInPartition x part [])

    insertInPartition :: DFAEntry -> [ (DFAEntry, [DFAEntry]) ] -> [ (DFAEntry, [DFAEntry]) ] -> [ (DFAEntry, [DFAEntry]) ]
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


insertDetChoiceAccepting :: SourceCfg -> Closure.ClosureMove -> DetChoice -> DetChoice
insertDetChoiceAccepting src cm detChoice =
  let q = singletonDFARegistry (DFAEntry src cm) in
  let acceptingDetChoice' =
        case acceptingDetChoice detChoice of
          Nothing -> Just q
          Just qs -> Just (unionDFARegistry q qs)
  in detChoice { acceptingDetChoice = acceptingDetChoice' }



insertDetChoiceInputHeadCondition :: SourceCfg -> SCfg.InputHeadCondition -> Closure.ClosureMove -> DetChoice -> DetChoice
insertDetChoiceInputHeadCondition src ih cm detChoice =
  let q = singletonDFARegistry (DFAEntry src cm) in
  case ih of
    SCfg.EndInput ->
      let endDetChoice' =
            case endDetChoice detChoice of
              Nothing -> Just q
              Just qs -> Just (unionDFARegistry q qs)
      in detChoice { endDetChoice = endDetChoice' }
    SCfg.HeadInput x ->
      let classDetChoice' =
            let c = classDetChoice detChoice
            in insertItvInOrderedList (x, q) c unionDFARegistry
      in detChoice { classDetChoice = classDetChoice' }


unionDetChoice :: DetChoice -> DetChoice -> DetChoice
unionDetChoice (DetChoice acc1 e1 cl1) (DetChoice acc2 e2 cl2) =
  let acc3 =
        case (acc1, acc2) of
          (Nothing, _) -> acc2
          (_, Nothing) -> acc1
          (Just a1, Just a2) -> Just (unionDFARegistry a1 a2)

      e3 =
        case (e1,e2) of
          (Nothing, Nothing) -> Nothing
          (Nothing, Just _tr2) -> e2
          (Just _tr1, Nothing) -> e1
          (Just tr1, Just tr2) -> Just (unionDFARegistry tr1 tr2)
  in
  let cl3 =
        foldr (\ (itv, s) acc -> insertItvInOrderedList (itv, s) acc unionDFARegistry) cl2 cl1
  in DetChoice acc3 e3 cl3


-- this function takes a tree representing a set of choices and
-- convert it to a Input factored deterministic transition.
determinizeMove :: SourceCfg -> Closure.ClosureMoveSet -> Result DetChoice
determinizeMove src tc =
  determinizeWithAccu tc Nothing emptyDetChoice

  where
    determinizeWithAccu :: Closure.ClosureMoveSet -> Maybe SCfg.SlkInput -> DetChoice -> Result DetChoice
    determinizeWithAccu lst minp acc =
      case lst of
        [] -> Result acc
        t : ms ->
          let
            cfg = Closure.closureCfg t
            inp = SCfg.cfgInput cfg
            resAcc =
              case t of
                Closure.ClosureMove {} ->
                  let
                    (_pos, act, _q) = Closure.moveCfg t
                  in
                    case getClassActOrEnd act of
                      Left (Left c) ->
                        let res = classToInterval c in
                        case res of
                          Abort AbortClassIsDynamic -> coerceAbort res
                          Abort (AbortClassNotHandledYet _) -> coerceAbort res
                          Result r ->
                            Result $ insertDetChoiceInputHeadCondition src (SCfg.HeadInput r) t acc

                          _ -> error "Impossible abort"
                      Left (Right (IGetByte _)) ->
                        Result $ insertDetChoiceInputHeadCondition src (SCfg.HeadInput (ClassBtw (CValue 0) (CValue 255))) t acc
                      Right IEnd ->
                        Result $ insertDetChoiceInputHeadCondition src SCfg.EndInput t acc
                      _ -> error "Impossible case"
                Closure.ClosureAccepting {} ->
                  Result $ insertDetChoiceAccepting src t acc
                _ -> error "impossible case"
          in
          if compatibleInput inp minp
          then
            case resAcc of
              Abort AbortClassIsDynamic -> coerceAbort resAcc
              Abort (AbortClassNotHandledYet _) -> coerceAbort resAcc
              Result newAcc -> determinizeWithAccu ms (Just $ inp) newAcc
              _ -> error "impossible abort"
          else
            Abort AbortDFAIncompatibleInput

    compatibleInput :: SCfg.SlkInput -> Maybe SCfg.SlkInput -> Bool
    compatibleInput inp minp =
      case minp of
        Nothing -> True
        Just inp2 -> if compare inp inp2 == EQ
                     then True
                     else False



deterministicSlkCfg :: Aut a => a -> SCfg.SlkCfg -> Result DetChoice
deterministicSlkCfg aut cfg =
  let res = Closure.closureLL aut cfg in
  case res of
    Abort AbortSlkCfgExecution -> coerceAbort res
    Abort AbortClosureOverflowMaxDepth -> coerceAbort res
    Abort AbortClosureInfiniteloop -> coerceAbort res
    Abort AbortClosureUnhandledInputAction -> coerceAbort res
    Abort AbortClosureUnhandledAction -> coerceAbort res
    Result r -> determinizeMove cfg r
    _ -> error "Impossible abort"


newtype DFAState =
  DFAState
  { dfaState :: Set.Set SCfg.SlkCfg
  }
  deriving Show

mkDFAState :: State -> DFAState
mkDFAState q =
    DFAState (Set.singleton (SCfg.initSlkCfg q))

isDFAStateInit :: DFAState -> Maybe State
isDFAStateInit q =
  if (Set.size (dfaState q) == 1)
  then
    let cfg = Set.elemAt 0 (dfaState q)
    in
    case cfg of
      SCfg.SlkCfg
        { SCfg.cfgState = qNFA } ->
        if (SCfg.initSlkCfg qNFA == cfg)
        then Just qNFA
        else Nothing
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

addDFAState :: SCfg.SlkCfg -> DFAState -> DFAState
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

nextIteratorDFAState :: IteratorDFAState -> Maybe (SCfg.SlkCfg, IteratorDFAState)
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
          let measCfg = SCfg.measureSlkCfg cfg
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


slkExecMoveRegistry :: Aut a => a -> SCfg.InputHeadCondition -> DFARegistry -> Result DFARegistry
slkExecMoveRegistry aut ih r =
  helper (initIteratorDFARegistry r)
  where
    helper s =
      case nextIteratorDFARegistry s of
        Nothing -> Result emptyDFARegistry
        Just (entry, es) ->
          let
            src = srcEntry entry
            cm = dstEntry entry
          in
          let mCm = Closure.simulateMoveClosure ih cm in
            case mCm of
              Nothing -> helper es
              Just cmMove ->
                let mCmEps = Closure.closureEpsUntilPush aut Set.empty cmMove
                in
                  case mCmEps of
                    Result Nothing -> helper es
                    Result (Just cmEps) ->
                      let newEntry =
                            DFAEntry
                            { srcEntry = src
                            , dstEntry = cmEps
                            }
                      in
                        case helper es of
                          Result h -> Result (addDFARegistry newEntry h)
                          a -> coerceAbort a
                    Abort _ -> coerceAbort mCmEps


determinizeDFAState :: Aut a => a -> DFAState -> Result DetChoice
determinizeDFAState aut s =
  determinizeAcc (initIteratorDFAState s) emptyDetChoice
  where
    determinizeAcc :: IteratorDFAState -> DetChoice -> Result DetChoice
    determinizeAcc states acc =
      case nextIteratorDFAState states of
        Nothing -> slkExecClassAndEps acc
        Just (cfg, rest) ->
          let r = deterministicSlkCfg aut cfg in
          case r of
            Abort AbortSlkCfgExecution -> coerceAbort r
            Abort AbortClosureOverflowMaxDepth -> coerceAbort r
            Abort AbortClosureInfiniteloop -> coerceAbort r
            Abort AbortClosureUnhandledInputAction -> coerceAbort r
            Abort AbortClosureUnhandledAction -> coerceAbort r
            Abort AbortClassIsDynamic -> coerceAbort r
            Abort AbortDFAIncompatibleInput -> coerceAbort r
            Abort (AbortClassNotHandledYet _) -> coerceAbort r
            Result r1 ->
              let newAcc = unionDetChoice r1 acc
              in determinizeAcc rest newAcc
            _ -> error "cannot be this abort"




    slkExecClassAndEps :: DetChoice -> Result DetChoice
    slkExecClassAndEps d =
      let
        cdc = classDetChoice d
        edc = endDetChoice d
      in
      let computeOnClass lst acc =
            case lst of
              [] -> Result $ reverse acc
              (cl, r) : rest ->
                let sr = slkExecMoveRegistry aut (SCfg.HeadInput cl) r in
                case sr of
                  Result r1 -> computeOnClass rest ((cl, r1) : acc)
                  Abort _ -> coerceAbort sr
      in
        case computeOnClass cdc [] of
          Result cd ->
            case edc of
              Nothing ->
                Result $ d { classDetChoice = cd, endDetChoice = Nothing}
              Just r ->
                case slkExecMoveRegistry aut (SCfg.EndInput) r of
                  Result r1 ->
                    Result $ d { classDetChoice = cd, endDetChoice = Just r1 }
                  a -> coerceAbort a
          a -> coerceAbort a

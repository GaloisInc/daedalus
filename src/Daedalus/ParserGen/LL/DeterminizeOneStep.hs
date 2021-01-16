{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DeterminizeOneStep
  ( SourceCfg,
    DFAEntry(..),
    DFARegistry,
    iterDFARegistry,
    findAllEntryInDFARegistry,
    DetChoice(..),
    DFAState(..),
    mkDFAState,
    isDFAStateInit,
    nullDFAState,
    convertDFARegistryToDFAState,
    iterDFAState,
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



{-
`DFAEntry` is a type capturing a path of execution in the NFA,
starting from a source to its destination.
-}

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



{-
DFARegistry is a type encapsulating a set of DFAEntry. In practice it
is the result of a factorization of a class operation or any operation
that interrupts a lockstep move.
-}

type DFARegistry = Set.Set DFAEntry

iterDFARegistry :: DFARegistry -> Maybe (DFAEntry, DFARegistry)
iterDFARegistry s =
  let lst = Set.toAscList s in
    case lst of
      [] -> Nothing
      x:xs -> Just (x, Set.fromAscList xs)

findEntryInDFARegistry :: DFARegistry -> (DFAEntry -> Bool) -> Maybe (DFAEntry, DFARegistry)
findEntryInDFARegistry s test =
  case iterDFARegistry s of
    Nothing -> Nothing
    Just (x, xs) ->
      if test x then Just (x,xs)
      else case findEntryInDFARegistry xs test of
             Nothing -> Nothing
             Just (r, rs) -> Just (r, Set.insert x rs)

findAllEntryInDFARegistry :: DFARegistry -> (DFAEntry -> Bool) -> ([DFAEntry], DFARegistry)
findAllEntryInDFARegistry s test =
  case findEntryInDFARegistry s test of
    Nothing -> ([], s)
    Just (x, xs) ->
      let (lst, rest) = findAllEntryInDFARegistry xs test
      in (x:lst, rest)

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
            Abort AbortIncompatibleInput

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
    Abort AbortOverflowMaxDepth -> coerceAbort res
    Abort AbortLoopWithNonClass -> coerceAbort res
    Abort (AbortNonClassInputAction _) -> coerceAbort res
    Abort AbortUnhandledAction -> coerceAbort res
    Abort AbortSymbolicExec -> coerceAbort res
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

iterDFAState :: DFAState -> Maybe (SCfg.SlkCfg, DFAState)
iterDFAState s =
  let lst = Set.toAscList (dfaState s) in
    case lst of
      [] -> Nothing
      x:xs -> Just (x, DFAState (Set.fromAscList xs))

measureDFAState :: DFAState -> Int
measureDFAState s =
  helper s 0
  where
    helper qq r =
      case iterDFAState qq of
        Nothing -> r
        Just (cfg, qs) ->
          let iCtrl = SCfg.lengthSymbolicStack (SCfg.cfgCtrl cfg)
              iSem = SCfg.lengthSymbolicStack (SCfg.cfgSem cfg)
              newR = max iSem (max iCtrl r)
          in helper qs newR

convertDFARegistryToDFAState :: DFARegistry -> DFAState
convertDFARegistryToDFAState r =
  helper r
  where
    helper s =
      case iterDFARegistry s of
        Nothing -> emptyDFAState
        Just (entry, es) ->
          addDFAState (Closure.lastCfg $ dstEntry entry) (helper es)


slkExecMoveRegistry :: Aut a => a -> SCfg.InputHeadCondition -> DFARegistry -> Result DFARegistry
slkExecMoveRegistry aut ih r =
  helper r
  where
    helper s =
      case iterDFARegistry s of
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
  determinizeAcc s emptyDetChoice
  where
    determinizeAcc :: DFAState -> DetChoice -> Result DetChoice
    determinizeAcc states acc =
      case iterDFAState states of
        Nothing -> slkExecClassAndEps acc
        Just (cfg, rest) ->
          let r = deterministicSlkCfg aut cfg in
          case r of
            Abort AbortOverflowMaxDepth -> coerceAbort r
            Abort AbortLoopWithNonClass -> coerceAbort r
            Abort (AbortNonClassInputAction _) -> coerceAbort r
            Abort AbortUnhandledAction -> coerceAbort r
            Abort AbortClassIsDynamic -> coerceAbort r
            Abort AbortIncompatibleInput -> coerceAbort r
            Abort (AbortClassNotHandledYet _) -> coerceAbort r
            Abort AbortSymbolicExec -> coerceAbort r
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

{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DeterminizeOneStep
  ( SourceCfg,
    DFAStateEntry(..),
    DFAState,
    iterDFAState,
    findAllEntryInDFAState,
    DetChoice,
    DFAStateQuotient(..),
    mkDFAStateQuotient,
    nullDFAStateQuotient,
    convertDFAStateToQuotient,
    iterDFAStateQuotient,
    measureDFAStateQuotient,
    determinizeDFAStateQuotient,
  ) where


-- import Debug.Trace

import qualified Data.Set as Set

import Daedalus.ParserGen.Action (State, InputAction(..), getClassActOrEnd)
import Daedalus.ParserGen.Aut (Aut(..))
import Daedalus.ParserGen.LL.ClassInterval
import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.CfgDet
import Daedalus.ParserGen.LL.Closure



type SourceCfg = CfgDet

data DFAStateEntry = DFAStateEntry
  { srcDFAState :: SourceCfg
  , dstDFAState :: ClosureMove
  }
  deriving Show


compareSrc :: DFAStateEntry -> DFAStateEntry -> Ordering
compareSrc p1 p2 =
  compareCfgDet (srcDFAState p1) (srcDFAState p2)

compareDst :: DFAStateEntry -> DFAStateEntry -> Ordering
compareDst p1 p2 =
  compare (dstDFAState p1) (dstDFAState p2)

compareDFAStateEntry :: DFAStateEntry -> DFAStateEntry -> Ordering
compareDFAStateEntry p1 p2 =
  case compareDst p1 p2 of
    LT -> LT
    GT -> GT
    EQ ->
      compareSrc p1 p2


instance Eq DFAStateEntry where
  (==) e1 e2 = compareDFAStateEntry e1 e2 == EQ


instance Ord DFAStateEntry where
  compare p1 p2 = compareDFAStateEntry p1 p2


type DFAState = Set.Set DFAStateEntry

iterDFAState :: DFAState -> Maybe (DFAStateEntry, DFAState)
iterDFAState s =
  let lst = Set.toAscList s in
    case lst of
      [] -> Nothing
      x:xs -> Just (x, Set.fromAscList xs)

findEntryInDFAState :: DFAState -> (DFAStateEntry -> Bool) -> Maybe (DFAStateEntry, DFAState)
findEntryInDFAState s test =
  case iterDFAState s of
    Nothing -> Nothing
    Just (x, xs) ->
      if test x then Just (x,xs)
      else case findEntryInDFAState xs test of
             Nothing -> Nothing
             Just (r, rs) -> Just (r, Set.insert x rs)

findAllEntryInDFAState :: DFAState -> (DFAStateEntry -> Bool) -> ([DFAStateEntry], DFAState)
findAllEntryInDFAState s test =
  case findEntryInDFAState s test of
    Nothing -> ([], s)
    Just (x, xs) ->
      let (lst, rest) = findAllEntryInDFAState xs test
      in (x:lst, rest)


unionDFAState :: DFAState -> DFAState -> DFAState
unionDFAState s1 s2 = Set.union s1 s2

singletonDFAState :: DFAStateEntry -> DFAState
singletonDFAState x = Set.singleton x


-- fst element is a list of class action transition, the snd possible element is for EndInput test
type DetChoice = ([ (ClassInterval, DFAState) ], Maybe DFAState)

emptyDetChoice :: DetChoice
emptyDetChoice = ([], Nothing)

insertDetChoice :: SourceCfg -> InputHeadCondition -> ClosureMove -> DetChoice -> DetChoice
insertDetChoice src ih cm d =
  let (classChoice, endChoice) = d
      q = singletonDFAState (DFAStateEntry src cm)
  in
  case ih of
    EndInput ->
      let endChoice' =
            case endChoice of
              Nothing -> Just q
              Just qs -> Just (unionDFAState q qs)
      in (classChoice, endChoice')
    HeadInput x ->
      let classChoice' = insertItvInOrderedList (x, q) classChoice unionDFAState
      in (classChoice', endChoice)


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



-- this function takes a tree representing a set of choices and
-- convert it to a Input factored deterministic transition.
determinizeMove :: SourceCfg -> ClosureMoveSet -> Result DetChoice
determinizeMove src tc =
  determinizeWithAccu tc Nothing emptyDetChoice

  where
    determinizeWithAccu :: ClosureMoveSet -> Maybe SlkInput -> DetChoice -> Result DetChoice
    determinizeWithAccu lst minp acc =
      case lst of
        [] -> Result acc
        t : ms ->
          let
            cfg = closureCfg t
            (_pos, act, _q) = moveCfg t
            resAcc =
              case getClassActOrEnd act of
                Left (Left c) ->
                  let res = classToInterval c in
                    case res of
                      Abort AbortClassIsDynamic -> coerceAbort res
                      Abort (AbortClassNotHandledYet _) -> coerceAbort res
                      Result r ->
                        Result $ insertDetChoice src (HeadInput r) t acc

                      _ -> error "Impossible abort"
                Left (Right (IGetByte _)) ->
                  Result $ insertDetChoice src (HeadInput (ClassBtw (CValue 0) (CValue 255))) t acc
                Right IEnd -> Result $ insertDetChoice src EndInput t acc
                _ -> error "Impossible abort"
          in
            if compatibleInput (cfgInput cfg) minp
            then
              case resAcc of
                Abort AbortClassIsDynamic -> coerceAbort resAcc
                Abort (AbortClassNotHandledYet _) -> coerceAbort resAcc
                Result newAcc -> determinizeWithAccu ms (Just $ cfgInput cfg) newAcc
                _ -> error "impossible abort"
            else
              Abort AbortIncompatibleInput

    compatibleInput :: SlkInput -> Maybe SlkInput -> Bool
    compatibleInput inp minp =
      case minp of
        Nothing -> True
        Just inp2 -> if compare inp inp2 == EQ
                     then True
                     else False



deterministicCfgDet :: Aut a => a -> CfgDet -> Result DetChoice
deterministicCfgDet aut cfg =
  let res = closureLL aut cfg in
  case res of
    Abort AbortOverflowMaxDepth -> coerceAbort res
    Abort AbortLoopWithNonClass -> coerceAbort res
    Abort AbortAcceptingPath -> coerceAbort res
    Abort (AbortNonClassInputAction _) -> coerceAbort res
    Abort AbortUnhandledAction -> coerceAbort res
    Abort AbortSymbolicExec -> coerceAbort res
    Result r -> determinizeMove cfg r
    _ -> error "Impossible abort"


newtype DFAStateQuotient = DFAQuo { dfaQuo :: Set.Set CfgDet }
  deriving Show

mkDFAStateQuotient :: State -> DFAStateQuotient
mkDFAStateQuotient q =
    DFAQuo (Set.singleton (initCfgDet q))

equivDFAStateQuotient :: DFAStateQuotient -> DFAStateQuotient -> Bool
equivDFAStateQuotient q1 q2 = dfaQuo q1 == dfaQuo q2

instance Eq DFAStateQuotient where
  (==) q1 q2 = equivDFAStateQuotient q1 q2

instance Ord DFAStateQuotient where
  compare q1 q2 =
    compare (dfaQuo q1) (dfaQuo q2)

emptyDFAStateQuotient :: DFAStateQuotient
emptyDFAStateQuotient = DFAQuo Set.empty

nullDFAStateQuotient :: DFAStateQuotient -> Bool
nullDFAStateQuotient q =
  Set.null (dfaQuo q)

addDFAStateQuotient :: CfgDet -> DFAStateQuotient -> DFAStateQuotient
addDFAStateQuotient cfg q =
  DFAQuo $ Set.insert cfg (dfaQuo q)

measureDFAStateQuotient :: DFAStateQuotient -> Int
measureDFAStateQuotient s =
  helper s 0
  where
    helper qq r =
      case iterDFAStateQuotient qq of
        Nothing -> r
        Just (cfg, qs) ->
          let iCtrl = lengthSymbolicStack (cfgCtrl cfg)
              iSem = lengthSymbolicStack (cfgSem cfg)
              newR = max iSem (max iCtrl r)
          in helper qs newR

convertDFAStateToQuotient :: InputHeadCondition -> DFAState -> DFAStateQuotient
convertDFAStateToQuotient ih s =
  helper s
  where
    helper set =
      case iterDFAState set of
        Nothing -> emptyDFAStateQuotient
        Just (entry, es) ->
          let closCfg = closureCfg $ dstDFAState entry
              (_,act,q) = moveCfg $ dstDFAState entry
          in
          let mCfg = moveCfgDetFromPrev ih closCfg act q in
            case mCfg of
              Nothing -> helper es
              Just newCfg -> addDFAStateQuotient newCfg (helper es)


iterDFAStateQuotient :: DFAStateQuotient -> Maybe (CfgDet, DFAStateQuotient)
iterDFAStateQuotient s =
  let lst = Set.toAscList (dfaQuo s) in
    case lst of
      [] -> Nothing
      x:xs -> Just (x, DFAQuo (Set.fromAscList xs))


determinizeDFAStateQuotient :: Aut a => a -> DFAStateQuotient -> Result DetChoice
determinizeDFAStateQuotient aut s =
  determinizeAcc s emptyDetChoice
  where
    determinizeAcc :: DFAStateQuotient -> DetChoice -> Result DetChoice
    determinizeAcc states acc =
      case iterDFAStateQuotient states of
        Nothing -> Result acc
        Just (cfg, rest) ->
          let r = deterministicCfgDet aut cfg in
          case r of
            Abort AbortOverflowMaxDepth -> coerceAbort r
            Abort AbortLoopWithNonClass -> coerceAbort r
            Abort AbortAcceptingPath -> coerceAbort r
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

{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DeterminizeOneStep
  ( SourceCfg,
    DFAStateEntry(..),
    DFAState,
    iterDFAState,
    findAllEntryInDFAState,
    DetChoice,
    emptyDetChoice,
    insertDetChoice,
    unionDetChoice,
    deterministicStep,
  ) where


-- import Debug.Trace

import qualified Data.Set as Set

import qualified Daedalus.Interp as Interp

import Daedalus.Type.AST
import Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), InputAction(..), getClassActOrEnd, evalNoFunCall, isSimpleVExpr)
import Daedalus.ParserGen.Aut (Aut(..))
import Daedalus.ParserGen.ClassInterval
import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.CfgDet
import Daedalus.ParserGen.LL.Closure



type SourceCfg = CfgDet

data DFAStateEntry = DFAStateEntry
  { srcDFAState :: SourceCfg
  , dstDFAState :: CfgDet
  , moveDFAState :: (ChoicePos, Action, State)
  }
  deriving Show


compareSrc :: DFAStateEntry -> DFAStateEntry -> Ordering
compareSrc p1 p2 =
  compareCfgDet (srcDFAState p1) (srcDFAState p2)

compareDst :: DFAStateEntry -> DFAStateEntry -> Ordering
compareDst p1 p2 =
  compareCfgDet (dstDFAState p1) (dstDFAState p2)

compareDFAStateEntry :: DFAStateEntry -> DFAStateEntry -> Ordering
compareDFAStateEntry p1 p2 =
  case compareDst p1 p2 of
    LT -> LT
    GT -> GT
    EQ -> compareSrc p1 p2


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

insertDetChoice :: SourceCfg -> InputHeadCondition -> (CfgDet, (ChoicePos, Action, State)) -> DetChoice -> DetChoice
insertDetChoice src ih (cfg, (pos, act, q)) d =
  let (classChoice, endChoice) = d
      tr = singletonDFAState (DFAStateEntry src cfg (pos, act, q))
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



classToInterval :: PAST.NCExpr -> Result ClassInterval
classToInterval e =
  case texprValue e of
    TCSetAny -> Result $ ClassBtw MinusInfinity PlusInfinity
    TCSetSingle e1 ->
      if not (isSimpleVExpr e1)
      then Abort AbortClassIsDynamic
      else
        let v = evalNoFunCall e1 [] [] in
        case v of
          Interp.VUInt 8 x ->
            let vx = fromIntegral x
            in Result $ ClassBtw (CValue vx) (CValue vx)
          _                -> Abort (AbortClassNotHandledYet "SetSingle")
    TCSetRange e1 e2 ->
      if isSimpleVExpr e1 && isSimpleVExpr e2
      then
        let v1 = evalNoFunCall e1 [] []
            v2 = evalNoFunCall e2 [] []
        in case (v1, v2) of
             (Interp.VUInt 8 x, Interp.VUInt 8 y) ->
               let x1 = fromIntegral x
                   y1 = fromIntegral y
               in if x1 <= y1
                  then Result $ ClassBtw (CValue x1) (CValue y1)
                  else error ("SetRange values not ordered:" ++
                              show (toEnum (fromIntegral x1) :: Char) ++ " " ++
                              show (toEnum (fromIntegral y1) :: Char))
             _ -> Abort (AbortClassNotHandledYet "SetRange")
      else Abort AbortClassIsDynamic
    _ -> Abort (AbortClassNotHandledYet "other class case")


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
        t@(cfg, (_pos, act, _q)) : ms ->
          let resAcc =
                case getClassActOrEnd act of
                  Left c ->
                    let res = classToInterval c in
                      case res of
                        Abort AbortClassIsDynamic -> coerceAbort res
                        Abort (AbortClassNotHandledYet _) -> coerceAbort res
                        Result r ->
                          Result $ insertDetChoice src (HeadInput r) t acc

                        _ -> error "Impossible abort"
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



deterministicStep :: Aut a => a -> CfgDet -> Result DetChoice
deterministicStep aut cfg =
  let res = closureLL aut Set.empty cfg in
  case res of
    Abort AbortOverflowMaxDepth -> coerceAbort res
    Abort AbortLoopWithNonClass -> coerceAbort res
    Abort AbortAcceptingPath -> coerceAbort res
    Abort (AbortNonClassInputAction _) -> coerceAbort res
    Abort AbortUnhandledAction -> coerceAbort res
    Abort AbortSymbolicExec -> coerceAbort res
    Result r -> determinizeMove cfg r
    _ -> error "Impossible abort"

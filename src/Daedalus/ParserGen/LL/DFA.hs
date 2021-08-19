{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DFA
  ( LinDFAState
  , DFA(..)
  , DFATransition(..)
  , AmbiguityDetection(..)
  , lookupLinDFAState
  , createAbortDFA
  , createDFA
  , getFinalStates
  , showDFA
  , lookupDFA
  , DDA(..)
  , getFinalStatesDDA
  , computeHasFullResolutionDDA
  , lookaheadDepth
  , revAppend
  , printDFAtoGraphviz
  , extractAmbiguity
  )
where

-- import Debug.Trace

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)


import Daedalus.ParserGen.Aut (Aut(..))


import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.ParamLL
import Daedalus.ParserGen.LL.ClassInterval
import Daedalus.ParserGen.LL.SlkCfg
import qualified Daedalus.ParserGen.LL.Closure as Closure
import Daedalus.ParserGen.LL.DFAStep


data AmbiguityDetection =
    Ambiguous
  | NotAmbiguous
  | DunnoAmbiguous
  deriving (Eq)

instance Show AmbiguityDetection where
  show Ambiguous      = "Ambiguous"
  show NotAmbiguous   = "NotAmbiguous"
  show DunnoAmbiguous = "DunnoAmbiguous"



data LinDFAState = LinDFAState { linDFAState :: {-# UNPACK #-} !Int }

instance Eq LinDFAState where
  (==) q1 q2 = linDFAState q1 == linDFAState q2

instance Ord LinDFAState where
  compare q1 q2 =
    compare (linDFAState q1) (linDFAState q2)


data DFATransition =
    DFATransition
    { ambiguityTrans :: AmbiguityDetection
    , nextTrans :: [ (InputHeadCondition, DFARegistry, AmbiguityDetection, DFAState, LinDFAState) ]
    , acceptTrans :: Maybe (DFARegistry, AmbiguityDetection)
    }

ambiguityDetChoice :: DetChoice -> AmbiguityDetection
ambiguityDetChoice tr =
  case (classDetChoice tr, endDetChoice tr, acceptingDetChoice tr) of
    ([], Nothing, Just _) -> DunnoAmbiguous
    (_, _, Just _) -> Ambiguous
    ([], Nothing, Nothing) -> NotAmbiguous
    (_, _, Nothing) -> DunnoAmbiguous

data DFA = DFA
  { startDFA :: DFAState
  , startLinDFAState :: LinDFAState
  , transitionDFA :: Map.Map LinDFAState (Result DFATransition)
  , mappingDFAStateToLin :: Map.Map DFAState LinDFAState
  , mappingLinToDFAState :: Map.Map LinDFAState DFAState
  , finalLinDFAState :: Set.Set LinDFAState
  , lastLin :: LinDFAState
  , flagHasNoAbort :: Maybe Bool
  , flagHasFullResolution :: Maybe Bool
  }

dummyLinDFAState :: LinDFAState
dummyLinDFAState = LinDFAState (-1)

initLinDFAState :: LinDFAState
initLinDFAState = LinDFAState 1

nextLinDFAState :: LinDFAState -> LinDFAState
nextLinDFAState st = LinDFAState (linDFAState st + 1)

lastLinDFAState :: DFA -> Int
lastLinDFAState dfa =
  case lastLin dfa of
    LinDFAState n -> n

initDFA :: DFAState ->  DFA
initDFA q =
  DFA
  { startDFA = q
  , startLinDFAState = initLinDFAState
  , transitionDFA = Map.empty
  , mappingDFAStateToLin = Map.insert q initLinDFAState Map.empty
  , mappingLinToDFAState = Map.insert initLinDFAState q Map.empty
  , finalLinDFAState = Set.empty
  , lastLin = initLinDFAState
  , flagHasNoAbort = Nothing
  , flagHasFullResolution = Nothing
  }


lookupDFA :: DFAState -> DFA -> Maybe (Result DFATransition)
lookupDFA q dfa =
  Map.lookup (fromJust $ Map.lookup q (mappingDFAStateToLin dfa)) (transitionDFA dfa)

lookupLinDFAState :: LinDFAState -> DFA -> Maybe (Result DFATransition)
lookupLinDFAState q dfa =
  Map.lookup q (transitionDFA dfa)


insertDFA :: LinDFAState -> Result DFATransition -> DFA -> DFA
insertDFA qState rtr =
  (\ dfa ->
  case rtr of
    Abort _ ->
      dfa { transitionDFA = Map.insert qState rtr (transitionDFA dfa) }
    Result tr ->
      -- Invariant: the state qState is already member of state mappings.
      let am = ambiguityTrans tr in
      case am of
        Ambiguous ->
          dfa
          { transitionDFA = Map.insert qState rtr (transitionDFA dfa)
          }
        _ ->
          let (atr, dfa1) = allocTransition (nextTrans tr) [] dfa in
          dfa1
          { transitionDFA = Map.insert qState (Result $ DFATransition am atr (acceptTrans tr)) (transitionDFA dfa1)
          }
  )
  where
    allocTransition lst ch dfa1 =
      case lst of
        [] -> (reverse ch, dfa1)
        (ih, registry, am, q, dummy) : rest ->
          if (dummy /= dummyLinDFAState)
          then error "borken invariant"
          else
            let mapping1 = mappingDFAStateToLin dfa1 in
            if Map.member q mapping1
            then
              let qlin = fromJust $ Map.lookup q mapping1
                  finalStates =
                    let m = finalLinDFAState dfa1
                    in case am of
                         Ambiguous -> m
                         DunnoAmbiguous -> m
                         NotAmbiguous -> Set.insert qlin m
              in
              let newDfa = dfa1 { finalLinDFAState = finalStates }
              in
              allocTransition rest ((ih, registry, am, q, qlin) : ch) newDfa
            else
              let qlin = nextLinDFAState (lastLin dfa1)
                  m1 = Map.insert q qlin (mappingDFAStateToLin dfa1)
                  m2 = Map.insert qlin q (mappingLinToDFAState dfa1)
                  finalStates =
                    let m = finalLinDFAState dfa1
                    in case am of
                         Ambiguous -> m
                         DunnoAmbiguous -> m
                         NotAmbiguous -> Set.insert qlin m
              in
              let newDfa =
                    dfa1
                    { mappingDFAStateToLin = m1
                    , mappingLinToDFAState = m2
                    , finalLinDFAState = finalStates
                    , lastLin = qlin
                    }
              in allocTransition rest ((ih, registry, am, q, qlin) : ch) newDfa

getFinalStates :: DFA -> [DFAState]
getFinalStates dfa =
  map (\ qlin -> fromJust (Map.lookup qlin (mappingLinToDFAState dfa) ))
  (Set.toList (finalLinDFAState dfa))


showDFA :: DFA -> String
showDFA dfa =
  showTrans [] 0 (startLinDFAState dfa)
  where
    showTrans :: [LinDFAState] -> Int -> LinDFAState -> String
    showTrans vis d qq =
      if elem qq vis
      then "**** loop ****"
      else
      case lookupLinDFAState qq dfa of
        Nothing -> error "missing state"
        Just r ->
          case r of
            Result
              ( DFATransition
                { ambiguityTrans = am, nextTrans = lst, acceptTrans = accTr
                }
              ) ->
              case am of
                NotAmbiguous -> ""
                Ambiguous -> "Ambiguous(acceptingPath + next)"
                DunnoAmbiguous ->
                  "DTrans [\n" ++
                  concatMap (showAccept (d+2)) accTr ++
                  concatMap (showT (qq : vis) (d+2)) lst ++
                  space d ++ "]"
            _ -> abortToString r ++ "\n"

    showT vis d (i, s, am, _qq, ql) =
      space d ++ "( " ++ showSet s ++ "\n" ++
      space d ++ ", " ++ show i ++ "\n" ++
      space d ++ ", " ++ showDown am ++ "\n" ++
      space d ++ "),\n"
      where
        showDown amb =
          case amb of
            NotAmbiguous -> "Resolution (" ++ show amb ++ ")"
            Ambiguous -> "Resolution (" ++ show amb ++ ")"
            DunnoAmbiguous -> showTrans vis (d+2) ql

    showAccept d (reg, am) =
      space d ++ "( " ++ showSet reg ++ "\n" ++
      space d ++ ", " ++ "AcceptingPath" ++ "\n" ++
      space d ++ ", " ++ show am ++ "\n" ++
      space d ++ "),\n"

    showSet s =
      "[" ++
      foldr (\ entry b ->
                let alts = Closure.getAltSeq $ dstEntry entry
                in
                  "(" ++ show (length alts) ++
                  -- ",q" ++ showSlkCfg (Closure.lastCfg (dstEntry entry)) ++
                  ")," ++ b) "" s  ++ "]"

    space d = spaceHelper 0
       where spaceHelper cnt = if cnt < d then " " ++ spaceHelper (cnt+1) else ""



printDFAtoGraphviz :: Int -> DFA -> [ String ]
printDFAtoGraphviz llaState dfa =
  showStates (mappingLinToDFAState dfa) ++
  showTrans [] (startLinDFAState dfa)
  where
    stateToNode linState =
      "N_" ++ (show llaState) ++ "_" ++ (show $ linDFAState linState)

    showStates m =
      Map.foldrWithKey
      (\ linState qDFA acc ->
         if not (elem linState (finalLinDFAState dfa))
         then
           ( stateToNode linState ++
             " [shape=record, label=\"" ++ showGraphvizDFAState qDFA ++ "\"];"
           ) : acc
         else acc
      ) [] m

    showTrans :: [LinDFAState] -> LinDFAState -> [ String ]
    showTrans vis qq =
      if elem qq vis
      then []
      else
      case lookupLinDFAState qq dfa of
        Nothing -> error "missing state"
        Just r ->
          case r of
            Result
              ( DFATransition
                { ambiguityTrans = am
                , nextTrans = lst
                , acceptTrans = accTr
                }
              ) ->
              case am of
                NotAmbiguous ->
                  let
                    nodeResolution = stateToNode qq ++ show am
                  in
                  [ nodeResolution ++ " [shape=box,style=filled,color=\".5 .5 1.\"]" ++ ";"
                  , (stateToNode qq ++ " -> " ++ nodeResolution ++ " [arrowhead=odiamond]")
                  ]
                Ambiguous ->
                  let
                    nodeResolution = stateToNode qq ++ "Ambiguous_acceptingPath_next"
                  in
                  [ nodeResolution ++ " [shape=box,style=filled,color=\"1. 0.7 0.7\"]" ++ ";"
                  , (stateToNode qq ++ " -> " ++ nodeResolution ++ " [arrowhead=odiamond]")
                  ]
                DunnoAmbiguous ->
                  concatMap (showAccept qq) accTr ++
                  concatMap (showT (qq : vis) qq) lst
            _ ->
              let
                nodeResolution = stateToNode qq ++ abortToString r
              in
                [ nodeResolution ++ " [shape=box,style=filled,color=\".1 0.2 1.\"]" ++ ";"
                , (stateToNode qq ++ " -> " ++ nodeResolution ++ " [arrowhead=odiamond]")
                ]

    showT vis qq (i, s, am, _qq, ql) =
      (nodeqq ++ " -> " ++ nodeql ++
      "[fontsize = 20, fontname = courrier, label=\"" ++ showGraphvizInputHeadCondition i ++ "\"];") :
      showDown am
      where
        nodeqq = stateToNode qq
        nodeql = case am of
                   Ambiguous ->
                     stateToNode ql ++ "_" ++ "Reg" ++ show (length s) ++ "_" ++ show am
                   NotAmbiguous ->
                     stateToNode ql ++ "_" ++ "Reg" ++ show (length s) ++ "_" ++ show am
                   _ -> stateToNode ql
        showDown amb =
          case amb of
            NotAmbiguous ->
              let nodeResolution = stateToNode ql ++ show amb in
              [ nodeql ++ " [shape=record, label=\"" ++ showGraphvizRegistry s ++ "\"];"
              , nodeResolution ++ " [shape=box,style=filled,color=\".5 .5 1.0\"];"
              , (nodeql ++ " -> " ++ nodeResolution ++ "[style=dotted,arrowhead=odiamond]")
              ]
            Ambiguous ->
              let nodeResolution = stateToNode ql ++ show amb in
              [ nodeql ++ " [shape=record, label=\"" ++ showGraphvizRegistry s ++ "\"];"
              , nodeResolution ++ " [shape=box,style=filled,color=\"1. .7 .7\"];"
              , (nodeql ++ " -> " ++ nodeResolution ++ "[style=dotted, arrowhead=odiamond]")
              ]
            DunnoAmbiguous -> showTrans vis ql

    showAccept qq (reg, am) =
      let nodeResolution = stateToNode qq ++ "AcceptingPath" ++ show am
      in
      [ nodeResolution ++  " [shape=box,style=filled,color=\".7 .3 1.0\"]" ++ ";"
      , (stateToNode qq ++ " -> " ++ nodeResolution ++
         "[" ++
         " style=dotted, arrowhead=odiamond, " ++
         " label=\"" ++ showSet reg ++ "\"" ++
         "]")
       ]

    showGraphvizDFAState qq =
      let iterator = initIteratorDFAState qq in
      foldr (\ a b -> a ++ b) "" (List.intersperse " | " (translateCfgs iterator []))
      where
        translateCfgs it acc =
          case nextIteratorDFAState it of
            Nothing -> reverse acc
            Just (cfg, it2) ->
              translateCfgs it2 (showGraphvizSlkCfg cst_DEMO_MODE cfg : acc)

    showSet s =
      if cst_DEMO_MODE
      then ""
      else "Reg" ++ show (length s)

    showGraphvizRegistry r =
      let iterator = initIteratorDFARegistry r in
      foldr (\ a b -> a ++ b) "" (List.intersperse " | " (translateRegistry iterator []))
      where
        translateRegistry it acc =
          case nextIteratorDFARegistry it of
            Nothing -> reverse acc
            Just (entry, it2) ->
              let cfg =
                    let cm = dstEntry entry in
                    case cm of
                      Closure.ClosureMove {} -> Closure.closureCfg cm
                      Closure.ClosurePath {} -> Closure.lastCfg cm
                      Closure.ClosureAccepting {} -> Closure.closureCfg cm
              in
              translateRegistry it2 (showGraphvizSlkCfg cst_DEMO_MODE cfg : acc)


data DDA = DDA
  { startDDA :: DFAState
  , transitionDDA :: (Closure.DataDepInstr, [LinDFAState])
  , mappingLinToDFAStateDDA :: Map.Map LinDFAState DFAState
  }

mkDDA :: DFAState -> Closure.DataDepInstr -> DDA
mkDDA q instr =
  case instr of
    Closure.DDManyBetween _lst scfg1 scfg2 ->
      let
        cfg1 = mkDFAStateFromSlkCfg scfg1
        cfg2 = mkDFAStateFromSlkCfg scfg2
        qLin1 = initLinDFAState
        qLin2 = nextLinDFAState initLinDFAState
        m = Map.insert qLin2 cfg2 (Map.insert qLin1 cfg1 Map.empty)
      in
      DDA
      { startDDA = q
      , transitionDDA = (instr, [qLin1, qLin2])
      , mappingLinToDFAStateDDA = m
      }
    Closure.DDSetStream _ scfg1 ->
      let
        cfg1 = mkDFAStateFromSlkCfg scfg1
        qLin1 = initLinDFAState
        m = Map.insert qLin1 cfg1 Map.empty
      in
      DDA
      { startDDA = q
      , transitionDDA = (instr, [qLin1])
      , mappingLinToDFAStateDDA = m
      }

getFinalStatesDDA :: DDA -> [DFAState]
getFinalStatesDDA dda =
  let (_, lst) = transitionDDA dda in
  map (\ x -> fromJust $ Map.lookup x (mappingLinToDFAStateDDA dda)) lst


lookaheadDepth :: DFA -> Int
lookaheadDepth dfa =
  let start = startLinDFAState dfa in
  helper [] start
  where
    helper :: [LinDFAState] -> LinDFAState -> Int
    helper vis qq =
      if elem qq vis
      then 0
      else
        case lookupLinDFAState qq dfa of
          Nothing -> 0
          Just rt ->
            case rt of
              Result (DFATransition { nextTrans = lst }) -> foldOverList vis qq lst 0
              _ -> 0

    foldOverList vis src lst acc =
      case lst of
        [] -> acc
        (_, _, _am, _, qq) : rest ->
          let i = helper (src:vis) qq
          in foldOverList vis src rest (max acc (i + 1))


getConflictSetsPerLoc :: DFARegistry -> [ [DFAEntry] ]
getConflictSetsPerLoc s =
  partitionDFARegistry s sameEntryPerLoc
  where
    sameEntryPerLoc
      (DFAEntry _src1 (Closure.ClosurePath _alts1 _ (_,_,_) dst1))
      (DFAEntry _src2 (Closure.ClosurePath _alts2 _ (_,_,_) dst2)) =
      dst1 == dst2
    sameEntryPerLoc
      (DFAEntry _src1 (Closure.ClosureAccepting _alts1 _dst1))
      (DFAEntry _src2 (Closure.ClosureAccepting _alts2 _dst2)) = True
    sameEntryPerLoc _ _ = error "broken invariant"

-- Inspired by the condition in `predictLL()` from ALL(*) paper
-- * `NotAmbiguous` when there is only one conflict set with only one possibility, or when the conflict set is empty
-- * `Ambiguous` if there is at least one conflict set with at least 2 possibilities
-- * `DunnoAmbiguous` if all the conflict sets have 1 possibility
analyzeConflicts :: DFARegistry -> (AmbiguityDetection, Maybe [DFAEntry])
analyzeConflicts ts =
  let conflictSets = getConflictSetsPerLoc ts
  in case conflictSets of
       [] -> (NotAmbiguous, Nothing)
       [ [] ] -> error "empty list"
       [ lst ] ->
         if length lst == 1
         then (NotAmbiguous, Nothing)
         else (Ambiguous, Just lst)
       lstLst -> isAnyAmbiguous lstLst
  where
    isAnyAmbiguous cs =
      case cs of
        [] -> (DunnoAmbiguous, Nothing)
        [] : _rest -> error "empty list"
        lst : rest ->
          if length lst > 1
          then (Ambiguous, Just lst)
          else isAnyAmbiguous rest

revAppend :: [a] -> [a] -> [a]
revAppend [] ys = ys
revAppend (x:xs) ys = revAppend xs (x:ys)


detChoiceToList :: DetChoice -> [(InputHeadCondition, DFARegistry)]
detChoiceToList detChoice =
  let tr = map (\ (i,t) -> (HeadInput (ByteCondition [i]), t)) (classDetChoice detChoice) in
    case endDetChoice detChoice of
      Nothing -> tr
      Just t -> tr ++ [(EndInput, t)]

mapAnalyzeConflicts :: DetChoice -> DFATransition
mapAnalyzeConflicts dc =
  let amall = ambiguityDetChoice dc in
  let tr = detChoiceToList dc in
  let acceptingTr =
        case acceptingDetChoice dc of
          Nothing -> Nothing
          Just reg -> fconvertAccepting reg
  in
    let nextTr = map fconvert tr
    in
      DFATransition
      { ambiguityTrans = amall
      , acceptTrans = acceptingTr
      , nextTrans = nextTr
      }

  where
    fconvert (ihc, reg) =
      let newCfg = convertDFARegistryToDFAState reg
          (am, _) = analyzeConflicts reg
      in (ihc, reg, am, newCfg, dummyLinDFAState)

    fconvertAccepting reg =
      let (am, _) = analyzeConflicts reg
      in Just (reg, am)


createAbortDFA :: DFAState -> Result DFATransition -> DFA
createAbortDFA qInit r =
  let idfa = initDFA qInit in
  let dfa =insertDFA (startLinDFAState idfa) r idfa in
  let dfa1 = computeHasFullResolution dfa in
  let dfa2 = computeHasNoAbort dfa1 in
  dfa2

createDFA ::
  Aut a =>
  a -> DFAState ->
  HTable -> (Either DFA DDA, HTable)
createDFA aut qInit tab =
  -- It starts attempting `closureDataDependentOnDFAState` and if not
  -- successful then it attempts the determinization with `detSubset`
  let c = closureDataDependentOnDFAState aut qInit tab in
  case c of
    (Result (Just t), tab2) ->
      (Right (mkDDA qInit t), tab2)
    (_, _) ->
      let idfa = initDFA qInit in
      let (dfa, tab2) = go [(startLinDFAState idfa, 0)] [] idfa tab in
      let dfa1 = computeHasFullResolution dfa in
      let dfa2 = computeHasNoAbort dfa1 in
      (Left dfa2, tab2)
  where
    go ::
      [ (LinDFAState, Int) ] -> [ (LinDFAState, Int) ] -> DFA ->
      HTable -> (DFA, HTable)
    go toVisit accToVisit dfa localTab =
      case toVisit of
        [] -> case accToVisit of
                [] -> (dfa, localTab)
                _ -> go (reverse accToVisit) [] dfa localTab
        (q, depth) : rest ->
          case lookupLinDFAState q dfa of
            Nothing ->
              let qDFA = fromJust $ Map.lookup q (mappingLinToDFAState dfa) in
              if (measureDFAState qDFA > cst_OVERFLOW_CFG ||
                  depth > cst_MAX_LOOKAHEAD_DEPTH ||
                 lastLinDFAState dfa > cst_MAX_DFA_NB_STATES)
              then
                let
                  newDfa =
                    if measureDFAState qDFA > cst_OVERFLOW_CFG
                    then insertDFA q (Abort AbortDFAOverflowCfg) dfa
                    else
                    if depth > cst_MAX_LOOKAHEAD_DEPTH
                    then insertDFA q (Abort AbortDFAOverflowLookahead) dfa
                    else insertDFA q (Abort AbortDFAOverflowNbStates) dfa
                in go rest accToVisit newDfa localTab
              else
                let (choices, tab1) = detSubset qDFA localTab in
                let newDfa = insertDFA q choices dfa in
                let allocatedChoice = fromJust $ lookupLinDFAState q newDfa
                in
                case allocatedChoice of
                  Result (DFATransition am r1 _) ->
                    let
                      newToVisit =
                        if am /= Ambiguous then collectVisit (depth+1) r1 else []
                      newAccToVisit = revAppend newToVisit accToVisit
                    in go rest newAccToVisit newDfa tab1
                  _ -> go rest accToVisit newDfa tab1
            Just _ -> -- trace ("********FOUND*****" ++ "\n" ++ show q) $
              go rest accToVisit dfa localTab

    collectVisit ::
      Int ->
      [(InputHeadCondition, DFARegistry, AmbiguityDetection, DFAState, LinDFAState)] ->
      [(LinDFAState, Int)]
    collectVisit depth lst =
      foldr
      (\ (_, _, am, _qq, q) vis ->
          if q == dummyLinDFAState
          then error "should not be dummy state"
          else
            case am of
              Ambiguous -> vis
              NotAmbiguous -> vis
              DunnoAmbiguous -> (q, depth) : vis
      )
      []
      lst

    detSubset ::
      DFAState ->
      HTable -> (Result DFATransition, HTable)
    detSubset s localTab =
      let (r, tab1) = determinizeDFAState aut s localTab in
      let
        rFinalized =
          case r of
            Abort AbortSlkCfgExecution -> coerceAbort r
            Abort AbortSlkCfgClassIsDynamic -> coerceAbort r
            Abort (AbortSlkCfgClassNotHandledYet _) -> coerceAbort r
            Abort AbortClosureOverflowMaxDepth -> coerceAbort r
            Abort AbortClosureInfiniteloop -> coerceAbort r
            Abort AbortClosureUnhandledInputAction -> coerceAbort r
            Abort AbortClosureUnhandledAction -> coerceAbort r
            Abort AbortDFAIncompatibleInput -> coerceAbort r
            Result r1 ->
              let r2 = mapAnalyzeConflicts r1 in
                Result r2
            _ -> error "cannot be this abort"
      in (rFinalized, tab1)



computeHasFullResolution :: DFA -> DFA
computeHasFullResolution dfa =
  let start = startLinDFAState dfa in
  dfa { flagHasFullResolution = Just $ traverseWithVisited [] start }
  where
    traverseWithVisited visited q =
      if elem q visited
      then True
      else
        case lookupLinDFAState q dfa of
          Nothing -> error "broken invariant"
          Just r ->
            case r of
              Result (DFATransition { ambiguityTrans = NotAmbiguous, nextTrans = lst, acceptTrans = acceptTr}) ->
                if (not (null lst) || not (isNothing acceptTr))
                then error "broken invariant"
                else True
              Result (DFATransition { ambiguityTrans = DunnoAmbiguous, nextTrans = lst, acceptTrans = Just (_, am)}) ->
                if not (null lst)
                then error "broken invariant"
                else
                case am of
                  NotAmbiguous -> True
                  Ambiguous -> False
                  DunnoAmbiguous -> error "broken invariant"
              Result (DFATransition { ambiguityTrans = DunnoAmbiguous, nextTrans = lst, acceptTrans = Nothing}) -> helper (q : visited) lst
              Result (DFATransition {ambiguityTrans = Ambiguous}) -> False
              Abort _ -> False

    helper visited lst =
      case lst of
        [] -> True
        (_, _, am, _, q) : rest ->
          case am of
            NotAmbiguous -> helper visited rest
            Ambiguous -> False
            DunnoAmbiguous -> traverseWithVisited visited q && helper visited rest

computeHasNoAbort :: DFA -> DFA
computeHasNoAbort dfa =
  let start = startLinDFAState dfa in
  dfa { flagHasNoAbort = Just $ traverseWithVisited [] start }
  where
    traverseWithVisited visited q =
      if elem q visited
      then True
      else
        case lookupLinDFAState q dfa of
          Nothing -> error "broken invariant"
          Just r ->
            case r of
              Result (DFATransition { ambiguityTrans = NotAmbiguous, nextTrans = lst, acceptTrans = acceptTr}) ->
                if (not (null lst) || not (isNothing acceptTr))
                then error "broken invariant"
                else True
              Result (DFATransition { ambiguityTrans = DunnoAmbiguous, nextTrans = lst, acceptTrans = Just (_, am)}) ->
                if not (null lst)
                then error "broken invariant"
                else
                case am of
                  NotAmbiguous -> True
                  Ambiguous -> False
                  DunnoAmbiguous -> error "broken invariant"
              Result (DFATransition { ambiguityTrans = DunnoAmbiguous, nextTrans = lst, acceptTrans = Nothing}) -> helper (q : visited) lst
              Result (DFATransition {ambiguityTrans = Ambiguous}) -> False
              Abort _ -> False

    helper visited lst =
      case lst of
        [] -> True
        (_, _, am, _, q) : rest ->
          case am of
            NotAmbiguous -> helper visited rest
            Ambiguous -> helper visited rest
            DunnoAmbiguous -> traverseWithVisited visited q && helper visited rest

computeHasFullResolutionDDA :: DDA -> Bool
computeHasFullResolutionDDA _dda = True

extractAmbiguity :: DFA -> Maybe ([InputHeadCondition], [DFAEntry])
extractAmbiguity dfa =
  let start = startLinDFAState dfa in
  traverseWithVisited [] start
  where
    traverseWithVisited visited q =
      if elem q visited
      then Nothing
      else
        case lookupLinDFAState q dfa of
          Nothing -> error "broken invariant"
          Just r ->
            case r of
              Result (DFATransition { ambiguityTrans = NotAmbiguous, nextTrans = lst, acceptTrans = acceptTr}) ->
                if (not (null lst) || not (isNothing acceptTr))
                then error "broken invariant"
                else Nothing
              Result (DFATransition { ambiguityTrans = DunnoAmbiguous, nextTrans = lst, acceptTrans = Just (_, am)}) ->
                if not (null lst)
                then error "broken invariant"
                else
                case am of
                  NotAmbiguous -> Nothing
                  Ambiguous -> Just ([], [])
                  DunnoAmbiguous -> error "broken invariant"
              Result (DFATransition { ambiguityTrans = DunnoAmbiguous, nextTrans = lst, acceptTrans = Nothing}) ->
                helper (q : visited) lst
              Result (DFATransition {ambiguityTrans = Ambiguous}) -> Just ([], [])
              Abort _ -> Nothing

    helper visited lst =
      case lst of
        [] -> Nothing
        (i, reg, am, _, q) : rest ->
          case am of
            NotAmbiguous -> helper visited rest
            Ambiguous ->
              case analyzeConflicts reg of
                (Ambiguous, conflicts) -> Just ([i], fromJust conflicts)
                _ -> error "Weird"
            DunnoAmbiguous ->
              case traverseWithVisited visited q of
                Nothing -> helper visited rest
                Just (r, conflicts) -> Just ((i : r), conflicts)

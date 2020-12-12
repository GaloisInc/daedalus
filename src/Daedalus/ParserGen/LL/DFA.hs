{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DFA
  ( createLLA
  , statsLLA
  , SynthLLAState
  , LLA(..)
  , Prediction
  , destrPrediction
  , predictLL
  )
where

-- import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)


import qualified RTS.Input as Input

import Daedalus.ParserGen.Action (State, isInputAction, isActivateFrameAction)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..), stateToString)


import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.CfgDet
import Daedalus.ParserGen.LL.Closure
import Daedalus.ParserGen.LL.DeterminizeOneStep



data AmbiguityDetection =
    Ambiguous
  | NotAmbiguous
  | DunnoAmbiguous

instance Show AmbiguityDetection where
  show Ambiguous      = "Ambiguous"
  show NotAmbiguous   = "NotAmbiguous"
  show DunnoAmbiguous = "DunnoAmbiguous"



newtype LinDFAState = LinDFAState { linDFAState :: Int }

instance Eq LinDFAState where
  (==) q1 q2 = linDFAState q1 == linDFAState q2

instance Ord LinDFAState where
  compare q1 q2 =
    compare (linDFAState q1) (linDFAState q2)


newtype DFATransition =
  DFATransition [ (InputHeadCondition, DFARegistry, AmbiguityDetection, DFAState, LinDFAState) ]

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
    Result (DFATransition tr) ->
      -- Invariant: the state qState is already member of state mappings.
      let (atr, dfa1) = allocTransition tr [] dfa in
        dfa1
        { transitionDFA = Map.insert qState (Result $ DFATransition atr) (transitionDFA dfa1)
        }
  )
  where
    allocTransition lst ch dfa1 =
      case lst of
        [] -> (ch, dfa1)
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
            Result (DFATransition lst) ->
              "DTrans [\n" ++
              concatMap (showT (qq : vis) (d+2)) lst ++
              space d ++ "]"
            _ -> abortToString r ++ "\n"

    showT vis d (i, s, am, _qq, ql) =
      space d ++ "( " ++ showSet s ++ "\n" ++
      space d ++ ", " ++ show i ++ "\n" ++
      space d ++ ", " ++ showDown am ++ "\n" ++
      -- space d ++ ", " ++ showCfgDet qq ++ "\n" ++
      space d ++ "),\n"
      where
         showDown amb =
           case amb of
             NotAmbiguous -> "Resolution (" ++ show amb ++ ")"
             Ambiguous -> "Resolution (" ++ show amb ++ ")"
             DunnoAmbiguous -> showTrans vis (d+2) ql

    showSet s = "[" ++ foldr (\ entry b ->
                                let alts = altSeq $ dstEntry entry in
                                 "(" ++ show (length alts) ++
                                 -- ",q" ++ show q ++
                                 ")," ++ b) "" s  ++ "]"
    space d = spaceHelper 0
       where spaceHelper cnt = if cnt < d then " " ++ spaceHelper (cnt+1) else ""

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
              Result (DFATransition lst) -> foldOverList vis qq lst 0
              _ -> 0

    foldOverList vis src lst acc =
      case lst of
        [] -> acc
        (_, _, _am, _, qq) : rest ->
          let i = helper (src:vis) qq
          in foldOverList vis src rest (max acc (i + 1))


maxDepthDet :: Int
maxDepthDet = 20

detChoiceToList :: DetChoice -> [(InputHeadCondition, DFARegistry)]
detChoiceToList (c,e) =
  let tr = map (\ (i,t) -> (HeadInput i, t)) c in
  case e of
    Nothing -> tr
    Just t -> tr ++ [(EndInput, t)]


getConflictSetsPerLoc :: DFARegistry -> [ [DFAEntry] ]
getConflictSetsPerLoc s =
  case iterDFARegistry s of
    Nothing -> []
    Just (e, es) ->
      case findAllEntryInDFARegistry es (sameEntryPerLoc e) of
        (lst, rs) ->
          let lstLst = getConflictSetsPerLoc rs
          in  (e : lst) : lstLst

  where
    sameEntryPerLoc
      (DFAEntry _src1 (ClosureMove _alts1 dst1 (_,_,q1)))
      (DFAEntry _src2 (ClosureMove _alts2 dst2 (_,_,q2))) =
      dst1 == dst2 && q1 == q2

-- Inspired by the condition in `predictLL()` of ALL(*) paper
-- * `NotAmbiguous` when there is only one conflict set with only one possibility
-- * `Ambiguous` if there is at least one conflict set with at least 2 possibilities
-- * `DunnoAmbiguous` if all the conflict sets have 1 posibility
analyzeConflicts :: DFARegistry -> AmbiguityDetection
analyzeConflicts ts =
  let conflictSets = getConflictSetsPerLoc ts
  in case conflictSets of
       [] -> error "empty DFARegistry"
       [ [] ] -> error "empty list"
       [ lst ] ->
         if length lst == 1
         then NotAmbiguous
         else Ambiguous
       lstLst -> isAnyAmbiguous lstLst
  where
    isAnyAmbiguous cs =
      case cs of
        [] -> DunnoAmbiguous
        [] : _rest -> error "empty list"
        lst : rest ->
          if length lst > 1
          then Ambiguous
          else isAnyAmbiguous rest

revAppend :: [a] -> [a] -> [a]
revAppend [] ys = ys
revAppend (x:xs) ys = revAppend xs (x:ys)



oVERFLOW_CFG :: Int
oVERFLOW_CFG = 10

createDFAtable :: Aut a => a -> DFAState -> DFA
createDFAtable aut qInit =
  let idfa = initDFA qInit in
  if measureDFAState qInit > oVERFLOW_CFG
  then insertDFA (startLinDFAState idfa) (Abort AbortOverflowCfg) idfa
  else
    let resDfa = go [(startLinDFAState idfa, 0)] [] idfa
    in resDfa
  where
    go :: [ (LinDFAState, Int) ] -> [ (LinDFAState, Int) ] -> DFA -> DFA
    go toVisit accToVisit dfa =
      case toVisit of
        [] -> case accToVisit of
                [] -> dfa
                _ -> go (reverse accToVisit) [] dfa
        (q, depth) : rest ->
          case lookupLinDFAState q dfa of
            Nothing ->
              if depth > maxDepthDet
              then
                let newDfa = insertDFA q (Abort AbortOverflowK) dfa
                in go rest accToVisit newDfa
              else
                let choices = detSubset (fromJust $ Map.lookup q (mappingLinToDFAState dfa)) in
                let newDfa = insertDFA q choices dfa in
                let allocatedChoice = fromJust $ lookupLinDFAState q newDfa
                in
                  case allocatedChoice of
                    Result (DFATransition r1) ->
                      let
                        newToVisit = collectVisit (depth+1) r1
                        newAccToVisit = revAppend newToVisit accToVisit
                      in go rest newAccToVisit newDfa
                    _ -> go rest accToVisit newDfa
            Just _ -> -- trace ("********FOUND*****" ++ "\n" ++ show q) $
              go rest accToVisit dfa

    collectVisit :: Int -> [(InputHeadCondition, DFARegistry, AmbiguityDetection, DFAState, LinDFAState)] -> [(LinDFAState, Int)]
    collectVisit depth lst =
      foldr (\ (_, _, am, _qq, q) vis ->
               if q == dummyLinDFAState
               then error "should not be dummy state"
               else
                 case am of
                   Ambiguous -> vis
                   NotAmbiguous -> vis
                   DunnoAmbiguous -> (q, depth) : vis) [] lst

    detSubset :: DFAState -> Result DFATransition
    detSubset s =
      let r = determinizeDFAState aut s in
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
          Result (DFATransition (mapAnalyzeConflicts r1))
        _ -> error "cannot be this abort"

    mapAnalyzeConflicts :: DetChoice ->
                           [(InputHeadCondition, DFARegistry, AmbiguityDetection, DFAState, LinDFAState)]
    mapAnalyzeConflicts dc =
      let lst = detChoiceToList dc in
      map fconvert lst
      where
        fconvert (ihc, s) =
          let newCfg = convertDFARegistryToDFAState ihc s
              am = analyzeConflicts s
          in (ihc, s, am, newCfg, dummyLinDFAState)




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
              Result (DFATransition lst) -> helper (q : visited) lst
              _ -> False

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
              Result (DFATransition lst) -> helper (q : visited) lst
              _ -> False

    helper visited lst =
      case lst of
        [] -> True
        (_, _, am, _, q) : rest ->
          case am of
            NotAmbiguous -> helper visited rest
            Ambiguous -> helper visited rest
            DunnoAmbiguous -> traverseWithVisited visited q && helper visited rest


-- LLA: Lockstep Lookadead Automaton
-- LLA: LL(*) Automaton
-- LLA: lalalalala

newtype SynthLLAState = SynthLLAState { synthLLAState :: Int }

instance Eq SynthLLAState where
  (==) q1 q2 = synthLLAState q1 == synthLLAState q2

instance Ord SynthLLAState where
  compare q1 q2 =
    compare (synthLLAState q1) (synthLLAState q2)

instance Show SynthLLAState where
  show (SynthLLAState q) = "l" ++ show q

initSynthLLAState :: SynthLLAState
initSynthLLAState = SynthLLAState 0


type MapFinalToSynthLLAState = Map.Map LinDFAState SynthLLAState

data LLA = LLA
  { transitionLLA :: Map.Map SynthLLAState DFA
  , mappingFinalToSynth :: Map.Map SynthLLAState MapFinalToSynthLLAState
  -- mapping from the Accepting states of the DFA to the SynthLLAState
  , mappingNFAToSynth :: IntMap.IntMap SynthLLAState
  -- mapping from the NFA states as init DFAState to SynthLLAState
  , mappingDFAStateToSynth :: Map.Map DFAState SynthLLAState
  , mappingSynthToDFAState :: Map.Map SynthLLAState DFAState
  , lastSynth :: SynthLLAState
  }

emptyLLA :: LLA
emptyLLA =
  LLA
  { transitionLLA = Map.empty
  , mappingFinalToSynth = Map.empty
  , mappingNFAToSynth = IntMap.empty
  , mappingDFAStateToSynth = Map.empty
  , mappingSynthToDFAState = Map.empty
  , lastSynth = initSynthLLAState
  }

nextSynthLLAState :: SynthLLAState -> SynthLLAState
nextSynthLLAState (SynthLLAState q) = SynthLLAState (q+1)

lookupLLAFromState :: State -> LLA -> Maybe (DFA, MapFinalToSynthLLAState)
lookupLLAFromState q aut =
  let mSynth = IntMap.lookup q (mappingNFAToSynth aut)
  in case mSynth of
       Nothing -> Nothing
       Just sq ->
         Just
         ( fromJust $ Map.lookup sq (transitionLLA aut)
         , fromJust $ Map.lookup sq (mappingFinalToSynth aut)
         )

lookupLLAFromSynth :: SynthLLAState -> LLA -> (DFA, MapFinalToSynthLLAState)
lookupLLAFromSynth q aut =
  let dfa = fromJust $ Map.lookup q (transitionLLA aut)
      finalMapping = fromJust $ Map.lookup q (mappingFinalToSynth aut)
  in (dfa, finalMapping)



memberLLA :: DFAState -> LLA -> Bool
memberLLA q aut =
  if Map.member q (mappingDFAStateToSynth aut)
  then
    let qSynth = fromJust $ Map.lookup q (mappingDFAStateToSynth aut)
    in Map.member qSynth (transitionLLA aut)
  else False

insertLLA :: DFAState -> DFA -> LLA -> LLA
insertLLA q dfa =
  (\ aut ->
  if Map.member q (mappingDFAStateToSynth aut)
  then
    let qSynth = fromJust $ Map.lookup q (mappingDFAStateToSynth aut) in
    if Map.member qSynth (transitionLLA aut)
    then error "broken invariant"
    else allocFinalAndTransition qSynth aut
  else
    let aut1 = addToMappings q aut in
    let qSynth = fromJust $ Map.lookup q (mappingDFAStateToSynth aut1) in
    allocFinalAndTransition qSynth aut1
  )

  where
    allocFinalAndTransition :: SynthLLAState -> LLA -> LLA
    allocFinalAndTransition qSynth aut1 =
      let (finalMapping, aut2) = allocFinal (Set.toList (finalLinDFAState dfa)) (Map.empty, aut1) in
      if (Map.size finalMapping) /= (Set.size (finalLinDFAState dfa))
      then error "broken invariant"
      else
        aut2
        { transitionLLA = Map.insert qSynth dfa (transitionLLA aut1)
        , mappingFinalToSynth = Map.insert qSynth finalMapping (mappingFinalToSynth aut1)
        }

    addToMappings qDFA aut1 =
      if Map.member qDFA (mappingDFAStateToSynth aut1)
      then aut1
      else
        let newSynth = nextSynthLLAState (lastSynth aut1) in
        let newMappingNFAToSynth =
              case isDFAStateInit qDFA of
                Nothing -> mappingNFAToSynth aut1
                Just qNFA -> IntMap.insert qNFA newSynth (mappingNFAToSynth aut1)
            newMappingDFAStateToSynth =
              Map.insert qDFA newSynth (mappingDFAStateToSynth aut1)
            newMappingSynthToDFAState =
              Map.insert newSynth qDFA (mappingSynthToDFAState aut1)
        in
          aut1
          { mappingNFAToSynth = newMappingNFAToSynth
          , mappingDFAStateToSynth = newMappingDFAStateToSynth
          , mappingSynthToDFAState = newMappingSynthToDFAState
          , lastSynth = newSynth
          }

    allocFinal :: [LinDFAState] -> (MapFinalToSynthLLAState, LLA) -> (MapFinalToSynthLLAState, LLA)
    allocFinal lst (finalMapping, aut1) =
      case lst of
        [] -> (finalMapping, aut1)
        qLinDFA : qs ->
          let qDFA = fromJust $ Map.lookup qLinDFA (mappingLinToDFAState dfa) in
          let newAut = addToMappings qDFA aut1 in
          let newFinalMapping =
                let qSynth = fromJust $ Map.lookup qDFA (mappingDFAStateToSynth newAut)
                in Map.insert qLinDFA qSynth finalMapping
          in allocFinal qs (newFinalMapping, newAut)



type Prediction = ChoiceSeq

destrPrediction :: Prediction -> Maybe (ChoicePos, Prediction)
destrPrediction pdx =
  case pdx of
    Seq.Empty -> Nothing
    c Seq.:<| cs -> Just (c, cs)


-- TODO: Explain this in some document. This is basically the key of
-- the new faithful determinization.
predictDFA :: DFA -> Input.Input -> Maybe (Prediction, LinDFAState)
predictDFA dfa i =
  if not (fromJust $ flagHasFullResolution dfa)
  then Nothing
  else
    let
      startLin = startLinDFAState dfa
      mpath = findMatchingPath startLin i []
    in
      case mpath of
        Nothing -> Nothing
        Just (path, qFinal) -> Just (extractPrediction path, qFinal)
  where
    findMatchingPath :: LinDFAState -> Input.Input -> [DFARegistry] -> Maybe ([DFARegistry], LinDFAState)
    findMatchingPath qLin inp acc =
      let elm = lookupLinDFAState qLin dfa
      in case elm of
       Nothing -> error "broken invariant"
       Just r ->
         case r of
           Result (DFATransition lst) -> findMatchLeftToRight lst inp acc
           _ -> error "should not reach this line"

    findMatchLeftToRight lst inp acc =
      case lst of
        [] -> Nothing
        (c, registry, am, _qDFA, qArriv) : rest ->
          case matchInputHeadCondition c inp of
            Nothing -> findMatchLeftToRight rest inp acc
            Just inp1 ->
              let newAcc = registry : acc in
              case am of
                NotAmbiguous -> Just (newAcc, qArriv)
                Ambiguous -> error "broken invariant, only applied on fully resolved"
                DunnoAmbiguous -> findMatchingPath qArriv inp1 newAcc


    extractPrediction :: [DFARegistry] -> Prediction
    extractPrediction lst =
      case lst of
        [] -> undefined
        s : rest ->
          let (backCfg, pdx) = extractSinglePrediction s
          in walkBackward backCfg rest pdx

    walkBackward :: SourceCfg -> [DFARegistry] -> Prediction -> Prediction
    walkBackward src lst acc =
      case lst of
        [] -> acc
        s : rest ->
          let (backCfg, pdx) = extractPredictionFromDFARegistry src s
          in walkBackward backCfg rest ((Seq.><) pdx acc)

    extractSinglePrediction :: DFARegistry -> (SourceCfg, Prediction)
    extractSinglePrediction s =
      case iterDFARegistry s of
        Just (DFAEntry c1 (ClosureMove alts _c2 (pos, _, _)), rest) ->
          if not (null rest)
          then error "ambiguous prediction"
          else (c1, addChoiceSeq pos alts )
          -- NOTE: pos is appended because this is the last transition
        _ -> error "ambiguous prediction"

    extractPredictionFromDFARegistry :: SourceCfg -> DFARegistry -> (SourceCfg, Prediction)
    extractPredictionFromDFARegistry src s =
      case iterDFARegistry s of
        Nothing -> error "could not find src from previous cfg"
        Just (DFAEntry c1 (ClosureMove alts c2 (pos, _, q2)), others) ->
          if q2 == cfgState src && cfgCtrl c2 == cfgCtrl src
          then (c1, addChoiceSeq pos alts)
          else extractPredictionFromDFARegistry src others

predictLL :: Either State SynthLLAState -> LLA -> Input.Input -> Maybe (Prediction, SynthLLAState)
predictLL qq aut inp =
  case qq of
    Left q ->
      let mdfa = lookupLLAFromState q aut in
        case mdfa of
          Nothing -> Nothing
          Just (dfa, finalMapping) ->
            predictFromPDXA dfa finalMapping
    Right qSynth ->
      let (dfa, finalMapping) = lookupLLAFromSynth qSynth aut in
        predictFromPDXA dfa finalMapping

  where
    predictFromPDXA dfa finalMapping =
      let mp = predictDFA dfa inp in
        case mp of
          Nothing -> Nothing
          Just (pdx, linState) ->
            let finalSynth = Map.lookup linState finalMapping in
            Just (pdx, fromJust finalSynth)


createLLA :: Aut a => a -> LLA
createLLA aut =
  let collectedStates =
        identifyStartStates ()
        -- Set.singleton $ initialState aut
  in
    go (Set.toList (Set.map mkDFAState collectedStates)) [] emptyLLA
  where
    identifyStartStates :: () -> Set.Set State
    identifyStartStates () =
      let transitions = allTransitions aut in
      foldr (\ (q1, ch) b -> Set.union b (collectStatesOnFanout q1 ch)) (Set.empty) transitions

    collectStatesOnFanout q1 ch =
      let helper lst = foldr (\ a b -> let sa = collectOnSingleTransition q1 a in Set.union b sa) Set.empty lst in
      case ch of
        UniChoice (act, q2) -> collectOnSingleTransition q1 (act, q2)
        ParChoice lst -> helper lst
        SeqChoice lst _ -> helper lst

    collectOnSingleTransition q1 (act, q2) =
      if isInputAction act
      then Set.singleton q2
      else
        if isActivateFrameAction act
        then Set.singleton q1
        else Set.empty

    go :: [DFAState] -> [DFAState] -> LLA -> LLA
    go toVisit nextRound res =
      if synthLLAState (lastSynth res) > 1000000 then error "Stop" else
      -- trace (show (Map.size res)) $
      case toVisit of
        [] -> case nextRound of
                [] -> res
                _ -> go (reverse nextRound) [] res
        q : qs ->
          -- trace (show q) $
          if memberLLA q res
          then go qs nextRound res
          else
            let dfa = createDFAtable aut q
                dfa1 = computeHasFullResolution dfa
                dfa2 = computeHasNoAbort dfa1
                newRes = insertLLA q dfa2 res
                newNextRound =
                  let finalStates = getFinalStates dfa in
                    revAppend finalStates nextRound
            in go qs newNextRound newRes


showStartDFA :: Aut a => a -> DFAState -> String
showStartDFA aut q =
  case iterDFAState q of
    Nothing -> "SINK STATE"
    Just (cfg, qs) ->
      if nullDFAState qs
      then stateToString (cfgState cfg) aut ++
           " " ++ showCfgDet cfg
      else error "broken invariant"

printLLA :: Aut a => a -> LLA -> IO ()
printLLA aut dfas =
  let t = Map.toAscList (transitionLLA dfas)
      tAnnotated = map (\ (q, dfa) -> ((showStartDFA aut (fromJust $ Map.lookup q (mappingSynthToDFAState dfas)), dfa))) t
      tMapped = Map.fromList tAnnotated
      tOrdered = Map.assocs tMapped
  in if length t > 10000
     then do return ()
     else mapM_ (\ (ann, dfa) ->
                    -- if ( -- (lookaheadDepth dfa < 10) ||
                    --      (fromJust $ flagHasFullResolution dfa))
                    -- then
                    --   return ()
                    -- else
                      do
                        putStrLn $ ann
                        putStrLn $ showDFA dfa
                        putStrLn ""
                ) tOrdered


statsLLA :: Aut a => a -> LLA -> IO ()
statsLLA aut dfas =
  let t = map snd (Map.toAscList (transitionLLA dfas))
  in do printLLA aut dfas
        putStrLn "\nReport:"
        putStrLn $ getReport t initReport
        putStrLn $ "\nTotal nb states: " ++ show (length t)
  where
    getReport lst report =
      case lst of
        [] -> foldr (\ a b -> show a ++ "\n" ++ b) "" (Map.assocs report)
        dfa : xs ->
          getReport xs (incrReport report dfa)

    result str = "Result" ++ str

    initReport :: Map.Map String Int
    initReport = Map.fromAscList []

    mapResultToKey :: DFA -> String
    mapResultToKey dfa =
      let r = fromJust (lookupDFA (startDFA dfa) dfa)
      in
      case r of
        Abort AbortAmbiguous -> result "-ambiguous-0"
        Abort AbortOverflowK -> abortToString r
        Result _t ->
          let k = lookaheadDepth dfa
              res
                | flagHasFullResolution dfa == Just True =
                    result ("-" ++ show k)
                | flagHasNoAbort dfa == Just True =
                    -- trace (show q) $
                    result ("-ambiguous-" ++ show k)
                | otherwise = "abort-" ++ show k
          in
          --if k == 1
          --then trace (show _t) $
               res
          --else res
        _ -> abortToString r


    incrReport :: Map.Map String Int -> DFA -> Map.Map String Int
    incrReport report r =
      let key = mapResultToKey r
      in
        Map.insertWith (\ a b -> a+b) key 1 report

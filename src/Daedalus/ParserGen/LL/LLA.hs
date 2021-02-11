{-# Language GADTs #-}

module Daedalus.ParserGen.LL.LLA
  ( createLLA
  , buildPipelineLLA
  , statsLLA
  , SynthLLAState
  , LLA(..)
  , llaToGraphviz
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
import qualified Data.List as List
import qualified Data.Array.IArray as Array
import System.IO

import qualified RTS.Input as Input

import Daedalus.ParserGen.Action (State, isInputAction, isActivateFrameAction)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..), stateToString, getMaxState)

import Daedalus.ParserGen.LL.Result
import Daedalus.ParserGen.LL.SlkCfg
import Daedalus.ParserGen.LL.Closure
import Daedalus.ParserGen.LL.DFAStep
import Daedalus.ParserGen.LL.DFA


-- LLA: Lockstep Lookahead Automaton
-- LLA: LL(*) Automaton
-- LLA: lalalalala
-- LLA: reverse of ALL(*)

data SynthLLAState = SynthLLAState { synthLLAState :: {-# UNPACK #-} !Int }

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
  { transitionLLA :: !(Map.Map SynthLLAState DFA)

    -- mapping from the Final states of the DFA to the SynthLLAState
  , mappingFinalToSynth :: Map.Map SynthLLAState MapFinalToSynthLLAState

    -- mapping from the NFA states to SynthLLAState
  , mappingNFAToSynth :: IntMap.IntMap SynthLLAState

  , mappingDFAStateToSynth :: Map.Map DFAState SynthLLAState
  , mappingSynthToDFAState :: Map.Map SynthLLAState DFAState
  , lastSynth :: SynthLLAState

    -- synthesized mapping of mappingNFAToSynth
  , synthesizedMappingNFAToSynth :: !(Array.Array Int (Maybe SynthLLAState))
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
  , synthesizedMappingNFAToSynth = Array.array (0,0) [ (0, Nothing) ]
  }

nextSynthLLAState :: SynthLLAState -> SynthLLAState
nextSynthLLAState (SynthLLAState q) = SynthLLAState (q+1)


lookupSynthArray :: State -> LLA -> Maybe SynthLLAState
lookupSynthArray q aut =
  {-# SCC breakpointLookupSynthArray #-}
  -- IntMap.lookup q (mappingNFAToSynth aut)
  let m = synthesizedMappingNFAToSynth aut in
    m Array.! q

lookupLLAFromState :: State -> LLA -> Maybe (DFA, MapFinalToSynthLLAState)
lookupLLAFromState q aut =
  let mSynth = lookupSynthArray q aut
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
predictDFA :: DFA -> Input.Input -> Maybe (Prediction, Maybe LinDFAState)
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
    findMatchingPath :: LinDFAState -> Input.Input -> [DFARegistry] -> Maybe ([DFARegistry], Maybe LinDFAState)
    findMatchingPath qLin inp acc =
      let elm = lookupLinDFAState qLin dfa
      in case elm of
       Nothing -> error "broken invariant"
       Just r ->
         case r of
           Result (DFATransition { ambiguityTrans = am, nextTrans = lst, acceptTrans = accTr}) ->
             case am of
               Ambiguous -> error "broken invariant"
               NotAmbiguous -> error "broken invariant: machine blocked"
               DunnoAmbiguous ->
                 case accTr of
                   Nothing ->
                     findMatchLeftToRight lst inp acc
                   Just (reg, amAcc) ->
                     case amAcc of
                       Ambiguous -> error "broken invariant"
                       DunnoAmbiguous -> error "should not be here"
                       NotAmbiguous -> Just (reg : acc, Nothing)
           _ -> error "should not reach this line"

    findMatchLeftToRight lst inp acc =
      case lst of
        [] -> Nothing
        (c, reg, am, _qDFA, qArriv) : rest ->
          case matchInputHeadCondition c inp of
            Nothing -> findMatchLeftToRight rest inp acc
            Just inp1 ->
              let newAcc = reg : acc in
              case am of
                NotAmbiguous -> Just (newAcc, Just qArriv)
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
          let (backCfg, pdx) = extractPredictionFromDFARegistry src (initIteratorDFARegistry s)
          in walkBackward backCfg rest ((Seq.><) pdx acc)

    extractSinglePrediction :: DFARegistry -> (SourceCfg, Prediction)
    extractSinglePrediction s =
      case nextIteratorDFARegistry (initIteratorDFARegistry s) of
        Just (DFAEntry c1 (ClosurePath alts _c2 (_pos, _, _) _c3), rest) ->
          if not (isEmptyIteratorDFARegistry rest)
          then error "ambiguous prediction"
          else (c1, alts)
          -- NOTE: pos is appended because this is the last transition
        Just (DFAEntry c1 (ClosureAccepting alts _c2), rest) ->
          if not (isEmptyIteratorDFARegistry rest)
          then error "ambiguous prediction"
          else (c1, alts)
        Just (DFAEntry _c1 (ClosureMove {}), _) ->
          error "broken invariant: cannot be ClosureMove here"
        _ -> error "ambiguous prediction"

    extractPredictionFromDFARegistry :: SourceCfg -> IteratorDFARegistry -> (SourceCfg, Prediction)
    extractPredictionFromDFARegistry src s =
      case nextIteratorDFARegistry s of
        Nothing -> error "could not find src from previous cfg"
        Just (DFAEntry c1 (ClosurePath alts _c2 (_pos, _, _q2) c3), others) ->
          if cfgState c3 == cfgState src && cfgCtrl c3 == cfgCtrl src
          then (c1, alts)
          else extractPredictionFromDFARegistry src others
        Just (DFAEntry _c1 (ClosureAccepting _alts _c2), _others) ->
          error "broken invariant: cannot be ClosureAccepting here"
        Just (DFAEntry _c1 (ClosureMove {}), _others) ->
           error "broken invariant: cannot be ClosureMove here"


predictLL :: Either State SynthLLAState -> LLA -> Input.Input -> Maybe (Prediction, Maybe SynthLLAState)
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
          Just (pdx, mlinState) ->
            let finalSynth = maybe Nothing (\ linState -> Map.lookup linState finalMapping) mlinState in
            Just (pdx, finalSynth)


synthesizeLLA :: Aut a => a -> LLA -> LLA
synthesizeLLA aut lla =
  let m = mappingNFAToSynth lla
      maxState = getMaxState aut -- List.maximum (map fst (IntMap.toAscList m))
      arr = generate (maxState + 1) m
  in lla { synthesizedMappingNFAToSynth = arr}
  where
    generate :: Int -> IntMap.IntMap SynthLLAState -> Array.Array Int (Maybe SynthLLAState)
    generate size mapping =
      Array.array (0, size) lst
      where
        lst = [ case IntMap.lookup i mapping of
                  Nothing -> (i, Nothing)
                  Just e -> (i, Just e)
              | i <- [0..size]
              ]

isFullyDeterminizedLLA :: LLA -> Bool
isFullyDeterminizedLLA lla =
  let m = mappingSynthToDFAState lla in
  foldr (\ q b ->
           case Map.lookup q (transitionLLA lla) of
             Nothing -> error "broken invariant"
             Just dfa ->
               if fromJust $ flagHasFullResolution dfa
               then b
               else False) True (map fst (Map.toAscList m))


buildPipelineLLA :: Aut a => a -> Either LLA (LLA, LLA)
buildPipelineLLA aut =
  let
    initStates = [ initialState aut ]
    (initDFAStates, tab) = allocate initStates emptyHTable
  in
  let
    (lla1, tab1) = go initDFAStates [] emptyLLA tab
  in
    if isFullyDeterminizedLLA lla1
    then
      let lla = synthesizeLLA aut lla1
      in Left lla
    else
      let collectedStates = identifyStartStates ()
      in
        let
          (nextInitStates, tab2) = allocate collectedStates tab1
          (lla2, _tab3) = go nextInitStates [] lla1 tab2 in
        let lla3 = synthesizeLLA aut lla2
        in Right (lla1, lla3)

  where
    allocate lst tab =
      foldl
      (\ (acc, accTab) q ->
         let (qDFA, tab1) = mkDFAState q accTab
         in (qDFA : acc, tab1)
      )
      ([], tab)
      lst

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


    go ::
      [DFAState] -> [DFAState] -> LLA ->
      HTable -> (LLA, HTable)
    go toVisit nextRound lla tab =
      if synthLLAState (lastSynth lla) > 1000000 then error "Stop" else
      case toVisit of
        [] -> case nextRound of
                [] -> (lla, tab)
                _ -> go (reverse nextRound) [] lla tab
        q : qs ->
          if memberLLA q lla
          then go qs nextRound lla tab
          else
            let (dfa, tab1) = createDFA aut q tab
                newlla = insertLLA q dfa lla
                newNextRound =
                  let finalStates = getFinalStates dfa in
                    revAppend finalStates nextRound
            in go qs newNextRound newlla tab1

createLLA :: Aut a => a -> LLA
createLLA aut =
  case buildPipelineLLA aut of
    Left lla -> lla
    Right (_lla1, lla2) -> lla2

showStartSynthLLAState :: Aut a => a -> LLA -> SynthLLAState -> [String]
showStartSynthLLAState aut dfas q =
  let dfaSt = fromJust $ Map.lookup q (mappingSynthToDFAState dfas) in
  case nextIteratorDFAState (initIteratorDFAState dfaSt) of
    Nothing -> [ "__ZSINK_STATE" ]
    Just (cfg, qs) ->
      if isEmptyIteratorDFAState qs
      then [ stateToString (cfgState cfg) aut
           , showSlkCfg cfg
           ]
      else error "broken invariant"

printLLA :: Aut a => a -> LLA -> (DFA -> Bool) -> IO ()
printLLA aut lla cond =
  let t = Map.toAscList (transitionLLA lla)
      tAnnotated = map (\ (q, dfa) -> (showStartSynthLLAState aut lla q, dfa)) t
      tMapped = Map.fromList tAnnotated
      tOrdered = Map.assocs tMapped
  in if length t > 10000
     then do return ()
     else mapM_ (\ (ann, dfa) ->
                    if ( cond dfa
                         -- (lookaheadDepth dfa < 10)
                         -- ||
                         -- (fromJust $ flagHasFullResolution dfa)
                       )
                    then
                      do
                        mapM_ putStrLn ann
                        putStrLn $ showDFA dfa
                        putStrLn ""
                    else
                      return ()

                ) tOrdered

printAmbiguities :: Aut a => a -> LLA -> IO ()
printAmbiguities aut lla =
  let t = Map.toAscList (transitionLLA lla)
      tAnnotated = map (\ (q, dfa) -> (showStartSynthLLAState aut lla q, dfa)) t
      tMapped = Map.fromList tAnnotated
      tOrdered = Map.assocs tMapped
  in if length t > 10000
     then do return ()
     else mapM_ (\ (ann, dfa) ->
                    case extractAmbiguity dfa of
                      Nothing ->
                        return ()
                      Just (l, conflicts) ->
                        do putStrLn "*******  Found Ambiguity  *******"
                           putStrLn ("  Start: " ++ (head ann))
                           putStrLn ("  Paths : ")
                           printConflicts conflicts
                           putStrLn "********  input witness  ********"
                           putStrLn $ printPath l
                ) tOrdered
  where
    printPath l =
      concat (List.intersperse "" (map showGraphvizInputHeadCondition l))

    printConflicts conflicts =
      do mapM_ (\ c -> do putStrLn ("  -  ")
                          mapM_ (\ s -> putStrLn ("     " ++ s)
                                ) (getInfoEntry aut c)
               ) conflicts



llaToGraphviz :: Aut a => a -> Either LLA (LLA, LLA) -> IO ()
llaToGraphviz _aut lla =
  do
    autFile <- openFile "lla.dot" WriteMode
    hPutStrLn autFile prelude
    mapM_ (hPutStrLn autFile) trans
    hPutStrLn autFile postlude
    hClose autFile
  where
    theLLA =
      case lla of
          Left lla1 -> lla1
          Right (lla1, _lla2) -> lla1
    m = transitionLLA theLLA
    f k a b =
      printDFAtoGraphviz (synthLLAState k) a ++ b

    trans = Map.foldrWithKey f [] m
    prelude =
      "// copy this to lla.dot and run\n"
      ++ "// dot -Tpdf lla.dot > lla.pdf \n"
      ++ "digraph G { size=\"8,5\"; rankdir=\"LR\";"
    postlude = "}" ++ "\n// dot -Tpdf lla.dot > lla.pdf " -- "f.view()\n"


statsLLA :: Aut a => a -> Either LLA (LLA, LLA) -> IO ()
statsLLA aut llas =
  let nofilter = \ _dfa -> True in
  case llas of
    Left lla ->
      let t = map snd (Map.toAscList (transitionLLA lla)) in
      do printLLA aut lla nofilter
         putStrLn "**********************"
         putStrLn "***** Strict LLA *****"
         putStrLn "**********************"
         putStrLn $ getReport t initReport
         putStrLn $ "Total nb states: " ++ show (length t) ++ "\n"
         putStrLn "SUCCESS: Fully determinized format"

    Right (lla1, lla) ->
      let t = map snd (Map.toAscList (transitionLLA lla))
          t1 = map snd (Map.toAscList (transitionLLA lla1)) in
      do putStrLn "**********************************"
         putStrLn "***** Strict LLA Transitions *****"
         putStrLn "**********************************"
         printLLA aut lla1 nofilter
         putStrLn "**********************"
         putStrLn "**** Extended LLA ****"
         putStrLn "**********************"
         putStrLn $ getReport t initReport
         putStrLn $ "Total nb states: " ++ show (length t) ++ "\n"
         putStrLn "**********************"
         putStrLn "***** Strict LLA *****"
         putStrLn "**********************"
         putStrLn $ getReport t1 initReport
         putStrLn $ "Total nb states: " ++ show (length t1) ++ "\n"
         putStrLn "\nWarning: LL(*) failures:\n"
         printLLA aut lla1 (\ dfa -> not (fromJust $ flagHasFullResolution dfa))
         printAmbiguities aut lla1

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

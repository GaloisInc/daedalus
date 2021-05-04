{-# Language GADTs #-}

module Daedalus.ParserGen.LL.LLA
  ( createLLA
  , buildPipelineLLA
  , statsLLA
  , LLAState
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
import Daedalus.ParserGen.LL.ParamLL
import Daedalus.ParserGen.LL.SlkCfg
import Daedalus.ParserGen.LL.Closure
import Daedalus.ParserGen.LL.DFAStep
import Daedalus.ParserGen.LL.DFA




-- LLA: Lockstep Lookahead Automaton
-- LLA: LL(*) Automaton
-- LLA: lalalalala
-- LLA: reverse of ALL(*)

data LLAState = LLAState { qLLAState :: {-# UNPACK #-} !Int }

instance Eq LLAState where
  (==) q1 q2 = qLLAState q1 == qLLAState q2

instance Ord LLAState where
  compare q1 q2 =
    compare (qLLAState q1) (qLLAState q2)

instance Show LLAState where
  show (LLAState q) = "l" ++ show q

initLLAState :: LLAState
initLLAState = LLAState 0


type MapFinalToLLAState = Map.Map LinDFAState LLAState

data LLA = LLA
  { transitionLLA :: !(Map.Map LLAState (Either DFA DDA))

    -- mapping from the Final states of the DFA to the `LLAState`
    -- This is necessary for fast linking final states of the DFA to
    -- `LLAState` during the prediction
  , mappingFinalToLLAState :: Map.Map LLAState MapFinalToLLAState

    -- mapping from the NFA states to LLAState
  , mappingNFAStateToLLAState :: IntMap.IntMap LLAState

  , mappingDFAStateToLLAState :: Map.Map DFAState LLAState
  , mappingLLAStateToDFAState :: Map.Map LLAState DFAState
  , _lastLLAState :: LLAState

    -- synthesized mapping of mappingNFAStateToLLAState
  , synthesizedMappingNFAStateToLLAState :: !(Array.Array Int (Maybe LLAState))
  }

emptyLLA :: LLA
emptyLLA =
  LLA
  { transitionLLA = Map.empty
  , mappingFinalToLLAState = Map.empty
  , mappingNFAStateToLLAState = IntMap.empty
  , mappingDFAStateToLLAState = Map.empty
  , mappingLLAStateToDFAState = Map.empty
  , _lastLLAState = initLLAState
  , synthesizedMappingNFAStateToLLAState = Array.array (0,0) [ (0, Nothing) ]
  }

nextLLAState :: LLAState -> LLAState
nextLLAState (LLAState q) = LLAState (q+1)


lookupSynthArray :: State -> LLA -> Maybe LLAState
lookupSynthArray q lla =
  {-# SCC breakpointLookupSynthArray #-}
  -- IntMap.lookup q (mappingNFAStateToLLAState lla)
  let m = synthesizedMappingNFAStateToLLAState lla in
    m Array.! q

lookupLLAFromState ::
  State ->
  LLA ->
  Maybe (Either (DFA, MapFinalToLLAState) DDA)
lookupLLAFromState q lla =
  let mSynth = lookupSynthArray q lla
  in
  case mSynth of
    Nothing -> Nothing
    Just sq ->
      let t = fromJust $ Map.lookup sq (transitionLLA lla)
      in
      case t of
        Left dfa ->
          Just $ Left
          ( dfa
          , fromJust $ Map.lookup sq (mappingFinalToLLAState lla)
          )
        Right dda ->
          Just $ Right dda


lookupLLAFromLLAState ::
  LLAState ->
  LLA ->
  Either (DFA, MapFinalToLLAState) DDA
lookupLLAFromLLAState q lla =
  let
    t = fromJust $ Map.lookup q (transitionLLA lla)
  in
  case t of
    Left dfa ->
      let finalMapping = fromJust $ Map.lookup q (mappingFinalToLLAState lla)
      in Left (dfa, finalMapping)
    Right dda -> Right dda


memberLLA :: DFAState -> LLA -> Bool
memberLLA q lla =
  if Map.member q (mappingDFAStateToLLAState lla)
  then
    let qLLA = fromJust $ Map.lookup q (mappingDFAStateToLLAState lla)
    in Map.member qLLA (transitionLLA lla)
  else False

insertLLA :: DFAState -> Either DFA DDA -> LLA -> LLA
insertLLA q ddfa =
  (\ aut ->
  let
    (qLLA, lla) =
      if Map.member q (mappingDFAStateToLLAState aut)
      then
        let q1 = fromJust $ Map.lookup q (mappingDFAStateToLLAState aut) in
        (q1, aut)
      else
        let aut1 = addDFAStateToMappings q aut in
        let q1 = fromJust $ Map.lookup q (mappingDFAStateToLLAState aut1) in
        (q1, aut1)
  in
  allocFinalAndTransition qLLA lla
  )

  where
    allocFinalAndTransition :: LLAState -> LLA -> LLA
    allocFinalAndTransition qLLA lla =
      case ddfa of
        Left dfa ->
          let finalStates = finalLinDFAState dfa in
          let
            (finalMapping, lla1) =
              allocFinal (Set.toList finalStates) dfa (Map.empty, lla)
          in
          lla1
          { transitionLLA = Map.insert qLLA ddfa (transitionLLA lla)
          , mappingFinalToLLAState =
              Map.insert qLLA finalMapping (mappingFinalToLLAState lla)
          }
        Right dda ->
          let finalStates = getFinalStatesDDA dda in
          let lla1 = allocFinalDDA finalStates lla in
          lla1
          { transitionLLA = Map.insert qLLA ddfa (transitionLLA lla1)
          }

    addDFAStateToMappings qDFA lla1 =
      -- Membership tested here bc function used at top level and by
      -- `allocFinal`
      if Map.member qDFA (mappingDFAStateToLLAState lla1)
      then lla1
      else
        let newSynth = nextLLAState (_lastLLAState lla1) in
        let
          newMappingNFAToSynth =
            case isDFAStateInit qDFA of
              Nothing -> mappingNFAStateToLLAState lla1
              Just qNFA -> IntMap.insert qNFA newSynth (mappingNFAStateToLLAState lla1)
          newMappingDFAStateToSynth =
            Map.insert qDFA newSynth (mappingDFAStateToLLAState lla1)
          newMappingSynthToDFAState =
            Map.insert newSynth qDFA (mappingLLAStateToDFAState lla1)
        in
          lla1
          { mappingNFAStateToLLAState = newMappingNFAToSynth
          , mappingDFAStateToLLAState = newMappingDFAStateToSynth
          , mappingLLAStateToDFAState = newMappingSynthToDFAState
          , _lastLLAState = newSynth
          }

    -- Allocate the final states, add them to the lla and return the
    -- mappingfrom the final states of the dfa/dda to the
    -- `LLAState`
    allocFinal ::
      [LinDFAState] ->
      DFA ->
      (MapFinalToLLAState, LLA) ->
      (MapFinalToLLAState, LLA)
    allocFinal lst dfa (finalMapping, lla) =
      case lst of
        [] -> (finalMapping, lla)
        qLinDFA : qs ->
          let qDFA = fromJust $ Map.lookup qLinDFA (mappingLinToDFAState dfa) in
          let newLla = addDFAStateToMappings qDFA lla in
          let
            newFinalMapping =
              let
                qLLA =
                  fromJust $ Map.lookup qDFA (mappingDFAStateToLLAState newLla)
              in
              Map.insert qLinDFA qLLA finalMapping
          in allocFinal qs dfa (newFinalMapping, newLla)


    allocFinalDDA ::
      [DFAState] ->
      LLA ->
      LLA
    allocFinalDDA lst lla =
      case lst of
        [] -> lla
        qDFA : qs ->
          let newLla = addDFAStateToMappings qDFA lla
          in allocFinalDDA qs newLla


type Prediction = ChoiceSeq

destrPrediction :: Prediction -> Maybe (ChoicePos, Prediction)
destrPrediction pdx =
  case pdx of
    Seq.Empty -> Nothing
    c Seq.:<| cs -> Just (c, cs)


-- TODO: Explain this in some document. This is basically the key to
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

    extractPredictionFromDFARegistry ::
      SourceCfg -> IteratorDFARegistry -> (SourceCfg, Prediction)
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


predictLL ::
  Either State LLAState ->
  LLA ->
  Input.Input ->
  Maybe (Either (Prediction, Maybe LLAState) DDA)
predictLL qq lla inp =
  case qq of
    Left q ->
      let r = lookupLLAFromState q lla in
      case r of
        Nothing -> Nothing
        Just (Left (dfa, finalMapping)) ->
          predictFromPDXA dfa finalMapping
        Just (Right dda) ->
          Just (Right dda)
    Right qLLA ->
      let r = lookupLLAFromLLAState qLLA lla in
      case r of
        Left (dfa, finalMapping) ->
          predictFromPDXA dfa finalMapping
        Right dda ->
          Just $ Right dda

  where
    predictFromPDXA dfa finalMapping =
      let mp = predictDFA dfa inp in
        case mp of
          Nothing -> Nothing
          Just (pdx, mlinState) ->
            let
              finalSynth =
                maybe
                Nothing
                (\ linState -> Map.lookup linState finalMapping)
                mlinState
            in
            Just (Left (pdx, finalSynth))


synthesizeLLA :: Aut a => a -> LLA -> LLA
synthesizeLLA aut lla =
  let m = mappingNFAStateToLLAState lla
      maxState = getMaxState aut -- List.maximum (map fst (IntMap.toAscList m))
      arr = generate (maxState + 1) m
  in lla { synthesizedMappingNFAStateToLLAState = arr }
  where
    generate :: Int -> IntMap.IntMap LLAState -> Array.Array Int (Maybe LLAState)
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
  let m = mappingLLAStateToDFAState lla in
  foldr
  (\ q b ->
      case Map.lookup q (transitionLLA lla) of
        Nothing -> error "broken invariant"
        Just (Left dfa) ->
          if fromJust $ flagHasFullResolution dfa
          then b
          else False
        Just (Right dda) ->
          if computeHasFullResolutionDDA dda
          then b
          else False
  )
  True
  (map fst (Map.toAscList m))


buildPipelineLLA :: Aut a => a -> Either LLA (LLA, LLA)
buildPipelineLLA aut =
  let
    initStates = [ initialState aut ]
    (initDFAStates, tab) = allocate initStates emptyHTable
  in
  let
    (lla1, tab1) = go initDFAStates [] emptyLLA tab
    lla11 = synthesizeLLA aut lla1
  in
    if isFullyDeterminizedLLA lla1 || flag_ONLY_STRICT_LLA
    then
      Left lla11
    else
      let collectedStates = identifyStartStates ()
      in
        let
          (nextInitStates, tab2) = allocate collectedStates tab1
          (lla2, _tab3) = go nextInitStates [] lla1 tab2 in
        let lla3 = synthesizeLLA aut lla2
        in Right (lla11, lla3)

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
      case toVisit of
        [] -> case nextRound of
                [] -> (lla, tab)
                _ -> go (reverse nextRound) [] lla tab
        q : qs ->
          if qLLAState (_lastLLAState lla) > cst_MAX_LLA_SIZE
          then
            let
              dfa = createAbortDFA q (Abort AbortLLAOverflow)
              newlla = insertLLA q (Left dfa) lla
            in
            go qs nextRound newlla tab
          else
          if memberLLA q lla
          then go qs nextRound lla tab
          else
            let r = createDFA aut q tab in
            case r of
              Left (dfa, tab1) ->
                let
                  newlla = insertLLA q (Left dfa) lla
                  newNextRound =
                    let finalStates = getFinalStates dfa in
                    revAppend finalStates nextRound
                in go qs newNextRound newlla tab1
              Right (dda, tab1) ->
                let
                  newlla = insertLLA q (Right dda) lla
                  newNextRound =
                    let finalStates = getFinalStatesDDA dda in
                    revAppend finalStates nextRound
                in
                  go qs newNextRound newlla tab1


createLLA :: Aut a => a -> LLA
createLLA aut =
  case buildPipelineLLA aut of
    Left lla -> lla
    Right (_lla1, lla2) ->
      lla2
      -- _lla1

showStartLLAState :: Aut a => a -> LLA -> LLAState -> [String]
showStartLLAState aut lla q =
  let qDFA = fromJust $ Map.lookup q (mappingLLAStateToDFAState lla) in
  case nextIteratorDFAState (initIteratorDFAState qDFA) of
    Nothing -> [ "__ZSINK_STATE" ]
    Just (cfg, qs) ->
      if isEmptyIteratorDFAState qs
      then
        [ "Stack:" ] ++
        map (\ x -> "    " ++ x) (showSlkControlData aut (cfgCtrl cfg)) ++
        [ "Start: " ++ stateToString (cfgState cfg) aut
        , showSlkCfg cfg
        ]
      else error "broken invariant"

printLLA :: Aut a => a -> LLA -> (DFA -> Bool) -> IO ()
printLLA aut lla cond =
  let t = Map.toAscList (transitionLLA lla)
      tAnnotated =
        map (\ (q, d) -> (showStartLLAState aut lla q, d)) t
      tMapped = Map.fromList tAnnotated
      tOrdered = Map.assocs tMapped
  in
  if length t > 10000
  then do return ()
  else
    mapM_
    (\ (ann, d) ->
        case d of
          Left dfa ->
            if
              ( cond dfa
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
          Right _dda -> return ()
    )
    tOrdered

printAmbiguities :: Aut a => a -> LLA -> IO ()
printAmbiguities aut lla =
  let t = Map.toAscList (transitionLLA lla)
      tAnnotated = map (\ (q, dfa) -> (showStartLLAState aut lla q, dfa)) t
      tMapped = Map.fromList tAnnotated
      tOrdered = Map.assocs tMapped
  in
  if length t > 10000
  then do return ()
  else
    mapM_
    (\ (ann, d) ->
       case d of
         Left dfa ->
           case extractAmbiguity dfa of
             Nothing ->
               return ()
             Just (l, conflicts) ->
               do putStrLn "*******  Found Ambiguity  *******"
                  putStrLn ("  Start: " ++ (head ann))
                  putStrLn ("  Paths :")
                  printConflicts conflicts
                  putStrLn "********  input witness  ********"
                  putStrLn $ printPath l
         Right _dda ->
           return ()
    )
    tOrdered
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
      case a of
        Left dfa ->
          printDFAtoGraphviz (qLLAState k) dfa ++ b
        Right _dda ->
          [""]

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
         putStrLn $ "Total nb DFAs: " ++ show (length t) ++ "\n"
         if isFullyDeterminizedLLA lla
           then
           putStrLn "SUCCESS: Fully determinized format"
           else
           putStrLn "\nWarning: LL(*) failures:\n"
         if flag_ONLY_STRICT_LLA
           then
           printLLA aut lla (\ dfa -> not (fromJust $ flagHasFullResolution dfa))
           else
           return ()

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
         putStrLn $ "Total nb DFAs: " ++ show (length t1) ++ "\n"
         putStrLn "\nWarning: LL(*) failures:\n"
         printLLA aut lla1 (\ dfa -> not (fromJust $ flagHasFullResolution dfa))
         printAmbiguities aut lla1

  where
    getReport lst report =
      case lst of
        [] -> foldr (\ a b -> show a ++ "\n" ++ b) "" (Map.assocs report)
        d : xs ->
          case d of
            Left dfa ->
              getReport xs (incrReport report dfa)
            Right dda ->
              getReport xs (incrReportDDA report dda)

    result str = "Lookahead" ++ str

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

    mapResultToKeyDDA :: DDA -> String
    mapResultToKeyDDA dda =
      case computeHasFullResolutionDDA dda of
        True -> "DataDepTrans"
        False -> "Abort-DataDepTrans"

    incrReport :: Map.Map String Int -> DFA -> Map.Map String Int
    incrReport report dfa =
      let key = mapResultToKey dfa in
      Map.insertWith (\ a b -> a+b) key 1 report

    incrReportDDA :: Map.Map String Int -> DDA -> Map.Map String Int
    incrReportDDA report dda =
      let key = mapResultToKeyDDA dda in
      Map.insertWith (\ a b -> a+b) key 1 report

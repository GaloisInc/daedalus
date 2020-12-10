{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DFA
  ( createDFA
  , statsDFA
  , AutDet
  , DFA(..)
  , lookupAutDet
  , Prediction
  , destrPrediction
  , predictLL
  )
where

-- import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
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


data DFATransition =
  DChoice [ (InputHeadCondition, DFARegistry, AmbiguityDetection, DFAState) ]

data DFA = DFA
  { startDFA :: DFAState
  , transitionDFA :: Map.Map DFAState (Result DFATransition)
  , stateMappingDFA :: Map.Map DFAState Int
  , flagHasNoAbort :: Maybe Bool
  , flagHasFullResolution :: Maybe Bool
  }

initDFA :: DFAState ->  DFA
initDFA q =
  DFA
  { startDFA = q
  , transitionDFA = Map.empty
  , stateMappingDFA = Map.empty
  , flagHasNoAbort = Nothing
  , flagHasFullResolution = Nothing
  }

lookupDFA :: DFAState -> DFA -> Maybe (Result DFATransition)
lookupDFA q dfa =
  Map.lookup q (transitionDFA dfa)

insertDFA :: DFAState -> Result DFATransition -> DFA -> DFA
insertDFA q tr dfa =
  dfa { transitionDFA = Map.insert q tr (transitionDFA dfa) }



showDFATransition :: (DFAState, DFA) -> String
showDFATransition (q, dfa) =
  showTrans [] 0 q
  where
    showTrans :: [DFAState] -> Int -> DFAState -> String
    showTrans vis d qq =
      if elem qq vis
      then "**** loop ****"
      else
      case lookupDFA qq dfa of
        Nothing -> error "sdfsdf"
        Just r ->
          case r of
            Result (DChoice lst) ->
              "DTrans [\n" ++
              concatMap (showT (qq : vis) (d+2)) lst ++
              space d ++ "]"
            _ -> abortToString r ++ "\n"

    showT vis d (i, s, am, qq) =
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
             DunnoAmbiguous -> showTrans vis (d+2) qq

    showSet s = "[" ++ foldr (\ entry b ->
                                let alts = altSeq $ dstEntry entry in
                                 "(" ++ show (length alts) ++
                                 -- ",q" ++ show q ++
                                 ")," ++ b) "" s  ++ "]"
    space d = spaceHelper 0
       where spaceHelper cnt = if cnt < d then " " ++ spaceHelper (cnt+1) else ""

lookaheadDepth :: DFA -> Int
lookaheadDepth dfa =
  let start = startDFA dfa in
  helper [] start
  where
    helper :: [DFAState] -> DFAState -> Int
    helper vis qq =
      if elem qq vis
      then 0
      else
        case lookupDFA qq dfa of
          Nothing -> 0
          Just rt ->
            case rt of
              Result (DChoice lst) -> foldOverList vis qq lst 0
              _ -> 0

    foldOverList vis src lst acc =
      case lst of
        [] -> acc
        (_, _, _am, qq) : rest ->
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

type DiscoveredDFAStates = [DFAState]

emptyDiscoveredDFAStates :: DiscoveredDFAStates
emptyDiscoveredDFAStates = []

addDiscoveredDFAStates :: DFAState -> DiscoveredDFAStates -> DiscoveredDFAStates
addDiscoveredDFAStates q cont = q : cont

mergeDiscoveredDFAStates :: DiscoveredDFAStates -> DiscoveredDFAStates -> DiscoveredDFAStates
mergeDiscoveredDFAStates cont1 cont2 = cont1 ++ cont2


oVERFLOW_CFG :: Int
oVERFLOW_CFG = 10

createDFAtable :: Aut a => a -> DFAState -> (DFA, DiscoveredDFAStates)
createDFAtable aut qInit =
  let idfa = initDFA qInit in
  if measureDFAState qInit > oVERFLOW_CFG
  then (insertDFA qInit (Abort AbortOverflowCfg) idfa, [])
  else
    let (resDfa, cont) = go [(qInit,0)] [] (idfa, [])
    in (resDfa, reverse cont)
  where
    go :: [ (DFAState, Int) ] -> [ (DFAState, Int) ] -> (DFA, DiscoveredDFAStates) -> (DFA, DiscoveredDFAStates)
    go toVisit accToVisit acc@(dfa, cont) =
      case toVisit of
        [] -> case accToVisit of
                [] -> acc
                _ -> go (reverse accToVisit) [] acc
        (q, depth) : rest ->
          case lookupDFA q dfa of
            Nothing ->
              if depth > maxDepthDet
              then
                let newDfa = insertDFA q (Abort AbortOverflowK) dfa
                in go rest accToVisit (newDfa, cont)
              else
                let choices = detSubset q
                    newDfa = insertDFA q choices dfa
                in
                  case choices of
                    Result (DChoice r1) ->
                      let
                        (newToVisit, newToCont) = collectVisitAndCont (depth+1) r1
                        newAccToVisit = revAppend newToVisit accToVisit
                        newCont = mergeDiscoveredDFAStates newToCont cont
                      in go rest newAccToVisit (newDfa, newCont)
                    _ -> go rest accToVisit (newDfa, cont)
            Just _ -> -- trace ("********FOUND*****" ++ "\n" ++ show q) $
              go rest accToVisit acc

    collectVisitAndCont :: Int -> [(InputHeadCondition, DFARegistry, AmbiguityDetection, DFAState)] -> ([(DFAState, Int)], DiscoveredDFAStates)
    collectVisitAndCont depth lst =
      foldr (\ (_, _, am, qq) (vis, cont) ->
               case am of
                 Ambiguous -> (vis, cont)
                 NotAmbiguous -> (vis, addDiscoveredDFAStates qq cont)
                 DunnoAmbiguous -> ((qq, depth) : vis, cont)) ([], emptyDiscoveredDFAStates) lst

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
          Result (DChoice (mapAnalyzeConflicts r1))
        _ -> error "cannot be this abort"

    mapAnalyzeConflicts :: DetChoice ->
                           [(InputHeadCondition, DFARegistry, AmbiguityDetection, DFAState)]
    mapAnalyzeConflicts dc =
      let lst = detChoiceToList dc in
      map fconvert lst
      where
        fconvert (ihc, s) =
          let newCfg = convertDFARegistryToDFAState ihc s
              am = analyzeConflicts s
          in (ihc, s, am, newCfg)



computeHasFullResolution :: DFA -> DFA
computeHasFullResolution dfa =
  let start = startDFA dfa in
  dfa { flagHasFullResolution = Just $ traverseWithVisited [] start }
  where
    traverseWithVisited visited q =
      if elem q visited
      then True
      else
        case lookupDFA q dfa of
          Nothing -> error "broken invariant"
          Just r ->
            case r of
              Result (DChoice lst) -> helper (q : visited) lst
              _ -> False

    helper visited lst =
      case lst of
        [] -> True
        (_, _, am, qq) : rest ->
          case am of
            NotAmbiguous -> helper visited rest
            Ambiguous -> False
            DunnoAmbiguous -> traverseWithVisited visited qq && helper visited rest

computeHasNoAbort :: DFA -> DFA
computeHasNoAbort dfa =
  let start = startDFA dfa in
  dfa { flagHasNoAbort = Just $ traverseWithVisited [] start }
  where
    traverseWithVisited visited q =
      if elem q visited
      then True
      else
        case lookupDFA q dfa of
          Nothing -> error "broken invariant"
          Just r ->
            case r of
              Result (DChoice lst) -> helper (q : visited) lst
              _ -> False

    helper visited lst =
      case lst of
        [] -> True
        (_, _, am, qq) : rest ->
          case am of
            NotAmbiguous -> helper visited rest
            Ambiguous -> helper visited rest
            DunnoAmbiguous -> traverseWithVisited visited qq && helper visited rest




type AutDet = Map.Map DFAState DFA

lookupAutDet :: State -> AutDet -> Maybe DFA
lookupAutDet q aut = Map.lookup (mkDFAState q) aut

insertAutDet :: DFAState -> DFA -> AutDet -> AutDet
insertAutDet q v aut = Map.insert q v aut

emptyAutDet :: AutDet
emptyAutDet = Map.empty

type Prediction = ChoiceSeq

destrPrediction :: Prediction -> Maybe (ChoicePos, Prediction)
destrPrediction pdx =
  case pdx of
    Seq.Empty -> Nothing
    c Seq.:<| cs -> Just (c, cs)


-- TODO: Explain this in some document. This is basically the key of
-- the new faithful determinization.
predictLL :: DFA -> Input.Input -> Maybe Prediction
predictLL dfa i =
  let
    start = startDFA dfa
    mpath = findMatchingPath start i []
  in
    case mpath of
      Nothing -> Nothing
      Just path -> Just $ extractPrediction path
  where
    findMatchingPath :: DFAState -> Input.Input -> [DFARegistry] -> Maybe [DFARegistry]
    findMatchingPath q inp acc =
      let elm = lookupDFA q dfa
      in case elm of
       Nothing -> error "broken invariant"
       Just r ->
         case r of
           Result (DChoice lst) -> findMatchLeftToRight lst inp acc
           _ -> error "should not reach this line"

    findMatchLeftToRight lst inp acc =
      case lst of
        [] -> Nothing
        (c, s, am, r1) : rest ->
          case matchInputHeadCondition c inp of
            Nothing -> findMatchLeftToRight rest inp acc
            Just inp1 ->
              let newAcc = s : acc in
              case am of
                NotAmbiguous -> Just newAcc
                Ambiguous -> error "broken invariant, only applied on fully resolved"
                DunnoAmbiguous -> findMatchingPath r1 inp1 newAcc


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



createDFA :: Aut a => a -> AutDet
createDFA aut =
  let collectedStates =
        identifyStartStates ()
        -- Set.singleton $ initialState aut
  in
    go (Set.toList (Set.map mkDFAState collectedStates)) [] emptyAutDet
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

    go :: [DFAState] -> [DFAState] -> AutDet -> AutDet
    go toVisit nextRound res =
      if Map.size res > 1000000 then error "Stop" else
      -- trace (show (Map.size res)) $
      case toVisit of
        [] -> case nextRound of
                [] -> res
                _ -> go (reverse nextRound) [] res
        q : qs ->
          -- trace (show q) $
          if Map.member q res
          then go qs nextRound res
          else
            let (dfa, discovered) = createDFAtable aut q
                dfa1 = computeHasFullResolution dfa
                dfa2 = computeHasNoAbort dfa1
                newRes = insertAutDet q dfa2 res
                newNextRound = revAppend discovered nextRound
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

printDFA :: Aut a => a -> AutDet -> IO ()
printDFA aut dfas =
  let t = Map.toAscList dfas
      tAnnotated = map (\ (q, dfa) -> ((showStartDFA aut q, q), (q, dfa))) t
      tMapped = Map.fromList tAnnotated
      tOrdered = Map.assocs tMapped
  in if length t > 1000
     then do return ()
     else mapM_ (\ ((ann, _), (q, dfa)) ->
                    -- if (lookaheadDepth dfa < 10)
                    -- then
                    --   return ()
                    -- else
                      do
                        putStrLn $ ann
                        putStrLn $ showDFATransition (q, dfa)
                        putStrLn ""
                ) tOrdered


statsDFA :: Aut a => a -> AutDet -> IO ()
statsDFA aut dfas =
  let t = Map.toAscList dfas
  in do printDFA aut dfas
        putStrLn "\nReport:"
        putStrLn $ getReport t initReport
        putStrLn $ "\nTotal nb states: " ++ show (length t)
  where
    getReport lst report =
      case lst of
        [] -> foldr (\ a b -> show a ++ "\n" ++ b) "" (Map.assocs report)
        (q, dfa) : xs ->
          getReport xs (incrReport report (q, dfa))

    result str = "Result" ++ str

    initReport :: Map.Map String Int
    initReport = Map.fromAscList []

    mapResultToKey :: (DFAState, DFA) -> String
    mapResultToKey (q, dfa) =
      let r = fromJust (lookupDFA q dfa)
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


    incrReport :: Map.Map String Int -> (DFAState, DFA) -> Map.Map String Int
    incrReport report r =
      let key = mapResultToKey r
      in
        Map.insertWith (\ a b -> a+b) key 1 report

{-# Language GADTs #-}

module Daedalus.ParserGen.LL.DFA
  ( createDFA
  , statsDFA
  , AutDet
  , lookupAutDet
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



data DFATransition =
  DChoice [ (InputHeadCondition, DFAState, AmbiguityDetection, DFAStateQuotient) ]

type ExplicitDFA = Map.Map DFAStateQuotient (Result DFATransition)

lookupExplicitDFA :: DFAStateQuotient -> ExplicitDFA -> Maybe (Result DFATransition)
lookupExplicitDFA q dfa =
  Map.lookup q dfa

insertExplicitDFA :: DFAStateQuotient -> Result DFATransition -> ExplicitDFA -> ExplicitDFA
insertExplicitDFA q tr dfa =
  Map.insert q tr dfa

showDFATransition :: (State, ExplicitDFA) -> String
showDFATransition (q, dfa) =
  showTrans [] 0 (mkDFAStateQuotient q)
  where
    showTrans :: [DFAStateQuotient] -> Int -> DFAStateQuotient -> String
    showTrans vis d qq =
      if elem qq vis
      then "**** loop ****"
      else
      case lookupExplicitDFA qq dfa of
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
      space d ++ "),\n"
      where
         showDown amb =
           case amb of
             NotAmbiguous -> "Resolution (" ++ show amb ++ ")"
             Ambiguous -> "Resolution (" ++ show amb ++ ")"
             DunnoAmbiguous -> showTrans vis (d+2) qq

    showSet s = "[" ++ foldr (\ entry b ->
                                let alts = altSeq $ dstDFAState entry in
                                 "(" ++ show (length alts) ++
                                 -- ",q" ++ show q ++
                                 ")," ++ b) "" s  ++ "]"
    space d = spaceHelper 0
       where spaceHelper cnt = if cnt < d then " " ++ spaceHelper (cnt+1) else ""

lookaheadDepth :: (State, ExplicitDFA) -> Int
lookaheadDepth (q, dfa) =
  helper [] (mkDFAStateQuotient q)
  where
    helper :: [DFAStateQuotient] -> DFAStateQuotient -> Int
    helper vis qq =
      if elem qq vis
      then 0
      else
        case lookupExplicitDFA qq dfa of
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

detChoiceToList :: DetChoice -> [(InputHeadCondition, DFAState)]
detChoiceToList (c,e) =
  let tr = map (\ (i,t) -> (HeadInput i, t)) c in
  case e of
    Nothing -> tr
    Just t -> tr ++ [(EndInput, t)]


data AmbiguityDetection =
    Ambiguous
  | NotAmbiguous
  | DunnoAmbiguous

instance Show AmbiguityDetection where
  show Ambiguous      = "Ambiguous"
  show NotAmbiguous   = "NotAmbiguous"
  show DunnoAmbiguous = "DunnoAmbiguous"


getConflictSetsPerLoc :: DFAState -> [ [DFAStateEntry] ]
getConflictSetsPerLoc s =
  case iterDFAState s of
    Nothing -> error "broken invariant: empty DFAState"
    Just (e, es) ->
      case findAllEntryInDFAState es (sameEntryPerLoc e) of
        (lst, rs) ->
          if Set.null rs
          then [ e:lst ]
          else let lstLst = getConflictSetsPerLoc rs
               in  (e : lst) : lstLst

  where
    sameEntryPerLoc (DFAStateEntry _src1 (ClosureMove _alts1 dst1 (_,_,q1))) (DFAStateEntry _src2 (ClosureMove _alts2 dst2 (_,_,q2))) =
      q1 == q2 && cfgCtrl dst1 == cfgCtrl dst2

-- Inspired by the condition in `predictLL()` of ALL(*) paper
-- * `NotAmbiguous` when there is only one conflict set with only one possibility
-- * `Ambiguous` if there is at least one conflict set with at least 2 possibilities
-- * `DunnoAmbiguous` if all the conflict sets have 1 posibility
analyzeConflicts :: DFAState -> AmbiguityDetection
analyzeConflicts ts =
  let conflictSets = getConflictSetsPerLoc ts
  in case conflictSets of
       [] -> error "empty DFAState"
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


createDFAtable :: Aut a => a -> DFAStateQuotient -> ExplicitDFA
createDFAtable aut qInit =
  go [(qInit,0)] [] Map.empty
  where
    go :: [ (DFAStateQuotient, Int) ] -> [ (DFAStateQuotient, Int) ] -> ExplicitDFA -> ExplicitDFA
    go toVisit accToVisit dfa =
      case toVisit of
        [] -> case accToVisit of
                [] -> dfa
                _ -> go (reverse accToVisit) [] dfa
        (q, depth) : rest ->
          case lookupExplicitDFA q dfa of
            Nothing ->
              if depth > maxDepthDet
              then
                let newDfa = insertExplicitDFA q (Abort AbortOverflowK) dfa
                in go rest accToVisit newDfa
              else
                let choices = detSubset q
                    newDfa = insertExplicitDFA q choices dfa
                in
                  case choices of
                    Result (DChoice r1) ->
                      let
                        newToVisit = collectToVisit (depth+1) r1
                        newAccToVisit = revAppend newToVisit accToVisit
                      in go rest newAccToVisit newDfa
                    _ -> go rest accToVisit newDfa
            Just _ -> -- trace ("********FOUND*****" ++ "\n" ++ show q) $
              go rest accToVisit dfa

    collectToVisit :: Int -> [(InputHeadCondition, DFAState, AmbiguityDetection, DFAStateQuotient)] -> [(DFAStateQuotient, Int)]
    collectToVisit depth lst =
      foldr (\ (_, _, am, qq) b ->
               case am of
                 Ambiguous -> b
                 NotAmbiguous -> b
                 DunnoAmbiguous -> (qq, depth) : b) [] lst

    revAppend :: [a] -> [a] -> [a]
    revAppend [] ys = ys
    revAppend (x:xs) ys = revAppend xs (x:ys)

    detSubset :: DFAStateQuotient -> Result DFATransition
    detSubset s =
      let r = determinizeDFAStateQuotient aut s in
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
                           [(InputHeadCondition, DFAState, AmbiguityDetection, DFAStateQuotient)]
    mapAnalyzeConflicts dc =
      let lst = detChoiceToList dc in
      map fconvert lst
      where
        fconvert (ihc, s) =
          let am = analyzeConflicts s in
            (ihc, s, am, convertDFAStateToQuotient ihc s)



hasFullResolution :: (State, ExplicitDFA) -> Bool
hasFullResolution (start, dfa) =
  traverseWithVisited [] (mkDFAStateQuotient start)
  where
    traverseWithVisited visited q =
      if elem q visited
      then True
      else
        case lookupExplicitDFA q dfa of
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

hasNoAbort :: (State, ExplicitDFA) -> Bool
hasNoAbort (start, dfa)  =
  traverseWithVisited [] (mkDFAStateQuotient start)

  where
    traverseWithVisited visited q =
      if elem q visited
      then True
      else
        case lookupExplicitDFA q dfa of
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




type AutDet = IntMap.IntMap (ExplicitDFA, Bool)

lookupAutDet :: State -> AutDet -> Maybe (ExplicitDFA, Bool)
lookupAutDet q aut = IntMap.lookup q aut



type Prediction = ChoiceSeq

destrPrediction :: Prediction -> Maybe (ChoicePos, Prediction)
destrPrediction pdx =
  case pdx of
    Seq.Empty -> Nothing
    c Seq.:<| cs -> Just (c, cs)

predictLL :: (State, ExplicitDFA) -> Input.Input -> Maybe Prediction
predictLL (start, dfa) i =
  findPrediction (mkDFAStateQuotient start) i []
  where
    findPrediction :: DFAStateQuotient -> Input.Input -> [DFAState] -> Maybe Prediction
    findPrediction q inp acc =
      let elm = lookupExplicitDFA q dfa
      in case elm of
       Nothing -> error "broken invariant"
       Just r ->
         case r of
           Result (DChoice lst) -> iterLeftToRight lst inp acc
           _ -> error "should not reach this line"

    iterLeftToRight lst inp acc =
      case lst of
        [] -> Nothing
        (c, s, am, r1) : rest ->
          case matchInputHeadCondition c inp of
            Nothing -> iterLeftToRight rest inp acc
            Just inp1 ->
              let newAcc = s : acc in
              case am of
                NotAmbiguous -> Just $ extractPrediction newAcc
                Ambiguous -> error "broken invariant, only applied on fully resolved"
                DunnoAmbiguous -> findPrediction r1 inp1 newAcc


    extractPrediction :: [DFAState] -> Prediction
    extractPrediction lst =
      case lst of
        [] -> undefined
        s : rest ->
          let (backCfg, pdx) = extractSinglePrediction s
          in
          walkBackward backCfg rest pdx

    walkBackward :: SourceCfg -> [DFAState] -> Prediction -> Prediction
    walkBackward src lst acc =
      case lst of
        [] -> acc
        s : rest ->
          let (backCfg, pdx) = extractPredictionFromDFAState src s
          in walkBackward backCfg rest ((Seq.><) pdx acc)

    extractSinglePrediction :: DFAState -> (SourceCfg, Prediction)
    extractSinglePrediction s =
      case iterDFAState s of
        Just (DFAStateEntry c1 (ClosureMove alts _c2 (pos, _, _)), rest) ->
          if not (null rest)
          then error "ambiguous prediction"
          else (c1, addChoiceSeq pos alts )
          -- NOTE: pos is appended because this is the last transition
        _ -> error "ambiguous prediction"

    extractPredictionFromDFAState :: SourceCfg -> DFAState -> (SourceCfg, Prediction)
    extractPredictionFromDFAState src s =
      case iterDFAState s of
        Nothing -> error "could not find src from previous cfg"
        Just (DFAStateEntry c1 (ClosureMove alts c2 (pos, _, q2)), others) ->
          if q2 == cfgState src && cfgCtrl c2 == cfgCtrl src
          then (c1, addChoiceSeq pos alts)
          else extractPredictionFromDFAState src others



createDFA :: Aut a => a -> AutDet
createDFA aut =
  let collectedStates = identifyStartStates ()
      statesDet =
        map
        (\ q ->
           let
             initStateQuotient = mkDFAStateQuotient q
             t = createDFAtable aut initStateQuotient in
             (q, (t, hasFullResolution (q,t) )))
        (Set.toList collectedStates)
  in
    IntMap.fromAscList statesDet
  where
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

printDFA :: Aut a => a -> AutDet -> IO ()
printDFA aut dfas =
  let t = IntMap.toList dfas
      tAnnotated = map (\ (q, (dfa, _)) -> ((stateToString q aut, q), (q, dfa))) t
      tMapped = Map.fromList tAnnotated
      tOrdered = Map.assocs tMapped
  in if length t > 10000
     then do return ()
     else mapM_ (\ ((ann, _), (q, (dfa))) ->
                    -- if (lookaheadDepth (q, dfa) < 10)
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
  let t = IntMap.toList dfas
  in do printDFA aut dfas
        putStrLn "\nReport:"
        putStrLn $ getReport t initReport
        putStrLn $ "\nTotal nb states: " ++ show (length t)
  where
    getReport lst report =
      case lst of
        [] -> foldr (\ a b -> show a ++ "\n" ++ b) "" (Map.assocs report)
        (q, (x,_)) : xs ->
          getReport xs (incrReport report (q,x))

    result str = "Result" ++ str

    initReport :: Map.Map String Int
    initReport = Map.fromAscList []

    mapResultToKey :: (State, ExplicitDFA) -> String
    mapResultToKey (q, dfa) =
      let r = fromJust (lookupExplicitDFA (mkDFAStateQuotient q) dfa)
      in
      case r of
        Abort AbortAmbiguous -> result "-ambiguous-0"
        Abort AbortOverflowK -> abortToString r
        Result _t ->
          let k = lookaheadDepth (q, dfa)
              res
                | hasFullResolution (q, dfa) = result ("-" ++ show k)
                | hasNoAbort (q, dfa) = -- trace (show q) $
                                        result ("-ambiguous-" ++ show k)
                | otherwise = "abort-" ++ show k
          in
          --if k == 1
          --then trace (show _t) $
               res
          --else res
        _ -> abortToString r


    incrReport :: Map.Map String Int -> (State, ExplicitDFA) -> Map.Map String Int
    incrReport report r =
      let key = mapResultToKey r
      in
        Map.insertWith (\ a b -> a+b) key 1 report

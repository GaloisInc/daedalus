{-# Language GADTs #-}

module Daedalus.ParserGen.Det
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
import qualified Daedalus.Interp as Interp

import Daedalus.Type.AST
import Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), InputAction(..), ControlAction(..), isClassActOrEnd, isInputAction, isNonClassInputAct, getClassActOrEnd, evalNoFunCall, isSimpleVExpr)
import Daedalus.ParserGen.Aut (Aut(..), Choice(..))

import Daedalus.ParserGen.DetUtils

import Daedalus.ParserGen.ClassInterval (IntervalEndpoint(..), ClassInterval(..))


data Result a =
    AbortNotStatic
  | AbortAcceptingPath
  | AbortNonClassInputAction Action
  | AbortOverflowMaxDepth
  | AbortLoopWithNonClass
  | AbortNonEmptyIntersection
  | AbortClassIsDynamic
  | AbortClassNotHandledYet String

  | AbortAmbiguous
  | AbortOverflowK

  | Result a
  deriving(Show)


abortToString :: Result a -> String
abortToString r =
  case r of
    AbortNotStatic -> "AbortNotStatic"
    AbortAcceptingPath -> "AbortAcceptingPath"
    AbortNonClassInputAction _ -> "AbortNonClassInputAction"
    AbortOverflowMaxDepth -> "AbortOverflowMaxDepth"
    AbortLoopWithNonClass -> "AbortLoopWithNonClass"
    AbortNonEmptyIntersection -> "AbortNonEmptyIntersection"
    AbortClassIsDynamic -> "AbortClassIsDynamic"
    AbortClassNotHandledYet _ -> "AbortClassNotHandledYet"
    AbortAmbiguous -> "AbortAmbiguous"
    AbortOverflowK -> "AbortOverflowK"
    _ -> error "No Abort result"

maxDepthRec :: Int
maxDepthRec = 800



closureLL :: Aut a => a -> Set.Set State -> CfgDet -> Result ClosureMoveSet
closureLL aut busy cfg =
  let
    q = cfgState cfg
    ch = nextTransition aut q
  in
    case ch of
      Nothing ->
        if isAcceptingState aut q
        then AbortAcceptingPath
        else iterateThrough (initChoicePos CPop) [(CAct Pop, q)]
      --error "should not happen"
      Just ch1 ->
        let (tag, lstCh) =
              case ch1 of
                UniChoice (act, q1) -> (CUni, [(act,q1)])
                SeqChoice lst _     -> (CSeq, lst)
                ParChoice lst       -> (CPar, lst)
        in iterateThrough (initChoicePos tag) lstCh

  where
    newBusy = Set.insert (cfgState cfg) busy

    closureStep :: ChoicePos -> (Action,State) -> Result ClosureMoveSet
    closureStep pos (act, q1)
      | isClassActOrEnd act                = Result [(cfg, (pos, act, q1))]
      | isNonClassInputAct act             = -- trace (show act) $
                                             AbortNonClassInputAction act
      | length (cfgAlts cfg) > maxDepthRec = AbortOverflowMaxDepth
      | Set.member q1 busy                 = -- trace (show q1 ++ " " ++ show cfg) $
                                             AbortLoopWithNonClass
      | otherwise =
          case simulateActionCfgDet aut pos act q1 cfg of
            Nothing -> Result []
            Just lstCfg -> combineResults (map (\p -> closureLL aut newBusy p) lstCfg)


    iterateThrough :: ChoicePos -> [(Action,State)] -> Result ClosureMoveSet
    iterateThrough pos ch =
      let (_ , lstRes) = foldl (\ (pos1, acc) (act, q1) -> (nextChoicePos pos1, closureStep pos1 (act, q1) : acc)) (pos,[]) ch
      in
      combineResults (reverse lstRes)

    combineResults lst =
      case lst of
        [] -> Result []
        r1 : rest ->
          case r1 of
            AbortOverflowMaxDepth -> r1
            AbortLoopWithNonClass -> r1
            AbortNonClassInputAction _ -> r1
            AbortAcceptingPath -> r1
            Result res1 ->
              let r2 = combineResults rest in
              case r2 of
                AbortOverflowMaxDepth -> r2
                AbortLoopWithNonClass -> r2
                AbortNonClassInputAction _ -> r2
                AbortAcceptingPath -> r2
                Result resForRest -> Result (res1 ++ resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"



classToInterval :: PAST.NCExpr -> Result ClassInterval
classToInterval e =
  case texprValue e of
    TCSetAny -> Result $ ClassBtw MinusInfinity PlusInfinity
    TCSetSingle e1 ->
      if not (isSimpleVExpr e1)
      then AbortClassIsDynamic
      else
        let v = evalNoFunCall e1 [] [] in
        case v of
          Interp.VUInt 8 x -> Result $ ClassBtw (CValue (fromIntegral x)) (CValue (fromIntegral x))
          _                -> AbortClassNotHandledYet "SetSingle"
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
                  else error ("SetRange values not ordered:" ++ show (toEnum (fromIntegral x1) :: Char) ++ " " ++ show (toEnum (fromIntegral y1) :: Char))
             _ -> AbortClassNotHandledYet "SetRange"
      else AbortClassIsDynamic
    _ -> AbortClassNotHandledYet "other class case"


-- this function takes a tree representing a set of choices and
-- convert it to a Input factored deterministic transition.
determinizeClosureMoveSet :: SourceCfg -> ClosureMoveSet -> Result DetChoice
determinizeClosureMoveSet src tc =
  determinizeWithAccu tc emptyDetChoice

  where
    determinizeWithAccu :: ClosureMoveSet -> DetChoice -> Result DetChoice
    determinizeWithAccu lst acc =
      case lst of
        [] -> Result acc
        t@(_cfg, (_pos, act, _q)) : ms ->
          case getClassActOrEnd act of
            Left c ->
              case classToInterval c of
                AbortClassIsDynamic -> AbortClassIsDynamic
                AbortClassNotHandledYet msg -> AbortClassNotHandledYet msg
                Result r ->
                  let newAcc = insertDetChoice src (HeadInput r) t acc
                  in determinizeWithAccu ms newAcc
                _ -> error "Impossible abort"
            Right IEnd ->
              let newAcc = insertDetChoice src EndInput t acc
              in determinizeWithAccu ms newAcc
            _ -> error "Impossible abort"


deterministicStep :: Aut a => a -> CfgDet -> Result DetChoice
deterministicStep aut cfg =
  case closureLL aut Set.empty cfg of
    AbortOverflowMaxDepth -> AbortOverflowMaxDepth
    AbortLoopWithNonClass -> AbortLoopWithNonClass
    AbortAcceptingPath -> AbortAcceptingPath
    AbortNonClassInputAction x -> AbortNonClassInputAction x
    Result r -> determinizeClosureMoveSet cfg r
    _ -> error "impossible"


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

    showSet s = "[" ++ foldr (\ (DFAStateEntry _src cfg (_pos, _act, _q)) b ->
                                 "(" ++ show (length (cfgAlts cfg)) ++
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
maxDepthDet = 10

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
    sameEntryPerLoc (DFAStateEntry _src1 dst1 (_,_,q1)) (DFAStateEntry _src2 dst2 (_,_,q2)) =
      q1 == q2 && cfgStack dst1 == cfgStack dst2

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


createDFAtable :: Aut a => a -> Int -> DFAStateQuotient -> ExplicitDFA -> ExplicitDFA
createDFAtable aut depth q dfa =
  case lookupExplicitDFA q dfa of
    Nothing ->
      if depth > maxDepthDet
      then insertExplicitDFA q AbortOverflowK dfa
      else
        let choices = detSubsetAccu q emptyDetChoice
            newDfa = insertExplicitDFA q choices dfa
        in
          case choices of
            Result (DChoice r1) -> iterateCreateDFA (depth+1) r1 newDfa
            _ -> newDfa
    Just _ -> dfa
  where
    iterateCreateDFA :: Int -> [(InputHeadCondition, DFAState, AmbiguityDetection, DFAStateQuotient)] -> ExplicitDFA -> ExplicitDFA
    iterateCreateDFA k lst m =
      case lst of
        [] -> m
        (_i, _q, am, qq) : rest ->
          case am of
            Ambiguous -> iterateCreateDFA k rest m
            NotAmbiguous -> iterateCreateDFA k rest m
            DunnoAmbiguous ->
              let newDFA = createDFAtable aut k qq m
              in iterateCreateDFA k rest newDFA

    detSubsetAccu :: DFAStateQuotient -> DetChoice -> Result DFATransition
    detSubsetAccu s acc =
      case iterDFAStateQuotient s of
        Nothing -> Result (DChoice (mapAnalyzeConflicts (detChoiceToList acc)))
        Just (cfg, rest) ->
          let r = deterministicStep aut cfg in
          case r of
            AbortOverflowMaxDepth -> AbortOverflowMaxDepth
            AbortLoopWithNonClass -> AbortLoopWithNonClass
            AbortAcceptingPath -> AbortAcceptingPath
            AbortNonClassInputAction x -> AbortNonClassInputAction x
            AbortClassIsDynamic -> AbortClassIsDynamic
            AbortClassNotHandledYet a -> AbortClassNotHandledYet a
            Result r1 ->
              let newAcc = unionDetChoice r1 acc
              in detSubsetAccu rest newAcc
            _ -> error "cannot be this abort"

    mapAnalyzeConflicts :: [(InputHeadCondition, DFAState)] ->
                           [(InputHeadCondition, DFAState, AmbiguityDetection, DFAStateQuotient)]
    mapAnalyzeConflicts lst =
      case lst of
        [] -> []
        (ihc, s) : rest ->
          let am = analyzeConflicts s in
            (ihc, s, am, convertDFAStateToQuotient s) : mapAnalyzeConflicts rest



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
            DunnoAmbiguous -> traverseWithVisited visited qq

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
            DunnoAmbiguous -> traverseWithVisited visited qq




type AutDet = IntMap.IntMap (ExplicitDFA, Bool)

lookupAutDet :: State -> AutDet -> Maybe (ExplicitDFA, Bool)
lookupAutDet q aut = IntMap.lookup q aut



type Prediction = Seq.Seq ChoicePos

destrPrediction :: Prediction -> Maybe (ChoicePos, Prediction)
destrPrediction pdx =
  case pdx of
    Seq.Empty -> Nothing
    c Seq.:<| cs -> Just (c, cs)

predictLL :: (State, ExplicitDFA) -> Input.Input -> Maybe Prediction
predictLL (start,dfa) i =
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
        Just (DFAStateEntry c1 c2 (pos, _, _), rest) ->
          if not (null rest)
          then error "ambiguous prediction"
          else (c1, cfgAlts c2 Seq.|> pos )
          -- NOTE: pos is appended because this is the last transition
        _ -> error "ambiguous prediction"

    extractPredictionFromDFAState :: SourceCfg -> DFAState -> (SourceCfg, Prediction)
    extractPredictionFromDFAState src s =
      case iterDFAState s of
        Nothing -> error "could not find src from previous cfg"
        Just (DFAStateEntry c1 c2 (pos, _, q2), others) ->
          if q2 == cfgState src && cfgStack c2 == cfgStack src
          then (c1, cfgAlts c2 Seq.|> pos)
          else extractPredictionFromDFAState src others


createDFA :: Aut a => a -> AutDet
createDFA aut =
  let transitions = allTransitions aut
      collectedStates = collectStatesArrivedByMove transitions
      statesDet =
        map
        (\ q ->
           let
             initState = mkDFAStateQuotient q
             t = createDFAtable aut 0 initState Map.empty in
             (q, (t, hasFullResolution (q,t) )))
        (Set.toList collectedStates)
  in
    IntMap.fromAscList statesDet
  where
    collectStatesArrivedByMove t =
      foldr (\ (_,ch) b -> Set.union b (choiceToArrivedByMove ch)) (Set.singleton (initialState aut)) t

    choiceToArrivedByMove ch =
      let helper lst = foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst in
      case ch of
        UniChoice (act, q) -> collectMove (act, q)
        ParChoice lst -> helper lst
        SeqChoice lst _ -> helper lst

    collectMove (act, q) =
      if isInputAction act then Set.singleton q else Set.empty

printDFA :: AutDet -> IO ()
printDFA dfas =
  let t = IntMap.toList dfas
  in if length t > 100
     then do return ()
     else mapM_ (\ (k, (dfa,_)) -> do
                    putStrLn $ showDFATransition (k, dfa)) t

statsDFA :: AutDet -> IO ()
statsDFA dfas =
  let t = IntMap.toList dfas
  in do printDFA dfas
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
        AbortAmbiguous -> result "-ambiguous-0"
        AbortOverflowK -> abortToString r
        Result _t ->
          let k = lookaheadDepth (q, dfa)
              res
                | hasFullResolution (q, dfa) = result ("-" ++ show k)
                | hasNoAbort (q, dfa) = result ("-ambiguous-" ++ show k)
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

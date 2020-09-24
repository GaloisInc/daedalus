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


import qualified RTS.Input as Input
import qualified Daedalus.Interp as Interp

import Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action (State, Action(..), InputAction(..), isClassActOrEnd, isInputAction, isNonClassInputAct, getClassActOrEnd, evalNoFunCall, isSimpleVExpr)
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
      else error "should not happen"
    Just ch1 ->
      let (tag, lst) =
            case ch1 of
              UniChoice (act, q1) -> (CUni, [(act,q1)])
              SeqChoice l _       -> (CSeq, l)
              ParChoice l         -> (CPar, l)
      in iterateThrough (initChoicePos tag) lst

  where
    newBusy = Set.insert (cfgState cfg) busy

    closureStep :: ChoicePos -> (Action,State) -> Result ClosureMoveSet
    closureStep pos (act, q1)
      | isClassActOrEnd act                = Result [(cfg, (pos, act, q1))]
      | isNonClassInputAct act             = AbortNonClassInputAction act
      | length (cfgRuleNb cfg) > maxDepthRec = AbortOverflowMaxDepth
      | Set.member q1 newBusy              = AbortLoopWithNonClass
      | otherwise =
          case simulateActionCfgDet pos act q1 cfg of
            Nothing -> Result []
            Just p -> closureLL aut newBusy p

    iterateThrough :: ChoicePos -> [(Action,State)] -> Result ClosureMoveSet
    iterateThrough pos ch =
      case ch of
        [] -> Result []
        (act, q1) : rest ->
          let cs = closureStep pos (act, q1) in
          case cs of
            AbortOverflowMaxDepth -> cs
            AbortLoopWithNonClass -> cs
            AbortNonClassInputAction _ -> cs
            AbortAcceptingPath -> cs
            Result res1 ->
              let ri = iterateThrough (nextChoicePos pos) rest in
              case ri of
                AbortOverflowMaxDepth -> ri
                AbortLoopWithNonClass -> ri
                AbortNonClassInputAction _ -> ri
                AbortAcceptingPath -> ri
                Result resForRest -> Result (res1 ++ resForRest)
                _ -> error "abort not handled here"
            _ -> error "abort not handled here"



classToInterval :: PAST.NCExpr -> Result ClassInterval
classToInterval e =
  case e of
    PAST.NSetAny -> Result $ ClassBtw MinusInfinity PlusInfinity
    PAST.NSetSingle e1 ->
      if not (isSimpleVExpr e1)
      then AbortClassIsDynamic
      else
        let v = evalNoFunCall e1 [] [] in
        case v of
          Interp.VUInt 8 x -> Result $ ClassBtw (CValue (fromIntegral x)) (CValue (fromIntegral x))
          _                -> AbortClassNotHandledYet "SetSingle"
    PAST.NSetRange e1 e2 ->
      if isSimpleVExpr e1 && isSimpleVExpr e2
      then
        let v1 = evalNoFunCall e1 [] []
            v2 = evalNoFunCall e2 [] []
        in case (v1, v2) of
             (Interp.VUInt 8 x, Interp.VUInt 8 y) ->
               Result $ ClassBtw (CValue (fromIntegral x)) (CValue (fromIntegral y))
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
    LResolve AmbiguityDetection
  | LChoice [ (InputHeadCondition, DFAState, Result DFATransition) ]

instance Show DFATransition where
 show t =
   showD (0::Int) t
   where
     showD _d (LResolve r) = "Resolution (" ++ show r ++ ")"
     showD d (LChoice lst) =
       "DTrans [\n" ++
       concatMap (showT (d+2)) lst ++
       space d ++ "]"

     showT d (i, s, r) =
       space d ++ "( " ++ showSet s ++ "\n" ++
       space d ++ ", " ++ show i ++ "\n" ++
       space d ++ ", " ++ showDown ++ "\n" ++
       space d ++ "),\n"
       where
         showDown =
           case r of
             Result a -> showD (d+2) a
             _ -> space d ++ abortToString r ++ "\n"

     showSet s = "[" ++ foldr (\ (_src, cfg, (_pos, _act, _q)) b ->
                                 "(" ++ show (length (cfgRuleNb cfg)) ++
                                 -- ",q" ++ show q ++
                                 ")," ++ b) "" s  ++ "]"

     space d = spaceHelper 0
       where spaceHelper cnt = if cnt < d then " " ++ spaceHelper (cnt+1) else ""


lookaheadDepth :: Result DFATransition -> Int
lookaheadDepth rt =
  case rt of
    Result t ->
      case t of
        LResolve _ -> 0
        LChoice lst -> 1 + foldr (\ (_,_,r) b -> max (lookaheadDepth r) b) 0 lst
    _ -> 0




maxDepthDet :: Int
maxDepthDet = 10

detChoiceToList :: DetChoice -> [(InputHeadCondition, DFAState)]
detChoiceToList (c,e) =
  let tr = map (\ (i,t) -> (HeadInput i,t)) c in
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

-- TODO: relate this to the ResolveAmbiguity function in ALL(*) paper
-- In the current implementation it returns
-- *  NotAmbiguous when there is only one alternative
-- *  Ambiguous when all the alternatives reach the same target state
-- *  DunnoAmbiguous otherwise
analyzeConflicts :: DFAState -> AmbiguityDetection
analyzeConflicts ts =
  case ts of
    [] -> error "empty DFAState"
    [ (_, _p, (_pos, _act, _q)) ] -> NotAmbiguous
    (_, p, (_pos, _act, q)) : rest -> checkAll p q rest
  where checkAll p q rest =
          -- TODO: also use the path to refine the decision
          case rest of
            [ (_, _p1, (_pos, _act, q1)) ] ->
              if q1 == q
              then Ambiguous
              else DunnoAmbiguous
            (_, _p1, (_pos, _act, q1)) : tss ->
              if q1 == q
              then checkAll p q tss
              else DunnoAmbiguous
            _ -> error "impossible"

deterministicK :: Aut a => a -> Int -> CfgDet -> Result DFATransition
deterministicK aut depth p =
  let det1 = deterministicStep aut p
  in case det1 of
       AbortOverflowMaxDepth -> AbortOverflowMaxDepth
       AbortLoopWithNonClass -> AbortLoopWithNonClass
       AbortAcceptingPath -> AbortAcceptingPath
       AbortNonClassInputAction x -> AbortNonClassInputAction x
       AbortClassIsDynamic -> AbortClassIsDynamic
       AbortClassNotHandledYet a -> AbortClassNotHandledYet a
       Result r ->
         let tr = detChoiceToList r
         in Result $ LChoice (mapDeterminize depth tr)
       _ -> error "cannot be this Abort"

  where
    mapDeterminize :: Int -> [(InputHeadCondition, DFAState)] -> [(InputHeadCondition, DFAState, Result DFATransition)]
    mapDeterminize k lst =
      case lst of
        [] -> []
        (i, s) : rest ->
          let t = detSubset k i s
          in (i, s, t) : mapDeterminize k rest

    detSubset :: Int -> InputHeadCondition -> DFAState -> Result DFATransition
    detSubset k itv s =
      let am = analyzeConflicts s in
      case am of
        Ambiguous -> Result (LResolve am)
        NotAmbiguous  -> Result (LResolve am)
        DunnoAmbiguous ->
          if k > maxDepthDet
          then AbortOverflowK
          else detSubsetAccu k itv s emptyDetChoice

    detSubsetAccu :: Int -> InputHeadCondition -> DFAState -> DetChoice -> Result DFATransition
    detSubsetAccu k i s acc =
      case s of
        [] -> Result (LChoice (mapDeterminize (k + 1) (detChoiceToList acc)))
        (_, p1, (_pos, _act, q)) : rest ->
          let newSrcCfg = setupCfgDetFromPrev q p1
              r = deterministicStep aut newSrcCfg in
          case r of
            AbortOverflowMaxDepth -> AbortOverflowMaxDepth
            AbortLoopWithNonClass -> AbortLoopWithNonClass
            AbortAcceptingPath -> AbortAcceptingPath
            AbortNonClassInputAction x -> AbortNonClassInputAction x
            Result r1 ->
              let newAcc = unionDetChoice r1 acc
              in detSubsetAccu k i rest newAcc
            _ -> error "cannot be this abort"


hasFullResolution :: Result DFATransition -> Bool
hasFullResolution r =
  case r of
    Result r1 ->
      case r1 of
         LResolve r2 ->
           case r2 of
             NotAmbiguous -> True
             Ambiguous -> False
             _ -> error "broken invariant"
         LChoice lst -> helper lst
    _ -> False
  where
    helper lst =
      case lst of
        [] -> True
        (_,_,pr) : rest ->
          if hasFullResolution pr then helper rest else False


hasNoAbort :: Result DFATransition -> Bool
hasNoAbort r =
  case r of
    Result r1 ->
      case r1 of
         LResolve r2 ->
           case r2 of
             NotAmbiguous -> True
             Ambiguous -> True
             _ -> error "broken invariant"
         LChoice lst -> helper lst
    _ -> False
  where
    helper lst =
      case lst of
        [] -> True
        (_,_,pr) : rest ->
          if hasNoAbort pr then helper rest else False




type AutDet = IntMap.IntMap (Result DFATransition, Bool)
type AutDet2 = IntMap.IntMap (Map.Map SymbolicStack (Result DFATransition, Bool))

lookupAutDet :: State -> AutDet -> Maybe (Result DFATransition, Bool)
lookupAutDet q aut = IntMap.lookup q aut


type Prediction = Seq.Seq ChoicePos

destrPrediction :: Prediction -> Maybe (ChoicePos, Prediction)
destrPrediction pdx =
  case pdx of
    Seq.Empty -> Nothing
    c Seq.:<| cs -> Just (c, cs)

predictLL :: Result DFATransition -> Input.Input -> Maybe Prediction
predictLL res i =
  findPrediction res i []
  where
    findPrediction :: Result DFATransition -> Input.Input -> [DFAState] -> Maybe Prediction
    findPrediction r inp acc =
      case r of
        Result (LChoice lst) -> iterLeftToRight lst inp acc
        _ -> error "should not reach this line"

    iterLeftToRight lst inp acc =
      case lst of
        [] -> Nothing
        (c, s, r1) : rest ->
          case matchInputHeadCondition c inp of
            Nothing -> iterLeftToRight rest inp acc
            Just inp1 ->
              let newAcc = s:acc in
              case r1 of
                Result (LResolve NotAmbiguous) -> Just $ extractPrediction newAcc
                _ -> findPrediction r1 inp1 newAcc

    extractPrediction :: [DFAState] -> Prediction
    extractPrediction lst =
      -- trace (concatMap (\ e -> show e ++"\n") lst) $
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
      case s of
        [ (c1, c2, (pos, _, _)) ] -> (c1, cfgRuleNb c2 Seq.|> pos )
          -- NOTE: pos is appended because this is the last transition
        _ -> error "ambiguous prediction"

    extractPredictionFromDFAState :: SourceCfg -> DFAState -> (SourceCfg, Prediction)
    extractPredictionFromDFAState src s =
      case s of
        [] -> error "could not find src from previous cfg"
        (c1, c2, (pos,_,q2)) : others ->
          if q2 == cfgState src
          then (c1, cfgRuleNb c2 Seq.|> pos)
          else extractPredictionFromDFAState src others


insertAutDet2 :: State -> SymbolicStack -> (Result DFATransition, Bool) -> AutDet2 -> AutDet2
insertAutDet2 q s r aut =
  case IntMap.lookup q aut of
    Nothing -> IntMap.insert q (Map.singleton s r) aut
    Just m -> IntMap.insert q (Map.insert s r m) aut


createDFA :: Aut a => a -> AutDet
createDFA aut =
  let transitions = allTransitions aut
      collectedStates = collectStatesArrivedByMove transitions
      statesDet =
        map
        (\ q ->
           let t = deterministicK aut 0 (initCfgDet q) in
             (q, (t, hasFullResolution t)))
        (Set.toList collectedStates)
  in
    -- discover [initialState aut] IntMap.empty
    IntMap.fromAscList statesDet
  where
    discover toVisit curr =
      case toVisit of
        [] -> curr
        q: qs ->
          if IntMap.member q curr
          then discover qs curr
          else
            let t = deterministicK aut 0 (initCfgDet q) in
            let newCurr = IntMap.insert q (t, hasFullResolution t) curr in
            discover (qs ++ collectStates t) newCurr

    collectStates t =
      case t of
        Result r1 ->
          case r1 of
            LResolve NotAmbiguous -> []
            LResolve Ambiguous -> []
            LChoice lst -> concatMap (
              \(_, ps, r2) -> map (\ (_, _,(_,_,q)) -> q) ps ++ collectStates r2
              ) lst
            _ -> []
        _ -> []

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
printDFA dfa =
  let t = IntMap.toList dfa
  in if length t > 100
     then do return ()
     else mapM_ (\ (_k, (tr,_)) -> do
                    putStrLn $ show tr) t

statsDFA :: AutDet -> IO ()
statsDFA dfa =
  let t = IntMap.toList dfa
  in do printDFA dfa
        putStrLn "\nReport:"
        putStrLn $ getReport t initReport
        putStrLn $ "\nTotal nb states: " ++ show (length t)
  where
    getReport lst report =
      case lst of
        [] -> foldr (\ a b -> show a ++ "\n" ++ b) "" (Map.assocs report)
        (_, (x,_)) : xs ->
          getReport xs (incrReport report x)

    result str = "Result" ++ str

    initReport :: Map.Map String Int
    initReport = Map.fromAscList []

    mapResultToKey :: Result DFATransition -> String
    mapResultToKey r =
      case r of
        AbortAmbiguous -> result "-ambiguous-0"
        AbortOverflowK -> abortToString r
        Result _t ->
          let k = lookaheadDepth r in
          let res =
                if hasFullResolution r
                then result ("-" ++ show k)
                else if hasNoAbort r
                     then result ("-ambiguous-" ++ show k)
                     else "abort-" ++ show k
          in
          -- if k == 3
          -- then trace (show _t) $
               res
          -- else res
        _ -> abortToString r


    incrReport :: Map.Map String Int -> Result DFATransition -> Map.Map String Int
    incrReport report r =
      let key = mapResultToKey r
      in
        Map.insertWith (\ a b -> a+b) key 1 report

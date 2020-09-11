module Daedalus.ParserGen.Det where

-- import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

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
maxDepthRec = 200



closureLLOne :: Aut a => a -> ClosurePath -> Result ClosureMoveSet
closureLLOne aut da =
  let
    q = getLastState da
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
    closureStep :: ChoicePos -> (Action,State) -> Result ClosureMoveSet
    closureStep pos (act, q1)
      | isClassActOrEnd act                = Result [Move (da, (act, q1))]
      | isNonClassInputAct act             = AbortNonClassInputAction act
      | lengthClosurePath da > maxDepthRec = AbortOverflowMaxDepth
      | stateInClosurePath q1 da           = AbortLoopWithNonClass
      | otherwise =
          case addClosurePath pos act q1 da of
            Nothing -> Result [NoMove]
            Just p -> closureLLOne aut p

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
determinizeClosureMoveSet :: ClosureMoveSet -> Result DetChoice
determinizeClosureMoveSet tc =
  let tc1 = filterNoMove tc in
  determinizeTree tc1 emptyDetChoice

  where
    determinizeTree :: ClosureMoveSetPoly Action -> DetChoice -> Result DetChoice
    determinizeTree lst acc =
      case lst of
        [] -> Result acc
        (da, (e, q)) : ms ->
          case getClassActOrEnd e of
            Left c ->
              case classToInterval c of
                AbortClassIsDynamic -> AbortClassIsDynamic
                AbortClassNotHandledYet msg -> AbortClassNotHandledYet msg
                Result r ->
                  let newAcc = insertDetChoice (da, (HeadInput r, q)) acc
                  in determinizeTree ms newAcc
                _ -> error "Impossible abort"
            Right IEnd ->
              let newAcc = insertDetChoice (da, (EndInput, q)) acc
              in determinizeTree ms newAcc
            _ -> error "Impossible abort"


deterministicStep :: Aut a => a -> ClosurePath -> Result DetChoice
deterministicStep aut p =
  case closureLLOne aut p of
    AbortOverflowMaxDepth -> AbortOverflowMaxDepth
    AbortLoopWithNonClass -> AbortLoopWithNonClass
    AbortAcceptingPath -> AbortAcceptingPath
    AbortNonClassInputAction x -> AbortNonClassInputAction x
    Result r -> determinizeClosureMoveSet r
    _ -> error "impossible"




data DFATransition =
    LResolve (Result ())
  | LChoice [ (InputHeadCondition, TraceSet, Result DFATransition) ]

instance Show DFATransition where
 show t =
   showD (0::Int) t
   where
     showD _d (LResolve r) = "Resolution (" ++ show r ++ ")"
     showD d (LChoice lst) =
       "DTrans [\n" ++
       concatMap (showT (d+2)) lst ++
       space d ++ "]"

     showT d (i, tr, r) =
       space d ++ "( " ++ show i ++ "\n" ++
       space d ++ ", " ++ showTr tr ++ "\n" ++
       space d ++ ", " ++ showDown ++ "\n" ++
       space d ++ "),\n"
       where
         showDown =
           case r of
             Result a -> showD (d+2) a
             _ -> space d ++ abortToString r ++ "\n"

     showTr tr = "[" ++ foldr (\ (acts,_q) b ->
                                 "(" ++ show (lengthClosurePath acts) ++
                                 -- ",q" ++ show q ++
                                 ")," ++ b) "" tr  ++ "]"

     space d = spaceHelper 0
       where spaceHelper cnt = if cnt < d then " " ++ spaceHelper (cnt+1) else ""


depthDFATransition :: DFATransition -> Int
depthDFATransition t =
  case t of
    LResolve _ -> 1
    LChoice lst -> 1 + foldr (\ (_,_,r) b -> case r of
                                               Result r1 -> max (depthDFATransition r1) b
                                               _ -> b) 0 lst



maxDepthDet :: Int
maxDepthDet = 10

detChoiceToList :: DetChoice -> [(InputHeadCondition, TraceSet)]
detChoiceToList (c,e) =
  let tr = map (\ (i,t) -> (HeadInput i,t)) c in
  case e of
    Nothing -> tr
    Just t -> tr ++ [(EndInput, t)]

data AmbiguityDetection =
    RiskAmbiguous
  | NotAmbiguous
  | DunnoAmbiguous


-- TODO: relate this to the ResolveAmbiguoity function in LL(*) paper
ambiguousTraceSet :: TraceSet -> AmbiguityDetection
ambiguousTraceSet ts =
  case ts of
    [] -> error "empty TraceSet"
    [ _ ] -> NotAmbiguous
    (p, q) : rest -> checkAll p q rest
  where checkAll p q rest =
          -- TODO: use the path to refine the decision
          case rest of
            [ (_p1, q1) ] ->
              if q1 == q
              then RiskAmbiguous
              else DunnoAmbiguous
            (_p1, q1) : tss ->
              if q1 == q
              then checkAll p q tss
              else DunnoAmbiguous
            _ -> error "impossible"

deterministicK :: Aut a => a -> Int -> ClosurePath -> Result DFATransition
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
         in Result $ LChoice (iterateDeterminize depth tr)
       _ -> error "cannot be this Abort"

  where
    iterateDeterminize :: Int -> [(InputHeadCondition, TraceSet)] -> [(InputHeadCondition, TraceSet, Result DFATransition)]
    iterateDeterminize k lst =
      case lst of
        [] -> []
        (i, s) : rest ->
          let t = detSubset k i s
          in (i, s, t) : iterateDeterminize k rest

    detSubset :: Int -> InputHeadCondition -> TraceSet -> Result DFATransition
    detSubset k itv s =
      case ambiguousTraceSet s of
        RiskAmbiguous -> Result (LResolve AbortAmbiguous)
        NotAmbiguous -> Result (LResolve (Result ()))
        DunnoAmbiguous ->
          if k > maxDepthDet
          then AbortOverflowK
          else detSubsetHelper k itv s emptyDetChoice

    detSubsetHelper :: Int -> InputHeadCondition -> TraceSet -> DetChoice -> Result DFATransition
    detSubsetHelper k i s acc =
      case s of
        [] -> Result (LChoice (iterateDeterminize (k + 1) (detChoiceToList acc)))
        (p1, q) : rest ->
          let r = deterministicStep aut (addInputHeadConditionClosurePath p1 i q) in
          case r of
            AbortOverflowMaxDepth -> AbortOverflowMaxDepth
            AbortLoopWithNonClass -> AbortLoopWithNonClass
            AbortAcceptingPath -> AbortAcceptingPath
            AbortNonClassInputAction x -> AbortNonClassInputAction x
            Result r1 ->
              let newAcc = unionDetChoice r1 acc
              in detSubsetHelper k i rest newAcc
            _ -> error "cannot be this abort"


hasFullResolution :: Result DFATransition -> Bool
hasFullResolution r =
  case r of
    Result r1 ->
      case r1 of
         LResolve r2 ->
           case r2 of
             Result () -> True
             _ -> False
         LChoice lst -> helper lst
    _ -> False
  where
    helper lst =
      case lst of
        [] -> True
        (_,_,pr) : rest ->
          if hasFullResolution pr then helper rest else False

createDFA :: Aut a => a -> Map.Map State (Result DFATransition)
createDFA aut =
  let transitions = allTransitions aut
      collectedStates = collectStatesArrivedByMove transitions
      statesDet = map (\ q -> (q, deterministicK aut 0 (initClosurePath (initCfgDet q)))) (Set.toList collectedStates)
  in
    Map.fromAscList statesDet
  where
    collectStatesArrivedByMove t =
      foldr (\ (_,ch) b -> Set.union b (choiceToArrivedByMove ch)) (Set.singleton (initialState aut)) t

    choiceToArrivedByMove ch =
      case ch of
        UniChoice (act, q) -> collectMove (act, q)
        ParChoice lst -> foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst
        SeqChoice lst _ -> foldr (\ a b -> let sa = collectMove a in Set.union b sa) Set.empty lst

    collectMove (act, q) =
      if isInputAction act then Set.singleton q else Set.empty

printDFA :: Map.Map State (Result DFATransition) -> IO ()
printDFA dfa =
  let t = Map.toList dfa
  in mapM_ (\ (_k, tr) ->
              do putStrLn $ show tr
           ) t

statsDFA :: Map.Map State (Result DFATransition) -> IO ()
statsDFA dfa =
  let t = Map.toList dfa
  in do printDFA dfa
        putStrLn "\nReport:"
        putStrLn $ getReport t initReport (0 :: Int)
  where
    getReport lst report total =
      case lst of
        [] ->
          foldr (\ a b -> show a ++ "\n" ++ b) "" (Map.assocs report) ++
          "\nTotal nb states: " ++ show total
        (_, x) : xs ->
          getReport xs (incrReport report x) (total+1)

    abortNotStatic = "AbortNotStatic"
    abortAcceptingPath = "AbortAcceptingPath"
    abortNonClassInputAction = "AbortNonClassInputAction"
    abortOverflowMaxDepth = "AbortOverflowMaxDepth"
    abortLoopWithNonClass = "AbortLoopWithNonClass"
    abortNonEmptyIntersection = "AbortNonEmptyIntersection"
    abortClassIsDynamic = "AbortClassIsDynamic"
    abortClassNotHandledYet = "AbortClassNotHandledYet"
    abortAmbiguous = "AbortAmbiguous"
    abortOverflowK = "AbortOverflowK"
    result str = "Result" ++ str

    initReport :: Map.Map String Int
    initReport = Map.fromAscList []

    mapResultToKey :: Result DFATransition -> String
    mapResultToKey r =
      case r of
        AbortNotStatic -> abortNotStatic
        AbortAcceptingPath -> abortAcceptingPath
        AbortNonClassInputAction _ -> abortNonClassInputAction
        AbortOverflowMaxDepth -> abortOverflowMaxDepth
        AbortLoopWithNonClass -> abortLoopWithNonClass
        AbortNonEmptyIntersection -> abortNonEmptyIntersection
        AbortClassIsDynamic -> abortClassIsDynamic
        AbortClassNotHandledYet _msg -> abortClassNotHandledYet

        AbortOverflowK -> abortOverflowK

        Result _t ->
          let res = if hasFullResolution r then result "" else result "-ambiguous" in
          -- if depthDFATransition t >= 2
          -- then -- (show t) ++
               res
          -- else res

    incrReport :: Map.Map String Int -> Result DFATransition -> Map.Map String Int
    incrReport report r =
      let key = mapResultToKey r
      in
        Map.insertWith (\ a b -> a+b) key 1 report

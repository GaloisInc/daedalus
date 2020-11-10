module Daedalus.ParserGen.DetUtils
  ( SymbolicStack,
    ChoiceTag(..),
    ChoicePos,
    initChoicePos,
    nextChoicePos,
    CfgDet(..),
    initCfgDet,
    simulateActionCfgDet,
    setupCfgDetFromPrev,
    ClosureMove,
    ClosureMoveSet,
    InputHeadCondition(..),
    matchInputHeadCondition,
    SourceCfg,
    DFAStateEntry(..),
    DFAState,
    iterDFAState,
    findAllEntryInDFAState,
    DetChoice,
    emptyDetChoice,
    insertDetChoice,
    unionDetChoice,
    DFAStateQuotient,
    mkDFAStateQuotient,
    convertDFAStateToQuotient,
    iterDFAStateQuotient
  ) where


-- import Debug.Trace

import Data.Sequence as Seq
import qualified Data.Set as Set

--import qualified Data.Set as Set
import qualified RTS.Input as Input

import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..))
import Daedalus.ParserGen.Aut (Aut(..), lookupPopTrans)
import Daedalus.ParserGen.ClassInterval (ClassInterval, insertItvInOrderedList, matchClassInterval)

data SymbolicStack =
    SWildcard
  | SEmpty
  | SCons State SymbolicStack
  deriving (Eq, Ord, Show)



data ChoiceTag = CUni | CPar | CSeq | CPop
  deriving(Eq, Show, Ord)

type ChoicePos = (ChoiceTag, Int)

initChoicePos :: ChoiceTag -> ChoicePos
initChoicePos tag = (tag, 0)

nextChoicePos :: ChoicePos -> ChoicePos
nextChoicePos pos = (fst pos, snd pos +1)


data CfgDet = CfgDet
  { cfgState  :: State
  , cfgAlts :: Seq.Seq ChoicePos
  , cfgStack  :: SymbolicStack
  }
  deriving (Eq, Show)


compareCfgDet :: CfgDet -> CfgDet -> Ordering
compareCfgDet cfg1 cfg2 =
  case compare (cfgState cfg1) (cfgState cfg2) of
    LT -> LT
    GT -> GT
    EQ ->
      case compare (cfgAlts cfg1) (cfgAlts cfg2) of
        LT -> LT
        GT -> GT
        EQ -> compare (cfgStack cfg1) (cfgStack cfg2)

compareCfgDetAsSrc :: CfgDet -> CfgDet -> Ordering
compareCfgDetAsSrc cfg1 cfg2 =
  case compare (cfgState cfg1) (cfgState cfg2) of
    LT -> LT
    GT -> GT
    EQ -> compare (cfgStack cfg1) (cfgStack cfg2)

instance Ord CfgDet where
  compare c1 c2 = compareCfgDetAsSrc c1 c2


initCfgDet :: State -> CfgDet
initCfgDet q =
  CfgDet
  { cfgState = q
  , cfgAlts = Seq.empty
  , cfgStack = SWildcard
  }


symbExecAction :: Aut a => a -> SymbolicStack -> Action -> State -> Maybe [(SymbolicStack, State)]
symbExecAction aut stk act n2 =
  case act of
    CAct c ->
      case c of
        Push _ _ q -> Just $ [(SCons q stk, n2)]
        Pop ->
          case stk of
            SWildcard ->
              case (lookupPopTrans n2 $ popTransAut aut) of
                Nothing -> Nothing
                Just targets -> -- trace (show targets) $
                  Just $ map (\ q -> (SWildcard, q)) targets
            SEmpty -> Nothing
            SCons q1 rest -> Just [(rest, q1)]
        _ -> Just [(stk, n2)]
    _ -> Just [(stk, n2)]


simulateActionCfgDet :: Aut a => a -> ChoicePos -> Action -> State -> CfgDet -> Maybe [CfgDet]
simulateActionCfgDet aut pos act q cfg =
  let stk = cfgStack cfg in
  case symbExecAction aut stk act q of
    Nothing -> Nothing
    Just lst ->
      Just $
      map
      ( \ (sd, q2) ->
        CfgDet
          { cfgState = q2
          , cfgAlts = cfgAlts cfg |> pos
          , cfgStack = sd
          }
      )
      lst


setupCfgDetFromPrev :: State -> CfgDet -> CfgDet
setupCfgDetFromPrev q cfg =
  CfgDet
    { cfgState = q
    , cfgAlts = Empty
    , cfgStack = cfgStack cfg
    }

resetCfgDet :: CfgDet -> CfgDet
resetCfgDet cfg =
  CfgDet
    { cfgState = cfgState cfg
    , cfgAlts = Empty
    , cfgStack = cfgStack cfg
    }

-- The conjonction of a closure path and a move (pair action, destination state)

type ClosureMove = (CfgDet, (ChoicePos, Action, State))

type ClosureMoveSet = [ClosureMove]


data InputHeadCondition =
    HeadInput ClassInterval
  | EndInput
  deriving (Show)

matchInputHeadCondition :: InputHeadCondition -> Input.Input -> Maybe Input.Input
matchInputHeadCondition c i =
  case c of
    HeadInput a ->
      case Input.inputByte i of
        Nothing -> Nothing
        Just (x, xs) -> if matchClassInterval a x then Just xs else Nothing
    EndInput ->
      if Input.inputEmpty i then Just i else Nothing


type SourceCfg = CfgDet

data DFAStateEntry = DFAStateEntry
  { srcDFAState :: SourceCfg
  , dstDFAState :: CfgDet
  , moveDFAState :: (ChoicePos, Action, State)
  }
  deriving Show


compareSrc :: DFAStateEntry -> DFAStateEntry -> Ordering
compareSrc p1 p2 =
  -- TODO: test replacing with
  -- compareCfgDetAsSrc (srcDFAState p1) (srcDFAState p2)
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


newtype DFAStateQuotient = DFAQuo { dfaQuo :: Set.Set CfgDet }
  deriving Show

mkDFAStateQuotient :: State -> DFAStateQuotient
mkDFAStateQuotient q =
    DFAQuo (Set.singleton (CfgDet q Empty SWildcard))

equivDFAStateQuotient :: DFAStateQuotient -> DFAStateQuotient -> Bool
equivDFAStateQuotient q1 q2 = dfaQuo q1 == dfaQuo q2

instance Eq DFAStateQuotient where
  (==) q1 q2 = equivDFAStateQuotient q1 q2

instance Ord DFAStateQuotient where
  compare q1 q2 =
    compare (dfaQuo q1) (dfaQuo q2)

convertDFAStateToQuotient :: DFAState -> DFAStateQuotient
convertDFAStateToQuotient s =
  DFAQuo (helper s)
  where
    helper set =
      case iterDFAState set of
        Nothing -> Set.empty
        Just (DFAStateEntry _ cfg (_,_,q) , es) ->
          Set.insert (resetCfgDet (setupCfgDetFromPrev q cfg)) (helper es)


iterDFAStateQuotient :: DFAStateQuotient -> Maybe (CfgDet, DFAStateQuotient)
iterDFAStateQuotient s =
  let lst = Set.toAscList (dfaQuo s) in
    case lst of
      [] -> Nothing
      x:xs -> Just (x, DFAQuo (Set.fromAscList xs))

{-# Language GADTs #-}

module Daedalus.ParserGen.LL.SlkCfg
  ( SlkInput
  , compatibleInput
  , SlkCfg(..)
  , compareSlkCfg
  , measureSlkCfg
  , initSlkCfg
  , isInitSlkCfg
  , showSlkCfg
  , showSlkControlData
  , showGraphvizSlkCfg
  , _showDebugSlkControlData
  , HTable
  , emptyHTable
  , isStreamSetDynamic
  , simulateDynamicStreamSet
  , simulateActionSlkCfg
  , isManyExactDependent
  , InputHeadCondition(..)
  , showGraphvizInputHeadCondition
  , convertActionToInputHeadCondition
  , matchInputHeadCondition
  , simulateMove
  ) where

-- import Debug.Trace

import Data.ByteString (unpack)
import qualified Data.Map.Strict as Map

import Daedalus.Type.AST
import Daedalus.Value as Interp
import Daedalus.Interp as Interp
import qualified RTS.Input as Input

import qualified Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action
  ( State
  , Action(..)
  , getClassActOrEnd
  , ControlAction(..)
  , SemanticAction(..)
  , InputAction(..)
  , BranchAction(..)
  , valToInt
  , defaultValue
  , evalLiteral
  , callCExpr
  )
import qualified Daedalus.ParserGen.Aut as Aut

import Daedalus.ParserGen.LL.ClassInterval
import qualified Daedalus.ParserGen.LL.Result as R


data Slk a =
    Wildcard
  | SConcrete a
  deriving (Show, Eq, Ord)

instance Functor Slk where
  fmap _ Wildcard = Wildcard
  fmap f (SConcrete a) = SConcrete (f a)


data SlkStack a =
  SlkStack
  { keySlkStack :: Maybe Int
  , valueSlkStack :: SlkStackShape a
  }
  deriving (Show)

data SlkStackShape a =
    SWildcard
  | SEmpty
  | SCons a (SlkStack a)
  deriving (Show)


instance Ord a => Eq (SlkStackShape a) where
  (==) s1 s2 = compare s1 s2 == EQ

instance Ord a => Eq (SlkStack a) where
  (==) s1 s2 = compare s1 s2 == EQ

instance Ord a => Ord (SlkStackShape a) where
  compare s1 s2 =
    case (s1, s2) of
      (SWildcard, SWildcard) -> EQ
      (SWildcard, _) -> LT
      (_, SWildcard) -> GT

      (SEmpty, SEmpty) -> EQ
      (SEmpty, _) -> LT
      (_, SEmpty) -> GT

      (SCons x xs, SCons y ys) -> compare (x,xs) (y,ys)

instance Ord a => Ord (SlkStack a) where
  compare s1 s2 = compareSlkStack s1 s2


compareSlkStack :: Ord a => SlkStack a -> SlkStack a -> Ordering
compareSlkStack s1 s2 =
  case (keySlkStack s1, keySlkStack s2) of
    (Just k1, Just k2) -> compare k1 k2
    _ -> compare (valueSlkStack s1) (valueSlkStack s2)


data HTableSlkStack a =
  HTableSlkStack
  { nextKeySlkStack :: Int
  , knownSlkStack :: Map.Map (SlkStackShape a) (SlkStack a)
  }

emptyHTableSlkStack :: () -> HTableSlkStack a
emptyHTableSlkStack () =
  HTableSlkStack
  { nextKeySlkStack = 0
  , knownSlkStack = Map.empty
  }

mkSlkStack ::
  (Ord a, Show a) =>
  SlkStackShape a ->
  HTableSlkStack a -> (SlkStack a, HTableSlkStack a)
mkSlkStack s tab =
  -- _createSlkStackDumb s tab
  let knownTab = (knownSlkStack tab) in
  -- trace (show knownTab) $
  case Map.lookup s knownTab of
    Nothing ->
      let k = nextKeySlkStack tab
          hs =
            SlkStack
            { keySlkStack = Just k
            , valueSlkStack = s
            }
          tab1 =
            HTableSlkStack
            { nextKeySlkStack = k + 1
            , knownSlkStack = Map.insert s hs knownTab
            }
      in
        -- trace (show (k+1)) $
        (hs, tab1)
    Just x ->
      -- trace "ALREADY IN" $
      (x, tab)

_mkSlkStackDumb ::
  Ord a =>
  SlkStackShape a -> HTableSlkStack a ->
  (SlkStack a, HTableSlkStack a)
_mkSlkStackDumb s tab =
  let
    hs =
      SlkStack
      { keySlkStack = Nothing
      , valueSlkStack = s
      }
  in (hs, tab)

showSlkStack :: SlkStack a -> String
showSlkStack s =
  "[" ++ helper s ++ "]"
  where
    helper st =
      case destrSlkStack st of
        SWildcard -> "*"
        SEmpty -> ""
        SCons _a s1 -> "?, " ++ helper s1


lengthSlkStack :: SlkStack a -> Int
lengthSlkStack s =
  case destrSlkStack s of
    SCons _ rs -> 1 + lengthSlkStack rs
    _ -> 0

destrSlkStack :: SlkStack a -> SlkStackShape a
destrSlkStack s =
  valueSlkStack s

destrSlkStack2 :: SlkStack a -> (SlkStackShape a, Maybe (SlkStackShape a), Maybe (SlkStackShape a))
destrSlkStack2 s =
  let x1 = destrSlkStack s in
  case x1 of
    SWildcard -> (x1, Nothing, Nothing)
    SEmpty -> (x1, Nothing, Nothing)
    SCons _ rest ->
      let x2 = destrSlkStack rest in
      case x2 of
        SWildcard -> (x1, Just x2, Nothing)
        SEmpty -> (x1, Just x2, Nothing)
        SCons _ rest1 ->
          let x3 = destrSlkStack rest1 in
          (x1, Just x2, Just x3)



-- NOTE: this type is more convoluted than expected because of how
-- symbolic vlues for streams/input are represented
type SlkValue = Slk (Either Interp.Value SlkInput)




data SlkBetweenItv =
    SlkCExactly (Slk Int)
  | SlkCBetween (Maybe (Slk Int)) (Maybe (Slk Int))
  deriving (Show, Eq, Ord)

data SlkActivationFrame =
    SlkListArgs [SlkValue]
  | SlkActivatedFrame (Map.Map Name SlkValue)
  deriving (Show, Eq, Ord)

data SlkControlElm =
    SlkManyFrame !(SlkBetweenItv) (Slk Int)
  | SlkCallFrame Name State SlkActivationFrame SlkSemanticData
  deriving (Show, Eq, Ord)

type SlkControlData = SlkStack SlkControlElm


showSlkControlData :: Aut.Aut a => a -> SlkControlData -> [String]
showSlkControlData aut ctrl =
  case destrSlkStack ctrl of
    SWildcard -> [ "*" ]
    SCons (SlkCallFrame _name q _ _) rest ->
      showSlkControlData aut rest ++ [ Aut.stateToString q aut ]
    SCons _ rest ->
      showSlkControlData aut rest
    SEmpty ->  []


_showDebugSlkControlData :: SlkControlData -> [String]
_showDebugSlkControlData ctrl =
  case destrSlkStack ctrl of
    SWildcard -> [ "*" ]
    SCons (SlkCallFrame name _q (SlkActivatedFrame m) sem) rest ->
      _showDebugSlkControlData rest ++
      [ PAST.showName name ++ " " ++
        concat (_showDebugSlkSemanticData sem) ++ " " ++
        concat [ Map.foldrWithKey (\ k v a -> "(" ++ PAST.showName k ++ "," ++ show v ++ ")," ++ a) "" m ]
      ]
    SCons (SlkManyFrame b _) rest ->
      _showDebugSlkControlData rest ++
      [ "MANY(" ++ show b ++ ")"]
    SCons _ rest ->
      _showDebugSlkControlData rest
    SEmpty ->  []



data SlkSemElm =
    SlkSEVal  !SlkValue
  | SlkSEnvMap !(Slk (Map.Map Name (SlkValue)))
  deriving (Show, Eq, Ord)

type SlkSemanticData = SlkStack SlkSemElm

_showDebugSlkSemanticData :: SlkSemanticData -> [String]
_showDebugSlkSemanticData ctrl =
  case destrSlkStack ctrl of
    SWildcard -> [ "*" ]
    SCons (SlkSEnvMap sm) rest ->
      case sm of
        Wildcard ->
          _showDebugSlkSemanticData rest ++ ["*"]
        SConcrete m ->
          _showDebugSlkSemanticData rest ++
          [ Map.foldrWithKey (\ k v a -> "(" ++ PAST.showName k ++ "," ++ show v ++ ")," ++ a) "" m
          ]
    SCons _ rest ->
      _showDebugSlkSemanticData rest
    SEmpty ->  []


data SlkInput =
    InpBegin
  | InpTake (Slk Int) SlkInput
  | InpDrop (Slk Int) SlkInput
  | InpNext (Slk Int) SlkInput
  | InpEnd
  deriving (Show, Eq, Ord)


showSlkInput :: SlkInput -> String
showSlkInput inp =
  case inp of
    InpBegin -> "---"
    InpTake (SConcrete n) _ -> "---[.." ++ show n ++ "]"
    InpTake Wildcard _ -> "---[.." ++ "?" ++ "]"
    InpDrop (SConcrete i) _ -> "---[" ++ show i ++ "..]"
    InpDrop Wildcard _ -> "---[" ++ "?" ++ "..]"
    InpNext (SConcrete i) (InpTake (SConcrete n) _) ->
      "---[" ++ show i ++ ".." ++ show n ++ "]"
    InpNext Wildcard (InpTake Wildcard _) ->
      "---[" ++ "?" ++ ".." ++ "?" ++ "]"
    InpEnd -> "---"
    _ ->  error "case not handled"

initSlkInput :: SlkInput
initSlkInput = InpBegin

nextSlkInput :: SlkInput -> Maybe SlkInput
nextSlkInput inp =
  case inp of
    InpBegin ->
      -- when the input is unconstrained the advancement of the input is not tracked
      Just InpBegin
    InpTake (SConcrete n) _ ->
      if 1 <= n then Just $ InpNext (SConcrete 1) inp else Nothing
    InpTake Wildcard _ ->
          Just $ InpNext Wildcard inp
    InpDrop _ _ -> Just inp
    InpNext (SConcrete i) inp1@(InpTake (SConcrete n) _) ->
      if (i+1 <= n)
      then Just $ InpNext (SConcrete (i+1)) inp1
      else Nothing
    InpNext Wildcard inp1@(InpTake Wildcard _) ->
      Just $ InpNext Wildcard inp1
    InpEnd ->
      -- TODO: change this to Nothing, because when reached the end
      -- there is not possible next character, so it should definitely
      -- fail. Cant do it now bc in the middle of something else.
      error "not possible InpEnd"
    _ -> error ("not possible: " ++ show inp)

endSlkInput :: SlkInput -> Maybe SlkInput
endSlkInput inp =
  case inp of
    InpBegin -> Just InpEnd
    InpEnd -> Just InpEnd
    InpTake (SConcrete n) _ ->
      if (n == 0) then Just InpEnd else Nothing
    InpTake Wildcard _ ->
      error "NOT SURE WE SHOULD REACH HERE"
    InpDrop _ _ -> Just InpEnd
    InpNext (SConcrete i) (InpTake (SConcrete n) _) ->
      if (i == n) then Just InpEnd else Nothing
    InpNext _ (InpTake Wildcard _) ->
      Just InpEnd
      -- TODO: NOT SURE ABOUT THIS CASE BUT ENCOUTERED inJPEG SomeSOF. error "REALLY? HERE?"
    _ -> error "impossible IEND"

isSlkInputDynamic :: SlkInput -> Bool
isSlkInputDynamic inp =
  go inp
  where
    go input =
      case input of
        InpBegin -> False
        InpTake (SConcrete _) inp1 -> go inp1
        InpTake Wildcard _ -> True
        InpDrop (SConcrete _) inp1 -> go inp1
        InpDrop Wildcard _ -> True
        InpNext (SConcrete _) inp1 -> go inp1
        InpNext Wildcard _ -> True
        InpEnd -> False


data InputWindow =
    InputWindow (Int, Maybe Int) -- Maybe when is input is not bounded
  | EndWindow
  | WildWindow
  deriving(Show, Eq)

positionFromBeginning :: SlkInput -> InputWindow
positionFromBeginning inp =
  go inp
  where
    go input =
      case input of
        InpBegin -> InputWindow (0, Nothing)
        InpTake (SConcrete n) inp' ->
          let p = go inp' in
          case p of
            WildWindow -> WildWindow
            EndWindow -> EndWindow
            InputWindow (i, Nothing) -> InputWindow (i, Just (i + n))
            InputWindow (i, Just j) ->
              if j - i < n
              then EndWindow
              else InputWindow (i, Just (i + n))
        InpTake (Wildcard) _ ->
          WildWindow
        InpDrop (SConcrete n) inp' ->
          let p = go inp' in
          case p of
            WildWindow -> WildWindow
            EndWindow -> EndWindow
            InputWindow (i, Nothing) -> InputWindow (i + n, Nothing)
            InputWindow (i, Just j) ->
              if j - i < n
              then EndWindow
              else InputWindow (i + n, Just j)
        InpDrop (Wildcard) _ ->
          WildWindow
        InpNext (SConcrete n) inp' ->
          let p = go inp' in
          case p of
            WildWindow -> WildWindow
            EndWindow -> EndWindow
            InputWindow (i, Nothing) -> InputWindow (i + n, Nothing)
            InputWindow (i, Just j) ->
              if i + n > j
              then EndWindow
              else InputWindow (i + n, Just j)
        InpNext Wildcard _ ->
          WildWindow
        InpEnd -> EndWindow



compatibleInput :: SlkInput -> SlkInput -> Bool
compatibleInput inp1 inp2 =
  -- inp1 == inp2
  let
    p1 = positionFromBeginning inp1
    p2 = positionFromBeginning inp2
  in
  case (p1, p2) of
    (WildWindow, _) -> False
    (_, WildWindow) -> False
    _ -> p1 == p2
  -- NOTE: this condition is somewhat strict but beware of relaxing it
  -- because it could non-terminate


data SlkCfg = SlkCfg
  { cfgState :: !State
  , cfgCtrl  :: !SlkControlData
  , cfgSem   :: !SlkSemanticData
  , cfgInput :: !SlkInput
  }
  deriving (Show)

data HTable =
  HTable
  { tabCtrl :: HTableSlkStack SlkControlElm
  , tabSem :: HTableSlkStack SlkSemElm
  }

emptyHTable :: HTable
emptyHTable =
  HTable
  { tabCtrl = emptyHTableSlkStack ()
  , tabSem = emptyHTableSlkStack ()
  }

showSlkCfg :: SlkCfg -> String
showSlkCfg
  (SlkCfg
  { cfgState = q
  , cfgCtrl = ctrl
  , cfgSem = sem
  , cfgInput = inp
  }) =
  "SlkCfg{ " ++
  "q:" ++ show q ++ ", " ++
  "ctrl:" ++ showSlkStack ctrl ++ ", " ++
  "sem:" ++ showSlkStack sem ++ ", " ++
  "inp:" ++ showSlkInput inp ++
  " }"

showGraphvizSlkCfg :: Bool -> SlkCfg -> String
showGraphvizSlkCfg
  demoMode
  (SlkCfg
  { cfgState = q
  , cfgCtrl = ctrl
  , cfgSem = sem
  , cfgInput = inp
  }) =
  if demoMode
  then
    "q" ++ show q
  else
    "q" ++ show q ++ " " ++
    "ctrl" ++ showSlkStack ctrl ++ " " ++
    "sem" ++ showSlkStack sem ++ " " ++
    "inp" ++ showSlkInput inp



compareSlkCfg :: SlkCfg -> SlkCfg -> Ordering
compareSlkCfg cfg1 cfg2 =
  case compare (cfgState cfg1) (cfgState cfg2) of
    LT -> LT
    GT -> GT
    EQ ->
      case compare (cfgCtrl cfg1) (cfgCtrl cfg2) of
        LT -> LT
        GT -> GT
        EQ ->
          case compare (cfgSem cfg1) (cfgSem cfg2) of
            LT -> LT
            GT -> GT
            EQ ->
              compare (cfgInput cfg1) (cfgInput cfg2)


instance Eq SlkCfg where
  (==) c1 c2 =
    case compareSlkCfg c1 c2 of
      EQ -> True
      _ -> False

instance Ord SlkCfg where
  compare c1 c2 = compareSlkCfg c1 c2


-- An approximate measure of `SlkCfg`
-- TODO: add the max with the Input
measureSlkCfg :: SlkCfg -> Int
measureSlkCfg cfg =
  let
    iCtrl = lengthSlkStack (cfgCtrl cfg)
    iSem = lengthSlkStack (cfgSem cfg)
  in max iCtrl iSem

initSlkCfg :: State -> HTable -> (SlkCfg, HTable)
initSlkCfg q (HTable { tabCtrl = c, tabSem = s }) =
  let (ctrl, tab1) = mkSlkStack SWildcard c
      (sem, tab2) = mkSlkStack SWildcard s
  in
  ( SlkCfg
    { cfgState = q
    , cfgCtrl = ctrl
    , cfgSem = sem
    , cfgInput = initSlkInput
    }
  , HTable tab1 tab2
  )

isInitSlkCfg :: SlkCfg -> Bool
isInitSlkCfg (SlkCfg { cfgCtrl = ctrl, cfgSem = sem, cfgInput = inp}) =
  case (destrSlkStack ctrl, destrSlkStack sem, inp) of
    (SWildcard, SWildcard, InpBegin) -> True
    _ -> False


headSem :: SlkSemanticData -> SlkSemElm
headSem sem =
  case destrSlkStack sem of
    SWildcard -> SlkSEVal Wildcard
    SCons v _ -> v
    SEmpty -> error "Should not happen"


symbolicLookupEnvName :: Name -> SlkControlData -> SlkSemanticData -> SlkValue
symbolicLookupEnvName nname ctrl out =
  case lookupSem out ctrl of
    Nothing -> error ("unexpected, missing var from ctrl and out:" ++ show nname)
    Just v  -> v
  where
    lookupSem semOut nextctrl =
      case destrSlkStack semOut of
        SCons (SlkSEnvMap (SConcrete m)) rest ->
          case Map.lookup nname m of
            Nothing -> lookupSem rest nextctrl
            Just v -> Just v
        SCons (SlkSEnvMap Wildcard) _ -> Just Wildcard
        SCons (SlkSEVal _) rest -> lookupSem rest nextctrl
        SEmpty -> lookupCtrl nextctrl
        SWildcard -> Just Wildcard

    lookupCtrl c =
      case destrSlkStack c of
        SWildcard -> Just Wildcard
        SCons (SlkCallFrame _ _ (SlkActivatedFrame m) _) _rest ->
          case Map.lookup nname m of
            Nothing -> Nothing
            Just v -> Just v
        SCons (SlkManyFrame _ _) rest ->
          lookupCtrl rest
        SEmpty -> error "missing var"
        _ -> error ("TODO: " ++ show c)

symbolicEval :: PAST.NVExpr -> SlkControlData -> SlkSemanticData -> SlkValue
symbolicEval e ctrl sem =
  case texprValue e of
    TCVar nname ->
      symbolicLookupEnvName (tcName nname) ctrl sem
    TCLiteral lit ty ->
      let v = evalLiteral lit ty in
      case v of
        Interp.VUInt {} -> SConcrete (Left v)
        _ -> -- trace "Not UINT" $
          Wildcard
    TCBinOp Cat e1 e2 _t ->
      -- trace ("") $
      -- trace (concat $ _showDebugSlkControlData ctrl) $
      -- trace (concat $ _showDebugSlkSemanticData sem) $
      let
        ev1 = symbolicEval e1 ctrl sem
        ev2 = symbolicEval e2 ctrl sem
      in
        case (ev1, ev2) of
          (Wildcard, _) ->
            Wildcard
          (_, Wildcard) ->
            Wildcard
          (SConcrete (Left v1), SConcrete (Left v2)) ->
            SConcrete (Left (Interp.evalBinOp Cat v1 v2))
          _ ->
            -- trace "" $
            -- trace (show e1) $
            -- trace (show ev1) $
            -- trace (show e1) $
            -- trace (show ev2) $
            error ""
    _ -> -- trace ("Not Var/Literal" ++ show e) $
      Wildcard

slkValToInt :: SlkValue -> Slk Int
slkValToInt s =
  fmap (\v -> case v of
                Left e -> valToInt e
                Right _ -> error "cannot be applied to a stream"
       ) s


-- The `Many` operator is symbolically executed in the following manner.
-- * If the bound is `Exactly` and determined then the count is concretely used.
-- * If the bound is `Exactly` and abstract then the count abstract.
-- * If the bound is `Between` and the lower bound is limited but abstract and
--   the uppper bound is unlimited, then the count is abstract
-- * If the bound is `Between` and the lower bound is limited and known and
--   the upper bound is unlimited, then the count is concrete until the lower bound is passed.
-- * If the bound is `Between` and the upper bound is unlimited or limited but abstract
--   then the count is abstract
-- * If the bound is `Between` and the upper bound is limited and known
--   then the count is concretely used
setupCountFromBound :: SlkBetweenItv -> Slk Int
setupCountFromBound sbet =
  case sbet of
    SlkCExactly Wildcard -> Wildcard
    SlkCExactly (SConcrete _) -> SConcrete 0

    SlkCBetween Nothing Nothing -> Wildcard
    SlkCBetween (Just Wildcard) Nothing -> Wildcard
    SlkCBetween (Just (SConcrete _)) Nothing -> SConcrete 0

    SlkCBetween _ (Just Wildcard) -> Wildcard
    SlkCBetween _ (Just (SConcrete _)) -> SConcrete 0

incrBound :: SlkBetweenItv -> Slk Int -> Slk Int
incrBound sbet scnt =
  case (sbet, scnt) of
    (SlkCExactly (Wildcard), Wildcard) -> Wildcard
    (SlkCExactly (SConcrete _), SConcrete cnt) -> SConcrete (cnt+1)

    (SlkCBetween Nothing Nothing, Wildcard) -> Wildcard
    (SlkCBetween (Just Wildcard) Nothing, Wildcard) -> Wildcard
    (SlkCBetween (Just (SConcrete ii)) Nothing, SConcrete cnt) ->
      -- once the current cnt exceeds the lower bound and there is no
      -- upper bound then it is set to wildcard
      let cnt1 = cnt + 1 in
      if ii <= cnt1
      then Wildcard
      else SConcrete cnt1
    (SlkCBetween (Just (SConcrete _)) Nothing, Wildcard) -> Wildcard
    (SlkCBetween _ (Just Wildcard), Wildcard) -> Wildcard
    (SlkCBetween _ (Just (SConcrete _)), SConcrete cnt) -> SConcrete (cnt+1)
    _ -> error "case not handled"

getCount :: Slk Int -> Int
getCount (SConcrete cnt) = cnt
getCount _ = error "broken invariant"



data CounterLowerBound =
    Pre Int
  | Post

getCountWithLowerBound :: Slk Int -> CounterLowerBound
getCountWithLowerBound (SConcrete cnt) = Pre cnt
getCountWithLowerBound Wildcard = Post


isManyExactDependent :: SlkCfg -> Bool
isManyExactDependent cfg =
  let
    ctrl = cfgCtrl cfg
  in
  case destrSlkStack ctrl of
    SCons (SlkManyFrame (SlkCExactly sv) _) _ ->
      case sv of
        SConcrete _ -> False
        _ -> True
    _ -> False

symbExecCtrlNonPop ::
  Aut.Aut a => a -> SlkControlData -> SlkSemanticData -> ControlAction ->
  HTable -> Maybe ((SlkControlData, SlkSemanticData), HTable)
symbExecCtrlNonPop _aut ctrl out act
  tab@(HTable { tabCtrl = tabC, tabSem = tabS}) =
  case act of
    BoundSetup bound ->
      let
        sbound =
          case bound of
            Exactly v ->
              let ev = symbolicEval v ctrl out
                  i = slkValToInt ev
              in SlkCExactly i
            Between v1 v2 ->
              let ev1 = fmap (\v -> slkValToInt (symbolicEval v ctrl out)) v1
                  ev2 = fmap (\v -> slkValToInt (symbolicEval v ctrl out)) v2
              in SlkCBetween ev1 ev2
      in
      let frame = (SlkManyFrame sbound (setupCountFromBound sbound))
          (newCtrl, newTabC) = mkSlkStack (SCons frame ctrl) tabC
      in
      returnn newCtrl out newTabC tabS
    BoundCheckSuccess ->
      case destrSlkStack ctrl of
        SEmpty -> error "Unexpected ctrl stack"
        SCons (SlkManyFrame (SlkCExactly si) cnt) rest ->
          case si of
            SConcrete i ->
              if i == getCount cnt
              then rJust (rest, out)
              else if i < 0
                   then
                     -- this case is aligned with DaeDaLus interp.
                     -- `Nothing` could be another option
                     rJust (rest, out)
                   else rNothing
            Wildcard ->
              rJust (rest, out)
        SCons (SlkManyFrame (SlkCBetween i j) cnt) rest ->
          case (i, j) of
            (Nothing, Nothing) -> rJust (rest, out)
            (Nothing, Just sjj) ->
              case sjj of
                SConcrete jj ->
                  if jj >= getCount cnt
                  then rJust (rest, out)
                  else rNothing
                Wildcard -> rJust (rest, out)
            (Just sii, Nothing) ->
              case sii of
                SConcrete _ ->
                  case getCountWithLowerBound cnt of
                    Pre _ -> rNothing
                    Post -> rJust (rest, out)
                Wildcard -> rJust (rest, out)
            (Just sii, Just sjj) ->
              case (sii, sjj) of
                (SConcrete ii, SConcrete jj) ->
                  if ii <= getCount cnt && jj >= getCount cnt
                  then rJust (rest, out)
                  else rNothing
                (SConcrete _ii, Wildcard) ->
                  rJust (rest, out)
                (Wildcard, SConcrete jj) ->
                  if jj >= getCount cnt
                  then rJust (rest, out)
                  else rNothing
                (Wildcard, Wildcard) -> rJust (rest, out)
        SWildcard -> rJust (ctrl, out)
        _ -> error "Unexpected ctrl stack top element"
    BoundIsMore ->
      case destrSlkStack ctrl of
        SEmpty -> error "Unexpected ctrl stack"
        SCons (SlkManyFrame (SlkCExactly si) cnt) _ ->
          case si of
            SConcrete i ->
              if i > getCount cnt
              then rJust (ctrl, out)
              else rNothing
            Wildcard -> rJust (ctrl, out)
        SCons (SlkManyFrame (SlkCBetween _ sj) cnt) _ ->
          case sj of
            Nothing -> rJust (ctrl, out)
            Just sjj ->
              case sjj of
                SConcrete jj ->
                  if jj > getCount cnt
                  then rJust (ctrl, out)
                  else rNothing
                Wildcard -> rJust (ctrl, out)
        SWildcard -> rJust (ctrl, out)
        _ -> error "Unexpected ctrl stack top element"
    BoundIncr ->
      case destrSlkStack ctrl of
        SEmpty -> error "Unexpected ctrl stack"
        SCons (SlkManyFrame bound cnt) rest ->
          let
            frame = SlkManyFrame bound (incrBound bound cnt)
            (newCtrl, newTabC) = mkSlkStack (SCons frame rest) tabC
          in
          returnn newCtrl out newTabC tabS
        SWildcard -> rJust (ctrl, out)
        _ -> error ("Unexpected ctrl stack top element:" ++ show ctrl)
    Push rname le q ->
      let evle = map (\ e -> symbolicEval e ctrl out) le
      in
      let
        frame = SlkCallFrame rname q (SlkListArgs evle) out
        (newCtrl, newTabC) = mkSlkStack (SCons frame ctrl) tabC
        (newSem, newTabS) = mkSlkStack SEmpty tabS
      in
      returnn newCtrl newSem newTabC newTabS
    Pop -> error "should be handled elsewhere"
    ActivateFrame ln ->
      case destrSlkStack ctrl of
        SCons (SlkCallFrame rname q (SlkListArgs lvs) savedFrame) ctrls ->
          let
            zipped =
              if Prelude.length ln == Prelude.length lvs
              then Prelude.zip lvs ln
              else error "activate"

            activatedFrame = SlkActivatedFrame (
              foldr (\ (val, name) set -> (Map.insert name val set)) Map.empty zipped)
          in
          let
            frame = SlkCallFrame rname q activatedFrame savedFrame
            (newCtrl, newTabC) = mkSlkStack (SCons frame ctrls) tabC
          in
          returnn newCtrl out newTabC tabS
        SWildcard -> rJust (ctrl, out)
        _ -> error "unexpected ctrl stack, not a CallFrame ListArgs"
    DeactivateReady -> (
      case destrSlkStack ctrl of
        SCons (SlkCallFrame _rname _q (SlkActivatedFrame _) _savedFrame) _ctrls ->
          rJust (ctrl, out)
        SWildcard -> rJust (ctrl, out)
        _ -> error "unexpected ctrl"
      )
    _ -> rJust (ctrl, out)

  where
    returnn c s tc ts = Just ((c, s), HTable tc ts)
    rJust (c, s) = Just ((c, s), tab)
    rNothing = Nothing

-- This functions returns possibly many new symbolic stack because of
-- the Pop transitions that are not deterministic when the Stack is
-- Wildcard
symbExecCtrl ::
  Aut.Aut a =>
  a -> SlkControlData -> SlkSemanticData -> ControlAction -> State ->
  HTable -> Maybe ([(SlkControlData, SlkSemanticData, State)], HTable)
symbExecCtrl aut ctrl out act q2
  tab@(HTable { tabCtrl = tabC, tabSem = tabS}) =
  -- trace (show out) $
  case act of
    Pop ->
      case destrSlkStack ctrl of
        SWildcard ->
          case (Aut.lookupPopTrans q2 $ Aut.popTransAut aut) of
            Nothing -> Nothing
            Just targets -> -- trace (show targets) $
              let
                (targetsr, (tcr, tsr)) =
                  foldr
                  (\ q (acc, (tc, ts)) ->
                     let
                       (wildcard1, newTabC)  = mkSlkStack SWildcard tc
                       (wildcard2, newTabS1) = mkSlkStack SWildcard ts
                       (elm, newTabS2) =
                         mkSlkStack (SCons (headSem out) wildcard2) newTabS1
                     in
                     ( ( wildcard1
                       , elm
                       , q
                       ) : acc
                     , ( newTabC
                       , newTabS2
                       )
                     )
                  )
                  ([], (tabC, tabS))
                  targets
              in
              Just (targetsr, HTable tcr tsr)
        SEmpty -> Nothing
        SCons (SlkCallFrame _ q1 _ savedOut) rest ->
          let
            (newSem, newTabS) = mkSlkStack (SCons (headSem out) savedOut) tabS
          in
          Just ([(rest, newSem, q1)], HTable tabC newTabS)
        _ -> error "broken invariant of symbolic Pop"
    _ ->
      let r = symbExecCtrlNonPop aut ctrl out act tab in
      case r of
        Nothing -> Nothing
        Just ((newCtrl, newOut), newTab) -> Just ([(newCtrl, newOut, q2)], newTab)

symbExecSem ::
  SlkControlData -> SlkSemanticData -> SemanticAction ->
  HTable -> Maybe (SlkSemanticData, HTable)
symbExecSem ctrl out act
  tab@(HTable { tabCtrl = tabC, tabSem = tabS}) =
  -- trace (show out) $
  case act of
    EnvFresh ->
      rJust (SCons (SlkSEnvMap (SConcrete Map.empty)) out)
    EnvStore mn ->
      case destrSlkStack2 out of
        (SWildcard, _, _) ->
          rJust SWildcard
        (SCons (SlkSEVal _) _rest, Just SWildcard, _) ->
          rJust SWildcard
        (SCons (SlkSEVal v) rest, Just (SCons (SlkSEnvMap (SConcrete y)) z), _) ->
          case mn of
            Nothing -> Just (rest, tab)
            Just name -> rJust (SCons (SlkSEnvMap (SConcrete (Map.insert name v y))) z)
        _ -> error "impossible"
    EvalPure _e -> rJust (SCons (SlkSEVal Wildcard) out)
    ReturnBind e ->
      case destrSlkStack out of
        SWildcard ->
          let (wildcard1, tabS1) = mkSlkStack SWildcard tabS
              (r, tabS2) = mkSlkStack (SCons (SlkSEVal Wildcard) wildcard1) tabS1
          in Just (r, HTable tabC tabS2)
        SCons (SlkSEnvMap _) rest ->
          let se = symbolicEval e ctrl out
          in rJust (SCons (SlkSEVal se) rest)
        _ -> error "impossible"
    ReturnLast -> -- Just (head out : tail (tail out))
      case destrSlkStack2 out of
        (SWildcard, _, _) ->
          rJust SWildcard
        (SCons x _, Just (SCons _ _), Just SWildcard) ->
          let (wildcard1, tabS1) = mkSlkStack SWildcard tabS
              (r, tabS2) = mkSlkStack (SCons x wildcard1) tabS1
          in Just (r, HTable tabC tabS2)
        (SCons x _, Just (SCons _ z), Just _z) ->
          rJust (SCons x z)
        (SCons x _, Just SWildcard, Nothing) ->
          let (wildcard1, tabS1) = mkSlkStack SWildcard tabS
              (r, tabS2) = mkSlkStack (SCons x wildcard1) tabS1
          in Just (r, HTable tabC tabS2)
        _ -> error "impossible"
    DropOneOut ->
      case destrSlkStack out of
        SWildcard -> rJust SWildcard
        SCons _ os -> Just (os, tab)
        _ -> error "Should not Happen: drop on empty sem stack"
    ManyFreshList _s -> rJust (SCons (SlkSEVal Wildcard) out)
    ManyAppend _s ->
      case destrSlkStack out of
        SWildcard -> rJust SWildcard
        SCons _ y -> Just (y, tab)
        _ -> error "impossible"

    -- TODO: move these to unhandled cases
    Guard _ -> rJust (SCons (SlkSEVal Wildcard) out)
    CoerceCheck _ _ _ _ -> rJust (SCons (SlkSEVal Wildcard) out)

    -- TODO: change this to Nothing, right!?
    _ -> Just (out, tab)

  where
    rJust o =
      let (newSem, tabS1) = mkSlkStack o tabS in
      Just (newSem, HTable tabC tabS1)

slkExecClass ::
  PAST.GblFuns ->
  PAST.NCExpr ->
  SlkControlData ->
  SlkSemanticData ->
  R.Result ByteCondition
slkExecClass gbl expr ctrl out =
  classToIntervalRec expr
  where
    classToIntervalRec :: PAST.NCExpr -> R.Result ByteCondition
    classToIntervalRec e =
      case texprValue e of
        TCSetAny ->
          R.Result $ ByteCondition [ ClassBtw (CValue 0) (CValue 255) ]
        TCSetSingle e1 ->
          case symbolicEval e1 ctrl out of
            Wildcard ->
              R.Abort R.AbortSlkCfgClassIsDynamic
            SConcrete b ->
              case b of
                Left v ->
                  case v of
                    VUInt 8 c ->
                      let c1 = fromIntegral c
                      in R.Result (ByteCondition [ClassBtw (CValue c1) (CValue c1)])
                    _ -> error "Expected UInt 8 value"
                Right _ ->
                  error "expected value not SlkInput"
        TCSetRange e1 e2 ->
          let
            ev1 = symbolicEval e1 ctrl out
            ev2 = symbolicEval e2 ctrl out
          in
          case (ev1, ev2) of
            (Wildcard, _) -> R.Abort R.AbortSlkCfgClassIsDynamic
            (_, Wildcard) -> R.Abort R.AbortSlkCfgClassIsDynamic
            (SConcrete (Left v1), SConcrete (Left v2)) ->
              case (v1, v2) of
                (Interp.VUInt 8 x, Interp.VUInt 8 y) ->
                  let x1 = fromIntegral x
                      y1 = fromIntegral y
                  in
                  if x1 <= y1
                  then R.Result $ ByteCondition [ ClassBtw (CValue x1) (CValue y1) ]
                  else
                    error
                    ( "SetRange values not ordered:" ++
                      show (toEnum (fromIntegral x1) :: Char) ++ " " ++
                      show (toEnum (fromIntegral y1) :: Char))
                _ -> error "Should not happen"
            _ -> R.Abort R.AbortSlkCfgClassIsDynamic
        TCSetUnion lst ->
          let mLItv = iterUnion lst [] in
          case mLItv of
            R.Result lItv ->
              R.Result (ByteCondition (map (\ (a,()) -> a) lItv))
            R.Abort (R.AbortSlkCfgClassNotHandledYet _) -> R.coerceAbort mLItv
            R.Abort R.AbortSlkCfgClassIsDynamic -> R.coerceAbort mLItv
            _ -> error "case not possible"

          where
            iterUnion l acc =
              case l of
                [] -> R.Result acc
                e1 : rest ->
                  let mbc = classToIntervalRec e1 in
                  let
                    mNewAcc =
                      case mbc of
                        R.Result bc ->
                          R.Result $ insertByteConditionInOrderedList (bc, ()) acc (\ () () -> ())
                        R.Abort (R.AbortSlkCfgClassNotHandledYet _) -> R.coerceAbort mbc
                        R.Abort R.AbortSlkCfgClassIsDynamic -> R.coerceAbort mbc
                        _ -> error "case not possible"
                  in
                  case mNewAcc of
                    R.Result newAcc -> iterUnion rest newAcc
                    R.Abort (R.AbortSlkCfgClassNotHandledYet _) -> R.coerceAbort mNewAcc
                    R.Abort R.AbortSlkCfgClassIsDynamic -> R.coerceAbort mNewAcc
                    _ -> error "case not possible"
        TCSetOneOf lst ->
          let lItv = iterOneOf (unpack lst) [] in
            R.Result (ByteCondition (map (\ (a,()) -> a) lItv))
          where
            iterOneOf l acc =
              case l of
                [] -> acc
                b : rest ->
                  let
                    newAcc =
                      insertItvInOrderedList
                      (ClassBtw (CValue b) (CValue b), ())
                      acc
                      (\ () () -> ())
                  in
                    iterOneOf rest newAcc

        TCSetComplement e1 ->
          let
            mbc = classToIntervalRec e1
            iterCompl i lst acc =
              case lst of
                [] ->
                  if i <= 255
                  then ByteCondition (reverse (ClassBtw (CValue i) (CValue 255) : acc))
                  else ByteCondition (reverse acc)
                ClassBtw (CValue j) (CValue k) : rest ->
                  if i < j
                  then
                    let newAcc = (ClassBtw (CValue i) (CValue (j -1)) : acc) in
                    if k == 255 -- test necessary to prevent overflow/wrapping-around k+1
                    then ByteCondition (reverse newAcc)
                    else iterCompl (k+1) rest newAcc
                  else
                    if k == 255 -- test necessary to prevent overflow/wrapping-around k+1
                    then ByteCondition (reverse acc)
                    else iterCompl (k+1) rest acc
          in
          case mbc of
            R.Result bc ->
              let compl = iterCompl 0 (byteCondition bc) [] in
              R.Result compl
            R.Abort (R.AbortSlkCfgClassNotHandledYet _) -> R.coerceAbort mbc
            R.Abort R.AbortSlkCfgClassIsDynamic -> R.coerceAbort mbc
            _ -> error "Should not be another"
        TCSetDiff _ _ -> error "Not implemented yet: SetDiff LL"
        TCCall {} ->
          let calledClass = callCExpr gbl e
          in  classToIntervalRec calledClass
        TCVar x ->
          case symbolicLookupEnvName (tcName x) ctrl out of
            Wildcard ->
              R.Abort R.AbortSlkCfgClassIsDynamic
            SConcrete b ->
              case b of
                Left v ->
                  case v of
                    VUInt 8 c ->
                      let c1 = fromIntegral c
                      in R.Result (ByteCondition [ClassBtw (CValue c1) (CValue c1)])
                    _ ->
                      R.Abort R.AbortSlkCfgClassIsDynamic
                Right _ -> error "Cannot be an SlkInput"
        _ ->
          -- trace (show (texprValue e)) $
          R.Abort (R.AbortSlkCfgClassNotHandledYet "other class case")

symbExecInp :: InputAction -> SlkControlData -> SlkSemanticData -> SlkInput ->
  HTable -> R.Result (Maybe ((SlkInput, SlkSemanticData), HTable))
symbExecInp act ctrl out inp
  (HTable { tabCtrl = tabC, tabSem = tabS}) =
  -- trace (show act) $
  case act of
    GetStream ->
      rJust (inp, SCons (SlkSEVal (SConcrete (Right inp))) out)
    SetStream name ->
      let ev = symbolicEval name ctrl out in
      case ev of
        SConcrete (Right inp1) ->
          rJust (inp1, SCons (SlkSEVal (SConcrete (Left defaultValue))) out)
        Wildcard -> R.Abort R.AbortSlkCfgExecution
        _ -> error "TODO"
    StreamTake _s e1 e2 ->
      let ev1 = symbolicEval e1 ctrl out
          ev2 = symbolicEval e2 ctrl out
      in
      case ev1 of
        SConcrete (Left val) ->
          let n = Interp.valueToIntegral val in
          case ev2 of
            SConcrete (Right x) ->
              let
                semInput = SConcrete (Right $ InpTake (SConcrete (fromIntegral n)) x)
              in
              rJust (inp, SCons (SlkSEVal semInput) out)
            Wildcard -> R.Abort R.AbortSlkCfgExecution
            _ -> error "TODO"
        SConcrete (Right _) -> error "Impossible Stream value"
        Wildcard ->
          case ev2 of
            SConcrete (Right x) ->
              let
                semInput = SConcrete (Right $ InpTake Wildcard x)
              in
              rJust (inp, SCons (SlkSEVal semInput) out)
            SConcrete (Left _) -> error "Impossible stream value"
            Wildcard -> R.Abort R.AbortSlkCfgExecution
          -- R.Abort R.AbortSlkCfgExecution
    StreamDrop _s e1 e2 ->
      let ev1 = symbolicEval e1 ctrl out
          ev2 = symbolicEval e2 ctrl out
      in
      case ev1 of
        SConcrete (Left val) ->
          let n = Interp.valueToIntegral val in
          case ev2 of
            SConcrete (Right x) ->
              let
                semInput = SConcrete (Right $ InpDrop (SConcrete (fromIntegral n)) x)
              in
              rJust (inp, SCons (SlkSEVal semInput) out)
            Wildcard -> R.Abort R.AbortSlkCfgExecution
            _ -> error "TODO"
        SConcrete (Right _) -> error "Impossible Stream value"
        Wildcard ->
          case ev2 of
            SConcrete (Right x) ->
              let
                semInput = SConcrete (Right $ InpDrop Wildcard x)
              in
              rJust (inp, SCons (SlkSEVal semInput) out)
            SConcrete (Left _) -> error "Impossible stream value"
            Wildcard -> R.Abort R.AbortSlkCfgExecution

    _ -> error "TODO"

  where
    rJust (inp1, o) =
      let (newSem, tabS1) = mkSlkStack o tabS in
      R.Result (Just ((inp1, newSem), HTable tabC tabS1))

isStreamSetDynamic :: Action -> SlkCfg -> Bool
isStreamSetDynamic act cfg =
  case act of
    CAct _ -> False
    SAct _ -> False
    BAct _ -> False
    IAct iact ->
      let
        ctrl = cfgCtrl cfg
        sem = cfgSem cfg
      in
      case iact of
        GetStream -> False
        SetStream name ->
          let ev = symbolicEval name ctrl sem in
          case ev of
            SConcrete (Right x) ->
              if isSlkInputDynamic x
              then True
              else False
            SConcrete (Left _) -> error "Impossible"
            Wildcard -> True
        _ -> False
    EpsA -> False

simulateDynamicStreamSet ::
  Action -> State -> SlkCfg ->
  HTable -> (SlkCfg, HTable)
simulateDynamicStreamSet act q2 cfg (HTable tabC tabS) =
  case act of
    IAct (SetStream _name) ->
      let
        ctrl = cfgCtrl cfg
        sem = cfgSem cfg
      in
      let
        (newSem, tabS') = mkSlkStack (SCons (SlkSEVal (SConcrete (Left defaultValue))) sem) tabS
        newInput = InpBegin
      in
      ( SlkCfg
        { cfgState = q2
        , cfgCtrl = ctrl
        , cfgSem = newSem
        , cfgInput = newInput
        }
      , HTable tabC tabS'
      )
    _ -> error "broken invariant, should not be called in this context"

simulateActionSlkCfg ::
  Aut.Aut a => a -> Action -> State -> SlkCfg ->
  HTable -> R.Result (Maybe ([SlkCfg], HTable))
simulateActionSlkCfg aut act q2 cfg tab =
  -- trace "\n" $
  -- trace (show act) $
  -- trace ("CTRL: " ++ show (cfgCtrl cfg)) $
  -- trace ("SEM : " ++ show (cfgSem cfg)) $
  -- trace ("Q   : " ++ Aut.stateToString q2 aut) $
  -- trace ("INP :" ++ show (cfgInput cfg)) $
  case act of
    CAct cact ->
      let ctrl = cfgCtrl cfg
          sem = cfgSem cfg
      in
      case symbExecCtrl aut ctrl sem cact q2 tab of
        Nothing -> R.Result $ Nothing
        Just (lst, tab1) ->
          R.Result $ Just $
          ( map
            ( \ (newCtrl, newSem, q2') ->
                SlkCfg
                { cfgState = q2'
                , cfgCtrl = newCtrl
                , cfgSem = newSem
                , cfgInput = cfgInput cfg
                }
            )
            lst
          , tab1
          )
    SAct sact ->
      let ctrl = cfgCtrl cfg
          sem = cfgSem cfg in
      case symbExecSem ctrl sem sact tab of
        Nothing -> R.Result Nothing
        Just (newSem, tab1) ->
          R.Result $ Just
          ( [ SlkCfg
              { cfgState = q2
              , cfgCtrl = cfgCtrl cfg
              , cfgSem = newSem
              , cfgInput = cfgInput cfg
              }
            ]
          , tab1
          )
    IAct iact ->
      let inp = cfgInput cfg
          ctrl = cfgCtrl cfg
          sem = cfgSem cfg
      in
      case symbExecInp iact ctrl sem inp tab of
        R.Result Nothing -> R.Result Nothing
        R.Result (Just ((newInp, newSem), tab1)) ->
          R.Result $ Just
          ( [ SlkCfg
              { cfgState = q2
              , cfgCtrl = cfgCtrl cfg
              , cfgSem = newSem
              , cfgInput = newInp
              }
            ]
          , tab1
          )
        R.Abort R.AbortSlkCfgExecution -> R.Abort R.AbortSlkCfgExecution
        _ -> error "impossible"
    BAct (FailAction _) ->
      R.Result Nothing
    BAct _ ->
      R.Result $ Just ([ cfg { cfgState = q2 } ], tab)
    EpsA ->
      R.Result $ Just ([ cfg { cfgState = q2 } ], tab)


data InputHeadCondition =
    HeadInput ByteCondition
  | EndInput
  deriving (Show)


showGraphvizInputHeadCondition :: InputHeadCondition -> String
showGraphvizInputHeadCondition c =
  case c of
    HeadInput a -> showGraphvizByteCondition a
    EndInput -> "END"


convertActionToInputHeadCondition ::
  PAST.GblFuns ->
  Action ->
  SlkCfg ->
  R.Result InputHeadCondition
convertActionToInputHeadCondition gbl act cfg =
  case getClassActOrEnd act of
    Left (Left c) ->
      let res = slkExecClass gbl c (cfgCtrl cfg) (cfgSem cfg) in
      case res of
        R.Abort R.AbortSlkCfgClassIsDynamic -> R.coerceAbort res
        R.Abort (R.AbortSlkCfgClassNotHandledYet _) -> R.coerceAbort res
        R.Result r -> R.Result $ HeadInput r
        _ -> error "Impossible abort"
    Left (Right (IGetByte _)) ->
      R.Result $ HeadInput (ByteCondition [ClassBtw (CValue 0) (CValue 255)])
    Right IEnd ->
      R.Result $ EndInput
    _ -> error "Impossible case"


matchInputHeadCondition :: InputHeadCondition -> Input.Input -> Maybe Input.Input
matchInputHeadCondition c i =
  case c of
    HeadInput a ->
      case Input.inputByte i of
        Nothing -> Nothing
        Just (x, xs) ->
          if matchByteCondition a x
          then Just xs
          else Nothing
    EndInput ->
      if Input.inputEmpty i then Just i else Nothing


simulateMove ::
  InputHeadCondition -> SlkCfg -> Action -> State ->
  HTable -> Maybe (SlkCfg, HTable)
simulateMove ih cfg act q tab =
  case (ih, act) of
    (HeadInput _itv, IAct (ClssAct w _)) ->
      let mNewInput = nextSlkInput (cfgInput cfg) in
      case mNewInput of
        Nothing -> Nothing
        Just newInput ->
          case w of
            YesSem -> rJust q cfg newInput tab
            NoSem -> rJust q cfg newInput tab
    (HeadInput _itv, IAct (IGetByte _)) ->
      let mNewInput = nextSlkInput (cfgInput cfg) in
      case mNewInput of
        Nothing -> Nothing
        Just newInput -> rJust q cfg newInput tab
    (EndInput, IAct (IEnd)) ->
      let mNewInput = endSlkInput (cfgInput cfg) in
      case mNewInput of
        Nothing -> Nothing
        Just newInput -> rJust q cfg newInput tab
    _ -> error "impossible"

    where
      rJust q1 c inp1 tab1 =
        let (newSem, tabS1) = mkSlkStack (SCons (SlkSEVal Wildcard) (cfgSem c)) (tabSem tab1)
        in
        Just $
        ( SlkCfg
          { cfgState = q1
          , cfgCtrl = cfgCtrl c
          , cfgSem = newSem
          , cfgInput = inp1
          }
        , HTable (tabCtrl tab1) tabS1
        )

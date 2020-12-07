{-# Language GADTs #-}

module Daedalus.ParserGen.LL.CfgDet
  ( SymbolicStack(..),
    SlkInput,
    CfgDet(..),
    compareCfgDet,
    initCfgDet,
    resetCfgDet,
    simulateActionCfgDet,
    InputHeadCondition(..),
    matchInputHeadCondition,
    moveCfgDetFromPrev
  ) where

-- import Debug.Trace

import qualified Data.Map.Strict as Map

import Daedalus.Type.AST
import Daedalus.Interp as Interp
import qualified RTS.Input as Input

import Daedalus.ParserGen.AST
import Daedalus.ParserGen.Action
  ( State
  , Action(..)
  , ControlAction(..)
  , SemanticAction(..)
  , InputAction(..)
  , valToInt
  , defaultValue
  , evalLiteral
  )
import qualified Daedalus.ParserGen.Aut as Aut

import Daedalus.ParserGen.ClassInterval
import qualified Daedalus.ParserGen.LL.Result as R


data Slk a =
    Wildcard
  | SConcrete a
  deriving (Show, Eq, Ord)

instance Functor Slk where
  fmap _ Wildcard = Wildcard
  fmap f (SConcrete a) = SConcrete (f a)


data SymbolicStack a =
    SWildcard
  | SEmpty
  | SCons a (SymbolicStack a)
  deriving (Eq, Ord, Show)


-- instance Show a => Show (SymbolicStack a) where
--   show SWildcard = "*"
--   show SEmpty = "[]"
--   show (SCons x rest) = show x ++ " " ++ show rest

data SlkBetweenItv =
    SlkCExactly (Slk Int)
  | SlkCBetween (Maybe (Slk Int)) (Maybe (Slk Int))
  deriving (Show, Eq, Ord)

data SlkActivationFrame =
    SlkListArgs [SlkValue]
  | SlkActivatedFrame (Map.Map Name SlkValue)
  deriving (Show, Eq, Ord)

data SlkControlElm =
    SlkManyFrame !(SlkBetweenItv) !Int
  | SlkCallFrame Name State SlkActivationFrame SlkSemanticData
  deriving (Show, Eq, Ord)

type SlkControlData = SymbolicStack SlkControlElm




-- NOTE: this type is more convoluted than expected because of how
-- symbolic vlues for streams/input are represented
type SlkValue = Slk (Either Interp.Value SlkInput)

data SlkSemElm =
    SlkSEVal  !SlkValue
  | SlkSEnvMap !(Slk (Map.Map Name (SlkValue)))
  deriving (Show, Eq, Ord)

type SlkSemanticData = SymbolicStack SlkSemElm


data SlkInput =
    InpBegin
  | InpTake Int SlkInput
  | InpDrop Int SlkInput
  | InpNext Int SlkInput
  | InpEnd
  deriving (Show, Ord)

nextSlkInput :: SlkInput -> SlkInput
nextSlkInput inp =
  case inp of
    InpBegin -> InpBegin
    InpTake _ _ -> InpNext 1 inp
    InpDrop _ _ -> inp
    InpNext n inp1 -> InpNext (n+1) inp1
    InpEnd -> error "not possible"

newtype InputWindow = InputWindow { _win :: (Int, Maybe Int) }
  deriving(Eq)

positionFromBeginning :: SlkInput -> Maybe InputWindow
positionFromBeginning inp =
  case go inp of
    Nothing -> Nothing
    Just p -> Just $ InputWindow p
  where
    go input =
      case input of
        InpBegin -> Just (0, Nothing)
        InpTake n inp' ->
          let p = go inp'
          in case p of
               Nothing -> Nothing
               Just (i, Nothing) -> Just (i, Just (i + n))
               Just (i, Just j) ->
                 if j - i < n
                 then Nothing
                 else Just (i, Just (i + n))
        InpDrop n inp' ->
          let p = go inp'
          in case p of
               Nothing -> Nothing
               Just (i, Nothing) -> Just (i + n, Nothing)
               Just (i, Just j) ->
                 if j - i < n
                 then Nothing
                 else Just (i + n, Just j)
        InpNext n inp' ->
          let p = go inp'
          in case p of
               Nothing -> Nothing
               Just (i, Nothing) -> Just (i + n, Nothing)
               Just (i, Just j) ->
                 if i + n > j
                 then Nothing
                 else Just (i + n, Just j)
        InpEnd -> Nothing


instance Eq (SlkInput) where
  (==) inp1 inp2 = positionFromBeginning inp1 == positionFromBeginning inp2

data CfgDet = CfgDet
  { cfgState :: State
  , cfgCtrl :: SlkControlData
  , cfgSem   :: SlkSemanticData
  , cfgInput :: SlkInput
  }
  deriving (Show)


compareCfgDet :: CfgDet -> CfgDet -> Ordering
compareCfgDet cfg1 cfg2 =
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


instance Eq CfgDet where
  (==) c1 c2 =
    case compareCfgDet c1 c2 of
      EQ -> True
      _ -> False

instance Ord CfgDet where
  compare c1 c2 = compareCfgDet c1 c2


initCfgDet :: State -> CfgDet
initCfgDet q =
  CfgDet
  { cfgState = q
  , cfgCtrl = SWildcard
  , cfgSem = SWildcard
  , cfgInput = InpBegin
  }


headSem :: SlkSemanticData -> SlkSemElm
headSem sem =
  case sem of
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
      case semOut of
        SCons (SlkSEnvMap (SConcrete m)) rest ->
          case Map.lookup nname m of
            Nothing -> lookupSem rest nextctrl
            Just v -> Just v
        SCons (SlkSEnvMap Wildcard) _ -> Just Wildcard
        SCons (SlkSEVal _) rest -> lookupSem rest nextctrl
        SEmpty -> lookupCtrl nextctrl
        SWildcard -> Just Wildcard

    lookupCtrl SWildcard = Just Wildcard
    lookupCtrl (SCons (SlkCallFrame _ _ (SlkActivatedFrame m) _) _rest) =
      case Map.lookup nname m of
        Nothing -> Nothing
        Just v -> Just v
    lookupCtrl SEmpty = error "missing var"
    lookupCtrl _ = error "TODO"

symbolicEval :: NVExpr -> SlkControlData -> SlkSemanticData -> SlkValue
symbolicEval e ctrl sem =
  case texprValue e of
    TCVar nname ->
      symbolicLookupEnvName (tcName nname) ctrl sem
    TCLiteral lit ty ->
      let v = evalLiteral lit ty in
      case v of
        Interp.VInteger _ -> SConcrete (Left v)
        _ -> Wildcard
    _ -> Wildcard

slkValToInt :: SlkValue -> Slk Int
slkValToInt s =
  fmap (\v -> case v of
                Left e -> valToInt e
                Right _ -> error "cannot be applied to a stream"
       ) s

unlimitedBound :: SlkBetweenItv -> Bool
unlimitedBound b =
  case b of
    SlkCExactly _ -> False
    SlkCBetween _ Nothing -> True
    _ -> False

-- This functions returns possibly many new symbolic stack because of
-- the Pop transitions that are not deterministic when the Stack is
-- Wildcard
symbExecCtrlNonPop :: Aut.Aut a => a -> SlkControlData -> SlkSemanticData -> ControlAction -> Maybe (SlkControlData, SlkSemanticData)
symbExecCtrlNonPop _aut ctrl out act =
  case act of
    BoundSetup bound ->
      case bound of
        Exactly v ->
          let ev = symbolicEval v ctrl out
              i = slkValToInt ev
          in Just (SCons (SlkManyFrame (SlkCExactly i) 0) ctrl, out)
        Between v1 v2 ->
          let ev1 = fmap (\v -> slkValToInt (symbolicEval v ctrl out)) v1
              ev2 = fmap (\v -> slkValToInt (symbolicEval v ctrl out)) v2
          in Just (SCons (SlkManyFrame (SlkCBetween ev1 ev2) 0) ctrl, out)
    BoundCheckSuccess ->
      case ctrl of
        SEmpty -> error "Unexpected ctrl stack"
        SCons (SlkManyFrame (SlkCExactly si) cnt) rest ->
          case si of
            SConcrete i ->
              if i == cnt
              then Just (rest, out)
              else if i < 0
                   then Just (rest, out) -- case aligned with DaeDaLus interp. `Nothing` could be another option
                   else Nothing
            Wildcard ->
              Just (rest, out)
        SCons (SlkManyFrame (SlkCBetween i j) cnt) rest ->
          case (i, j) of
            (Nothing, Nothing) -> Just (rest, out)
            (Nothing, Just sjj) ->
              case sjj of
                SConcrete jj ->
                  if jj >= cnt then Just (rest, out) else Nothing
                Wildcard -> Just (rest, out)
            (Just sii, Nothing) ->
              case sii of
                SConcrete ii -> if ii <= cnt then Just (rest, out) else Nothing
                Wildcard -> Just (rest, out)
            (Just sii, Just sjj) ->
              case (sii, sjj) of
                (SConcrete ii, SConcrete jj) ->
                  if ii <= cnt && jj >= cnt then Just (rest, out) else Nothing
                (SConcrete ii, Wildcard) ->
                  if ii <= cnt then Just (rest, out) else Nothing
                (Wildcard, SConcrete jj) ->
                  if jj >= cnt then Just (rest, out) else Nothing
                (Wildcard, Wildcard) -> Just (rest, out)
        SWildcard -> Just (ctrl, out)
        _ -> error "Unexpected ctrl stack top element"
    BoundIsMore ->
      case ctrl of
        SEmpty -> error "Unexpected ctrl stack"
        SCons (SlkManyFrame (SlkCExactly si) cnt) _ ->
          case si of
            SConcrete i ->
              if i > cnt then Just (ctrl, out) else Nothing
            Wildcard -> Just (ctrl, out)
        SCons (SlkManyFrame (SlkCBetween _ sj) cnt) _ ->
          case sj of
            Nothing -> Just (ctrl, out)
            Just sjj ->
              case sjj of
                SConcrete jj ->
                  if jj > cnt then Just (ctrl, out) else Nothing
                Wildcard -> Just (ctrl, out)
        SWildcard -> Just (ctrl, out)
        _ -> error "Unexpected ctrl stack top element"
    BoundIncr ->
      case ctrl of
        SEmpty -> error "Unexpected ctrl stack"
        SCons (SlkManyFrame bound cnt) rest ->
          if unlimitedBound bound
          then Just (SCons (SlkManyFrame bound cnt) rest, out) -- only increment the cnt when the bound is limited
          else Just (SCons (SlkManyFrame bound (cnt+1)) rest, out)
        SWildcard -> Just (ctrl, out)
        _ -> error ("Unexpected ctrl stack top element:" ++ show ctrl)
    Push rname le q ->
      let evle = map (\ e -> symbolicEval e ctrl out) le
      in
      Just $ (SCons (SlkCallFrame rname q (SlkListArgs evle) out) ctrl, SEmpty)
    Pop -> error "should be handled elsewhere"
    ActivateFrame ln ->
      case ctrl of
        SCons (SlkCallFrame rname q (SlkListArgs lvs) savedFrame) ctrls ->
          let zipped =
                if Prelude.length ln == Prelude.length lvs
                then Prelude.zip lvs ln
                else error "activate"

              activatedFrame = SlkActivatedFrame (
                foldr (\ (val, name) set -> (Map.insert name val set)) Map.empty zipped)
              in
              Just (SCons (SlkCallFrame rname q activatedFrame savedFrame) ctrls, out)
        SWildcard -> Just (SWildcard, out)
        _ -> error "unexpected ctrl stack, not a CallFrame ListArgs"
    DeactivateReady -> (
      case ctrl of
        SCons (SlkCallFrame _rname _q (SlkActivatedFrame _) _savedFrame) _ctrls -> Just (ctrl, out)
        SWildcard -> Just (SWildcard, out)
        _ -> error "unexpected out"
      )
    _ -> Just (ctrl, out)

symbExecCtrl :: Aut.Aut a => a -> SlkControlData -> SlkSemanticData -> ControlAction -> State -> Maybe [(SlkControlData, SlkSemanticData, State)]
symbExecCtrl aut ctrl out act q2 =
  case act of
    Pop ->
      case ctrl of
        SWildcard ->
          case (Aut.lookupPopTrans q2 $ Aut.popTransAut aut) of
            Nothing -> Nothing
            Just targets -> -- trace (show targets) $
              Just $ map (\ q -> (SWildcard, SCons (headSem out) SWildcard, q)) targets
        SEmpty -> Nothing
        SCons (SlkCallFrame _ q1 _ savedOut) rest -> Just [(rest, SCons (headSem out) savedOut, q1)]
        _ -> error "broken invariant of symbolic Pop"
    _ ->
      let r = symbExecCtrlNonPop aut ctrl out act in
      case r of
        Nothing -> Nothing
        Just (newCtrl, newOut) -> Just [(newCtrl, newOut, q2)]

symbExecSem :: SlkControlData -> SlkSemanticData -> SemanticAction -> Maybe SlkSemanticData
symbExecSem ctrl out act =
  -- trace (show out) $
  case act of
    EnvFresh -> Just (SCons (SlkSEnvMap (SConcrete Map.empty)) out)
    EnvStore mn ->
      case out of
        SWildcard -> Just SWildcard
        SCons (SlkSEVal _) SWildcard ->
          Just SWildcard
        SCons (SlkSEVal v) rest@(SCons (SlkSEnvMap (SConcrete y)) z) ->
          case mn of
            Nothing -> Just rest
            Just name -> Just (SCons (SlkSEnvMap (SConcrete (Map.insert name v y))) z)
        _ -> error "impossible"
    EvalPure _e -> Just (SCons (SlkSEVal Wildcard) out)
    ReturnBind e ->
      case out of
        SWildcard -> Just (SCons (SlkSEVal Wildcard) SWildcard)
        SCons (SlkSEnvMap _) rest ->
          let se = symbolicEval e ctrl out
          in Just (SCons (SlkSEVal se) rest)
        _ -> error "impossible"
    ReturnLast -> -- Just (head out : tail (tail out))
      case out of
        SWildcard -> Just SWildcard
        SCons x (SCons _ SWildcard) -> Just (SCons x SWildcard)
        SCons x (SCons _ z) -> Just (SCons x z)
        SCons x SWildcard -> Just (SCons x SWildcard)
        _ -> error "impossible"
    DropOneOut ->
      case out of
        SWildcard -> Just SWildcard
        SCons _ os -> Just os
        _ -> error "Should not Happen: drop on empty sem stack"
    ManyFreshList _s -> Just (SCons (SlkSEVal Wildcard) out)
    ManyAppend _s ->
      -- trace (show out) $
      -- trace (show ctrl) $
      case out of
        SWildcard -> Just SWildcard
        SCons _ y -> Just y
        _ -> error "impossible"

    -- TODO: move these to unhandled cases
    SelUnion _ _ _ -> Just (SCons (SlkSEVal Wildcard) out)
    Guard _ -> Just (SCons (SlkSEVal Wildcard) out)
    _ -> Just out

symbExecInp :: InputAction -> SlkControlData -> SlkSemanticData -> SlkInput ->
  R.Result (Maybe (SlkInput, SlkSemanticData))
symbExecInp act ctrl out inp =
  case act of
    GetStream -> R.Result $ Just (inp, SCons (SlkSEVal (SConcrete (Right inp))) out)
    SetStream name ->
      let ev = symbolicEval name ctrl out in
      case ev of
        SConcrete (Right x) -> R.Result $ Just (x, SCons (SlkSEVal (SConcrete (Left defaultValue))) out)
        Wildcard -> R.Abort R.AbortSymbolicExec
        _ -> -- trace (show ev) $
             error "TODO"
    StreamLen _s e1 e2 ->
      let ev1 = symbolicEval e1 ctrl out
          ev2 = symbolicEval e2 ctrl out
      in
        case ev1 of
          SConcrete (Left (Interp.VInteger n)) ->
            case ev2 of
              SConcrete (Right x) -> R.Result $ Just (inp, SCons (SlkSEVal (SConcrete (Right $ InpTake (fromIntegral n) x))) out)
              Wildcard -> R.Abort R.AbortSymbolicExec
              _ -> error "TODO"
          _ -> -- trace "nont integer const" $
            R.Abort R.AbortSymbolicExec
    StreamOff _s e1 e2 ->
      let ev1 = symbolicEval e1 ctrl out
          ev2 = symbolicEval e2 ctrl out
      in
        case ev1 of
          SConcrete (Left (Interp.VInteger n)) ->
            case ev2 of
              SConcrete (Right x) ->
                R.Result $ Just (inp, SCons (SlkSEVal (SConcrete (Right $ InpDrop (fromIntegral n) x))) out)
              Wildcard -> R.Abort R.AbortSymbolicExec
              _ -> error "TODO"
          _ -> R.Abort R.AbortSymbolicExec

    _ -> error "TODO"


simulateActionCfgDet :: Aut.Aut a => a -> Action -> State -> CfgDet -> R.Result (Maybe [CfgDet])
simulateActionCfgDet aut act q2 cfg =
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
      case symbExecCtrl aut ctrl sem cact q2 of
        Nothing -> R.Result $ Nothing
        Just lst ->
          R.Result $ Just $
          map
          ( \ (newCtrl, newSem, q2') ->
              CfgDet
              { cfgState = q2'
              , cfgCtrl = newCtrl
              , cfgSem = newSem
              , cfgInput = cfgInput cfg
              }
          )
          lst
    SAct sact ->
      let ctrl = cfgCtrl cfg
          sem = cfgSem cfg in
      case symbExecSem ctrl sem sact of
        Nothing -> R.Result Nothing
        Just newSem ->
          R.Result $ Just
          [ CfgDet
            { cfgState = q2
            , cfgCtrl = cfgCtrl cfg
            , cfgSem = newSem
            , cfgInput = cfgInput cfg
            }
          ]
    IAct iact ->
      let inp = cfgInput cfg
          ctrl = cfgCtrl cfg
          sem = cfgSem cfg
      in
      case symbExecInp iact ctrl sem inp of
        R.Result Nothing -> R.Result Nothing
        R.Result (Just (newInp, newSem)) ->
          R.Result $ Just
          [ CfgDet
            { cfgState = q2
            , cfgCtrl = cfgCtrl cfg
            , cfgSem = newSem
            , cfgInput = newInp
            }
          ]
        R.Abort R.AbortSymbolicExec -> R.Abort R.AbortSymbolicExec
        _ -> error "impossible"
    _ ->
      R.Result $ Just
      [ CfgDet
        { cfgState = q2
        , cfgCtrl = cfgCtrl cfg
        , cfgSem = cfgSem cfg
        , cfgInput = cfgInput cfg
        }
      ]

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


moveCfgDetFromPrev :: InputHeadCondition -> Action -> State -> CfgDet -> CfgDet
moveCfgDetFromPrev ih act q cfg =
  case (ih, act) of
    (HeadInput _itv, IAct (ClssAct w _)) ->
      case w of
        YesSem ->
          CfgDet
          { cfgState = q
          , cfgCtrl = cfgCtrl cfg
          , cfgSem = SCons (SlkSEVal Wildcard) (cfgSem cfg)
          , cfgInput = nextSlkInput (cfgInput cfg)
          }
        NoSem ->
          CfgDet
          { cfgState = q
          , cfgCtrl = cfgCtrl cfg
          , cfgSem = SCons (SlkSEVal Wildcard) (cfgSem cfg)
          , cfgInput = nextSlkInput (cfgInput cfg)
          }
    (EndInput, IAct (IEnd)) ->
      CfgDet
      { cfgState = q
      , cfgCtrl = cfgCtrl cfg
      , cfgSem = SCons (SlkSEVal Wildcard) (cfgSem cfg)
      , cfgInput = nextSlkInput (cfgInput cfg)
      }
    _ -> error "impossible"

resetCfgDet :: CfgDet -> CfgDet
resetCfgDet cfg =
  CfgDet
    { cfgState = cfgState cfg
    , cfgCtrl = cfgCtrl cfg
    , cfgSem = cfgSem cfg
    , cfgInput = cfgInput cfg
    }

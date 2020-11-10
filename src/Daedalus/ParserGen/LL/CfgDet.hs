module Daedalus.ParserGen.LL.CfgDet
  ( SymbolicStack(..),
    ChoiceTag(..),
    ChoicePos,
    initChoicePos,
    nextChoicePos,
    CfgDet(..),
    compareCfgDet,
    initCfgDet,
    resetCfgDet,
    simulateActionCfgDet,
    InputHeadCondition(..),
    matchInputHeadCondition,
    setupCfgDetFromPrev
  ) where

import Debug.Trace

import Data.Sequence as Seq
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
  )
import Daedalus.ParserGen.Aut (Aut(..), stateToString, lookupPopTrans)

import Daedalus.ParserGen.ClassInterval


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


data Slk a =
    Wildcard
  | SConcrete a
  deriving (Show)


data SlkSemElm =
    SlkSEVal  !(Slk Interp.Value)
  | SlkSEnvMap !(Slk (Map.Map Name (Slk Interp.Value)))
  deriving (Show)

data SlkSem =
    SemWildcard
  | SemEmpty
  | SemCons SlkSemElm SlkSem
  deriving (Show)

instance Eq SlkSem where
  (==) SemWildcard SemWildcard = True
  -- (==) SemEmpty SemEmpty = True
  -- (==) SemWildcard SemWildcard = True
  -- (==) (SemCons _ _) (SemCons _ _) = False
  (==) _ _ = True


data CfgDet = CfgDet
  { cfgState :: State
  , cfgAlts  :: Seq.Seq ChoicePos
  , cfgStack :: SymbolicStack
  , cfgSem   :: SlkSem
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
  , cfgSem = SemWildcard
  }


-- This functions returns possibly many new symbolic stack because of
-- the Pop transitions that are not deterministic when the Stack is
-- Wildcard
symbExecStack :: Aut a => a -> SymbolicStack -> ControlAction -> State -> Maybe [(SymbolicStack, State)]
symbExecStack aut stk act n2 =
  case act of
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

symbolicEval :: NVExpr -> SlkSem -> (Slk Interp.Value)
symbolicEval _e _sem =
  Wildcard

symbExecSem :: SlkSem -> SemanticAction -> Maybe SlkSem
symbExecSem sem act =
  -- trace (show sem) $
  case act of
    EnvFresh -> Just (SemCons (SlkSEnvMap (SConcrete Map.empty)) sem)
    EnvStore mn ->
      case sem of
        SemWildcard -> Just SemWildcard
        SemCons (SlkSEVal _) SemWildcard ->
          Just SemWildcard
        SemCons (SlkSEVal v) rest@(SemCons (SlkSEnvMap (SConcrete y)) z) ->
          case mn of
            Nothing -> Just rest
            Just name -> Just (SemCons (SlkSEnvMap (SConcrete (Map.insert name v y))) z)
        _ -> error "impossible"
    EvalPure _e -> Just (SemCons (SlkSEVal Wildcard) sem)
    ReturnBind e ->
      case sem of
        SemWildcard -> Just (SemCons (SlkSEVal Wildcard) SemWildcard)
        SemCons (SlkSEnvMap _) rest ->
          let se = symbolicEval e sem
          in Just (SemCons (SlkSEVal se) rest)
        _ -> error "impossible"
    ReturnLast -> -- Just (head out : tail (tail out))
      case sem of
        SemWildcard -> Just SemWildcard
        SemCons x (SemCons _ SemWildcard) -> Just (SemCons x SemWildcard)
        SemCons x (SemCons _ z@(SemCons _ _)) -> Just (SemCons x z)
        SemCons x SemWildcard -> Just (SemCons x SemWildcard)
        _ -> error "impossible"
    DropOneOut ->
      case sem of
        SemWildcard -> Just SemWildcard
        SemCons _ os -> Just os
        _ -> error "Should not Happen: drop on empty sem stack"
    ManyFreshList _s -> Just (SemCons (SlkSEVal Wildcard) sem)
    ManyAppend _s ->
      case sem of
        SemWildcard -> Just SemWildcard
        SemCons _ y -> Just y
        _ -> error "impossible"

    -- TODO: move these to unhandled cases
    SelUnion _ _ _ -> Just (SemCons (SlkSEVal Wildcard) sem)
    Guard _ -> Just (SemCons (SlkSEVal Wildcard) sem)
    _ -> Just sem

symbExecInp :: SlkSem -> InputAction -> Maybe SlkSem
symbExecInp _sem act =
  case act of
    _ -> error "TODO"


simulateActionCfgDet :: Aut a => a -> ChoicePos -> Action -> State -> CfgDet -> Maybe [CfgDet]
simulateActionCfgDet aut pos act q2 cfg =
  -- trace (show act) $
  -- trace (show (cfgSem cfg)) $
  trace (stateToString q2 aut) $
  case act of
    CAct cact ->
      let stk = cfgStack cfg in
      case symbExecStack aut stk cact q2 of
        Nothing -> Nothing
        Just lst ->
          Just $
          map
          ( \ (sd, q2') ->
              CfgDet
              { cfgState = q2'
              , cfgAlts = cfgAlts cfg |> pos
              , cfgStack = sd
              , cfgSem = cfgSem cfg
              }
          )
          lst
    SAct sact ->
      let sem = cfgSem cfg in
      case symbExecSem sem sact of
        Nothing -> Nothing
        Just newSem ->
          Just
          [ CfgDet
            { cfgState = q2
            , cfgAlts = cfgAlts cfg |> pos
            , cfgStack = cfgStack cfg
            , cfgSem = newSem
            }
          ]
    _ -> Just
         [ CfgDet
           { cfgState = q2
           , cfgAlts = cfgAlts cfg |> pos
           , cfgStack = cfgStack cfg
           , cfgSem = cfgSem cfg
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


setupCfgDetFromPrev :: InputHeadCondition -> Action -> State -> CfgDet -> CfgDet
setupCfgDetFromPrev ih act q cfg =
  case (ih, act) of
    (HeadInput _itv, IAct (ClssAct w _)) ->
      case w of
        YesSem ->
          CfgDet
          { cfgState = q
          , cfgAlts = Empty
          , cfgStack = cfgStack cfg
          , cfgSem = SemCons (SlkSEVal Wildcard) (cfgSem cfg)
          }
        NoSem ->
          CfgDet
          { cfgState = q
          , cfgAlts = Empty
          , cfgStack = cfgStack cfg
          , cfgSem = SemCons (SlkSEVal Wildcard) (cfgSem cfg)
          }
    (EndInput, IAct (IEnd)) ->
      CfgDet
      { cfgState = q
      , cfgAlts = Empty
      , cfgStack = cfgStack cfg
      , cfgSem = SemCons (SlkSEVal Wildcard) (cfgSem cfg)
      }
    _ -> error "impossible"

resetCfgDet :: CfgDet -> CfgDet
resetCfgDet cfg =
  CfgDet
    { cfgState = cfgState cfg
    , cfgAlts = Empty
    , cfgStack = cfgStack cfg
    , cfgSem = cfgSem cfg
    }

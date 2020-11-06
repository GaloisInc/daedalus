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
    setupCfgDetFromPrev
  ) where

-- import Debug.Trace

import Data.Sequence as Seq

import Daedalus.ParserGen.Action (State, Action(..), ControlAction(..))
import Daedalus.ParserGen.Aut (Aut(..), lookupPopTrans)


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

{-# Language BlockArguments #-}
{-# Language FlexibleInstances #-}
module Daedalus.Core.Effect(mayFail, annotateMayFail, mayFailModule) where

import Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.Core

-- | Given known failing functions, determine which of
-- the definitions in the module also may fail
mayFailModule :: Module -> Set FName -> Set FName
mayFailModule m failing =
  let new = foldr mayFailFun failing (mGFuns m)
  in if Set.size new == Set.size failing
        then failing
        else mayFailModule m new

-- | Given a set of known failing functions, annotate parsers with info
-- on if they may fail.
annotateMayFail :: Set FName -> Module -> Module
annotateMayFail failingExt m = m { mGFuns = map annotFun (mGFuns m) }
  where
  failing = mayFailModule m failingExt
  annotFun fu = fu { fMayFail = if fName fu `Set.member` failing
                                   then MayFail else MayNotFail }

-- | Given a set of functions that are known to fail,
-- determine if we should extend thet set with this function
mayFailFun :: Fun Grammar -> Set FName -> Set FName
mayFailFun fu failing
  | name `Set.member` failing = failing
  | otherwise = if fails then Set.insert name failing else failing
  where
  name = fName fu
  fails = case fMayFail fu of
            MayFail -> True
            MayNotFail -> False
            Unknown ->
              case fDef fu of
                External -> True
                Def e    -> mayFail failing e

-- | Given a set of functions that are known to fail,
-- determine if grammar may fail.
mayFail :: Set FName -> Grammar -> Bool
mayFail failing gram =
  case gram of
    Pure {}           -> False
    GetStream {}      -> False
    SetStream {}      -> False
    Match _ m         -> mayFailMatch failing m
    Fail {}           -> True
    Call f _          -> f `Set.member` failing

    Annot a g         -> case a of
                           NoFail -> False
                           _ -> mayFail failing g

    Do_ g1 g2         -> mayFail failing g1 || mayFail failing g2
    Do _ g1 g2        -> mayFail failing g1 || mayFail failing g2
    Let _ _ g         -> mayFail failing g
    OrBiased g1 g2    -> mayFail failing g1 && mayFail failing g2
    OrUnbiased g1 g2  -> mayFail failing g1 && mayFail failing g2

    GCase (Case _ alts) ->
      any (mayFail failing . snd) alts || partial (map fst alts)
      where
      partial xs =
        case xs of
          [] -> True

          PAny     : _                    -> False
          PNothing : PJust : _            -> False
          PJust    : PNothing : _         -> False
          PBool x  : PBool y : _ | x /= y  -> False

          _ : ys                          -> partial ys

    -- Overly pessimistic as Many (0..) can't fail
    Loop lc -> mayFailLoop failing lc

mayFailMatch :: Set FName -> Match -> Bool
mayFailMatch _failing ma =
  case ma of
    MatchByte {} -> True
    MatchBytes e ->
      case e of
        ApN (ArrayL _) [] -> False
        _ -> True
    MatchEnd -> True

mayFailLoop :: Set FName -> LoopClass Grammar -> Bool
mayFailLoop failing lp =
  case lp of
    ManyLoop _ _ lower upper _ ->
      case (lower,upper) of
        (Ap0 (IntL 0 _), Nothing) -> False
        _                         -> True
    RepeatLoop {} -> False
    MorphismLoop e -> mayFail failing (morphismBody e)









